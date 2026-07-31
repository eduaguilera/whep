#!/usr/bin/env python3
"""Build monthly-mean downwelling radiation from ERA5 for the LPJmL forcing.

Why this exists: LPJmL 6.x removed the `cloudiness` radiation option and the
`cloud` input, so the CRU cloud fraction that drives 5.9.7 is unusable. 6.x must
be driven by downwelling shortwave and longwave (`radiation_lwdown` mode, which
reads `swdown` and `lwdown`). The ISIMIP3a obsclim products that supply those
(`rsds`, `rlds` from GSWP3-W5E5) end in **2019** -- W5E5 itself ends there, and
the v1.1/v1.2/v1.3 releases are corrections, not extensions. That makes
radiation the forcing that blocks a 1901-2023 run under 6.x, exactly as wind was
under 5.9.7 (see fetch_era5_wind.py and issue #340).

Source: ARCO-ERA5, the analysis-ready Zarr mirror of ERA5 on Google Cloud
(gs://gcp-public-data-arco-era5). Public, no credentials needed.

Method notes:

* **Units.** ERA5 `surface_solar_radiation_downwards` and
  `surface_thermal_radiation_downwards` are *accumulations in J m-2* over the
  hour ending at the timestamp, not instantaneous fluxes. Dividing by 3600 s
  gives the mean flux in W m-2, which is what LPJmL expects. Getting this wrong
  is a silent factor-3600 error, so the conversion is applied once, here, and
  the output units attribute records it.

* **Diurnal sampling.** The wind fetcher samples four fixed UTC hours, which is
  fine for a field with a weak diurnal cycle. Shortwave radiation is zero at
  night and peaks at local noon, and a fixed UTC hour corresponds to a
  *different local time at every longitude* -- so fixed-hour sampling would
  alias the diurnal cycle systematically by longitude. This script therefore
  reads **all 24 hours** of each sampled day.

* **Day subsampling.** Reading every hour of every day is ~4x the volume of the
  whole wind fetch per variable. Chunks are one global field per timestep, so
  cost scales with timesteps read and cannot be reduced by subsetting in space.
  Instead a regular subset of days per month is read in full (default: every
  5th day). Monthly means of radiation are smooth, and a full-diurnal sample on
  6 days is a far better estimator than a 4-hour sample on 30 days. The residual
  sampling error is quantified against ISIMIP over the 2017-2019 overlap by
  compare_era5_isimip_radiation.R -- do not skip that check.

* Output stays on ERA5's native 0.25 degree grid. Regridding to LPJmL's 0.5
  degree grid is done afterwards with `cdo remapcon`, which handles the
  half-cell offset between the grids (ERA5 centres start at 90 N, the LPJmL grid
  at 89.75 N) correctly.

Writes <out_path>: monthly mean flux, (time, latitude, longitude), W m-2.
"""

from __future__ import annotations

import argparse
import calendar
import concurrent.futures as cf
import datetime as dt
import sys

import fsspec
import numpy as np
import zarr
from netCDF4 import Dataset

ARCO_ERA5 = (
    "https://storage.googleapis.com/gcp-public-data-arco-era5/"
    "ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
)
VARS = {
    "rsds": (
        "surface_solar_radiation_downwards",
        "Surface Downwelling Shortwave Radiation",
    ),
    "rlds": (
        "surface_thermal_radiation_downwards",
        "Surface Downwelling Longwave Radiation",
    ),
}
EPOCH = dt.datetime(1900, 1, 1)
# ERA5 accumulations are over the preceding hour.
ACCUM_SECONDS = 3600.0


def hour_index(when: dt.datetime) -> int:
    """Index into the ARCO-ERA5 hourly time axis (hours since 1900-01-01)."""
    return int((when - EPOCH).total_seconds() // 3600)


def month_indices(year: int, month: int, day_step: int) -> list[int]:
    """Hourly indices for all 24 hours of every `day_step`-th day of a month."""
    ndays = calendar.monthrange(year, month)[1]
    out: list[int] = []
    for day in range(1, ndays + 1, day_step):
        base = dt.datetime(year, month, day)
        out.extend(hour_index(base + dt.timedelta(hours=h)) for h in range(24))
    return out


def monthly_mean_flux(arr, year: int, month: int, day_step: int, workers: int):
    """Mean flux in W m-2 for one month, from hourly J m-2 accumulations."""
    indices = month_indices(year, month, day_step)
    total = None

    def field(i):
        return arr[i]

    with cf.ThreadPoolExecutor(max_workers=workers) as pool:
        for chunk in pool.map(field, indices):
            arr64 = chunk.astype(np.float64)
            total = arr64 if total is None else total + arr64
    # J m-2 per hour -> W m-2, then average over sampled hours.
    return (total / len(indices) / ACCUM_SECONDS).astype(np.float32)


def write_netcdf(out_path, months, data, lat, lon, var_name, long_name,
                 day_step):
    with Dataset(out_path, "w", format="NETCDF4") as nc:
        nc.createDimension("time", len(months))
        nc.createDimension("latitude", len(lat))
        nc.createDimension("longitude", len(lon))

        v_lat = nc.createVariable("latitude", "f8", ("latitude",))
        v_lat.units = "degrees_north"
        v_lat.standard_name = "latitude"
        v_lat[:] = lat

        v_lon = nc.createVariable("longitude", "f8", ("longitude",))
        v_lon.units = "degrees_east"
        v_lon.standard_name = "longitude"
        v_lon[:] = lon

        v_time = nc.createVariable("time", "f8", ("time",))
        v_time.units = "days since 1900-01-01"
        v_time.calendar = "standard"
        v_time.long_name = "time"
        # Mid-month stamps, so `cdo selyear`/`monmean` bucket them correctly.
        v_time[:] = [(dt.datetime(y, m, 15) - EPOCH).days for y, m in months]

        v = nc.createVariable(
            var_name, "f4", ("time", "latitude", "longitude"), zlib=True,
            complevel=1,
        )
        v.units = "W m-2"
        v.long_name = long_name
        v[:] = data

        nc.Conventions = "CF-1.4"
        nc.source = ARCO_ERA5
        nc.method = (
            "hourly J m-2 accumulations divided by "
            f"{ACCUM_SECONDS:.0f} s to give W m-2; all 24 hours read on every "
            f"{day_step}th day of each month, averaged per calendar month"
        )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--var", choices=sorted(VARS), required=True)
    parser.add_argument("--start-year", type=int, default=2017)
    parser.add_argument("--end-year", type=int, default=2023)
    parser.add_argument(
        "--day-step", type=int, default=5,
        help="read all 24 hours of every Nth day (default 5)",
    )
    parser.add_argument("--workers", type=int, default=16)
    parser.add_argument("--out", required=True)
    args = parser.parse_args()

    era5_name, long_name = VARS[args.var]
    root = zarr.open(fsspec.get_mapper(ARCO_ERA5), mode="r")
    arr = root[era5_name]
    lat = np.asarray(root["latitude"][:], dtype=np.float64)
    lon = np.asarray(root["longitude"][:], dtype=np.float64)

    months = [
        (y, m)
        for y in range(args.start_year, args.end_year + 1)
        for m in range(1, 13)
    ]
    data = np.empty((len(months), len(lat), len(lon)), dtype=np.float32)
    for k, (y, m) in enumerate(months):
        data[k] = monthly_mean_flux(arr, y, m, args.day_step, args.workers)
        print(
            f"{args.var} {y}-{m:02d}  mean {float(data[k].mean()):7.2f} W m-2",
            flush=True,
        )

    write_netcdf(
        args.out, months, data, lat, lon, args.var, long_name, args.day_step
    )
    print(f"wrote {args.out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
