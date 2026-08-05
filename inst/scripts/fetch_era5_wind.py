#!/usr/bin/env python3
"""Build monthly-mean 10 m wind speed from ERA5 for the LPJmL wind forcing.

Why this exists: the ISIMIP obsclim products that supply LPJmL's wind input
end in 2019 (W5E5 v2.0 ends there), and wind is one of the few LPJmL inputs
with no fallback -- readclimate() aborts with ERROR130/ERROR131 on a year
outside the file's range rather than holding the last year constant. That
made wind the single forcing blocking the 1901-2023 run asked for in
issue #340.

Source: ARCO-ERA5, the analysis-ready Zarr mirror of ERA5 on Google Cloud
(gs://gcp-public-data-arco-era5). Public, no credentials needed.

Method notes:

* Wind speed is computed per timestep as sqrt(u^2 + v^2) and *then* averaged.
  Averaging the components first and taking the magnitude afterwards
  underestimates mean speed wherever direction varies within the month.
* The series is sampled 6-hourly rather than hourly. Chunks are one whole
  global field per hour, so cost is proportional to timesteps read; 6-hourly
  samples the diurnal cycle evenly at a quarter of the volume. Any residual
  sampling bias is absorbed by the bias correction applied downstream, which
  is calibrated from these same samples over the 2017-2019 overlap.
* Output stays on ERA5's native 0.25 degree grid. Regridding to LPJmL's 0.5
  degree grid is done afterwards with `cdo remapcon`, which handles the
  half-cell offset between the two grids (ERA5 centres start at 90 N, the
  LPJmL grid at 89.75 N) correctly.

Writes <out_path>: monthly mean wind speed, (time, latitude, longitude).
"""

from __future__ import annotations

import argparse
import calendar
import datetime as dt
import sys
from concurrent.futures import ThreadPoolExecutor

import fsspec
import numpy as np
import zarr
from netCDF4 import Dataset

ARCO_ERA5 = (
    "https://storage.googleapis.com/gcp-public-data-arco-era5/"
    "ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
)
U_VAR = "10m_u_component_of_wind"
V_VAR = "10m_v_component_of_wind"
EPOCH = dt.datetime(1900, 1, 1)
SAMPLE_HOURS = (0, 6, 12, 18)


def hour_index(when: dt.datetime) -> int:
    """Index into the ARCO-ERA5 hourly time axis (hours since 1900-01-01)."""
    return int((when - EPOCH).total_seconds() // 3600)


def month_indices(year: int, month: int) -> list[int]:
    """Every sampled timestep index within one calendar month."""
    ndays = calendar.monthrange(year, month)[1]
    return [
        hour_index(dt.datetime(year, month, day, hour))
        for day in range(1, ndays + 1)
        for hour in SAMPLE_HOURS
    ]


def monthly_mean_speed(u_arr, v_arr, year: int, month: int, workers: int):
    """Mean of per-timestep wind speed over one month."""
    indices = month_indices(year, month)

    def speed(i):
        u = u_arr[i]
        v = v_arr[i]
        return np.hypot(u, v)

    with ThreadPoolExecutor(workers) as pool:
        total = None
        for field in pool.map(speed, indices):
            total = field.astype(np.float64) if total is None else total + field
    return (total / len(indices)).astype(np.float32)


def write_netcdf(out_path, months, data, lat, lon):
    """Write the monthly stack with a CF time axis LPJmL/CDO can read."""
    with Dataset(out_path, "w", format="NETCDF4_CLASSIC") as nc:
        nc.createDimension("time", len(months))
        nc.createDimension("latitude", lat.size)
        nc.createDimension("longitude", lon.size)

        v_lat = nc.createVariable("latitude", "f8", ("latitude",))
        v_lat.units = "degrees_north"
        v_lat.long_name = "latitude"
        v_lat[:] = lat

        v_lon = nc.createVariable("longitude", "f8", ("longitude",))
        v_lon.units = "degrees_east"
        v_lon.long_name = "longitude"
        v_lon[:] = lon

        v_time = nc.createVariable("time", "f8", ("time",))
        v_time.units = "days since 1900-01-01"
        v_time.calendar = "standard"
        v_time.long_name = "time"
        # Mid-month stamps, so `cdo selyear`/`monmean` bucket them correctly.
        v_time[:] = [
            (dt.datetime(y, m, 15) - EPOCH).days for y, m in months
        ]

        v_wind = nc.createVariable(
            "wind", "f4", ("time", "latitude", "longitude"), zlib=True,
            complevel=1,
        )
        v_wind.units = "m/s"
        v_wind.long_name = "Near-Surface Wind Speed"
        v_wind[:] = data

        nc.Conventions = "CF-1.4"
        nc.source = ARCO_ERA5
        nc.method = (
            "sqrt(u10^2+v10^2) per timestep, sampled at "
            + ",".join(f"{h:02d}Z" for h in SAMPLE_HOURS)
            + ", averaged per calendar month"
        )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--start-year", type=int, default=2017)
    parser.add_argument("--end-year", type=int, default=2023)
    parser.add_argument("--workers", type=int, default=16)
    parser.add_argument("--out", required=True)
    args = parser.parse_args()

    store = fsspec.get_mapper(ARCO_ERA5)
    root = zarr.open(store, mode="r")
    u_arr, v_arr = root[U_VAR], root[V_VAR]
    lat = np.asarray(root["latitude"][:], dtype=np.float64)
    lon = np.asarray(root["longitude"][:], dtype=np.float64)

    months = [
        (y, m)
        for y in range(args.start_year, args.end_year + 1)
        for m in range(1, 13)
    ]
    stack = np.empty((len(months), lat.size, lon.size), dtype=np.float32)

    for n, (year, month) in enumerate(months):
        field = monthly_mean_speed(u_arr, v_arr, year, month, args.workers)
        if not np.isfinite(field).all():
            print(f"ERROR: {year}-{month:02d} has non-finite cells", flush=True)
            return 1
        stack[n] = field
        print(
            f"{year}-{month:02d}  mean={field.mean():.3f} m/s  "
            f"({n + 1}/{len(months)})",
            flush=True,
        )

    write_netcdf(args.out, months, stack, lat, lon)
    print(f"wrote {args.out}: {len(months)} months", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
