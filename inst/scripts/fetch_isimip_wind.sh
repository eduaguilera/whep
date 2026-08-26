#!/usr/bin/env bash
# Rebuilds the monthly near-surface wind base LPJmL is forced with, 1901-2019,
# from the ISIMIP files it was originally assembled from. Wind is a *hard*
# LPJmL input -- readclimate() aborts with ERROR130/ERROR131 on a year outside
# the file range rather than holding the last year constant -- so this series
# caps the run length (issue #340).
#
# The base has two provenance segments, and they are NOT the same release:
#
#   1901-2016  ISIMIP2a  HistObs/GSWP3-W5E5  wind_gswp3-w5e5_<chunk>.nc4
#             "GSWP3 ... bias-adjusted to W5E5 with ISIMIP3BASD v2.4.1
#              for ISIMIP2a", published 18 June 2020. Variable `wind`.
#   2017-2019  ISIMIP3a  obsclim/GSWP3-W5E5  ..._sfcwind_..._2011_2019.nc
#             "GSWP3 v1.09 bias-adjusted to W5E5 v2.0 with ISIMIP3BASD
#              v2.5.0 (1901-1978) combined with W5E5 v2.0 (1979-2019)".
#              Variable `sfcwind`, renamed to `wind` here.
#
# ISIMIP2a stops at 2016 and ISIMIP3a stops at 2019 (W5E5 ends there; the
# v1.1-v1.3 releases are corrections, not extensions), which is why the series
# is spliced at all, and why 2020-2023 comes from ERA5 instead -- see
# fetch_era5_wind.py and extend_lpjml_wind() in prepare_spatialize_all.R.
#
# Both ISIMIP variables are scalar wind *speed*, not u/v components, so a
# monthly mean of the published field is the mean speed. There is no vector
# averaging to get wrong here.
#
# VERIFIED AGAINST THE PIN (2026-08-26, issue #371). Every one of the 1428
# monthly steps in `lpjml-wind-isimip-1901-2019` was reproduced from the URLs
# above and compared value for value:
#   1901-2016  all 12 ISIMIP2a chunks, 1392 steps: max |diff| = 0, exactly
#   2017-2019  the ISIMIP3a chunk,       36 steps: max |diff| = 0, exactly
# Not "within tolerance" -- bit-identical, because both sides are `cdo monmean`
# over the same float32 daily field. `validation/lpjml_wind_provenance.R`
# re-runs the ISIMIP2a half of that audit on demand.
#
# The 2016 overlap between the two rounds was also measured, since the splice
# assumes they agree: over 720x360x12, max |2a - 3a| = 7.87e-05 m/s and
# mean |2a - 3a| = 1.27e-06 m/s, against a global mean of 6.32887 m/s. That is
# float32 round-off (ISIMIP3a stores the field bit-groomed), not a release
# difference, so the joint needs no correction -- as issue #371 assumed.
#
# DO NOT "SIMPLIFY" THIS BY TAKING 1901-2016 FROM ISIMIP3a TOO.
#
# ISIMIP3a publishes sfcwind for 1901-2019, so fetching one dataset instead of
# two looks like an obvious cleanup. It is not: it would change the forcing.
# The 2016 agreement above holds only because 2016 is in W5E5's OBSERVATIONAL
# era (ISIMIP3a is "combined with W5E5 v2.0" from 1979). In the bias-adjusted
# era the two releases really do differ -- measured over 1901-1910:
#
#   unweighted global mean   2a 6.347690   3a 6.347587   m/s
#   mean |2a - 3a|           0.0386 m/s
#   p99  |2a - 3a|           0.620  m/s
#   max  |2a - 3a|           2.552  m/s
#
# Note the shape: the GLOBAL MEAN is unchanged to 1e-4, so a global sanity
# check sees nothing, while 1% of cell-months move by more than 0.62 m/s
# (~10% of the mean speed). ISIMIP3BASD v2.5.0 redistributes wind spatially
# while conserving the global mean, and LPJmL is forced per cell -- so gridded
# output would move on a change no summary statistic would catch.
#
# Switching the base to ISIMIP3a is therefore a methodological decision for
# the maintainer, not a tidy-up, and it must not happen as a side effect of
# editing this script. The upside would be consistency with rsds/rlds, which
# are pure ISIMIP3a.
#
# Segment 1 alone streams ~31 GB of daily global fields. Each chunk is reduced
# to monthly means and the daily file deleted immediately, so peak disk stays
# ~3 GB instead of ~34 GB.
set -uo pipefail

# Usage: fetch_isimip_wind.sh [dest_dir]
#
# Provenance script, not part of a normal run. The product it builds is pinned
# (lpjml-wind-isimip-1901-2019) and fetched by download_climate.R, because
# rebuilding it streams ~34 GB of daily files. Run this only to regenerate the
# pinned artefact from source -- for a different year range, a corrected
# ISIMIP release, or to audit the pin.
#
# Requires: curl, cdo.

BASE_2A="https://files.isimip.org/ISIMIP2a/InputData/climate_co2/climate/HistObs/GSWP3-W5E5"
BASE_3A="https://files.isimip.org/ISIMIP3a/InputData/climate/atmosphere/obsclim/global/daily/historical/GSWP3-W5E5"
DEST="${1:-${WHEP_L_FILES_DIR:-LPJmL_inputs}/wind}"
WORK="$DEST/.work"
LOG="$DEST/fetch.log"
mkdir -p "$WORK"

# ISIMIP2a ships wind in ten-year chunks; the last one is short (2011-2016).
CHUNKS_2A="1901_1910 1911_1920 1921_1930 1931_1940 1941_1950 1951_1960 1961_1970 1971_1980 1981_1990 1991_2000 2001_2010 2011_2016"

# The ISIMIP3a chunk holding 2017-2019. It also covers 2011-2016, which
# overlaps segment 1; only 2017-2019 is kept, so the ISIMIP2a release stays
# authoritative wherever it reaches.
CHUNK_3A="2011_2019"
TAIL_YEARS="2017/2019"

say() { printf '%s %s\n' "$(date +%H:%M:%S)" "$*" >>"$LOG"; }
: >"$LOG"
say "start; dest=$DEST"

# ---- Segment 1: ISIMIP2a daily `wind` -> monthly means ----------------
for chunk in $CHUNKS_2A; do
  monthly="$WORK/wind_monthly_${chunk}.nc"
  if [ -s "$monthly" ]; then
    say "2a $chunk: monthly already present, skip"
    continue
  fi
  daily="$WORK/wind_daily_${chunk}.nc4"
  url="$BASE_2A/wind_gswp3-w5e5_${chunk}.nc4"
  say "2a $chunk: downloading"
  if ! curl -sfL --max-time 7200 -o "$daily" "$url"; then
    say "2a $chunk: DOWNLOAD FAILED, skipping"
    rm -f "$daily"
    continue
  fi
  sz=$(stat -c%s "$daily")
  say "2a $chunk: got $((sz / 1000000)) MB, reducing to monthly"
  if cdo -s monmean "$daily" "$monthly" 2>>"$LOG"; then
    say "2a $chunk: monthly ok ($(cdo -s ntime "$monthly" 2>/dev/null) steps)"
  else
    say "2a $chunk: CDO monmean FAILED"
    rm -f "$monthly"
  fi
  # Free the ~2.7 GB immediately; the monthly product is what LPJmL reads.
  rm -f "$daily"
done

# ---- Segment 2: ISIMIP3a daily `sfcwind` -> monthly means, 2017-2019 --
tail_monthly="$WORK/wind_monthly_2017_2019.nc"
if [ -s "$tail_monthly" ]; then
  say "3a tail: monthly already present, skip"
else
  daily="$WORK/sfcwind_daily_${CHUNK_3A}.nc"
  url="$BASE_3A/gswp3-w5e5_obsclim_sfcwind_global_daily_${CHUNK_3A}.nc"
  say "3a $CHUNK_3A: downloading"
  if ! curl -sfL --max-time 7200 -o "$daily" "$url"; then
    say "3a $CHUNK_3A: DOWNLOAD FAILED"
    rm -f "$daily"
  else
    sz=$(stat -c%s "$daily")
    say "3a $CHUNK_3A: got $((sz / 1000000)) MB, reducing to monthly"
    # -chname last: LPJmL is configured with input.wind.var = "wind", so the
    # ISIMIP3a name must match segment 1 before the two can be merged.
    if cdo -s -chname,sfcwind,wind -selyear,"$TAIL_YEARS" -monmean \
      "$daily" "$tail_monthly" 2>>"$LOG"; then
      say "3a tail: monthly ok ($(cdo -s ntime "$tail_monthly") steps)"
    else
      say "3a tail: CDO reduction FAILED"
      rm -f "$tail_monthly"
    fi
    rm -f "$daily"
  fi
fi

# ---- Merge into one 1901-2019 monthly series -------------------------
target="$DEST/wind_gswp3-w5e5_1901_2019_monthly.nc"
pieces=$(ls "$WORK"/wind_monthly_*.nc 2>/dev/null | sort)
n=$(printf '%s\n' "$pieces" | grep -c . || true)
if [ "$n" -eq 0 ]; then
  say "no monthly pieces, nothing to merge"
else
  say "merging $n pieces"
  if cdo -s -O mergetime $pieces "$target" 2>>"$LOG"; then
    steps=$(cdo -s ntime "$target")
    say "merged -> $(basename "$target") ($steps steps)"
    # 1901-2019 inclusive is 119 years x 12 months. Anything else means a
    # chunk failed to download and the series has a hole -- which LPJmL will
    # not report as a hole, it will simply force the wrong years.
    if [ "$steps" != "1428" ]; then
      say "WARNING: expected 1428 monthly steps for 1901-2019, got $steps"
    fi
  else
    say "MERGE FAILED"
  fi
fi

say "done"
