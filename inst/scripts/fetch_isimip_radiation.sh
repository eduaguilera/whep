#!/usr/bin/env bash
# Sources the two radiation forcings LPJmL 6.x requires, which 5.9.7 did not:
# 6.x removed the "cloudiness" radiation option (and the `cloud` input), so CRU
# cld is unusable and swdown + lwnet/lwdown are mandatory.
#
# ISIMIP publishes downwelling fluxes (rsds, rlds), not net longwave, so these
# pair with `radiation = "radiation_lwdown"`: rsds -> swdown, rlds -> lwdown,
# both W/m2.
#
# Each 10-year chunk is ~2.2 GB daily. It is reduced to monthly means and the
# daily file deleted immediately, so peak disk stays ~3 GB rather than ~53 GB.
# 2011_2019 is fetched first so a smoke run can start before the rest lands.
set -uo pipefail

# Usage: fetch_isimip_radiation.sh [dest_dir]
#
# Provenance script, not part of a normal run. The product it builds is pinned
# (lpjml-rsds-isimip-1901-2019 / lpjml-rlds-isimip-1901-2019) and fetched by
# download_climate.R, because rebuilding it streams ~53 GB of daily files. Run
# this only to regenerate the pinned artefact from source.

BASE="https://files.isimip.org/ISIMIP3a/InputData/climate/atmosphere/obsclim/global/daily/historical/GSWP3-W5E5"
DEST="${1:-${WHEP_L_FILES_DIR:-LPJmL_inputs}/radiation}"
WORK="$DEST/.work"
LOG="$DEST/fetch.log"
mkdir -p "$WORK"

CHUNKS="2011_2019 1901_1910 1911_1920 1921_1930 1931_1940 1941_1950 1951_1960 1961_1970 1971_1980 1981_1990 1991_2000 2001_2010"

say() { printf '%s %s\n' "$(date +%H:%M:%S)" "$*" >>"$LOG"; }
: >"$LOG"
say "start; dest=$DEST"

for var in rsds rlds; do
  for chunk in $CHUNKS; do
    monthly="$WORK/${var}_monthly_${chunk}.nc"
    if [ -s "$monthly" ]; then
      say "$var $chunk: monthly already present, skip"
      continue
    fi
    daily="$WORK/${var}_daily_${chunk}.nc"
    url="$BASE/gswp3-w5e5_obsclim_${var}_global_daily_${chunk}.nc"
    say "$var $chunk: downloading"
    if ! curl -sfL --max-time 7200 -o "$daily" "$url"; then
      say "$var $chunk: DOWNLOAD FAILED, skipping"
      rm -f "$daily"
      continue
    fi
    sz=$(stat -c%s "$daily")
    say "$var $chunk: got $((sz / 1000000)) MB, reducing to monthly"
    if cdo -s monmean "$daily" "$monthly" 2>>"$LOG"; then
      say "$var $chunk: monthly ok ($(cdo -s ntime "$monthly" 2>/dev/null) steps)"
    else
      say "$var $chunk: CDO monmean FAILED"
      rm -f "$monthly"
    fi
    # Free the 2.2 GB immediately; the monthly product is what LPJmL reads.
    rm -f "$daily"
  done
done

# Concatenate per-variable monthly pieces into one 1901-2019 series each.
for var in rsds rlds; do
  target="$DEST/${var}_gswp3-w5e5_obsclim_1901_2019_monthly.nc"
  pieces=$(ls "$WORK/${var}_monthly_"*.nc 2>/dev/null | sort)
  n=$(printf '%s\n' "$pieces" | grep -c . || true)
  if [ "$n" -eq 0 ]; then
    say "$var: no monthly pieces, nothing to merge"
    continue
  fi
  say "$var: merging $n pieces"
  if cdo -s -O mergetime $pieces "$target" 2>>"$LOG"; then
    say "$var: merged -> $(basename "$target") ($(cdo -s ntime "$target") steps)"
  else
    say "$var: MERGE FAILED"
  fi
done

say "done"
