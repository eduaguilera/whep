#!/usr/bin/env bash
set -u

mem_available_mb() {
  awk '/^MemAvailable:/ { printf "%d\n", $2 / 1024 }' /proc/meminfo
}

MEM_FLOOR_MB="${MEM_FLOOR_MB:-25000}"
SAMPLE_SECONDS="${SAMPLE_SECONDS:-2}"

start_avail="$(mem_available_mb)"
min_avail="$start_avail"

echo "GUARD start_avail_mb=${start_avail} mem_floor_mb=${MEM_FLOOR_MB}"
echo "GUARD years=${YEARS:-unset} chunk_size=${CHUNK_SIZE:-unset} workers=${N_WORKERS:-unset} auto=${AUTO:-false}"

setsid /usr/bin/time -v env \
  YEARS="${YEARS:-}" \
  CHUNK_SIZE="${CHUNK_SIZE:-1}" \
  N_WORKERS="${N_WORKERS:-1}" \
  MAX_ITER="${MAX_ITER:-1000}" \
  WRITE_OUTPUT="${WRITE_OUTPUT:-false}" \
  OVERWRITE_OUTPUT="${OVERWRITE_OUTPUT:-false}" \
  USE_TYPE_CROPLAND="${USE_TYPE_CROPLAND:-true}" \
  USE_MULTICROPPING="${USE_MULTICROPPING:-true}" \
  AUTO="${AUTO:-false}" \
  TARGET_MEM_FRACTION="${TARGET_MEM_FRACTION:-0.70}" \
  WORKER_MEM_MB="${WORKER_MEM_MB:-4500}" \
  Rscript scripts/run_gridded_landuse_150years.R &

pid="$!"
while kill -0 "$pid" 2>/dev/null; do
  avail="$(mem_available_mb)"
  if [ "$avail" -lt "$min_avail" ]; then
    min_avail="$avail"
  fi
  if [ "$avail" -lt "$MEM_FLOOR_MB" ]; then
    echo "GUARD killing run: mem_available_mb=${avail} below floor ${MEM_FLOOR_MB}" >&2
    kill -TERM "-$pid" 2>/dev/null || true
    sleep 5
    kill -KILL "-$pid" 2>/dev/null || true
    break
  fi
  sleep "$SAMPLE_SECONDS"
done

wait "$pid"
rc="$?"
end_avail="$(mem_available_mb)"
echo "GUARD_METRICS start_avail_mb=${start_avail} min_avail_mb=${min_avail} end_avail_mb=${end_avail} exit_code=${rc}"
exit "$rc"
