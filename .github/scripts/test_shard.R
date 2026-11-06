# Runs one shard of the test suite: shard SHARD of SHARDS. Used by
# offline-tests.yaml (#1347); see that file's header for why the suite is
# sharded.
#
# Files are assigned by measured duration, not round-robin: the suite's time is
# concentrated in a handful of files, so taking every SHARDS-th file left one
# shard with several of the slowest ones. Each file, slowest first, goes to the
# currently lightest shard (longest-processing-time greedy). Durations come
# from .github/scripts/test_durations.csv (columns `file`, `seconds`); a file
# missing from it is charged the median of the listed ones, so a new file still
# lands in some shard with no list to update. Refresh the CSV occasionally; a
# stale entry only costs balance, never coverage, because every file is
# assigned to exactly one shard whatever the CSV says.

# Assigns each name a shard in 1..shards, balancing the summed `seconds`.
assign_shards <- function(names, seconds, shards) {
  # Stable order: slowest first, ties broken alphabetically.
  ord <- order(-seconds, names)
  load <- numeric(shards)
  out <- integer(length(names))
  for (i in ord) {
    s <- which.min(load)
    out[i] <- s
    load[s] <- load[s] + seconds[i]
  }
  out
}

# Per-file seconds for `names`, with unlisted files charged the listed median.
file_seconds <- function(names, durations_path) {
  known <- utils::read.csv(durations_path, stringsAsFactors = FALSE)
  secs <- known$seconds[match(names, known$file)]
  fallback <- stats::median(known$seconds)
  secs[is.na(secs)] <- fallback
  secs
}

main <- function() {
  shard <- as.integer(Sys.getenv("SHARD"))
  shards <- as.integer(Sys.getenv("SHARDS"))
  if (is.na(shard) || is.na(shards) || shard < 1L || shard > shards) {
    stop("Set SHARD to a whole number in 1..SHARDS.")
  }

  files <- sort(list.files("tests/testthat", "^test.*\\.[rR]$"))
  # The names `devtools::test(filter = )` matches against. They go into a regex
  # unescaped, so refuse any that is not plain.
  names <- sub("\\.[rR]$", "", sub("^test[-_]?", "", files))
  odd <- names[!grepl("^[A-Za-z0-9_]+$", names)]
  if (length(odd) > 0L) {
    stop("Rename these test files to [A-Za-z0-9_]: ", toString(odd))
  }
  secs <- file_seconds(names, ".github/scripts/test_durations.csv")
  owner <- assign_shards(names, secs, shards)
  mine <- names[owner == shard]
  if (length(mine) == 0L) {
    stop("Shard ", shard, " of ", shards, " selected no test files.")
  }
  cat(sprintf(
    "Shard %d of %d: %d of %d test files, ~%.0f s expected.\n",
    shard,
    shards,
    length(mine),
    length(files),
    sum(secs[owner == shard])
  ))

  devtools::test(
    filter = paste0("^(", paste(mine, collapse = "|"), ")$"),
    stop_on_failure = TRUE
  )
}

if (!identical(Sys.getenv("TEST_SHARD_NO_MAIN"), "true")) {
  main()
}
