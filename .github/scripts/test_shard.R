# Runs one shard of the test suite: shard SHARD of SHARDS takes every SHARDS-th
# test file in sorted order, starting at file SHARD. Used by offline-tests.yaml
# (#1347); see that file's header for why the suite is sharded.
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
mine <- names[(seq_along(names) - 1L) %% shards == shard - 1L]
if (length(mine) == 0L) {
  stop("Shard ", shard, " of ", shards, " selected no test files.")
}
cat(sprintf(
  "Shard %d of %d: %d of %d test files.\n",
  shard,
  shards,
  length(mine),
  length(files)
))

devtools::test(
  filter = paste0("^(", paste(mine, collapse = "|"), ")$"),
  stop_on_failure = TRUE
)
