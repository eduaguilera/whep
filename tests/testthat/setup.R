# Keep the derived-HWSD cache out of the user's real cache directory.
#
# `.aggregate_hwsd_multi()` caches its result, so without this every test that
# aggregates a fixture HWSD would write into `user_cache_dir("whep")`, and a
# later test could read a cached grid instead of exercising the aggregation it
# means to test. Pointing the cache at a per-run temporary directory keeps the
# suite hermetic and self-cleaning.
withr::local_envvar(
  WHEP_HWSD_CACHE_DIR = withr::local_tempdir(.local_envir = teardown_env()),
  .local_envir = teardown_env()
)
