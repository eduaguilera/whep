.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  .bind_lazydata(ns$.__NAMESPACE__.$lazydata, ns)
}

.bind_lazydata <- function(lazydata, target) {
  if (!is.environment(lazydata) || !is.environment(target)) {
    return(invisible(character()))
  }
  bind_names <- setdiff(
    ls(lazydata, all.names = TRUE),
    ls(target, all.names = TRUE)
  )
  purrr::walk(bind_names, \(name) .delayed_bind(name, lazydata, target))
  invisible(bind_names)
}

.delayed_bind <- function(name, from, to) {
  delayedAssign(
    name,
    get(name, envir = from, inherits = FALSE),
    assign.env = to
  )
  invisible(NULL)
}
