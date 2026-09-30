# Reference values for the logarithmic mean L(a, b) = (a - b) / log(a / b),
# used to pin its accuracy rather than its value (#1089).

# L(a, b) = b * S(x) with x = (a - b) / b and S(x) = x / log1p(x), from the
# series S(x) = 1 + x/2 - x^2/12 + x^3/24 - 19 x^4/720 + 3 x^5/160 - ...
# (Gregory coefficients). For |x| <= 2^-16 the first omitted term is below
# 1e-30 relative. `a - b` is exact for such close pairs (Sterbenz), and the
# rounding of the division moves S by only about x * eps, so the reference
# is correct to rounding.
log_mean_series_reference <- function(a, b) {
  x <- (a - b) / b
  b *
    (1 +
      x * (1 / 2 - x * (1 / 12 - x * (1 / 24 - x * (19 / 720 - x * 3 / 160)))))
}

# Pairs (a, b) whose relative difference straddles the switch point
# sqrt(.Machine$double.eps) = 2^-26, where the previous implementations moved
# from log1p() to log(a) - log(b). Bases that are not powers of two matter:
# for b = 1, log(b) is exactly 0 and log(a) - log(b) is exact by accident.
log_mean_switch_pairs <- function() {
  steps <- 2^26 + c(-4:4, 16, 256, 2^20)
  multiples <- 2^26 * c(2, 4, 16, 256, 1024)
  x <- c(steps, multiples) * 2^-52
  x <- c(x, -x / 2)
  b <- rep(c(3, 1234.5678, 0.07, 5e6), each = length(x))
  list(a = b * (1 + x), b = b)
}
