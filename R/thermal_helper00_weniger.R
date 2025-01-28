# # Helper function: Compute factorial (using gamma function)
# factorial_fast <- function(n) {
#   if (n == 0) return(1)
#   mpfr(gamma(n + 1), precBits = 256)
# }
#
# # Helper function: Compute Pochhammer symbol (a)_k = a * (a + 1) * ... * (a + k - 1)
# pochhammer <- function(a, k) {
#   if (k == 0) return(1)
#   Rmpfr::mpfr(gamma(a + k), precBits = 256) / mpfr(gamma(a), precBits = 256)
# }
#
# # Helper function: Initialize cache for Weniger's transformation
# weniger_cache_1F2 <- function(terms) {
#   cache <- numeric(terms) # Preallocate memory for cache
#   for (k in 0:(terms - 1)) {
#     cache[k + 1] <- factorial_fast(k) # Cache factorial values
#   }
#   return(cache)
# }
#
# # Weniger's sequence transformation for ₁F₂
# weniger_1F2_mpfr <- function(a, b, z, cache, tol = 1e-30, max_iter = 1000) {
#   # Error handling for input
#   if (length(b) != 2) stop("b must contain exactly two parameters.")
#
#   terms <- length(cache)   # Number of terms to evaluate
#   series <- numeric(terms)  # Initialize the hypergeometric series
#   partial_sum <- mpfr(0, precBits = 256)  # Initialize the partial sum
#   prev_sum <- mpfr(-Inf, precBits = 256)  # To track convergence
#
#   for (k in 0:(terms - 1)) {
#     # Compute the series term (a)_k * z^k / ((b1)_k * (b2)_k * k!)
#     term <- pochhammer(a, k) * mpfr(z, precBits = 256)^k /
#       (pochhammer(b[1], k) * pochhammer(b[2], k) * cache[k + 1])
#     series[k + 1] <- term
#     partial_sum <- partial_sum + term
#
#     # Check for convergence
#     if (k > 1 && abs(partial_sum - prev_sum) < mpfr(tol, precBits = 256)) {
#       cat("Converged after", k, "iterations.\n")
#       break
#     }
#
#     # Stop if maximum iterations are reached
#     if (k >= max_iter - 1) {
#       warning("Maximum iterations reached without convergence.")
#       break
#     }
#
#     prev_sum <- partial_sum
#   }
#
#   # Apply Weniger transformation
#   transformed <- numeric(terms)
#   for (k in 0:(terms - 1)) {
#     if (k + 2 <= terms) {
#       transformed[k + 1] <- series[k + 1] -
#         (series[k + 2]^2 / (series[k + 1] - series[k + 3]))
#     } else {
#       transformed[k + 1] <- series[k + 1] # Edge case
#     }
#   }
#
#   # Return the sum of the transformed series
#   result <- sum(transformed, na.rm = TRUE)
#
#   # Error check
#   if (!is.finite(result)) stop("Computation failed: result is not finite.")
#
#   return(result)
# }
#
# # Example usage
# a <- mpfr(0.5, precBits = 256)
# b <- c(mpfr(1.0, precBits = 256), mpfr(1.5, precBits = 256))
# z <- mpfr(0.5, precBits = 256)
# cache <- weniger_cache_1F2(1000)
# result <- weniger_1F2_mpfr(a, b, z, cache)
#
# cat("Result of ₁F₂(", a, ";", b[1], ",", b[2], ";", z, ") =", result, "\n")
