#' Estimate the Number of Communicating Civilizations via the Drake Equation
#'
#' Computes **N**, the estimated number of active, technologically advanced
#' civilizations capable of interstellar communication in the Milky Way, using
#' the Drake equation:
#'
#' \deqn{N = R_* \times f_p \times n_e \times f_l \times f_i \times f_c \times L}
#'
#' Default values reflect widely cited contemporary estimates (see References).
#' All fraction parameters must lie in \eqn{[0, 1]}.
#'
#' @param R_star  Numeric. Average rate of star formation per year in the Milky
#'   Way (stars yr\eqn{^{-1}}). Default: \code{1.5}.
#' @param fp      Numeric. Fraction of stars that have planetary systems.
#'   Must be in \eqn{[0, 1]}. Default: \code{1.0} (current exoplanet surveys
#'   suggest nearly all Sun-like stars host planets).
#' @param ne      Numeric. Mean number of planets per planetary system that
#'   could potentially support life (Earth-like / habitable-zone planets).
#'   Default: \code{0.4}.
#' @param fl      Numeric. Fraction of suitable planets on which life actually
#'   appears. Must be in \eqn{[0, 1]}. Default: \code{1.0}.
#' @param fi      Numeric. Fraction of life-bearing planets on which intelligent
#'   life evolves. Must be in \eqn{[0, 1]}. Default: \code{1.0}.
#' @param fc      Numeric. Fraction of intelligent civilizations that develop
#'   detectable communication technology. Must be in \eqn{[0, 1]}.
#'   Default: \code{0.1}.
#' @param L       Numeric. Mean longevity (years) of a communicating
#'   civilization. Default: \code{1000}.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{N}}{Numeric. Estimated number of communicating civilizations.}
#'     \item{\code{inputs}}{Named numeric vector of all seven input parameters.}
#'     \item{\code{equation}}{Character. Human-readable form of the equation with
#'       values substituted.}
#'     \item{\code{interpretation}}{Character. A qualitative interpretation of \code{N}
#'       spanning six tiers from \emph{effectively zero} (\eqn{N < 0.001}) to
#'       \emph{galaxy teeming with life} (\eqn{N \geq 10{,}000}).}
#'   }
#'
#' @references
#' Drake, F. D. (1961). Discussion at the first SETI conference, Green Bank,
#' West Virginia. \emph{NRAO}.
#'
#' Vakoch, D. A., & Dowd, M. F. (Eds.) (2015). \emph{The Drake Equation:
#' Estimating the Prevalence of Extraterrestrial Life through the Ages}.
#' Cambridge University Press.
#'
#' @export
#'
#' @examples
#' # Using all defaults (optimistic estimate)
#' drake_equation()
#'
#' # Pessimistic "Rare Earth" scenario
#' drake_equation(R_star = 1.5, fp = 1.0, ne = 0.4,
#'                fl = 0.01, fi = 0.01, fc = 0.01, L = 304)
#'
#' # Custom values
#' result <- drake_equation(R_star = 3, fp = 0.9, ne = 0.5,
#'                          fl = 0.5, fi = 0.1, fc = 0.1, L = 10000)
#' result$N
drake_equation <- function(
    R_star = 1.5,
    fp     = 1.0,
    ne     = 0.4,
    fl     = 1.0,
    fi     = 1.0,
    fc     = 0.1,
    L      = 1000
) {
  # --- Input validation -------------------------------------------------------

  if (!is.numeric(R_star) || length(R_star) != 1 || R_star < 0) {
    stop("'R_star' must be a single non-negative number.")
  }
  for (param_name in c("fp", "fl", "fi", "fc")) {
    val <- get(param_name)
    if (!is.numeric(val) || length(val) != 1 || val < 0 || val > 1) {
      stop(paste0("'", param_name, "' must be a single number in [0, 1]."))
    }
  }
  if (!is.numeric(ne) || length(ne) != 1 || ne < 0) {
    stop("'ne' must be a single non-negative number.")
  }
  if (!is.numeric(L) || length(L) != 1 || L < 0) {
    stop("'L' must be a single non-negative number.")
  }

  # --- Computation ------------------------------------------------------------

  N <- R_star * fp * ne * fl * fi * fc * L

  # --- Human-readable equation string ----------------------------------------

  equation <- paste0(
    "N = R* x fp x ne x fl x fi x fc x L\n",
    "  = ", R_star, " x ", fp, " x ", ne,
    " x ", fl, " x ", fi, " x ", fc, " x ", L,
    "\n  = ", N
  )

  # --- Qualitative interpretation of N ----------------------------------------

  interpretation <- if (N < 0.001) {
    paste0(
      "N \u2248 ", signif(N, 3), ": Effectively zero. Under these parameters, the ",
      "chance of even one communicating civilisation existing in the Milky Way ",
      "at any given moment is vanishingly small. We are, in all likelihood, alone."
    )
  } else if (N < 1) {
    paste0(
      "N \u2248 ", signif(N, 3), ": Less than one expected civilisation. Intelligent, ",
      "communicating life is exceedingly rare; any civilisation that does arise ",
      "is probably isolated by enormous distances or time."
    )
  } else if (N < 10) {
    paste0(
      "N \u2248 ", signif(N, 3), ": Only a handful of civilisations may exist ",
      "simultaneously across the galaxy. Contact is theoretically possible but ",
      "the vast distances make it extraordinarily difficult."
    )
  } else if (N < 1000) {
    paste0(
      "N \u2248 ", signif(N, 3), ": A modest number of civilisations. The galaxy ",
      "could harbour detectable signals, but they remain rare enough that ",
      "targeted searches would be needed to find them."
    )
  } else if (N < 10000) {
    paste0(
      "N \u2248 ", signif(N, 3), ": Thousands of civilisations. The galaxy is ",
      "reasonably well-populated with intelligent life; persistent, wide-sky ",
      "surveys have a realistic chance of detecting a signal."
    )
  } else {
    paste0(
      "N \u2248 ", signif(N, 3), ": The galaxy is teeming with communicating ",
      "civilisations. Under these optimistic parameters, interstellar contact ",
      "may be almost inevitable given sufficient technology and patience."
    )
  }

  # --- Return -----------------------------------------------------------------

  list(
    N              = N,
    inputs         = c(
      R_star = R_star,
      fp     = fp,
      ne     = ne,
      fl     = fl,
      fi     = fi,
      fc     = fc,
      L      = L
    ),
    equation       = equation,
    interpretation = interpretation
  )
}
