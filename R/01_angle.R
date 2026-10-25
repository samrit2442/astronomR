#' Convert Decimal Degrees to Degrees, Minutes, and Seconds (DMS) Format
#'
#' Converts a decimal degree angle to the Degrees-Minutes-Seconds (DMS) format
#' commonly used in astronomy for expressing declination and other angles.
#'
#' The angle is rounded once, to the nearest \eqn{10^{-digit}}{10^-digit}
#' arcsecond, and then split exactly into degrees, minutes and seconds, so the
#' minutes and seconds are always below 60: \code{deg_to_dms(10.1)} gives 10
#' degrees, 6 minutes and 0 seconds. Seconds are written in fixed notation
#' without trailing zeros. The sign is \code{"-"} only when the angle is
#' negative and still non-zero after rounding, so \code{deg_to_dms(-1e-9)}
#' prints \code{+0} degrees.
#'
#' @param deg Numeric vector of angles in decimal degrees. All values must be
#'   between -90 and 90 (the declination range). \code{NA} values give
#'   \code{NA} in the output.
#' @param type Character string specifying the output format. Options are:
#'   \itemize{
#'     \item \code{"cat"} (default): Prints the angles in DMS notation, such
#'       as \code{"+45\u00B030'15.12\""}, separated by spaces and followed by
#'       a newline.
#'     \item \code{"mat"}: Returns a matrix with columns for sign, degrees,
#'       minutes, and seconds.
#'   }
#' @param digit Whole number from 0 to 10: the number of decimal places of
#'   the seconds. Default is \code{5}.
#' @return When \code{type = "cat"}, prints the DMS string and returns
#'   \code{NULL} invisibly. When \code{type = "mat"}, returns a character
#'   matrix with one row per angle and columns \code{SIGN} (\code{"+"} or
#'   \code{"-"}), \code{DEG} (degrees, 0 to 90), \code{MIN} (arcminutes, 0 to
#'   59) and \code{SEC} (arcseconds, below 60). The row for an \code{NA}
#'   angle is all \code{NA}.
#' @seealso \code{\link{dms_to_deg}} for the reverse conversion,
#'   \code{\link{deg_to_hms}} for right ascension, and
#'   \code{\link{deg_to_rad}}.
#' @examples
#' deg_to_dms(45.5042)
#' deg_to_dms(c(10.1, -0.5))
#' deg_to_dms(-12.5, type = "mat")
#' deg_to_dms(c(10.25, 45.5), type = "mat")
#'
#' # Paste the "mat" columns into strings to convert them back
#' x <- deg_to_dms(c(-0.5, 45.5042), type = "mat")
#' dms_to_deg(paste0(x[, "SIGN"], x[, "DEG"], "d", x[, "MIN"], "m",
#'                   x[, "SEC"], "s"))
#' @export
deg_to_dms <- function(deg, type = c("cat", "mat"), digit = 5) {
  deg <- .angle_numeric(deg, "deg")
  type <- match.arg(type)
  digit <- .angle_digit(digit)
  if (any(deg < -90 | deg > 90, na.rm = TRUE)) {
    stop("All degree values should be between -90\u00B0 and 90\u00B0.")
  }

  # Round once, in units of the last printed digit, then split exactly.
  k <- 10^digit
  n <- round(abs(deg) * 3600 * k)
  ok <- !is.na(n)
  SIGN <- ifelse(deg < 0 & n > 0, "-", "+")
  DEG <- as.character(n %/% (3600 * k))
  MIN <- as.character((n %% (3600 * k)) %/% (60 * k))
  SEC <- formatC((n %% (60 * k)) / k, format = "f", digits = digit,
                 drop0trailing = TRUE)
  SIGN[!ok] <- DEG[!ok] <- MIN[!ok] <- SEC[!ok] <- NA_character_

  if (type == "cat") {
    txt <- paste0(SIGN, DEG, "\u00B0", MIN, "'", SEC, "\"")
    txt[!ok] <- "NA"
    cat(paste(txt, collapse = " "), "\n", sep = "")
    return(invisible(NULL))
  }
  matrix(c(SIGN, DEG, MIN, SEC), ncol = 4,
         dimnames = list(NULL, c("SIGN", "DEG", "MIN", "SEC")))
}


#' Convert Degrees, Minutes, and Seconds (DMS) to Decimal Degrees
#'
#' Converts an angle expressed in Degrees-Minutes-Seconds (DMS) format to
#' decimal degrees. Accepts either separate numeric arguments or a single
#' formatted string.
#'
#' With numeric input, \code{d}, \code{m} and \code{s} are recycled to a
#' common length and the result is
#' \eqn{\pm(|d| + m/60 + s/3600)}{+/-(|d| + m/60 + s/3600)}, with the sign
#' taken from \code{d}. For a negative angle smaller than one degree, use R's
#' negative zero: \code{dms_to_deg(-0, 30)} is \code{-0.5}, while
#' \code{dms_to_deg(0, 30)} is \code{0.5}.
#'
#' A character \code{d} is parsed element by element. Each string holds an
#' optional sign (\code{"+"}, \code{"-"} or the Unicode minus sign U+2212),
#' the degrees, and optionally the minutes and then the seconds. The degrees
#' may be followed by the degree sign \code{"\u00B0"}, \code{"d"}, \code{"D"}
#' or \code{":"}; the minutes by \code{"'"}, the Unicode prime U+2032,
#' \code{"m"}, \code{"M"} or \code{":"}; and the seconds by \code{"\""}, the
#' Unicode double prime U+2033, \code{"s"} or \code{"S"}. Spaces alone may
#' also separate the fields. Only the last field may have a decimal part.
#' Accepted strings include \code{"12\u00B034'56\""}, \code{"-0\u00B030'00\""},
#' \code{"+12\u00B034'56.7\""}, \code{"12d34m56s"}, \code{"12:34:56"},
#' \code{"12 34 56"}, \code{"12\u00B034'"} and \code{"12.5\u00B0"}. A string
#' that cannot be parsed gives \code{NA} with a warning; \code{NA} gives
#' \code{NA} silently.
#'
#' For both kinds of input the minutes and seconds must be in
#' \eqn{[0, 60)}{[0, 60)} and the angle in \eqn{[-90, 90]}{[-90, 90]} degrees
#' (the declination range, as in \code{\link{deg_to_dms}}); other values are
#' an error.
#'
#' @param d Numeric or character vector. The degrees component, whose sign is
#'   the sign of the angle, or DMS strings such as \code{"12\u00B034'56\""}
#'   (see Details), in which case \code{m} and \code{s} must not be supplied.
#' @param m Numeric vector. The minutes component, from 0 to less than 60.
#'   Default is \code{0}.
#' @param s Numeric vector. The seconds component, from 0 to less than 60.
#'   Default is \code{0}.
#' @param digit Whole number from 0 to 10: the number of decimal places to
#'   round the result to. Default is \code{5}.
#' @return Numeric vector of angles in decimal degrees (between -90 and 90),
#'   rounded to \code{digit} decimal places.
#' @seealso \code{\link{deg_to_dms}} for the reverse conversion,
#'   \code{\link{hms_to_deg}} for right ascension, and
#'   \code{\link{deg_to_rad}}.
#' @examples
#' dms_to_deg(d = 12, m = 34, s = 56)
#' dms_to_deg(d = "12\u00B034'56\"", digit = 3)
#'
#' # The sign comes from 'd'; use -0 for angles between -1 and 0 degrees
#' dms_to_deg(c(-12, -0, 0), m = 30)
#'
#' # Several string formats, and an NA
#' dms_to_deg(c("12d34m56s", "12:34:56", "-0\u00B030'", "12.5\u00B0", NA))
#' @export
dms_to_deg <- function(d, m = 0, s = 0, digit = 5) {
  digit <- .angle_digit(digit)
  if (is.character(d)) {
    if (!missing(m) || !missing(s)) {
      stop("'m' and 's' must not be supplied when 'd' is a character string.")
    }
    p <- .angle_parse(d, "dms", "d")
    if (any(p$m >= 60 | p$s >= 60, na.rm = TRUE)) {
      stop("Minutes and seconds in 'd' must be less than 60.")
    }
    neg <- p$neg
    deg <- p$a + p$m / 60 + p$s / 3600
  } else {
    x <- .angle_components(d, m, s, "d")
    # The sign of d, including R's negative zero (1 / -0 is -Inf).
    neg <- x[[1]] < 0 | 1 / x[[1]] < 0
    deg <- abs(x[[1]]) + x[[2]] / 60 + x[[3]] / 3600
  }
  if (any(deg > 90, na.rm = TRUE)) {
    stop("Angles must be between -90 and 90 degrees.")
  }
  flip <- which(neg)
  deg[flip] <- -deg[flip]
  round(deg, digit)
}


#' Convert Decimal Degrees to Hours, Minutes, and Seconds (HMS) Format
#'
#' Converts a decimal degree angle to Hours-Minutes-Seconds (HMS) format,
#' which is used in astronomy to express Right Ascension (RA).
#'
#' Any finite angle is first wrapped into \eqn{[0, 360)}{[0, 360)} degrees, so
#' \code{-15} gives 23 hours and \code{375} gives 1 hour (one hour is 15
#' degrees). The angle is then rounded once, to the nearest
#' \eqn{10^{-digit}}{10^-digit} second of time, and split exactly, so the
#' minutes and seconds are always below 60 and the hours run from 0 to 23 (an
#' angle that rounds up to 24 hours is shown as 0 hours). Seconds are written
#' in fixed notation without trailing zeros.
#'
#' @param deg Numeric vector of angles in decimal degrees. Any finite value is
#'   accepted and wrapped into \eqn{[0, 360)}{[0, 360)}. \code{NA} values give
#'   \code{NA} in the output.
#' @param type Character string specifying the output format. Options are:
#'   \itemize{
#'     \item \code{"cat"} (default): Prints a formatted string
#'       (e.g., \code{3H0M0S}), separated by spaces for several angles and
#'       followed by a newline.
#'     \item \code{"mat"}: Returns a matrix with columns for hours, minutes,
#'       and seconds.
#'   }
#' @param digit Whole number from 0 to 10: the number of decimal places of
#'   the seconds. Default is \code{5}.
#' @return When \code{type = "cat"}, prints the HMS string and returns
#'   \code{NULL} invisibly. When \code{type = "mat"}, returns a numeric matrix
#'   with one row per angle and columns \code{HRS} (hours, 0 to 23),
#'   \code{MIN} (minutes of time, 0 to 59) and \code{SEC} (seconds of time,
#'   below 60). The row for an \code{NA} angle is all \code{NA}.
#' @seealso \code{\link{hms_to_deg}} for the reverse conversion,
#'   \code{\link{deg_to_dms}} for declination, and \code{\link{deg_to_rad}}.
#' @examples
#' deg_to_hms(deg = 45)
#' deg_to_hms(deg = 45, type = "mat")
#' deg_to_hms(deg = 177.74208, digit = 3)
#' deg_to_hms(c(-15, 20, 375))
#'
#' # The "mat" columns convert straight back
#' x <- deg_to_hms(177.74208, type = "mat")
#' hms_to_deg(x[, "HRS"], x[, "MIN"], x[, "SEC"])
#' @export
deg_to_hms <- function(deg, type = c("cat", "mat"), digit = 5) {
  deg <- .angle_numeric(deg, "deg")
  type <- match.arg(type)
  digit <- .angle_digit(digit)
  if (any(is.infinite(deg))) {
    stop("'deg' must be finite.")
  }

  # Round once, in units of the last printed digit of the seconds of time
  # (1 degree = 240 s), then split exactly.
  k <- 10^digit
  n <- round((deg %% 360) * 240 * k)
  ok <- !is.na(n)
  HRS <- (n %/% (3600 * k)) %% 24
  MIN <- (n %% (3600 * k)) %/% (60 * k)
  SEC <- (n %% (60 * k)) / k
  HRS[!ok] <- MIN[!ok] <- SEC[!ok] <- NA

  if (type == "cat") {
    txt <- paste0(HRS, "H", MIN, "M",
                  formatC(SEC, format = "f", digits = digit,
                          drop0trailing = TRUE), "S")
    txt[!ok] <- "NA"
    cat(paste(txt, collapse = " "), "\n", sep = "")
    return(invisible(NULL))
  }
  matrix(c(HRS, MIN, SEC), ncol = 3,
         dimnames = list(NULL, c("HRS", "MIN", "SEC")))
}


#' Convert Hours, Minutes, and Seconds (HMS) to Decimal Degrees
#'
#' Converts an angle expressed in Hours-Minutes-Seconds (HMS) format to
#' decimal degrees. Accepts either separate numeric arguments or a single
#' formatted string.
#'
#' With numeric input, \code{h}, \code{m} and \code{s} are recycled to a
#' common length and the result is
#' \eqn{15 (h + m/60 + s/3600)}{15 * (h + m/60 + s/3600)} degrees. \code{h}
#' must be in \eqn{[0, 24]}{[0, 24]}, \code{m} and \code{s} in
#' \eqn{[0, 60)}{[0, 60)}, and the total may not exceed 24 hours.
#'
#' A character \code{h} is parsed element by element, ignoring case: the
#' hours, then optionally the minutes and then the seconds, as in
#' \code{"03h15m30s"}, \code{"3h 15m 30s"}, \code{"3H15M30S"},
#' \code{"03:15:30"}, \code{"3 15 30"}, \code{"3h15m30.5s"}, \code{"3h15m"}
#' or \code{"3h"}. Only the last field may have a decimal part, and a leading
#' \code{"+"} is allowed. The parsed values must lie in the same ranges as
#' numeric input (so a negative value is an error). A string that cannot be
#' parsed gives \code{NA} with a warning; \code{NA} gives \code{NA} silently.
#'
#' @param h Numeric or character vector. The hours component, or HMS strings
#'   such as \code{"03h15m30s"} (see Details), in which case \code{m} and
#'   \code{s} must not be supplied.
#' @param m Numeric vector. The minutes component, from 0 to less than 60.
#'   Default is \code{0}.
#' @param s Numeric vector. The seconds component, from 0 to less than 60.
#'   Default is \code{0}.
#' @param digit Whole number from 0 to 10: the number of decimal places to
#'   round the result to. Default is \code{5}.
#' @return Numeric vector of angles in decimal degrees (between 0 and 360),
#'   rounded to \code{digit} decimal places.
#' @seealso \code{\link{deg_to_hms}} for the reverse conversion,
#'   \code{\link{dms_to_deg}} for declination, and \code{\link{deg_to_rad}}.
#' @examples
#' hms_to_deg(h = 3, m = 15, s = 30)
#' hms_to_deg(h = 3, m = 15, s = 30.123)
#' hms_to_deg(h = "03h15m30s")
#' hms_to_deg(c("3h 15m 30s", "03:15:30", "3h15m", "3h", NA))
#' @export
hms_to_deg <- function(h, m = 0, s = 0, digit = 5) {
  digit <- .angle_digit(digit)
  if (is.character(h)) {
    if (!missing(m) || !missing(s)) {
      stop("'m' and 's' must not be supplied when 'h' is a character string.")
    }
    p <- .angle_parse(h, "hms", "h")
    if (any(p$m >= 60 | p$s >= 60, na.rm = TRUE)) {
      stop("Minutes and seconds in 'h' must be less than 60.")
    }
    hours <- p$a + p$m / 60 + p$s / 3600
    if (any(hours > 24 | (p$neg & hours > 0), na.rm = TRUE)) {
      stop("Values in 'h' must be between 0 and 24 hours.")
    }
  } else {
    x <- .angle_components(h, m, s, "h")
    if (any(x[[1]] < 0 | x[[1]] > 24, na.rm = TRUE)) {
      stop("'h' must be between 0 and 24.")
    }
    hours <- x[[1]] + x[[2]] / 60 + x[[3]] / 3600
    if (any(hours > 24, na.rm = TRUE)) {
      stop("'h', 'm' and 's' must not add up to more than 24 hours.")
    }
  }
  round(15 * hours, digit)
}


#' Convert Degrees to Radians
#'
#' Converts angles from degrees to radians. Supports vectorized input.
#'
#' @param deg Numeric. The angle(s) in degrees to convert to radians.
#'   \code{NA} values give \code{NA}.
#' @return Numeric. The corresponding angle(s) in radians,
#'   \eqn{deg \times \pi / 180}{deg * pi / 180}.
#' @seealso \code{\link{rad_to_deg}} for the reverse conversion;
#'   \code{\link{deg_to_dms}} and \code{\link{deg_to_hms}} for
#'   sexagesimal output.
#' @examples
#' deg_to_rad(180)
#' deg_to_rad(c(0, 90, 180))
#' @export
deg_to_rad <- function(deg) {
  deg <- .angle_numeric(deg, "deg")
  deg * pi / 180
}


#' Convert Radians to Degrees
#'
#' Converts angles from radians to degrees. Supports vectorized input.
#'
#' @param rad Numeric. The angle(s) in radians to convert to degrees.
#'   \code{NA} values give \code{NA}.
#' @return Numeric. The corresponding angle(s) in degrees,
#'   \eqn{rad \times 180 / \pi}{rad * 180 / pi}.
#' @seealso \code{\link{deg_to_rad}} for the reverse conversion;
#'   \code{\link{deg_to_dms}} and \code{\link{deg_to_hms}} for
#'   sexagesimal output.
#' @examples
#' rad_to_deg(pi)
#' rad_to_deg(c(0, pi / 2, pi))
#' @export
rad_to_deg <- function(rad) {
  rad <- .angle_numeric(rad, "rad")
  rad * 180 / pi
}


# ---- Internal helpers --------------------------------------------------------
# Errors are reported against the exported function that called the helper
# (sys.call(-1)), so helpers must be called directly from those functions.

# Numeric input check. An all-NA vector (a bare NA is logical) counts as
# numeric NA.
.angle_numeric <- function(x, arg, what = "numeric", call = sys.call(-1)) {
  if (is.logical(x) && all(is.na(x))) {
    storage.mode(x) <- "double"
  }
  if (!is.numeric(x)) {
    stop(simpleError(paste0("'", arg, "' must be ", what, "."), call))
  }
  x
}

.angle_digit <- function(digit, call = sys.call(-1)) {
  if (!is.numeric(digit) || length(digit) != 1 || is.na(digit) ||
      digit < 0 || digit > 10 || digit != round(digit)) {
    stop(simpleError(
      "'digit' must be a single whole number between 0 and 10.", call
    ))
  }
  as.integer(digit)
}

# Checks the numeric (degrees or hours, minutes, seconds) components and
# recycles them to a common length. Returns a list of three vectors.
.angle_components <- function(x, m, s, arg, call = sys.call(-1)) {
  x <- .angle_numeric(x, arg, "numeric or character", call = call)
  m <- .angle_numeric(m, "m", call = call)
  s <- .angle_numeric(s, "s", call = call)
  len <- c(length(x), length(m), length(s))
  n <- if (any(len == 0)) 0L else max(len)
  if (any(len != 1 & len != n)) {
    stop(simpleError(paste0(
      "'", arg, "', 'm' and 's' must have the same length (or length 1)."
    ), call))
  }
  if (any(m < 0 | m >= 60, na.rm = TRUE)) {
    stop(simpleError("'m' must be between 0 and 60 (0 <= m < 60).", call))
  }
  if (any(s < 0 | s >= 60, na.rm = TRUE)) {
    stop(simpleError("'s' must be between 0 and 60 (0 <= s < 60).", call))
  }
  list(rep_len(x, n), rep_len(m, n), rep_len(s, n))
}

# Parses DMS (kind = "dms") or HMS (kind = "hms") strings with one anchored
# regular expression: optional sign, then degrees (or hours), optional minutes
# and optional seconds. Returns a list with the sign (neg) and the three
# non-negative fields (a, m, s; missing fields are 0). Elements that do not
# parse become NA, with a single warning listing them.
.angle_parse <- function(x, kind, arg, call = sys.call(-1)) {
  if (kind == "dms") {
    mark1 <- "\u00B0dD"
    mark2 <- "'\u2032mM"
    mark3 <- "\"\u2033sS"
  } else {
    mark1 <- "hH"
    mark2 <- "mM"
    mark3 <- "sS"
  }
  num <- "([0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)"
  pattern <- paste0(
    "^\\s*([+\\-\u2212]?)\\s*", num,
    "(?:(?:\\s*[", mark1, ":]\\s*|\\s+)", num,
    "(?:(?:\\s*[", mark2, ":]\\s*|\\s+)", num, "(?:\\s*[", mark3, "])?",
    "|(?:\\s*[", mark2, "])?)",
    "|(?:\\s*[", mark1, "])?)\\s*$"
  )

  len <- length(x)
  out <- list(neg = rep(NA, len), a = rep(NA_real_, len),
              m = rep(NA_real_, len), s = rep(NA_real_, len))
  todo <- which(!is.na(x))
  if (length(todo) == 0) {
    return(out)
  }
  g <- regmatches(x[todo], regexec(pattern, x[todo], perl = TRUE))
  g <- vapply(g, function(v) {
    if (length(v) == 5) v[-1] else rep(NA_character_, 4)
  }, character(4))
  # Rows of g: sign, first, second, third field ("" when absent).
  ok <- !is.na(g[1, ]) &
    !(nzchar(g[3, ]) & grepl(".", g[2, ], fixed = TRUE)) &
    !(nzchar(g[4, ]) & grepl(".", g[3, ], fixed = TRUE))

  i <- todo[ok]
  out$neg[i] <- g[1, ok] %in% c("-", "\u2212")
  fields <- g[2:4, ok, drop = FALSE]
  fields[!nzchar(fields)] <- "0"
  out$a[i] <- as.numeric(fields[1, ])
  out$m[i] <- as.numeric(fields[2, ])
  out$s[i] <- as.numeric(fields[3, ])

  bad <- x[todo[!ok]]
  if (length(bad) > 0) {
    shown <- encodeString(bad[seq_len(min(length(bad), 5))], quote = "\"")
    more <- ""
    if (length(bad) > 5) more <- paste0(" and ", length(bad) - 5, " more")
    warning(simpleWarning(paste0(
      "Could not parse ", length(bad),
      if (length(bad) == 1) " element" else " elements", " of '", arg,
      "', returning NA: ", paste(shown, collapse = ", "), more, "."
    ), call))
  }
  out
}
