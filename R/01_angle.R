#' Convert Decimal Degrees to Degrees, Minutes, and Seconds (DMS) Format
#'
#' Converts a decimal degree angle to the Degrees-Minutes-Seconds (DMS) format
#' commonly used in astronomy for expressing declination and other angles.
#'
#' @param deg Numeric vector of angles in decimal degrees. All values must be
#'   between -90 and 90.
#' @param type Character string specifying the output format. Options are:
#'   \itemize{
#'     \item \code{"cat"} (default): Prints the angle as a formatted string
#'       in DMS notation.
#'     \item \code{"mat"}: Returns a matrix with columns for sign, degrees,
#'       minutes, and seconds.
#'   }
#' @param digit Integer specifying the number of digits to round the seconds
#'   to. Default is \code{5}.
#' @return When \code{type = "cat"}, prints the DMS string and returns
#'   \code{NULL} invisibly. When \code{type = "mat"}, returns a character
#'   matrix with columns \code{SIGN}, \code{DEG}, \code{MIN}, and \code{SEC}.
#' @examples
#' deg_to_dms(45.5042)
#' deg_to_dms(-12.5, type = "mat")
#' deg_to_dms(c(10.25, 45.5), type = "mat")
#' @export
deg_to_dms <- function(deg, type = "cat", digit = 5) {
  if (any(deg < -90 | deg > 90)) {
    stop("All degree values should be between -90\u00B0 and 90\u00B0.")
  }

  df <- tibble::tibble(deg) |>
    dplyr::mutate(
      deg_sign = base::sign(deg),
      deg = abs(deg),
      DEG = floor(deg),
      MIN = floor((deg - DEG) * 60),
      SEC = (deg - DEG - MIN / 60) * 3600,
      SEC = dplyr::case_when(SEC <= 0 ~ 0, SEC > 60 ~ 60, .default = SEC),
      MIN = dplyr::case_when(SEC == 60 ~ MIN + 1, MIN == 60 ~ 0, .default = MIN),
      DEG = dplyr::case_when(MIN == 60 ~ DEG + 1, .default = DEG),
      SEC = round(SEC, digits = digit),
      SIGN = dplyr::if_else(deg_sign == -1, "-", "+"),
      output2 = paste0(SIGN, DEG, "\u00B0", MIN, "'", SEC, '"')
    )

  if (type == "cat") {
    cat(df$output2)
    return(invisible(NULL))
  }
  if (type == "mat") {
    return(df |> dplyr::select(SIGN, DEG, MIN, SEC) |> as.matrix())
  }
}


#' Convert Degrees, Minutes, and Seconds (DMS) to Decimal Degrees
#'
#' Converts an angle expressed in Degrees-Minutes-Seconds (DMS) format to
#' decimal degrees. Accepts either separate numeric arguments or a single
#' formatted string.
#'
#' @param d Numeric or character. The degrees component. If a single character
#'   string is provided (e.g., \code{"12\u00B034'56\""}), the degrees, minutes,
#'   and seconds are parsed automatically, and \code{m} and \code{s} should be
#'   omitted.
#' @param m Numeric. The minutes component. Must be less than 60. Optional when
#'   \code{d} is a string.
#' @param s Numeric. The seconds component. Must be less than 60. Optional when
#'   \code{d} is a string.
#' @param digit Integer. Number of decimal places to round the result to.
#'   Default is \code{5}.
#' @return Numeric. The angle in decimal degrees.
#' @examples
#' dms_to_deg(d = 12, m = 34, s = 56)
#' dms_to_deg(d = "12\u00B034'56\"", digit = 3)
#' @export
dms_to_deg <- function(d, m, s, digit = 5) {
  if (is.character(d) && missing(m) && missing(s)) {
    d2 <- readr::parse_number(d)
    m2 <- stringr::str_remove(d, as.character(d2))
    m <- readr::parse_number(m2)
    s2 <- stringr::str_remove(m2, as.character(m))
    s <- readr::parse_number(s2)
    d <- d2
  }

  if (d < -90 | d > 90) {
    stop("'d' must be between -90 and 90.")
  }
  if (m >= 60 | s >= 60) {
    stop("'m' and 's' must each be less than 60.")
  }

  df <- tibble::tibble(d, m, s) |>
    dplyr::mutate(
      sign = base::sign(d),
      deg = abs(d) + (m / 60) + (s / 3600),
      deg = round(deg, digit) * sign
    )

  df |> dplyr::pull(deg)
}


#' Convert Decimal Degrees to Hours, Minutes, and Seconds (HMS) Format
#'
#' Converts a decimal degree angle to Hours-Minutes-Seconds (HMS) format,
#' which is used in astronomy to express Right Ascension (RA).
#'
#' @param deg Numeric vector of angles in decimal degrees. Values can range
#'   from 0 to 360.
#' @param type Character string specifying the output format. Options are:
#'   \itemize{
#'     \item \code{"cat"} (default): Prints a formatted string
#'       (e.g., \code{3H0M0S}).
#'     \item \code{"mat"}: Returns a matrix with columns for hours, minutes,
#'       and seconds.
#'   }
#' @param digit Integer specifying the number of decimal places to round
#'   seconds to. Default is \code{5}.
#' @return When \code{type = "cat"}, prints the HMS string and returns
#'   \code{NULL} invisibly. When \code{type = "mat"}, returns a numeric matrix
#'   with columns \code{HRS}, \code{MIN}, and \code{SEC}.
#' @examples
#' deg_to_hms(deg = 45)
#' deg_to_hms(deg = 45, type = "mat")
#' deg_to_hms(deg = 177.74208, digit = 3)
#' @export
deg_to_hms <- function(deg, type = "cat", digit = 5) {
  df <- tibble::tibble(deg) |>
    dplyr::mutate(
      DEG = dplyr::if_else(deg < 0, deg + 360, deg),
      HRS = floor(DEG / 15),
      MIN = floor((DEG / 15 - HRS) * 60),
      SEC = (DEG / 15 - HRS - MIN / 60) * 3600,
      SEC = dplyr::case_when(SEC <= 0 ~ 0, SEC > 60 ~ 60, .default = SEC),
      MIN = dplyr::case_when(SEC == 60 ~ MIN + 1, MIN == 60 ~ 0, .default = MIN),
      HRS = dplyr::case_when(MIN == 60 ~ HRS + 1, .default = HRS),
      HRS = HRS %% 24,
      SEC = round(SEC, digits = digit),
      output2 = paste0(HRS, "H", MIN, "M", SEC, "S")
    )

  if (type == "cat") {
    cat(df$output2)
    return(invisible(NULL))
  }
  if (type == "mat") {
    return(df |> dplyr::select(HRS, MIN, SEC) |> as.matrix())
  }
}


#' Convert Hours, Minutes, and Seconds (HMS) to Decimal Degrees
#'
#' Converts an angle expressed in Hours-Minutes-Seconds (HMS) format to
#' decimal degrees. Accepts either separate numeric arguments or a single
#' formatted string.
#'
#' @param h Numeric or character. The hours component. If a single character
#'   string is provided (e.g., \code{"03h15m30s"}), the hours, minutes, and
#'   seconds are parsed automatically, and \code{m} and \code{s} should be
#'   omitted.
#' @param m Numeric. The minutes component. Optional when \code{h} is a
#'   string.
#' @param s Numeric. The seconds component. Optional when \code{h} is a
#'   string.
#' @param digit Integer. Number of decimal places to round the result to.
#'   Default is \code{5}.
#' @return Numeric. The angle in decimal degrees.
#' @examples
#' hms_to_deg(h = 3, m = 15, s = 30)
#' hms_to_deg(h = 3, m = 15, s = 30.123)
#' hms_to_deg(h = "03h15m30s")
#' @export
hms_to_deg <- function(h, m, s, digit = 5) {
  if (is.character(h) && missing(m) && missing(s)) {
    df <- tibble::tibble(h) |>
      dplyr::mutate(h = tolower(h)) |>
      tidyr::separate_wider_regex(
        h,
        patterns = c(H = "[0-9]+", "h", M = "[0-9]+", "m", S = "[0-9.]+", "s"),
        too_few = "align_start"
      ) |>
      dplyr::mutate(
        dplyr::across(c(H, M, S), as.numeric),
        DEG = round((H * 15) + (M * 15 / 60) + (S * 15 / 3600), digits = digit)
      )
  } else {
    df <- tibble::tibble(H = h, M = m, S = s) |>
      dplyr::mutate(DEG = round((H * 15) + (M * 15 / 60) + (S * 15 / 3600),
                                digits = digit))
  }

  df |> dplyr::pull(DEG)
}


#' Convert Degrees to Radians
#'
#' Converts angles from degrees to radians. Supports vectorized input.
#'
#' @param deg Numeric. The angle(s) in degrees to convert to radians.
#' @return Numeric. The corresponding angle(s) in radians.
#' @examples
#' deg2rad(180)
#' deg2rad(c(0, 90, 180))
#' @export
deg2rad <- function(deg) {
  deg * pi / 180
}


#' Convert Radians to Degrees
#'
#' Converts angles from radians to degrees. Supports vectorized input.
#'
#' @param rad Numeric. The angle(s) in radians to convert to degrees.
#' @return Numeric. The corresponding angle(s) in degrees.
#' @examples
#' rad2deg(pi)
#' rad2deg(c(0, pi / 2, pi))
#' @export
rad2deg <- function(rad) {
  rad * 180 / pi
}
