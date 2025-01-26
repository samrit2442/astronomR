#' Convert Decimal Degrees to Degrees, Minutes, and Seconds (DMS) Format
#'
#' Normally in astronomy, different angular systems are used for location of a given planet, star or other phenomena. To deal with different angular systems and their conversions a suite of conversion functions are employed. `deg_to_dms()` ...[TODO]
#'
#'@param deg  Numeric vector of angles in decimal degrees. All values must be between -90° and 90°.
#'@param type  Character string specifying the output format. Options are:
#'  - `cat` (default): Output in concatenated format (e.g., 45°30'15").
#'  - `mat`: Output in matrix with columns for sign, degrees, minutes, and seconds.
#'  Default is `'cat'`.
#'@param digit  Integer specifying the number of digits to round the seconds to. Default is `5`.
#'@export
deg_to_dms <- function(deg, type = 'cat', digit = 5) {
  DEG <- 1
  MIN <- 60
  SEC <- 3600
  sign <- function(x)
    ifelse(x >= 0, 1, -1)
  deg_sign <- function(deg)
    ifelse(deg >= 0, "+", "-")
  SIGN <- " "

  if (any(deg < -90 | deg > 90)) {
    stop("All degree values should be greater than -90\u00B0 or less than 90\u00B0")
  }

  df <- tibble::tibble(deg) |>
    dplyr::mutate(
      deg_sign = sign(deg),
      deg = abs(deg),
      DEG = floor(deg),
      MIN = floor((deg - DEG) * 60),
      SEC = (deg - DEG - MIN / 60) * 3600,
      SEC = dplyr::case_when(SEC <= 0 ~ 0, SEC > 60 ~ 60, .default = SEC),
      MIN = dplyr::case_when(SEC == 60 ~ MIN + 1, MIN == 60 ~ 0, .default = MIN),
      DEG = dplyr::case_when(MIN == 60 ~ DEG + 1, .default = DEG),
      SEC = round(SEC, digits = digit),
      SIGN = dplyr::if_else(deg_sign == -1, "-", "+"),
      output1 = paste0(SIGN, stringr::str_c(DEG, MIN, SEC, sep = ":")),
      output2 = paste0(SIGN, DEG, "\u00B0", MIN, "'", SEC, '"')
    )

  if (type == 'cat') {
    return(cat(df$output2))
  }
  if (type == 'mat') {
    return(df |> dplyr::select(SIGN, DEG, MIN, SEC) |> as.matrix())
  }
}


#' Convert Degrees, Minutes, and Seconds (DMS) to Decimal Degrees
#'
#' Normally in astronomy, different angular systems are used for location of a given planet, star or other phenomena. To deal with different angular systems and their conversions a suite of conversion functions are employed. `dms_to_deg()` ...[TODO]
#'
#'@param d  Numeric or character. Represents the degrees component of the input.
#'  If provided as a single character string (e.g., "12°34'56\""), the degrees, minutes, and seconds are automatically parsed.
#'@param m  Numeric. Represents the minutes component of the input. Should be less than 60. Optional if `d` is a string.
#'@param s  Numeric. Represents the seconds component of the input. Should be less than 60. Optional if `d` is a string.
#'@param digit Integer specifying the number of digits to round the output to. Default is `5`.
#'@examples
#'  # Example 1: Using separate numeric inputs for d, m, and s
#'  dms_to_deg(d = 12, m = 34, s = 56) # Output: 12.58222
#'
#'  # Example 2: Using a single string input in DMS format
#'  dms_to_deg(d = "12°34'56\"", digit = 3) # Output: 12.582
#'
#'@export
dms_to_deg <- function(d, m, s, digit = 5) {
  sign <- 1
  deg <- 0

  if (is.character(d) & missing(m) & missing(s)) {
    d2 <- readr::parse_number(d)
    m2 <- stringr::str_remove(d, as.character(d2))
    m <- readr::parse_number(m2)
    s2 <- stringr::str_remove(m2, as.character(m))
    s <- readr::parse_number(s2)
    d <- d2
  }

  if (d < -90 | d > 90) {
    stop("All d values should be less than 90 and greater than -90.")
  }
  if (m >= 60 | s >= 60) {
    stop("Minutes and Seconds should be less than 60 and greater than 0.")
  }
  df <- tibble::tibble(d, m, s) |>
    dplyr::mutate(
      sign = sign(d),
      deg = abs(d) + (m / 60) + (s / 3600),
      deg = round(deg, digit) * sign
    )
  return(df |> dplyr::pull(deg))
}


#' Convert Decimal Degrees to Hours, Minutes, and Seconds (HMS) Format
#'
#' Normally in astronomy, different angular systems are used for location of a given planet, star or other phenomena. To deal with different angular systems and their conversions a suite of conversion functions are employed. `deg_to_hms()` ...[TODO]
#'
#'@param deg  Numeric vector of angles in decimal degrees. Values can range from 0° to 360°.
#'@param type  Character string specifying the output format. Options are:
#'   - `'cat'`: Outputs a concatenated string in the format HHhMMmSS.SSs.
#'   - `'mat'`: Outputs a matrix with columns for hours, minutes, and seconds
#'@param digit  Integer specifying the number of digits to round the output to. Default is `5`.
#'@examples
#'# Example 1: Convert 45 degrees to HMS
#' deg_to_hms(deg = 45) # Output: "03H00M00S"
#'# Example 2: Get HMS as a matrix
#' deg_to_hms(deg = 45, type = 'mat')
#'# Example 3: Customize the number of decimal places
#' deg_to_hms(deg = 45, digit = 2) # Output: "03H00M00.00S"
#'@export
deg_to_hms <- function(deg, type = 'cat', digit = 5) {
  DEG <- 1
  HRS <- 1
  MIN <- 60
  SEC <- 3600

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
      SEC = round(SEC, digits = 5),
      output1 = paste0(stringr::str_c(HRS, MIN, SEC, sep = ":")),
      output2 = paste0(HRS, "H", MIN, "M", SEC, "S")
    )

  if (type == 'cat') {
    return(cat(df$output2))
  }
  if (type == 'mat') {
    return(df |> dplyr::select(HRS, MIN, SEC) |> as.matrix())
  }
}


#' Convert Hours, Minutes, and Seconds (HMS) to Decimal Degrees
#'
#' Normally in astronomy, different angular systems are used for location of a given planet, star or other phenomena. To deal with different angular systems and their conversions a suite of conversion functions are employed. `hms_to_deg()` ...[TODO]
#'
#'@param h Numeric or character. Represents the hours component of the input.
#'   If provided as a single character string (e.g., "03h15m30s"), the hours, minutes, and seconds are automatically parsed.
#'@param m Numeric. Represents the minutes component of the input. Optional if `h` is a single string.
#'@param s Numeric. Represents the seconds component of the input. Optional if `h` is a single string.
#'@param digit Integer. Specifies the number of decimal places to which the output should be rounded. Default is 5.
#'@examples
#' # Example 1: Using separate numeric inputs for h, m, and s
#' hms_to_deg(h = 3, m = 15, s = 30) # Output: 48.875
#'# Example 2: Handling fractional seconds
#' hms_to_deg(h = 3, m = 15, s = 30.123) # Output: 48.87551
#' # Example 2: Using a single string input in HMS format
#' hms_to_deg(h = "03h15m30s") # Output: 48.875
#'@export
hms_to_deg <- function(h, m, s, digit = 5) {
  DEG <- 0
  H <- 0
  M <- 0
  S <- 0

  if (is.character(h) & missing(m) & missing(s)) {
    df <- tibble::tibble(h) |>
      dplyr::mutate(h = tolower(h)) |>
      tidyr::separate(
        h,
        into = c("H", "M", "S"),
        sep = "[hms]",
        extra = "drop",
        convert = TRUE
      ) |>
      dplyr::mutate(DEG = round((H * 15) + (M * 15 / 60) + (S * 15 / 3600), digits = digit))
  } else {
    # If numeric h, m, s are provided
    df <- tibble::tibble(H = h, M = m, S = s) |>
      dplyr::mutate(DEG = round((H * 15) + (M * 15 / 60) + (S * 15 / 3600), digits = digit))
  }

  return(df |> dplyr::pull(DEG))
}


#' Convert Degrees to Radians
#'
#' This function converts angles from degrees to radians. It supports vectorized input.
#'
#' @param deg Numeric. The angle(s) in degrees to convert to radians.
#' @return Numeric. The corresponding angle(s) in radians.
#' @examples
#' deg2rad(180)        # Output: 3.141593 (pi)
#' deg2rad(c(0, 90))   # Output: 0, 1.570796
#'@export
deg2rad <- function(deg) {
  return(deg * pi / 180)
}


#' Convert Radians to Degrees
#'
#' This function converts angles from radians to degrees. It supports vectorized input.
#' @param rad Numeric. The angle(s) in radians to convert to degrees.
#' @return Numeric. The corresponding angle(s) in degrees.
#' @examples
#' rad2deg(pi)        # Output: 180
#' rad2deg(c(0, pi/2)) # Output: 0, 90
#'@export
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}


options(error = NULL)
options(browser = NULL)
# undebug(deg_to_dms)
