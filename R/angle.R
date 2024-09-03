deg_to_dms <- function(deg, type = 'cat', digit = 5) {

  DEG <- 1
  MIN <- 60
  SEC <- 3600
  sign <- function(x) ifelse(x >= 0, 1, -1)
  deg_sign <- function(deg) ifelse(deg >= 0, "+", "-")
  SIGN <- " "

  if(any(deg < -90 | deg > 90)) {
    stop("All degree values should be greater than -90\u00B0 or less than 90\u00B0")
  }

  df <- tibble::tibble(deg) |>
    dplyr::mutate(deg_sign = sign(deg),
                  deg = abs(deg),
                  DEG = floor(deg),
                  MIN = floor((deg - DEG) * 60),
                  SEC = (deg - DEG - MIN/60) * 3600,
                  SEC = dplyr::case_when(SEC <= 0 ~ 0,
                                         SEC > 60 ~ 60,
                                         .default = SEC),
                  MIN = dplyr::case_when(SEC == 60 ~ MIN + 1,
                                         MIN == 60 ~ 0,
                                         .default = MIN),
                  DEG = dplyr::case_when(MIN == 60 ~ DEG + 1,
                                         .default = DEG),
                  SEC = round(SEC, digits = digit),
                  SIGN = dplyr::if_else(deg_sign == -1, "-", "+"),
                  output1 = paste0(SIGN, stringr::str_c(DEG, MIN, SEC, sep = ":")),
                  output2 = paste0(SIGN, DEG, "\u00B0", MIN, "'", SEC, '"'))

  if(type == 'cat') {
    return(cat(df$output2))
  }
  if(type == 'mat') {
    return(df |> dplyr::select(SIGN, DEG, MIN, SEC) |> as.matrix())
  }
}

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
    dplyr::mutate(sign = sign(d),
           deg = abs(d) + (m / 60) + (s / 3600),
           deg = round(deg, digit) * sign)
  return(df |> dplyr::pull(deg))
}



