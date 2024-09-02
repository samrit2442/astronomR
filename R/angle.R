deg_to_dms <- function(deg, type, digit = 5) {

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

