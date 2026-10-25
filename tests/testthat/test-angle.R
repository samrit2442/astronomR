# Reference values are hand calculations from the definitions:
# 1 degree = 60 arcmin = 3600 arcsec, and 1 hour of right ascension = 15
# degrees, so 1 degree = 4 minutes = 240 seconds of time.

deg_sym <- "\u00B0"

# Anchored pattern for one exact line of printed output. Only "+" and "." in
# the expected text below are regex metacharacters, so only those are escaped.
line_is <- function(x) paste0("^", gsub("([+.])", "\\\\\\1", x), "$")

dms_str <- function(sign, d, m, s) paste0(sign, d, deg_sym, m, "'", s, "\"")

bad_digits <- list(-1, 11, 2.5, c(1, 2), NA, NA_real_, "5", Inf)
digit_msg <- "'digit' must be a single whole number between 0 and 10."


# ---- deg_to_rad() / rad_to_deg() ---------------------------------------------

test_that("deg_to_rad and rad_to_deg are inverses", {
  expect_equal(deg_to_rad(180), pi)
  expect_equal(deg_to_rad(0), 0)
  expect_equal(rad_to_deg(pi), 180)
  expect_equal(rad_to_deg(0), 0)
  expect_equal(rad_to_deg(deg_to_rad(45)), 45)
  x <- c(-720, -90, -1e-8, 0, 33.3, 359.9)
  expect_equal(rad_to_deg(deg_to_rad(x)), x)
})

test_that("deg_to_rad is vectorized", {
  expect_equal(deg_to_rad(c(0, 90, 180)), c(0, pi / 2, pi))
  expect_equal(deg_to_rad(c(-30, 360)), c(-pi / 6, 2 * pi))
  expect_equal(rad_to_deg(c(-pi / 2, pi / 6, 2 * pi)), c(-90, 30, 360))
  expect_equal(deg_to_rad(90L), pi / 2)
})

test_that("deg_to_rad and rad_to_deg keep NA and reject non-numeric input", {
  expect_equal(deg_to_rad(c(90, NA)), c(pi / 2, NA))
  expect_identical(deg_to_rad(NA), NA_real_)
  expect_identical(rad_to_deg(NA), NA_real_)
  expect_identical(deg_to_rad(numeric(0)), numeric(0))
  expect_error(deg_to_rad("180"), "'deg' must be numeric.", fixed = TRUE)
  expect_error(deg_to_rad(TRUE), "'deg' must be numeric.", fixed = TRUE)
  expect_error(deg_to_rad(factor(10)), "'deg' must be numeric.", fixed = TRUE)
  expect_error(rad_to_deg("pi"), "'rad' must be numeric.", fixed = TRUE)
  expect_error(rad_to_deg(list(1)), "'rad' must be numeric.", fixed = TRUE)
})


# ---- deg_to_dms() ------------------------------------------------------------

test_that("deg_to_dms carries rounded seconds into minutes and degrees", {
  # 10.1 deg = 10 deg 6' 0" and 1.4 deg = 1 deg 24' 0" (the old code printed
  # 5'60" and 23'60"). 59.9999999" rounds to 60" at 5 decimals -> 1' 0".
  expect_output(deg_to_dms(10.1), line_is(dms_str("+", 10, 6, 0)))
  expect_output(deg_to_dms(1.4), line_is(dms_str("+", 1, 24, 0)))
  expect_output(deg_to_dms(-1.4), line_is(dms_str("-", 1, 24, 0)))
  expect_output(deg_to_dms(59.9999999 / 3600), line_is(dms_str("+", 0, 1, 0)))
  # 89 deg 59' 59.9999999" rounds up to exactly 90 deg.
  expect_output(deg_to_dms(90 - 1e-7 / 3600), line_is(dms_str("+", 90, 0, 0)))

  res <- deg_to_dms(c(10.1, 1.4, 59.9999999 / 3600), type = "mat")
  expect_equal(as.numeric(res[, "DEG"]), c(10, 1, 0))
  expect_equal(as.numeric(res[, "MIN"]), c(6, 24, 1))
  expect_equal(as.numeric(res[, "SEC"]), c(0, 0, 0))
})

test_that("deg_to_dms prints exact text, space separated, with a newline", {
  # 45.5042 deg: 0.5042 * 60 = 30.252', 0.252 * 60 = 15.12"
  out <- capture.output(res <- withVisible(deg_to_dms(c(-12.5, 45.5042))),
                        cat("next\n"))
  expect_identical(out, c(paste(dms_str("-", 12, 30, 0),
                                dms_str("+", 45, 30, "15.12")), "next"))
  expect_null(res$value)
  expect_false(res$visible)
  expect_output(deg_to_dms(45.5042, digit = 1),
                line_is(dms_str("+", 45, 30, "15.1")))
  # digit = 0: 10.5083 deg = 10 deg 30' 29.88" -> 30"
  expect_output(deg_to_dms(10.5083, digit = 0),
                line_is(dms_str("+", 10, 30, 30)))
  expect_output(deg_to_dms(10.5083, digit = 10),
                line_is(dms_str("+", 10, 30, "29.88")))
})

test_that("deg_to_dms prints seconds without scientific notation", {
  # 1e-8 deg = 3.6e-5" -> 0.00004" at 5 decimals (old code printed 4e-05)
  expect_output(deg_to_dms(1e-8), line_is(dms_str("+", 0, 0, "0.00004")))
  # -1e-9 deg = -3.6e-6"
  expect_output(deg_to_dms(-1e-9, digit = 10),
                line_is(dms_str("-", 0, 0, "0.0000036")))
})

test_that("deg_to_dms signs zero, negative zero and tiny negatives as +", {
  expect_output(deg_to_dms(0), line_is(dms_str("+", 0, 0, 0)))
  expect_output(deg_to_dms(-0), line_is(dms_str("+", 0, 0, 0)))
  expect_output(deg_to_dms(-1e-9), line_is(dms_str("+", 0, 0, 0)))
  expect_identical(unname(deg_to_dms(-1e-9, type = "mat")[, "SIGN"]), "+")
  # A negative angle smaller than one degree keeps its sign.
  expect_output(deg_to_dms(-0.5), line_is(dms_str("-", 0, 30, 0)))
})

test_that("deg_to_dms mat output has correct structure", {
  result <- deg_to_dms(45.5, type = "mat")
  expect_true(is.matrix(result))
  expect_type(result, "character")
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("SIGN", "DEG", "MIN", "SEC"))
  expect_identical(
    deg_to_dms(c(-12.5, 45.5042), type = "mat"),
    matrix(c("-", "+", "12", "45", "30", "30", "0", "15.12"), ncol = 4,
           dimnames = list(NULL, c("SIGN", "DEG", "MIN", "SEC")))
  )
  expect_identical(dim(deg_to_dms(numeric(0), type = "mat")), c(0L, 4L))
  expect_identical(deg_to_dms(10, type = "m"), deg_to_dms(10, type = "mat"))
})

test_that("deg_to_dms gives NA output for NA input", {
  expect_output(deg_to_dms(c(10, NA)), line_is(paste(dms_str("+", 10, 0, 0),
                                                     "NA")))
  expect_output(deg_to_dms(NA), "^NA$")
  res <- deg_to_dms(c(NA, -0.5, NaN), type = "mat")
  expect_true(all(is.na(res[c(1, 3), ])))
  expect_identical(unname(res[2, ]), c("-", "0", "30", "0"))
})

test_that("deg_to_dms: MIN and SEC stay below 60, within half a unit", {
  # A regular grid plus values just either side of every arcminute boundary
  # up to 2 degrees, where the old floor()-based split failed.
  edges <- (0:120) / 60
  x <- c(seq(-90, 90, by = 0.0137), edges - 1e-9, edges + 1e-9,
         -(edges + 1e-9), 10.1, 1.4, -1.4, 90 - 1e-12)
  x <- x[abs(x) <= 90]
  for (digit in 0:6) {
    res <- deg_to_dms(x, type = "mat", digit = digit)
    D <- as.numeric(res[, "DEG"])
    M <- as.numeric(res[, "MIN"])
    S <- as.numeric(res[, "SEC"])
    info <- paste("digit =", digit)
    expect_true(all(res[, "SIGN"] %in% c("+", "-")), info = info)
    expect_true(all(D %in% 0:90), info = info)
    expect_true(all(M %in% 0:59), info = info)
    expect_true(all(S >= 0 & S < 60), info = info)
    # Fixed notation, at most 'digit' decimals, no trailing zeros
    expect_true(all(grepl("^[0-9]+(\\.[0-9]*[1-9])?$", res[, "SEC"])),
                info = info)
    expect_true(all(nchar(sub("^[0-9]+\\.?", "", res[, "SEC"])) <= digit),
                info = info)
    back <- ifelse(res[, "SIGN"] == "-", -1, 1) * (D + M / 60 + S / 3600)
    # Half a unit of the last printed digit is 0.5 * 10^-digit arcsec.
    expect_lt(max(abs(back - x)) * 3600, 0.5 * 10^-digit + 1e-9)
  }
})

test_that("deg_to_dms errors on out-of-range input", {
  expect_error(deg_to_dms(91))
  expect_error(deg_to_dms(-91))
  msg <- "All degree values should be between -90"
  expect_error(deg_to_dms(c(0, 90.0001)), msg, fixed = TRUE)
  expect_error(deg_to_dms(c(NA, -95)), msg, fixed = TRUE)
  expect_error(deg_to_dms(Inf), msg, fixed = TRUE)
  expect_output(deg_to_dms(c(-90, 90)), line_is(paste(dms_str("-", 90, 0, 0),
                                                      dms_str("+", 90, 0, 0))))
})

test_that("deg_to_dms validates deg, type and digit", {
  expect_error(deg_to_dms("10"), "'deg' must be numeric.", fixed = TRUE)
  expect_error(deg_to_dms(TRUE), "'deg' must be numeric.", fixed = TRUE)
  expect_error(deg_to_dms(10, type = "foo"), "should be one of")
  expect_error(deg_to_dms(10, type = "cat2"), "should be one of")
  for (digit in bad_digits) {
    expect_error(deg_to_dms(10, digit = digit), digit_msg, fixed = TRUE)
  }
})


# ---- dms_to_deg() ------------------------------------------------------------

test_that("dms_to_deg gives correct result with numeric inputs", {
  # 12 + 34/60 + 56/3600 = 12.582222...
  expect_equal(dms_to_deg(d = 12, m = 34, s = 56), 12.58222)
  expect_equal(dms_to_deg(d = 0, m = 0, s = 0), 0)
  expect_equal(dms_to_deg(d = -12, m = 34, s = 56), -12.58222)
  expect_equal(dms_to_deg(12), 12)
  expect_equal(dms_to_deg(12, 30), 12.5)
  expect_equal(dms_to_deg(90), 90)
  expect_equal(dms_to_deg(-90), -90)
  expect_equal(dms_to_deg(12.25, 15), 12.5)
  expect_equal(dms_to_deg(12, 34, 56, digit = 3), 12.582)
  expect_equal(dms_to_deg(12, 34, 56, digit = 0), 13)
  expect_equal(dms_to_deg(12, 34, 56, digit = 10), 12.5822222222,
               tolerance = 1e-14)
})

test_that("dms_to_deg handles zero degrees and R's negative zero", {
  # The old code returned 0 for any angle under one degree.
  expect_equal(dms_to_deg(0, 30, 0), 0.5)
  expect_equal(dms_to_deg(-0, 30, 0), -0.5)
  expect_equal(dms_to_deg(-0, 0, 36), -0.01)
  expect_equal(dms_to_deg(0, 0, 36), 0.01)
  expect_equal(dms_to_deg(-1 * 0, 45), -0.75)
  expect_equal(dms_to_deg(c(-12, -0, 0), m = 30), c(-12.5, -0.5, 0.5))
})

test_that("dms_to_deg is vectorised with recycling", {
  # The old code failed with "the condition has length > 1".
  expect_equal(dms_to_deg(c(1, 2), c(0, 0), c(0, 0)), c(1, 2))
  # 45' 36" = 0.75 + 0.01 deg
  expect_equal(dms_to_deg(c(10, -20, -0), c(30, 15, 45), c(0, 0, 36)),
               c(10.5, -20.25, -0.76))
  expect_equal(dms_to_deg(c(1, 2, 3), 30), c(1.5, 2.5, 3.5))
  expect_equal(dms_to_deg(10, c(0, 30), c(36, 0)), c(10.01, 10.5))
  expect_identical(dms_to_deg(numeric(0)), numeric(0))
  expect_error(dms_to_deg(c(1, 2, 3), c(1, 2)),
               "'d', 'm' and 's' must have the same length (or length 1).",
               fixed = TRUE)
})

test_that("dms_to_deg keeps NA silently", {
  expect_identical(dms_to_deg(c(10, NA), 30), c(10.5, NA))
  expect_identical(dms_to_deg(10, NA), NA_real_)
  expect_identical(dms_to_deg(NA), NA_real_)
  expect_no_warning(res <- dms_to_deg(c(NA, "12d30m", NA_character_)))
  expect_identical(res, c(NA, 12.5, NA))
})

test_that("dms_to_deg errors on out-of-range inputs", {
  expect_error(dms_to_deg(d = 91, m = 0, s = 0))
  expect_error(dms_to_deg(d = 0, m = 60, s = 0))
  angle_msg <- "Angles must be between -90 and 90 degrees."
  expect_error(dms_to_deg(91), angle_msg, fixed = TRUE)
  expect_error(dms_to_deg(-90, 0, 1), angle_msg, fixed = TRUE)
  expect_error(dms_to_deg(90, 0.5), angle_msg, fixed = TRUE)
  expect_error(dms_to_deg(c(10, -95)), angle_msg, fixed = TRUE)
  m_msg <- "'m' must be between 0 and 60 (0 <= m < 60)."
  s_msg <- "'s' must be between 0 and 60 (0 <= s < 60)."
  expect_error(dms_to_deg(0, 60, 0), m_msg, fixed = TRUE)
  # The old code silently returned 11.5 here.
  expect_error(dms_to_deg(12, -30, 0), m_msg, fixed = TRUE)
  expect_error(dms_to_deg(12, c(0, NA, 75)), m_msg, fixed = TRUE)
  expect_error(dms_to_deg(12, 0, 60), s_msg, fixed = TRUE)
  expect_error(dms_to_deg(12, 0, -0.5), s_msg, fixed = TRUE)
})

test_that("dms_to_deg validates argument types and digit", {
  expect_error(dms_to_deg(TRUE), "'d' must be numeric or character.",
               fixed = TRUE)
  expect_error(dms_to_deg(factor("12")), "'d' must be numeric or character.",
               fixed = TRUE)
  expect_error(dms_to_deg(12, "30"), "'m' must be numeric.", fixed = TRUE)
  expect_error(dms_to_deg(12, 30, "1"), "'s' must be numeric.", fixed = TRUE)
  for (digit in bad_digits) {
    expect_error(dms_to_deg(10, digit = digit), digit_msg, fixed = TRUE)
  }
})

test_that("dms_to_deg parses the documented string formats", {
  expect_equal(dms_to_deg(dms_str("", 12, 34, 56)), 12.58222)
  # Zero-padded fields (the old parser gave 12.06667, 12 and 5.00833)
  expect_equal(dms_to_deg(dms_str("", 12, "04", "05")), 12.06806)
  expect_equal(dms_to_deg(dms_str("", 12, "00", 30)), 12.00833)
  expect_equal(dms_to_deg(dms_str("", "05", 30, "00")), 5.5)
  expect_equal(dms_to_deg(dms_str("-", 0, 30, "00")), -0.5)
  expect_equal(dms_to_deg("12d34m56s"), 12.58222)
  expect_equal(dms_to_deg("12D34M56S"), 12.58222)
  expect_equal(dms_to_deg("12:34:56"), 12.58222)
  expect_equal(dms_to_deg("12 34 56"), 12.58222)
  expect_equal(dms_to_deg(paste0("12", deg_sym, " 34' 56\"")), 12.58222)
  expect_equal(dms_to_deg("12\u00B034\u203256\u2033"), 12.58222)
  # 12 + 34/60 + 56.7/3600 = 12.582417
  expect_equal(dms_to_deg(dms_str("+", 12, 34, "56.7")), 12.58242)
  expect_equal(dms_to_deg(paste0("12", deg_sym, "34'")), 12.56667)
  expect_equal(dms_to_deg(paste0("12.5", deg_sym)), 12.5)
  expect_equal(dms_to_deg("12.5"), 12.5)
  expect_equal(dms_to_deg(paste0("-12", deg_sym, "30'")), -12.5)
  expect_equal(dms_to_deg(paste0("\u221212", deg_sym, "30'")), -12.5)
  expect_equal(dms_to_deg("  -12 30  "), -12.5)
  expect_equal(dms_to_deg(paste0("12", deg_sym, "30.5'")), 12.50833)
  expect_equal(dms_to_deg(dms_str("", 0, 0, 0)), 0)
  expect_equal(
    dms_to_deg(c("12d34m56s", "12:34:56", paste0("-0", deg_sym, "30'"))),
    c(12.58222, 12.58222, -0.5)
  )
  expect_equal(dms_to_deg(dms_str("", 12, 34, 56), digit = 2), 12.58)
})

test_that("dms_to_deg returns NA with one warning for unparsable strings", {
  x <- c(dms_str("", 12, 34, 56), "abc", "12x34", "",
         paste0("12.5", deg_sym, "30'"))
  w <- capture_warnings(res <- dms_to_deg(x))
  expect_length(w, 1)
  expect_match(w, "Could not parse 4 elements of 'd'", fixed = TRUE)
  expect_match(w, "\"abc\", \"12x34\", \"\", \"12.5", fixed = TRUE)
  expect_equal(res, c(12.58222, NA, NA, NA, NA))

  w <- capture_warnings(res <- dms_to_deg(c("1e2", "12h30m", "12--30")))
  expect_length(w, 1)
  expect_identical(res, rep(NA_real_, 3))

  w <- capture_warnings(res <- dms_to_deg(letters[1:7]))
  expect_length(w, 1)
  expect_match(w, "\"e\" and 2 more.", fixed = TRUE)
  expect_false(grepl("\"f\"", w, fixed = TRUE))
})

test_that("dms_to_deg string input errors", {
  msg <- "'m' and 's' must not be supplied when 'd' is a character string."
  expect_error(dms_to_deg(paste0("12", deg_sym), m = 30), msg, fixed = TRUE)
  expect_error(dms_to_deg("12", s = 0), msg, fixed = TRUE)
  expect_error(dms_to_deg(dms_str("", 95, "00", "00")),
               "Angles must be between -90 and 90 degrees.", fixed = TRUE)
  ms_msg <- "Minutes and seconds in 'd' must be less than 60."
  expect_error(dms_to_deg(dms_str("", 12, 75, "00")), ms_msg, fixed = TRUE)
  expect_error(dms_to_deg(c("12:30:00", "12:30:60")), ms_msg, fixed = TRUE)
})

test_that("deg_to_dms output converts back with dms_to_deg", {
  x <- c(-90, -45.5042, -12.5, -0.5, -1 / 3600, 0, 0.25, 12.582222, 45.5042,
         89.9999999, 90)
  tol <- 0.5e-5 / 3600 + 1e-10  # half a unit of the 5th decimal of seconds
  res <- deg_to_dms(x, type = "mat")
  # Pasting the columns into strings
  s <- paste0(res[, "SIGN"], res[, "DEG"], deg_sym, res[, "MIN"], "'",
              res[, "SEC"], "\"")
  expect_lt(max(abs(dms_to_deg(s, digit = 10) - x)), tol)
  s2 <- paste0(res[, "SIGN"], res[, "DEG"], "d", res[, "MIN"], "m",
               res[, "SEC"], "s")
  expect_lt(max(abs(dms_to_deg(s2, digit = 10) - x)), tol)
  # Numeric columns: the sign goes on the degrees (-1 * 0 is negative zero)
  sgn <- ifelse(res[, "SIGN"] == "-", -1, 1)
  back <- dms_to_deg(sgn * as.numeric(res[, "DEG"]), as.numeric(res[, "MIN"]),
                     as.numeric(res[, "SEC"]), digit = 10)
  expect_lt(max(abs(back - x)), tol)
  # The printed text parses back too
  for (v in x) {
    expect_lt(abs(dms_to_deg(capture.output(deg_to_dms(v)), digit = 10) - v),
              tol)
  }
})


# ---- deg_to_hms() ------------------------------------------------------------

test_that("deg_to_hms carries rounded seconds into minutes and hours", {
  # 20 deg = 80 min of time = 1h 20m 0s (the old code printed 1H19M60S)
  expect_output(deg_to_hms(20), "^1H20M0S$")
  # 177.74208 deg * 240 = 42658.0992 s = 11h 50m 58.0992s
  expect_output(deg_to_hms(177.74208), line_is("11H50M58.0992S"))
  expect_output(deg_to_hms(177.74208, digit = 3), line_is("11H50M58.099S"))
  # 59.9999999 s of time rounds to 1 min at 5 decimals
  expect_output(deg_to_hms(59.9999999 / 240), "^0H1M0S$")
  # Rounds up to 24h, which is shown as 0h
  expect_output(deg_to_hms(360 - 1e-12), "^0H0M0S$")
  res <- deg_to_hms(c(20, 59.9999999 / 240), type = "mat")
  expect_equal(unname(res[, "HRS"]), c(1, 0))
  expect_equal(unname(res[, "MIN"]), c(20, 1))
  expect_equal(unname(res[, "SEC"]), c(0, 0))
})

test_that("deg_to_hms wraps any real angle into [0, 360)", {
  expect_output(deg_to_hms(45), "^3H0M0S$")
  expect_output(deg_to_hms(0), "^0H0M0S$")
  expect_output(deg_to_hms(-0), "^0H0M0S$")
  expect_output(deg_to_hms(360), "^0H0M0S$")
  expect_output(deg_to_hms(-15), "^23H0M0S$")
  expect_output(deg_to_hms(375), "^1H0M0S$")
  # -720.5 %% 360 = 359.5 deg = 86280 s = 23h 58m
  expect_output(deg_to_hms(-720.5), "^23H58M0S$")
  expect_output(deg_to_hms(-1e-12), "^0H0M0S$")
})

test_that("deg_to_hms prints exact text, space separated, with a newline", {
  out <- capture.output(res <- withVisible(deg_to_hms(c(45, -15, NA))),
                        cat("next\n"))
  expect_identical(out, c("3H0M0S 23H0M0S NA", "next"))
  expect_null(res$value)
  expect_false(res$visible)
  # 1e-7 deg = 2.4e-5 s, printed in fixed notation
  expect_output(deg_to_hms(1e-7), line_is("0H0M0.00002S"))
})

test_that("deg_to_hms mat output has correct structure", {
  result <- deg_to_hms(45, type = "mat")
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("HRS", "MIN", "SEC"))
  expect_equal(
    deg_to_hms(c(45, 177.74208, NA), type = "mat"),
    matrix(c(3, 11, NA, 0, 50, NA, 0, 58.0992, NA), ncol = 3,
           dimnames = list(NULL, c("HRS", "MIN", "SEC")))
  )
  expect_true(all(is.na(deg_to_hms(NA, type = "mat"))))
  expect_identical(dim(deg_to_hms(numeric(0), type = "mat")), c(0L, 3L))
})

test_that("deg_to_hms: HRS 0-23, MIN and SEC below 60, within half a unit", {
  edges <- (0:96) / 4  # every minute of time (0.25 deg) up to 24 deg
  x <- c(seq(-720, 720, by = 0.0731), edges - 1e-9, edges + 1e-9,
         360 - 1e-12, -1e-12, 20, 177.74208)
  for (digit in 0:6) {
    res <- deg_to_hms(x, type = "mat", digit = digit)
    info <- paste("digit =", digit)
    expect_true(all(res[, "HRS"] %in% 0:23), info = info)
    expect_true(all(res[, "MIN"] %in% 0:59), info = info)
    expect_true(all(res[, "SEC"] >= 0 & res[, "SEC"] < 60), info = info)
    back <- 15 * (res[, "HRS"] + res[, "MIN"] / 60 + res[, "SEC"] / 3600)
    err <- (back - x + 180) %% 360 - 180  # difference modulo 360 deg
    # Half a unit of the last printed digit is 0.5 * 10^-digit s of time.
    expect_lt(max(abs(err)) * 240, 0.5 * 10^-digit + 1e-9)
  }
})

test_that("deg_to_hms validates deg, type and digit", {
  expect_error(deg_to_hms("45"), "'deg' must be numeric.", fixed = TRUE)
  expect_error(deg_to_hms(Inf), "'deg' must be finite.", fixed = TRUE)
  expect_error(deg_to_hms(c(1, -Inf)), "'deg' must be finite.", fixed = TRUE)
  expect_error(deg_to_hms(10, type = "txt"), "should be one of")
  for (digit in bad_digits) {
    expect_error(deg_to_hms(10, digit = digit), digit_msg, fixed = TRUE)
  }
})


# ---- hms_to_deg() ------------------------------------------------------------

test_that("hms_to_deg gives correct result with numeric inputs", {
  # 15 * (3 + 15/60 + 30/3600) = 48.875
  expect_equal(hms_to_deg(h = 3, m = 15, s = 30), 48.875)
  expect_equal(hms_to_deg(h = 0, m = 0, s = 0), 0)
  expect_equal(hms_to_deg(3), 45)
  expect_equal(hms_to_deg(3, 15), 48.75)
  expect_equal(hms_to_deg(24), 360)
  # 15 * (3 + 15/60 + 30.123/3600) = 48.8755125
  expect_equal(hms_to_deg(3, 15, 30.123), 48.87551)
  expect_equal(hms_to_deg(3, 15, 30.123, digit = 2), 48.88)
  expect_equal(hms_to_deg(11, 50, 58.09925), 177.74208)
  expect_equal(hms_to_deg(c(0, 6, 12, 18)), c(0, 90, 180, 270))
  expect_equal(hms_to_deg(c(1, 2), c(0, 30), 36), c(15.15, 37.65))
  expect_identical(hms_to_deg(c(NA, 3)), c(NA, 45))
  expect_identical(hms_to_deg(numeric(0)), numeric(0))
})

test_that("hms_to_deg errors on out-of-range or invalid numeric input", {
  expect_error(hms_to_deg(25, 0, 0), "'h' must be between 0 and 24.",
               fixed = TRUE)
  expect_error(hms_to_deg(-1), "'h' must be between 0 and 24.", fixed = TRUE)
  expect_error(hms_to_deg(c(1, NA, 30)), "'h' must be between 0 and 24.",
               fixed = TRUE)
  expect_error(hms_to_deg(3, 75, 0),
               "'m' must be between 0 and 60 (0 <= m < 60).", fixed = TRUE)
  expect_error(hms_to_deg(3, -1, 0),
               "'m' must be between 0 and 60 (0 <= m < 60).", fixed = TRUE)
  expect_error(hms_to_deg(3, 0, 60),
               "'s' must be between 0 and 60 (0 <= s < 60).", fixed = TRUE)
  expect_error(hms_to_deg(24, 30),
               "'h', 'm' and 's' must not add up to more than 24 hours.",
               fixed = TRUE)
  expect_error(hms_to_deg(TRUE), "'h' must be numeric or character.",
               fixed = TRUE)
  expect_error(hms_to_deg(3, "15"), "'m' must be numeric.", fixed = TRUE)
  expect_error(hms_to_deg(3, 15, "30"), "'s' must be numeric.", fixed = TRUE)
  expect_error(hms_to_deg(c(1, 2, 3), c(0, 1)),
               "'h', 'm' and 's' must have the same length (or length 1).",
               fixed = TRUE)
  for (digit in bad_digits) {
    expect_error(hms_to_deg(1, digit = digit), digit_msg, fixed = TRUE)
  }
})

test_that("hms_to_deg parses string input", {
  expect_equal(hms_to_deg(h = "03h15m30s"), 48.875)
  for (x in c("3h 15m 30s", "3H15M30S", "03:15:30", "3 15 30", "+3h15m30s",
              "  03h 15m 30s ", "3h15m30")) {
    expect_equal(hms_to_deg(x), 48.875, info = x)
  }
  # 15 * (3 + 15/60 + 30.5/3600) = 48.877083
  expect_equal(hms_to_deg("3h15m30.5s"), 48.87708)
  expect_equal(hms_to_deg("3h15m"), 48.75)
  expect_equal(hms_to_deg("03:15"), 48.75)
  expect_equal(hms_to_deg("3h"), 45)
  expect_equal(hms_to_deg("3"), 45)
  expect_equal(hms_to_deg("3.5h"), 52.5)
  expect_equal(hms_to_deg("24h00m00s"), 360)
  expect_equal(hms_to_deg("0h0m0s"), 0)
  expect_no_warning(res <- hms_to_deg(c("11h50m58.09925s", NA, "0:0:0")))
  expect_equal(res, c(177.74208, NA, 0))
})

test_that("hms_to_deg returns NA with one warning for unparsable strings", {
  w <- capture_warnings(res <- hms_to_deg(c("3h", "3x15", "abc", "3.5h15m",
                                            "3h15m30sx")))
  expect_length(w, 1)
  expect_match(w, "Could not parse 4 elements of 'h'", fixed = TRUE)
  expect_match(w, "\"3x15\", \"abc\", \"3.5h15m\", \"3h15m30sx\"",
               fixed = TRUE)
  expect_identical(res, c(45, NA, NA, NA, NA))
  w <- capture_warnings(res <- hms_to_deg("12d30m"))
  expect_length(w, 1)
  expect_match(w, "1 element of 'h'", fixed = TRUE)
  expect_identical(res, NA_real_)
})

test_that("hms_to_deg string input errors", {
  msg <- "'m' and 's' must not be supplied when 'h' is a character string."
  # The old code failed with an internal dplyr error here.
  expect_error(hms_to_deg("3h", 15, 0), msg, fixed = TRUE)
  expect_error(hms_to_deg("3h", s = 1), msg, fixed = TRUE)
  range_msg <- "Values in 'h' must be between 0 and 24 hours."
  expect_error(hms_to_deg("-3h"), range_msg, fixed = TRUE)
  expect_error(hms_to_deg("-0h30m"), range_msg, fixed = TRUE)
  expect_error(hms_to_deg("25h"), range_msg, fixed = TRUE)
  expect_error(hms_to_deg("24h30m"), range_msg, fixed = TRUE)
  ms_msg <- "Minutes and seconds in 'h' must be less than 60."
  expect_error(hms_to_deg("3h75m"), ms_msg, fixed = TRUE)
  expect_error(hms_to_deg("3h15m60s"), ms_msg, fixed = TRUE)
})

test_that("deg_to_hms output converts back with hms_to_deg", {
  x <- c(0, 1e-7, 20, 45, 177.74208, 300.123456, 359.9)
  tol <- 0.5e-5 / 240 + 1e-10  # half a unit of the 5th decimal of seconds
  res <- deg_to_hms(x, type = "mat")
  back <- hms_to_deg(res[, "HRS"], res[, "MIN"], res[, "SEC"], digit = 10)
  expect_lt(max(abs(back - x)), tol)
  for (v in x) {
    expect_lt(abs(hms_to_deg(capture.output(deg_to_hms(v)), digit = 10) - v),
              tol)
  }
})
