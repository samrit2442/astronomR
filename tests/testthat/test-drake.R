# Tier texts of drake_equation()$interpretation, from lowest to highest N.
# The tier boundaries are N = 0.001, 1, 10, 1000 and 10000.
tier_text <- c(
  paste0(
    "Effectively zero. Under these parameters, the chance of even one ",
    "communicating civilisation existing in the Milky Way at any given ",
    "moment is vanishingly small. We are, in all likelihood, alone."
  ),
  paste0(
    "Less than one expected civilisation. Intelligent, communicating life ",
    "is exceedingly rare; any civilisation that does arise is probably ",
    "isolated by enormous distances or time."
  ),
  paste0(
    "Only a handful of civilisations may exist simultaneously across the ",
    "galaxy. Contact is theoretically possible but the vast distances make ",
    "it extraordinarily difficult."
  ),
  paste0(
    "A modest number of civilisations. The galaxy could harbour detectable ",
    "signals, but they remain rare enough that targeted searches would be ",
    "needed to find them."
  ),
  paste0(
    "Thousands of civilisations. The galaxy is reasonably well-populated ",
    "with intelligent life; persistent, wide-sky surveys have a realistic ",
    "chance of detecting a signal."
  ),
  paste0(
    "The galaxy is teeming with communicating civilisations. Under these ",
    "optimistic parameters, interstellar contact may be almost inevitable ",
    "given sufficient technology and patience."
  )
)

# Expected interpretation: "N ~ <label>: <tier text>", where "~" stands for
# the almost-equal sign U+2248
interp <- function(label, tier) {
  paste0("N ", intToUtf8(0x2248), " ", label, ": ", tier_text[tier])
}

# With the other six factors equal to 1, N is exactly R_star
drake_at <- function(N) {
  drake_equation(R_star = N, fp = 1, ne = 1, fl = 1, fi = 1, fc = 1, L = 1)
}

# Interpretation with the "N ~ <label>: " prefix removed
tier_of <- function(N) {
  sub("^[^:]*: ", "", drake_at(N)$interpretation)
}


test_that("default parameters give N = 60", {
  # Hand calculation: 1.5 * 1 * 0.4 * 1 * 1 * 0.1 * 1000 = 60
  # (60.000000000000014 in floating point, hence expect_equal)
  expect_equal(drake_equation()$N, 60)
})

test_that("N matches published and hand-calculated values", {
  # Drake's 1961 Green Bank estimates: the low and high ends of his ranges
  # (R* = 1, fp = 0.2-0.5, ne = 1-5, fl = fi = 1, fc = 0.1-0.2,
  # L = 1000-1e8) give N = 20 and N = 5e7
  expect_equal(
    drake_equation(R_star = 1, fp = 0.2, ne = 1, fl = 1, fi = 1,
                   fc = 0.1, L = 1000)$N,
    20
  )
  expect_equal(
    drake_equation(R_star = 1, fp = 0.5, ne = 5, fl = 1, fi = 1,
                   fc = 0.2, L = 1e8)$N,
    5e7
  )
  # Help-page examples by hand: 1.5 * 0.4 * 0.01^3 * 304 = 1.824e-4 and
  # 3 * 0.9 * 0.5 * 0.5 * 0.1 * 0.1 * 10000 = 67.5
  expect_equal(
    drake_equation(R_star = 1.5, fp = 1, ne = 0.4, fl = 0.01, fi = 0.01,
                   fc = 0.01, L = 304)$N,
    1.824e-4
  )
  expect_equal(
    drake_equation(R_star = 3, fp = 0.9, ne = 0.5, fl = 0.5, fi = 0.1,
                   fc = 0.1, L = 10000)$N,
    67.5
  )
})

test_that("every argument enters N as a multiplicative factor", {
  # Halving any one of the seven default inputs halves N = 60 to 30
  defaults <- c(R_star = 1.5, fp = 1, ne = 0.4, fl = 1, fi = 1, fc = 0.1,
                L = 1000)
  for (p in names(defaults)) {
    args <- as.list(defaults)
    args[[p]] <- defaults[[p]] / 2
    expect_equal(do.call(drake_equation, args)$N, 30,
                 label = paste("N with", p, "halved"))
  }
})

test_that("a zero factor gives N = 0 and the lowest tier", {
  for (p in c("R_star", "fp", "ne", "fl", "fi", "fc", "L")) {
    res <- do.call(drake_equation, stats::setNames(list(0), p))
    expect_identical(res$N, 0, label = paste("N with", p, "= 0"))
    expect_identical(res$interpretation, interp("0", 1),
                     label = paste("interpretation with", p, "= 0"))
  }
})

test_that("the return value is a list with the documented elements", {
  res <- drake_equation()
  expect_type(res, "list")
  expect_named(res, c("N", "inputs", "equation", "interpretation"))
  expect_type(res$N, "double")
  expect_length(res$N, 1)
})

test_that("inputs are returned as a named numeric vector", {
  expect_identical(
    drake_equation()$inputs,
    c(R_star = 1.5, fp = 1, ne = 0.4, fl = 1, fi = 1, fc = 0.1, L = 1000)
  )
  expect_identical(
    drake_equation(R_star = 3, fp = 0.9, ne = 0.5, fl = 0.5, fi = 0.1,
                   fc = 0.1, L = 10000)$inputs,
    c(R_star = 3, fp = 0.9, ne = 0.5, fl = 0.5, fi = 0.1, fc = 0.1,
      L = 10000)
  )
  # Integer input is accepted and stored as double
  res <- drake_equation(L = 1000L)
  expect_identical(res$inputs[["L"]], 1000)
  expect_equal(res$N, 60)
})

test_that("the equation string substitutes the inputs and the result", {
  expect_identical(
    drake_equation()$equation,
    paste0(
      "N = R* x fp x ne x fl x fi x fc x L\n",
      "  = 1.5 x 1 x 0.4 x 1 x 1 x 0.1 x 1000\n",
      "  = 60"
    )
  )
  expect_identical(
    drake_equation(R_star = 3, fp = 0.9, ne = 0.5, fl = 0.5, fi = 0.1,
                   fc = 0.1, L = 10000)$equation,
    paste0(
      "N = R* x fp x ne x fl x fi x fc x L\n",
      "  = 3 x 0.9 x 0.5 x 0.5 x 0.1 x 0.1 x 10000\n",
      "  = 67.5"
    )
  )
  # Unlike the interpretation label, the inputs and the result are not
  # rounded: not to three significant figures, nor to the seven that print()
  # and format() use by default. Hand calculation with the other inputs at
  # their defaults: 1.234 * 0.4 * 0.1 * 1000 = 49.36 and
  # 1.23456789 * 0.4 * 0.1 * 1000 = 49.3827156
  expect_identical(
    drake_equation(R_star = 1.234)$equation,
    paste0(
      "N = R* x fp x ne x fl x fi x fc x L\n",
      "  = 1.234 x 1 x 0.4 x 1 x 1 x 0.1 x 1000\n",
      "  = 49.36"
    )
  )
  expect_identical(
    drake_equation(R_star = 1.23456789)$equation,
    paste0(
      "N = R* x fp x ne x fl x fi x fc x L\n",
      "  = 1.23456789 x 1 x 0.4 x 1 x 1 x 0.1 x 1000\n",
      "  = 49.3827156"
    )
  )
})

test_that("interpretation of the worked examples", {
  expect_identical(drake_equation()$interpretation, interp("60", 4))
  expect_identical(
    drake_equation(R_star = 1.5, fp = 1, ne = 0.4, fl = 0.01, fi = 0.01,
                   fc = 0.01, L = 304)$interpretation,
    interp("0.000182", 1)
  )
})

test_that("interpretation tier text on both sides of every boundary", {
  # N = 0.001
  expect_identical(drake_at(0.000999)$interpretation, interp("0.000999", 1))
  expect_identical(drake_at(0.001)$interpretation, interp("0.001", 2))
  # N = 1
  expect_identical(drake_at(0.999)$interpretation, interp("0.999", 2))
  expect_identical(drake_at(1)$interpretation, interp("1", 3))
  # N = 10
  expect_identical(drake_at(9.99)$interpretation, interp("9.99", 3))
  expect_identical(drake_at(10)$interpretation, interp("10", 4))
  # N = 1000
  expect_identical(drake_at(999)$interpretation, interp("999", 4))
  expect_identical(drake_at(1000)$interpretation, interp("1000", 5))
  # N = 10000
  expect_identical(drake_at(9990)$interpretation, interp("9990", 5))
  expect_identical(drake_at(10000)$interpretation, interp("10000", 6))
  expect_identical(drake_at(1e6)$interpretation, interp("1e+06", 6))
})

test_that("interpretation reports N to three significant figures", {
  # Tiers 1, 2, 4 and 6 span more than one decade, so each is checked in two
  # decades: no fixed number of decimal places (round(N, k)) gives the
  # three-significant-figure label in both. Tiers 3 ([1, 10)) and 5
  # ([1000, 10000)) span one decade each, where signif(N, 3) and rounding to
  # two decimals or to tens agree for every N.
  expect_identical(drake_at(0.000012345)$interpretation, interp("1.23e-05", 1))
  expect_identical(drake_at(0.00012345)$interpretation, interp("0.000123", 1))
  expect_identical(drake_at(0.0012345)$interpretation, interp("0.00123", 2))
  expect_identical(drake_at(0.12345)$interpretation, interp("0.123", 2))
  expect_identical(drake_at(1.2345)$interpretation, interp("1.23", 3))
  expect_identical(drake_at(12.345)$interpretation, interp("12.3", 4))
  expect_identical(drake_at(123.45)$interpretation, interp("123", 4))
  expect_identical(drake_at(1234.5)$interpretation, interp("1230", 5))
  expect_identical(drake_at(12345)$interpretation, interp("12300", 6))
  expect_identical(drake_at(123456)$interpretation, interp("123000", 6))
})

test_that("tiers switch exactly at each boundary (lower bound inclusive)", {
  eps <- 1e-9
  expect_identical(tier_of(0.001 * (1 - eps)), tier_text[1])
  expect_identical(tier_of(0.001 * (1 + eps)), tier_text[2])
  expect_identical(tier_of(1 - eps), tier_text[2])
  expect_identical(tier_of(1 + eps), tier_text[3])
  expect_identical(tier_of(10 * (1 - eps)), tier_text[3])
  expect_identical(tier_of(10 * (1 + eps)), tier_text[4])
  expect_identical(tier_of(1000 * (1 - eps)), tier_text[4])
  expect_identical(tier_of(1000 * (1 + eps)), tier_text[5])
  expect_identical(tier_of(10000 * (1 - eps)), tier_text[5])
  expect_identical(tier_of(10000 * (1 + eps)), tier_text[6])
})

test_that("fractions outside [0, 1] are rejected", {
  expect_error(drake_equation(fp = 2),
               "'fp' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(fl = -0.1),
               "'fl' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(fi = 1 + 1e-9),
               "'fi' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(fc = -1e-9),
               "'fc' must be a single number in [0, 1].", fixed = TRUE)
})

test_that("the end points 0 and 1 are accepted for fractions", {
  # 1.5 * 1 * 0.4 * 1 * 1 * 1 * 1000 = 600
  expect_equal(drake_equation(fp = 1, fl = 1, fi = 1, fc = 1)$N, 600)
  expect_identical(drake_equation(fp = 0, fl = 0, fi = 0, fc = 0)$N, 0)
})

test_that("negative rates, counts and lifetimes are rejected", {
  expect_error(drake_equation(R_star = -1),
               "'R_star' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(ne = -1),
               "'ne' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(L = -5),
               "'L' must be a single non-negative number.", fixed = TRUE)
})

test_that("non-numeric input is rejected", {
  expect_error(drake_equation(R_star = "a"),
               "'R_star' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(ne = "0.4"),
               "'ne' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(fc = "0.1"),
               "'fc' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(fi = TRUE),
               "'fi' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(L = list(1000)),
               "'L' must be a single non-negative number.", fixed = TRUE)
})

test_that("vector and zero-length input is rejected", {
  expect_error(drake_equation(R_star = c(1, 2)),
               "'R_star' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(fp = c(0.5, 0.6)),
               "'fp' must be a single number in [0, 1].", fixed = TRUE)
  expect_error(drake_equation(ne = c(0.4, 0.5)),
               "'ne' must be a single non-negative number.", fixed = TRUE)
  expect_error(drake_equation(L = numeric(0)),
               "'L' must be a single non-negative number.", fixed = TRUE)
})

test_that("missing values are rejected", {
  # Logical NA fails the is.numeric() check and gets the package message
  expect_error(drake_equation(R_star = NA),
               "'R_star' must be a single non-negative number.", fixed = TRUE)
  # NA_real_ and NaN pass is.numeric() and stop in the range comparison with
  # R's own (translatable) "missing value" error, so only the error itself
  # is asserted: N must never come back as NA
  expect_error(drake_equation(fp = NA_real_))
  expect_error(drake_equation(ne = NA_real_))
  expect_error(drake_equation(L = NaN))
})
