# Reference values are from Meeus, J. (1998), Astronomical Algorithms, 2nd ed.
# (chapters 7, 12 and 13) unless noted otherwise. Julian Days are about
# 2.4e6, so they are compared with absolute tolerances.

# Smallest angle between two azimuths [deg]
az_dist <- function(a, b) abs((a - b + 180) %% 360 - 180)

# Local mean sidereal time [h] from the documented GMST formula; an object
# with this right ascension is on the meridian.
lst_hours <- function(datetime, lon) {
  gmst <- (18.697374558 + 24.06570982441908 *
             (julian_day(datetime) - 2451545.0)) %% 24
  (gmst + lon / 15) %% 24
}

t_equinox <- as.POSIXct("2024-03-20 03:06:00", tz = "UTC")


# ---- julian_day() ----------------------------------------------------------

test_that("julian_day() gives J2000.0 and Meeus Example 7.a", {
  # 2000 January 1.5 -> JD 2451545.0
  jd <- julian_day(as.POSIXct("2000-01-01 12:00:00", tz = "UTC"))
  expect_lt(abs(jd - 2451545.0), 1e-9)
  # Example 7.a: 1957 October 4.81 = 19:26:24 UT -> JD 2436116.31. The 0.3.0
  # internal version dropped the seconds and came out 24 s (2.8e-4 day) early.
  jd <- julian_day(as.POSIXct("1957-10-04 19:26:24", tz = "UTC"))
  expect_lt(abs(jd - 2436116.31), 1e-8)
  # The Unix epoch 1970-01-01 0h UTC is JD 2440587.5
  expect_identical(julian_day(as.POSIXct("1970-01-01", tz = "UTC")), 2440587.5)
})

test_that("julian_day() reproduces Meeus's Gregorian-calendar examples", {
  # Examples listed in Meeus (1998), chapter 7
  ref <- c("1999-01-01 00:00:00" = 2451179.5, "1987-01-27 00:00:00" = 2446822.5,
           "1987-06-19 12:00:00" = 2446966.0, "1988-01-27 00:00:00" = 2447187.5,
           "1988-06-19 12:00:00" = 2447332.0, "1900-01-01 00:00:00" = 2415020.5,
           "1600-01-01 00:00:00" = 2305447.5, "1600-12-31 00:00:00" = 2305812.5)
  jd <- julian_day(as.POSIXct(names(ref), tz = "UTC"))
  expect_length(jd, length(ref))
  expect_lt(max(abs(jd - unname(ref))), 1e-9)
})

test_that("julian_day() counts seconds", {
  # One second is 1/86400 day; a JD near 2.46e6 resolves about 4.7e-10 day.
  jd0 <- julian_day(t_equinox)
  expect_lt(abs((julian_day(t_equinox + 1) - jd0) - 1 / 86400), 1e-9)
  expect_lt(abs((julian_day(t_equinox + 59) - jd0) * 86400 - 59), 1e-4)
  expect_lt(abs((julian_day(t_equinox + 0.25) - jd0) * 86400 - 0.25), 1e-4)
  # 2024-03-20 03:06:00 UTC = 2024 March 20.129167 -> JD 2460389.629167
  expect_lt(abs(jd0 - (2460389.5 + (3 + 6 / 60) / 24)), 1e-9)
})

test_that("julian_day() does not depend on the display time zone", {
  skip_if_not("America/New_York" %in% OlsonNames(),
              "time zone America/New_York is not available")
  t_utc <- as.POSIXct("2024-07-04 16:00:00", tz = "UTC")
  t_ny <- t_utc
  attr(t_ny, "tzone") <- "America/New_York"  # same instant shown as 12:00 EDT
  expect_identical(julian_day(t_ny), julian_day(t_utc))
  # 2024 July 4, 16h UT = JD 2460495.5 + 16/24
  expect_lt(abs(julian_day(t_ny) - (2460495.5 + 16 / 24)), 1e-9)
  # Clock times read in New York: EDT is UTC-4 in summer, EST is UTC-5 in winter
  expect_identical(
    julian_day(as.POSIXct("2024-07-04 12:00:00", tz = "America/New_York")),
    julian_day(t_utc)
  )
  expect_identical(
    julian_day(as.POSIXlt("2024-01-15 07:00:00", tz = "America/New_York")),
    julian_day(as.POSIXct("2024-01-15 12:00:00", tz = "UTC"))
  )
})

test_that("julian_day() accepts Date, character and POSIXlt input", {
  # A non-UTC session time zone, so that reading strings or Dates as local
  # time would fail here even on a UTC machine
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "Asia/Tokyo")
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz),
          add = TRUE)
  # A Date is midnight UTC: 2000 January 1.0 = JD 2451544.5
  expect_identical(julian_day(as.Date("2000-01-01")), 2451544.5)
  # Character input is read as UTC
  expect_lt(abs(julian_day("2000-01-01 12:00:00") - 2451545.0), 1e-9)
  expect_lt(abs(julian_day("1957-10-04 19:26:24") - 2436116.31), 1e-8)
  expect_identical(julian_day("2000-01-01"), 2451544.5)
  lt <- as.POSIXlt("1957-10-04 19:26:24", tz = "UTC")
  expect_lt(abs(julian_day(lt) - 2436116.31), 1e-8)
})

test_that("julian_day() reads each string on its own, as UTC", {
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "Asia/Tokyo")
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz),
          add = TRUE)
  j2000 <- 2451545.0  # 2000-01-01 12:00:00 UTC
  expect_lt(abs(julian_day("2000-01-01T12:00:00") - j2000), 1e-9)
  expect_lt(abs(julian_day("2000-01-01T12:00:00Z") - j2000), 1e-9)
  expect_lt(abs(julian_day("2000/01/01 12:00") - j2000), 1e-9)
  expect_lt(abs(julian_day("2000-01-01 12:00:00.5") - (j2000 + 0.5 / 86400)),
            1e-9)
  # Dates and date-times mixed in one vector keep their times of day
  expect_lt(max(abs(julian_day(c("2000-01-01", "2000-01-01T12:00", NA)) -
                      c(j2000 - 0.5, j2000, NA)), na.rm = TRUE), 1e-9)
  expect_true(is.na(julian_day(c("2000-01-01", NA))[2]))

  msg <- "'datetime' strings must look like \"2000-01-01 12:00:00\""
  expect_error(julian_day("2000-01-01 12:00:00+05:30"), msg, fixed = TRUE)
  expect_error(julian_day("2000-01-01T12:00:00-0500"), msg, fixed = TRUE)
  expect_error(julian_day("04/10/1987"), msg, fixed = TRUE)
  expect_error(julian_day("not a date"), msg, fixed = TRUE)
  expect_error(julian_day(c("2000-01-01", "yesterday")), msg, fixed = TRUE)
  impossible <- "'datetime' contains an impossible date or time."
  expect_error(julian_day("2000-02-30"), impossible, fixed = TRUE)
  expect_error(julian_day("2000-01-01 25:00"), impossible, fixed = TRUE)
})

test_that("julian_day() treats an all-NA logical vector as missing", {
  expect_identical(julian_day(NA), NA_real_)
  expect_identical(julian_day(c(NA, NA)), c(NA_real_, NA_real_))
  expect_identical(julian_day(logical(0)), numeric(0))
})

test_that("julian_day() is vectorized and passes NA through", {
  x <- as.POSIXct(c("2000-01-01 12:00:00", NA, "1957-10-04 19:26:24"),
                  tz = "UTC")
  jd <- julian_day(x)
  expect_length(jd, 3)
  expect_true(is.na(jd[2]))
  expect_lt(max(abs(jd[c(1, 3)] - c(2451545.0, 2436116.31))), 1e-8)
  expect_identical(julian_day(c("2000-01-01 12:00:00", NA))[2], NA_real_)
  expect_identical(julian_day(as.Date(c(NA, "2000-01-01"))),
                   c(NA, 2451544.5))
  expect_identical(julian_day(as.POSIXct(character(0), tz = "UTC")),
                   numeric(0))
})

test_that("julian_day() rejects unsupported input", {
  msg <- "'datetime' must be a POSIXct, POSIXlt, Date or character vector."
  expect_error(julian_day(2451545), msg, fixed = TRUE)
  expect_error(julian_day(946728000L), msg, fixed = TRUE)
  expect_error(julian_day(factor("2000-01-01")), msg, fixed = TRUE)
  expect_error(julian_day(TRUE), msg, fixed = TRUE)
  expect_error(julian_day(list(t_equinox)), msg, fixed = TRUE)
  expect_error(julian_day(NULL), msg, fixed = TRUE)
  expect_error(julian_day("not a date"),
               "'datetime' strings must look like", fixed = TRUE)
})


# ---- radec_to_altaz() ------------------------------------------------------

test_that("radec_to_altaz() reproduces the README example", {
  # Values printed in the README, computed by the 0.3.0 internal code
  res <- radec_to_altaz(16.695, 36.466667, 52.5, -1.9166667,
                        as.POSIXct("1998-08-10 23:10:00", tz = "UTC"))
  expect_s3_class(res, "data.frame")
  expect_named(res, c("altitude", "azimuth"))
  expect_identical(nrow(res), 1L)
  expect_lt(abs(res$altitude - 49.1688687197424), 1e-9)
  expect_lt(abs(res$azimuth - 269.146669462321), 1e-9)
})

test_that("radec_to_altaz() agrees with Meeus Example 13.b", {
  # Venus from the US Naval Observatory, 1987 April 10, 19:21:00 UT.
  # Meeus: A = 68.0337 deg from South (248.0337 from North), h = 15.1249 deg.
  ra <- 23 + 9 / 60 + 16.641 / 3600         # 23h09m16.641s
  dec <- -(6 + 43 / 60 + 11.61 / 3600)      # -6d43m11.61s
  lat <- 38 + 55 / 60 + 17 / 3600           # +38d55m17s
  lon <- -(77 + 3 / 60 + 56 / 3600)         # 77d03m56s West
  t <- as.POSIXct("1987-04-10 19:21:00", tz = "UTC")
  res <- radec_to_altaz(ra, dec, lat, lon, t)
  # Meeus uses the apparent sidereal time; the mean one used here differs
  # by 0.2366 s of time, i.e. about 0.001 deg of hour angle.
  expect_lt(abs(res$altitude - 15.1249), 0.002)
  expect_lt(abs(res$azimuth - 248.0337), 0.002)
  expect_identical(radec_to_altaz(ra, dec, lat, lon, "1987-04-10 19:21:00"),
                   res)

  # Meeus's hour angle (64.352133 deg) uses the apparent sidereal time
  # (mean - 0.2366 s, Example 12.b) and the longitude 5h08m15.7s. With the
  # same sidereal time and longitude his rounded results are reproduced.
  t_app <- t - 0.2366 / 1.00273790935       # sidereal s -> UT s
  res_app <- radec_to_altaz(ra, dec, lat, -(5 + 8 / 60 + 15.7 / 3600) * 15,
                            t_app)
  expect_lt(abs(res_app$altitude - 15.1249), 1e-4)
  expect_lt(abs(res_app$azimuth - 248.0337), 1e-4)
})

test_that("radec_to_altaz() uses the mean sidereal time of Meeus 12.a/12.b", {
  # An object whose RA equals the local mean sidereal time is on the
  # meridian: from Greenwich (lon 0, lat 51.4769) with dec 0 it is due South
  # at altitude 90 - 51.4769. Meeus Example 12.a: 1987 April 10, 0h UT ->
  # 13h10m46.3668s; Example 12.b: 19h21m00s UT -> 8h34m57.0896s.
  ra <- c(13 + 10 / 60 + 46.3668 / 3600, 8 + 34 / 60 + 57.0896 / 3600)
  t <- as.POSIXct(c("1987-04-10 00:00:00", "1987-04-10 19:21:00"), tz = "UTC")
  res <- radec_to_altaz(ra, 0, 51.4769, 0, t)
  # 1e-4 deg of azimuth here corresponds to about 0.02 s of sidereal time
  expect_lt(max(abs(res$azimuth - 180)), 1e-4)
  expect_lt(max(abs(res$altitude - (90 - 51.4769))), 1e-9)
})

test_that("radec_to_altaz() gives a finite azimuth on the meridian", {
  # On the meridian the 0.3.0 acos() formula returned NaN for several of
  # these declinations (e.g. -30, 30, 50 and 60 deg).
  t <- as.POSIXct("1987-04-10 19:21:00", tz = "UTC")
  lat <- 38 + 55 / 60 + 17 / 3600
  lon <- -(77 + 3 / 60 + 56 / 3600)
  dec <- seq(-80, 80, by = 10)
  expect_no_warning(res <- radec_to_altaz(lst_hours(t, lon), dec, lat, lon, t))
  expect_false(anyNA(res$azimuth))
  # Due South when the object passes south of the zenith, due North otherwise
  expect_lt(max(az_dist(res$azimuth, ifelse(dec < lat, 180, 0))), 1e-9)
  # Upper culmination: altitude = 90 - |lat - dec|
  expect_lt(max(abs(res$altitude - (90 - abs(lat - dec)))), 1e-9)
})

test_that("radec_to_altaz() clamps the altitude at the zenith and nadir", {
  # For these latitudes sin(alt) rounds to just above 1 at the zenith
  # (dec = lat on the meridian) and just below -1 at the nadir.
  lat <- c(-87.5, -12, -8, -5.5, -2.5, 2.5, 5.5, 8, 12, 87.5)
  lst <- lst_hours(t_equinox, 0)
  # Without the clamp asin() would give NaN with a warning. The 1e-6
  # tolerance allows a libm that rounds sin(alt) to 1 - 2^-53 instead.
  expect_no_warning(zen <- radec_to_altaz(lst, lat, lat, 0, t_equinox))
  expect_lt(max(abs(zen$altitude - 90)), 1e-6)
  expect_false(anyNA(zen$azimuth))
  expect_no_warning(
    nad <- radec_to_altaz((lst + 12) %% 24, -lat, lat, 0, t_equinox)
  )
  expect_lt(max(abs(nad$altitude + 90)), 1e-6)
  expect_false(anyNA(nad$azimuth))
})

test_that("radec_to_altaz() measures azimuth from North through East", {
  lst <- lst_hours(t_equinox, 0)
  # An object on the celestial equator rises due East and sets due West at
  # any latitude: 6 h before / after transit it is on the horizon.
  lat <- c(-60, -20, 0, 20, 45, 70)
  east <- radec_to_altaz((lst + 6) %% 24, 0, lat, 0, t_equinox)
  west <- radec_to_altaz((lst - 6) %% 24, 0, lat, 0, t_equinox)
  expect_lt(max(abs(c(east$altitude, west$altitude))), 1e-9)
  expect_lt(max(abs(east$azimuth - 90)), 1e-9)
  expect_lt(max(abs(west$azimuth - 270)), 1e-9)

  # Circumpolar star (dec 80) from lat 52.5: due North at both culminations,
  # at 90 - (80 - 52.5) = 62.5 and 52.5 - (90 - 80) = 42.5 deg.
  up <- radec_to_altaz(lst, 80, 52.5, 0, t_equinox)
  low <- radec_to_altaz((lst + 12) %% 24, 80, 52.5, 0, t_equinox)
  expect_lt(abs(up$altitude - 62.5), 1e-9)
  expect_lt(abs(low$altitude - 42.5), 1e-9)
  expect_lt(az_dist(up$azimuth, 0), 1e-9)
  expect_lt(az_dist(low$azimuth, 0), 1e-9)

  # Southern sky (lat -33.87): dec -60 culminates due South at
  # 33.87 + 30 = 63.87 deg and 33.87 - 30 = 3.87 deg.
  up <- radec_to_altaz(lst, -60, -33.87, 0, t_equinox)
  low <- radec_to_altaz((lst + 12) %% 24, -60, -33.87, 0, t_equinox)
  expect_lt(abs(up$altitude - 63.87), 1e-9)
  expect_lt(abs(low$altitude - 3.87), 1e-9)
  expect_lt(abs(up$azimuth - 180), 1e-9)
  expect_lt(abs(low$azimuth - 180), 1e-9)

  # Before transit (HA < 0) the object is in the East (azimuth in (0, 180));
  # the path is symmetric about the meridian.
  ha <- c(1, 2.5, 4, 7)
  e <- radec_to_altaz((lst + ha) %% 24, 20, 45, 0, t_equinox)
  w <- radec_to_altaz((lst - ha) %% 24, 20, 45, 0, t_equinox)
  expect_true(all(e$azimuth > 0 & e$azimuth < 180))
  expect_lt(max(abs(e$altitude - w$altitude)), 1e-9)
  expect_lt(max(abs(e$azimuth + w$azimuth - 360)), 1e-9)

  # The celestial pole stays at altitude = latitude, due North
  pole <- radec_to_altaz(seq(0, 23, by = 1), 90, 45, 0, t_equinox)
  expect_lt(max(abs(pole$altitude - 45)), 1e-9)
  expect_lt(max(az_dist(pole$azimuth, 0)), 1e-9)
  expect_true(all(pole$azimuth >= 0 & pole$azimuth < 360))
})

test_that("radec_to_altaz() keeps the azimuth in [0, 360)", {
  g <- expand.grid(ra = seq(0, 23.75, by = 0.25),
                   dec = c(-90, -60, -1e-9, 0, 30, 89.999, 90),
                   lat = c(-90, -45, 0, 45, 90))
  res <- radec_to_altaz(g$ra, g$dec, g$lat, 0, t_equinox)
  expect_identical(nrow(res), nrow(g))
  expect_false(anyNA(res))
  expect_true(all(res$azimuth >= 0 & res$azimuth < 360))
  expect_true(all(res$altitude >= -90 & res$altitude <= 90))
})

test_that("radec_to_altaz() vectorizes over time like element-wise calls", {
  times <- seq(as.POSIXct("2024-10-02 00:00:00", tz = "UTC"),
               as.POSIXct("2024-10-03 00:00:00", tz = "UTC"), by = "20 mins")
  track <- radec_to_altaz(78.634467 / 15, -8.20164, 43.1566, -77.6088, times)
  expect_identical(nrow(track), length(times))
  one_by_one <- do.call(rbind, lapply(seq_along(times), function(i) {
    radec_to_altaz(78.634467 / 15, -8.20164, 43.1566, -77.6088, times[i])
  }))
  expect_identical(track, one_by_one)
  # Rigel culminates at 90 - (43.1566 + 8.20164) = 38.64176 deg
  expect_lte(max(track$altitude), 38.64176 + 1e-9)
  expect_gt(max(track$altitude), 38.5)
  expect_lt(min(track$altitude), -50)
})

test_that("radec_to_altaz() vectorizes over every argument", {
  ra <- c(0, 6, 12, 18.5)
  dec <- c(-45, 0, 30, 89)
  lat <- c(-30, 0, 45, 60)
  lon <- c(-120, 0, 15, 200)
  times <- t_equinox + c(0, 3600, 7200, 86400)
  vec <- radec_to_altaz(ra, dec, lat, lon, times)
  each <- do.call(rbind, Map(radec_to_altaz, ra, dec, lat, lon, as.list(times)))
  expect_identical(vec, each)
  # length-1 arguments are recycled
  vec1 <- radec_to_altaz(ra, 10, 45, -30, t_equinox)
  each1 <- do.call(rbind, lapply(ra, radec_to_altaz, dec = 10, lat = 45,
                                 lon = -30, datetime = t_equinox))
  expect_identical(vec1, each1)
})

test_that("radec_to_altaz() accepts every date-time type and any longitude", {
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "Asia/Tokyo")
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz),
          add = TRUE)
  ref <- radec_to_altaz(5, 20, 45, 10, as.POSIXct("2024-03-20", tz = "UTC"))
  expect_identical(radec_to_altaz(5, 20, 45, 10, as.POSIXlt("2024-03-20",
                                                            tz = "UTC")), ref)
  expect_identical(radec_to_altaz(5, 20, 45, 10, as.Date("2024-03-20")), ref)
  expect_identical(radec_to_altaz(5, 20, 45, 10, "2024-03-20 00:00:00"), ref)
  # Longitudes 360 deg apart are the same place
  same <- radec_to_altaz(5, 20, 45, c(-350, 370, 730), as.Date("2024-03-20"))
  expect_lt(max(abs(same$altitude - ref$altitude)), 1e-9)
  expect_lt(max(az_dist(same$azimuth, ref$azimuth)), 1e-9)
})

test_that("radec_to_altaz() returns NA rows for missing input", {
  res <- radec_to_altaz(c(1, NA, 3), c(0, 0, NA), 45, 0, t_equinox)
  expect_identical(nrow(res), 3L)
  expect_false(anyNA(res[1, ]))
  expect_true(all(is.na(res[2:3, ])))
  res_ll <- radec_to_altaz(1, 0, c(45, NA, 45), c(0, 0, NA), t_equinox)
  expect_false(anyNA(res_ll[1, ]))
  expect_true(all(is.na(res_ll[2:3, ])))
  # A bare NA is logical; it counts as a missing number, as in deg_to_rad()
  expect_true(all(is.na(radec_to_altaz(NA, 0, 45, 0, t_equinox))))
  expect_true(all(is.na(radec_to_altaz(1, NA, NA, NA, t_equinox))))
  expect_true(all(is.na(radec_to_altaz(1, 0, 45, 0, NA))))
  res_t <- radec_to_altaz(1, 0, 45, 0, c(t_equinox, NA))
  expect_true(all(is.na(res_t[2, ])))
  expect_equal(unlist(res_t[1, ]),
               unlist(radec_to_altaz(1, 0, 45, 0, t_equinox)))
  empty <- radec_to_altaz(numeric(0), 0, 45, 0, t_equinox)
  expect_s3_class(empty, "data.frame")
  expect_named(empty, c("altitude", "azimuth"))
  expect_identical(nrow(empty), 0L)
})

test_that("radec_to_altaz() validates its arguments", {
  t <- t_equinox
  expect_error(radec_to_altaz("12", 0, 45, 0, t), "'ra' must be numeric.",
               fixed = TRUE)
  ra_msg <- "'ra' must be in hours, at least 0 and less than 24."
  expect_error(radec_to_altaz(24, 0, 45, 0, t), ra_msg, fixed = TRUE)
  expect_error(radec_to_altaz(-0.1, 0, 45, 0, t), ra_msg, fixed = TRUE)
  expect_error(radec_to_altaz(c(1, 250), 0, 45, 0, t), ra_msg, fixed = TRUE)
  expect_error(radec_to_altaz(1, "0", 45, 0, t), "'dec' must be numeric.",
               fixed = TRUE)
  dec_msg <- "'dec' must be between -90 and 90 degrees."
  expect_error(radec_to_altaz(1, 90.5, 45, 0, t), dec_msg, fixed = TRUE)
  expect_error(radec_to_altaz(1, c(0, -91), 45, 0, t), dec_msg, fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, TRUE, 0, t), "'lat' must be numeric.",
               fixed = TRUE)
  lat_msg <- "'lat' must be between -90 and 90 degrees."
  expect_error(radec_to_altaz(1, 0, 91, 0, t), lat_msg, fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, c(45, -90.1), 0, t), lat_msg,
               fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, 45, "W", t), "'lon' must be numeric.",
               fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, 45, Inf, t), "'lon' must be finite.",
               fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, 45, c(0, -Inf), t),
               "'lon' must be finite.", fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, 45, 0, 2451545),
               "'datetime' must be a POSIXct, POSIXlt, Date or character",
               fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, 45, 0, "yesterday"),
               "'datetime' strings must look like", fixed = TRUE)
  len_msg <- paste("'ra', 'dec', 'lat', 'lon' and 'datetime' must have",
                   "length 1 or a common length.")
  expect_error(radec_to_altaz(c(1, 2), c(0, 10, 20), 45, 0, t), len_msg,
               fixed = TRUE)
  expect_error(radec_to_altaz(c(1, 2, 3), 0, 45, 0, c(t, t + 60)), len_msg,
               fixed = TRUE)
  expect_error(radec_to_altaz(1, 0, c(10, 20), c(0, 1, 2), t), len_msg,
               fixed = TRUE)
  expect_error(radec_to_altaz(numeric(0), c(0, 10), 45, 0, t), len_msg,
               fixed = TRUE)
  # Range boundaries are accepted
  expect_no_error(radec_to_altaz(c(0, 23.999999), c(-90, 90), c(90, -90),
                                 c(-180, 540), t))
})
