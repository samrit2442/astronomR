#' Julian Day of a Date-Time
#'
#' Converts date-times to Julian Days (JD): the continuous count of days, with
#' the fraction of the day, since noon Universal Time on 1 January 4713 BC of
#' the proleptic Julian calendar.
#'
#' The Julian Day is computed from the instant itself,
#' \deqn{JD = \frac{t}{86400} + 2440587.5,}{JD = t / 86400 + 2440587.5,}
#' where \eqn{t} is the number of seconds since 1970-01-01 00:00:00 UTC, i.e.
#' \code{as.numeric(as.POSIXct(datetime))}. The result therefore does not
#' depend on the time zone a date-time is displayed in, and it includes
#' seconds and fractions of a second.
#'
#' Calendar dates are read on the proleptic Gregorian calendar, as everywhere
#' in R, so for dates before 1582-10-15 the result differs from the
#' Julian-calendar convention used by Meeus (1998). Leap seconds and the
#' difference UT1 - UTC (always less than 0.9 s) are ignored: UTC is treated
#' as Universal Time.
#'
#' @param datetime A \code{POSIXct}, \code{POSIXlt} or \code{Date} vector, or
#'   a character vector. Character strings are always read as UTC and must
#'   be a year-first date, optionally followed by a time:
#'   \code{"2000-01-01"}, \code{"2000-01-01 12:00"},
#'   \code{"2000-01-01 12:00:00.5"}, with \code{"/"} instead of \code{"-"} or
#'   \code{"T"} instead of the space also accepted, and an optional trailing
#'   \code{"Z"}. Each element is read on its own, so dates and date-times can
#'   be mixed. Anything else, including time-zone offsets such as
#'   \code{"+05:30"} and day-first dates, is an error; for local clock times
#'   build a \code{POSIXct} with the right \code{tz} yourself. \code{Date}
#'   values refer to midnight UTC.
#' @return A numeric vector of Julian Days (in days), the same length as
#'   \code{datetime}. Missing date-times (including an all-\code{NA} logical
#'   vector) give \code{NA}.
#' @references
#' Meeus, J. (1998). \emph{Astronomical Algorithms}, 2nd edition.
#' Willmann-Bell, Richmond, Virginia. Chapter 7.
#' @seealso \code{\link{radec_to_altaz}}, which uses the Julian Day to obtain
#'   the sidereal time.
#' @examples
#' # The J2000.0 epoch
#' julian_day(as.POSIXct("2000-01-01 12:00:00", tz = "UTC"))
#'
#' # Meeus (1998) Example 7.a: 1957 October 4.81 UT
#' print(julian_day("1957-10-04 19:26:24"), digits = 10)
#'
#' # The same instant displayed in two time zones has one Julian Day
#' noon_new_york <- as.POSIXct("2024-07-04 12:00:00", tz = "America/New_York")
#' print(julian_day(noon_new_york), digits = 10)
#' print(julian_day("2024-07-04 16:00:00"), digits = 10)
#'
#' # Dates refer to midnight UTC
#' print(julian_day(as.Date("2024-01-01")), digits = 10)
#' @export
julian_day <- function(datetime) {
  if (is.logical(datetime) && all(is.na(datetime))) {
    return(rep(NA_real_, length(datetime)))
  }
  if (is.character(datetime)) {
    datetime <- parse_utc(datetime)
  } else if (!inherits(datetime, c("POSIXct", "POSIXlt", "Date"))) {
    stop("'datetime' must be a POSIXct, POSIXlt, Date or character vector.")
  }
  as.numeric(as.POSIXct(datetime)) / 86400 + 2440587.5
}


# Reads year-first date or date-time strings as UTC, one element at a time.
# as.POSIXct() alone picks a single format for the whole vector and silently
# drops the time of day for "T" separators or mixed vectors, ignores offsets
# such as "+05:30" and reads "04/10/1987" as the year 4, so those inputs are
# normalised or rejected here.
parse_utc <- function(x) {
  missing_x <- is.na(x)
  s <- sub("Z$", "", trimws(x))
  pattern <- paste0("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}",
                    "([ T][0-9]{1,2}:[0-9]{2}(:[0-9]{2}(\\.[0-9]+)?)?)?$")
  if (!all(missing_x | grepl(pattern, s))) {
    stop("'datetime' strings must look like \"2000-01-01 12:00:00\" ",
         "(read as UTC); time-zone offsets are not supported.")
  }
  s <- gsub("/", "-", sub("T", " ", s), fixed = TRUE)
  s <- ifelse(grepl(" ", s, fixed = TRUE), s, paste(s, "00:00"))
  s <- ifelse(grepl(" [0-9]{1,2}:[0-9]{2}$", s), paste0(s, ":00"), s)
  s[missing_x] <- NA_character_
  out <- as.POSIXct(s, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  if (any(is.na(out) & !missing_x)) {
    stop("'datetime' contains an impossible date or time.")
  }
  out
}


#' Convert Right Ascension and Declination to Altitude and Azimuth
#'
#' Converts equatorial coordinates (right ascension and declination) to
#' horizontal coordinates (altitude and azimuth) for an observer at a given
#' place on Earth and a given instant.
#'
#' The Greenwich mean sidereal time, in hours, is
#' \deqn{GMST = 18.697374558 + 24.06570982441908 (JD - 2451545.0),}{
#'   GMST = 18.697374558 + 24.06570982441908 * (JD - 2451545.0),}
#' which is equation 12.4 of Meeus (1998) without its very small quadratic
#' and cubic terms, with \eqn{JD} from \code{\link{julian_day}}. For
#' longitude \eqn{\lambda}{lon} and right ascension \eqn{\alpha}{ra} the
#' local sidereal time is
#' \eqn{LST = GMST + \lambda / 15}{LST = GMST + lon / 15} and the hour angle
#' is \eqn{H = 15 (LST - \alpha)}{H = 15 * (LST - ra)} degrees. With
#' declination \eqn{\delta}{dec} and latitude \eqn{\phi}{lat}, the altitude
#' \eqn{h}{alt} and azimuth \eqn{A}{az} are
#' \deqn{\sin h = \sin\delta \sin\phi + \cos\delta \cos\phi \cos H,}{
#'   sin(alt) = sin(dec) sin(lat) + cos(dec) cos(lat) cos(H),}
#' \deqn{A = \mathrm{atan2}(-\cos\delta \sin H,
#'   \sin\delta \cos\phi - \cos\delta \sin\phi \cos H),}{
#'   az = atan2(-cos(dec) sin(H),
#'              sin(dec) cos(lat) - cos(dec) sin(lat) cos(H)),}
#' where the sine of the altitude is clamped to \eqn{[-1, 1]} to absorb
#' rounding errors and the azimuth is reduced to \eqn{[0, 360)}.
#'
#' Conventions and limitations:
#' \itemize{
#'   \item Longitude is positive east of Greenwich and negative west of it.
#'   \item Azimuth is measured from North through East: North is 0, East 90,
#'     South 180 and West 270 degrees. Meeus (1998) measures azimuth from
#'     South, so add 180 degrees to his values to compare.
#'   \item \code{ra} and \code{dec} must be apparent coordinates, referred to
#'     the true equator and equinox of the date. No precession, nutation or
#'     aberration is applied. Catalog (J2000.0) positions are fine for rough
#'     work, but precession alone shifts them by roughly 0.01 degrees for
#'     every year away from 2000.
#'   \item The altitude is geometric: atmospheric refraction, which raises
#'     objects near the horizon by about 0.5 degrees, is not applied.
#'   \item The sidereal time is mean, not apparent (the equation of the
#'     equinoxes, at most about 1.2 s of time, is ignored), and
#'     \code{datetime} is taken as Universal Time (UT1 - UTC is ignored).
#' }
#'
#' For Meeus (1998) Example 13.b (Venus seen from the US Naval Observatory in
#' Washington on 1987 April 10 at 19:21:00 UT) the function returns an
#' altitude of 15.1243 and an azimuth of 248.0343 degrees, within 0.0007
#' degrees of the published 15.1249 and 248.0337 (68.0337 measured from
#' South). The difference comes mainly from the mean sidereal time: given
#' the apparent sidereal time and the longitude (5h08m15.7s) that Meeus
#' uses, the results agree with his to better than 0.0001 degrees.
#'
#' All arguments are vectorized: each must have length 1 or a common length
#' \eqn{n}, and the result has \eqn{n} rows. A missing value in any argument
#' gives \code{NA} in the corresponding row.
#'
#' @param ra Numeric vector. Apparent right ascension in hours, in
#'   \eqn{[0, 24)}.
#' @param dec Numeric vector. Apparent declination in degrees, in
#'   \eqn{[-90, 90]}.
#' @param lat Numeric vector. Geographic latitude of the observer in degrees,
#'   in \eqn{[-90, 90]} (north positive).
#' @param lon Numeric vector. Geographic longitude of the observer in degrees,
#'   positive east of Greenwich and negative west of it. Any finite value is
#'   accepted (280 is the same as -80).
#' @param datetime Instant(s) of the observation: anything accepted by
#'   \code{\link{julian_day}}, i.e. a \code{POSIXct}, \code{POSIXlt},
#'   \code{Date} or character vector (character strings are read as UTC).
#' @return A data frame with one row per observation and the columns
#'   \describe{
#'     \item{\code{altitude}}{Altitude above the horizon in degrees, in
#'       \eqn{[-90, 90]}.}
#'     \item{\code{azimuth}}{Azimuth in degrees, in \eqn{[0, 360)}, measured
#'       from North through East.}
#'   }
#' @references
#' Meeus, J. (1998). \emph{Astronomical Algorithms}, 2nd edition.
#' Willmann-Bell, Richmond, Virginia. Chapters 12 and 13.
#' @seealso \code{\link{julian_day}}; \code{\link{dms_to_deg}} for a
#'   sexagesimal \code{dec}, and \code{\link{hms_to_deg}} for a sexagesimal
#'   right ascension. \code{hms_to_deg()} returns degrees, so pass
#'   \code{hms_to_deg(h, m, s) / 15} as \code{ra}.
#' @examples
#' # The globular cluster M13 (RA 16h41.7m, Dec +36d28m) seen from
#' # Birmingham, UK
#' radec_to_altaz(ra = 16.695, dec = 36.466667, lat = 52.5, lon = -1.9166667,
#'                datetime = as.POSIXct("1998-08-10 23:10:00", tz = "UTC"))
#'
#' # Meeus (1998) Example 13.b: Venus seen from Washington, D.C.
#' radec_to_altaz(ra = 23 + 9 / 60 + 16.641 / 3600,
#'                dec = -(6 + 43 / 60 + 11.61 / 3600),
#'                lat = 38 + 55 / 60 + 17 / 3600,
#'                lon = -(77 + 3 / 60 + 56 / 3600),
#'                datetime = "1987-04-10 19:21:00")
#'
#' # Track of Rigel seen from Rochester, NY, every three hours
#' times <- seq(as.POSIXct("2024-10-02 00:00:00", tz = "UTC"),
#'              by = "3 hours", length.out = 9)
#' track <- radec_to_altaz(ra = 78.634467 / 15, dec = -8.20164,
#'                         lat = 43.1566, lon = -77.6088, datetime = times)
#' data.frame(time = times, track)
#' @export
radec_to_altaz <- function(ra, dec, lat, lon, datetime) {
  # an all-NA logical vector (e.g. an empty data-frame column) counts as
  # numeric NA, as in deg_to_rad()
  as_num_na <- function(x) {
    if (is.logical(x) && all(is.na(x))) as.numeric(x) else x
  }
  ra <- as_num_na(ra)
  dec <- as_num_na(dec)
  lat <- as_num_na(lat)
  lon <- as_num_na(lon)
  if (!is.numeric(ra)) stop("'ra' must be numeric.")
  if (any(ra < 0 | ra >= 24, na.rm = TRUE)) {
    stop("'ra' must be in hours, at least 0 and less than 24.")
  }
  if (!is.numeric(dec)) stop("'dec' must be numeric.")
  if (any(dec < -90 | dec > 90, na.rm = TRUE)) {
    stop("'dec' must be between -90 and 90 degrees.")
  }
  if (!is.numeric(lat)) stop("'lat' must be numeric.")
  if (any(lat < -90 | lat > 90, na.rm = TRUE)) {
    stop("'lat' must be between -90 and 90 degrees.")
  }
  if (!is.numeric(lon)) stop("'lon' must be numeric.")
  if (any(is.infinite(lon))) stop("'lon' must be finite.")
  jd <- julian_day(datetime)

  lens <- lengths(list(ra, dec, lat, lon, jd))
  n <- if (any(lens == 0L)) 0L else max(lens)
  if (any(lens != 1L & lens != n)) {
    stop("'ra', 'dec', 'lat', 'lon' and 'datetime' must have length 1 ",
         "or a common length.")
  }

  gmst <- (18.697374558 + 24.06570982441908 * (jd - 2451545.0)) %% 24
  ha <- deg_to_rad((gmst + lon / 15 - ra) * 15)
  dec <- deg_to_rad(dec)
  lat <- deg_to_rad(lat)

  sin_alt <- sin(dec) * sin(lat) + cos(dec) * cos(lat) * cos(ha)
  altitude <- rad_to_deg(asin(pmin(pmax(sin_alt, -1), 1)))
  azimuth <- rad_to_deg(atan2(
    -cos(dec) * sin(ha),
    sin(dec) * cos(lat) - cos(dec) * sin(lat) * cos(ha)
  )) %% 360
  # %% can round a tiny negative angle up to exactly 360
  azimuth[which(azimuth >= 360)] <- 0

  data.frame(altitude = as.vector(altitude), azimuth = as.vector(azimuth))
}
