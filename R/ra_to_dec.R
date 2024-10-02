# Function to convert RA and Dec to altitude and azimuth
RA_dec2Alt_azi <- function(ra, dec, lat, lon, datetime) {
  # Convert datetime to Julian Day
  jd <- swephR::swe_julday(as.numeric(format(datetime, "%Y")),
                   as.numeric(format(datetime, "%m")),
                   as.numeric(format(datetime, "%d")),
                   as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60,
                   1)  # 1 for Gregorian calendar
  #print(paste("Julian Date = ",jd))
  # Calculate sidereal time at Greenwich at 0h UT
  gst <- swephR::swe_sidtime(jd)

  # Convert GST to LST
  lst <- gst + lon / 15.0  # Convert longitude to hours and add to GST
  #print(paste("LST  = ",lst))

  # Convert RA from hours to degrees
  ra_deg <- ra * 15.0

  # Calculate hour angle (HA)
  ha <- lst * 15.0 - ra_deg  # Convert LST from hours to degrees and subtract RA

  # Convert degrees to radians for trigonometric functions
  ha_rad <- deg2rad(ha)
  dec_rad <- deg2rad(dec)
  lat_rad <- deg2rad(lat)
  # Calculate altitude (alt) and azimuth (az) using spherical trigonometry

  sin_alt <- sin(dec_rad) * sin(lat_rad) + cos(dec_rad) * cos(lat_rad) * cos(ha_rad)
  alt <- asin(sin_alt)

  cos_az <- (sin(dec_rad) - sin(alt) * sin(lat_rad)) / (cos(alt) * cos(lat_rad))
  az <- acos(cos_az)

  # Adjust azimuth for hour angle
  if (sin(ha_rad) > 0) {
    az <- 2 * pi - az
  }

  # Convert radians to degrees
  alt_deg <- rad2deg(alt)
  az_deg <- rad2deg(az)

  return(list(altitude = alt_deg, azimuth = az_deg))
}
