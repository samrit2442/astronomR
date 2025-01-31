ra_hour <- 16.695 # RA in hours
dec_deg <- 36.466667 # Dec in degrees
lat_obs <- 52.5 # Observer's latitude
lon_obs <- -1.9166667 # Observer's longitude
datetime <- as.POSIXct("1998-08-10 23:10:00", tz = "UT") # Observation time




star_location <- astronomR:::RA_dec2Alt_azi(ra_hour, dec_deg, lat_obs, lon_obs, datetime)
print(paste("Altitude:", star_location$altitude, "degrees"))
print(paste("Azimuth:", star_location$azimuth, "degrees"))
