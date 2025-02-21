# astronomR: Astronomy and Cosmology Analysis in R 🌌✨

An R package designed to bridge the gap between data science and the cosmos!

## Why {astronomR}?

While R is widely used in data science and statistical computing, there has been a lack of tools specifically tailored for astronomical data. {astronomR} aims to fill this gap, offering easy-to-use functions and tools for anyone looking to explore space and cosmology through data. Whether you’re an astrophysicist or just a data enthusiast, {astronomR} is here to help you navigate the universe.

## Installing this Package

``` r
# Install devtools package if you don't have it
# install.packages("devtools")

devtools::install_github("samrit2442/astronomR")
```

## Introduction

This package was developed for astronomy, cosmological computation, and analysis with R.
The source code can be found here https://github.com/samrit2442/astronomR.git

## Some Usage and Example Codes
Normally in astronomy, we use a different sort of angular system for location. We can easily do that in our package. Let’s see how. Suppose, we have an angular value of `d = 177.74208°` We want to convert it into an hour-minute-second. There is a very simple function.
``` r
deg_to_hms(177.74208)
#> 11H50M58.0992S
hms_to_deg(11, 50, 58.09925)
#> [1] 177.7421
```
What else can be done? Let’s say we want to find the path of some star in a particular location for a time interval. This can also be done using our package.

To do that, first, we need to define the RA and Dec value of the star. Let’s see to do this. Also, let’s define the time of observation and its location.

``` r
ra_hour <- 16.695  # RA in hours
dec_deg <- 36.466667  # Dec in degrees
lat_obs <- 52.5  # Observer's latitude
lon_obs <- -1.9166667  # Observer's longitude
datetime <- as.POSIXct("1998-08-10 23:10:00", tz = "UT")  # Observation time
```
Now, create the star location as seen from that location on the time mentioned. This returns Altitude and Azimuth as normally used.
``` r
star_location <- RA_dec2Alt_azi(ra_hour, dec_deg, lat_obs, lon_obs, datetime)
print(paste("Altitude:", star_location$altitude, "degrees"))
#> [1] "Altitude: 49.1688687197424 degrees"

print(paste("Azimuth:", star_location$azimuth, "degrees"))
#> [1] "Azimuth: 269.146669462321 degrees"
```
This tells us from some location, what is the position of the star so that we can use a telescope to watch it!
Using this simple function, we can trace out the path any star travels.
Let’s see how and also maybe plot it.
For that, let’s first make a time range, for which we want to see the location.
``` r
# Plotting Rigel's Motion
start_datetime <- as.POSIXct("2024-10-02 00:00:00", tz = "UTC")
end_datetime <- as.POSIXct("2024-10-03 00:00:00", tz = "UTC")

# Generate timestamps at 20-minute intervals
timestamps <- seq(from = start_datetime, to = end_datetime, by = "20 mins")
altitude <- numeric(length(timestamps))
azimuth <- numeric(length(timestamps))

observer_lat <- 43.1566  # Latitude in degrees
observer_lon <- -77.6088  # Longitude in degrees

# RA and Dec for Rigel (converted RA to hours)
rigel_ra <- 78.634467 / 15  # RA in hours
rigel_dec <- -8.20164  # Dec in degrees

# Calculate altitude and azimuth for each timestamp
for (i in seq_along(timestamps)) {
  datetime <- timestamps[i]
  result <- RA_dec2Alt_azi(rigel_ra, rigel_dec, observer_lat, observer_lon, datetime)
  altitude[i] <- result$altitude
  azimuth[i] <- result$azimuth
}

# Create a data frame with results
rigel_positions <- data.frame(datetime = timestamps, altitude = altitude, azimuth = azimuth)

# 2D Plot for Altitude over Time
library(ggplot2)
ggplot(rigel_positions, aes(x = datetime, y = altitude)) +
  geom_line() +
  labs(x = "Time", y = "Altitude (degrees)", title = "Altitude of Rigel over 24 hours")
```
![image](https://github.com/user-attachments/assets/e7052a09-3142-4796-a505-d60c65a991f7)

``` r
# 2D Plot for Azimuth over Time
ggplot(rigel_positions, aes(x = datetime, y = azimuth)) +
  geom_line() +
  labs(x = "Time", y = "Azimuth (degrees)", title = "Azimuth of Rigel over 24 hours")
```
![image](https://github.com/user-attachments/assets/80dd618d-d7fc-40b3-af08-606ed7d9846c)

``` r
# 2D Scatter Plot for Azimuth vs Altitude
ggplot(rigel_positions, aes(x = azimuth, y = altitude)) +
  geom_point(color = "firebrick") +
  labs(x = "Azimuth (degrees)", y = "Altitude (degrees)", title = "Azimuth vs Altitude for Rigel over 24 hours")
```
![image](https://github.com/user-attachments/assets/46e75c1b-79c8-4163-bdd1-6a2d74311f91)

## Gaia Data Archive
The **Gaia Data Archive** is a comprehensive database that houses the data collected by the European Space Agency’s Gaia mission. Launched in December 2013, Gaia is designed to create the most accurate three-dimensional map of the Milky Way galaxy by observing and cataloguing the positions, distances, and motions of over a billion stars.
In Python **Astropy** and **Astroquery** is used to import data directly. We can do the same thing in our package. Let’s see how:
``` r
df <- get_gaia_data(vars = "ra, dec, parallax", condition = "parallax > 50")
head(df)

#>         ra       dec  parallax
#> 1 316.7537  38.75607 286.00534
#> 2 316.7485  38.76386 285.99493
#> 3 298.4819  44.41291 214.57451
#> 4 249.3875 -53.69952  53.22840
#> 5 243.4453 -57.57679  73.54824
#> 6 312.2788  37.47123  56.86465
```
Nice! Isn’t it?
Let’s use this for some analysis. Why not create an H-R diagram.

For this first, let’s convert parallax to absolute magnitude.

``` r
library(dplyr)
calculate_absolute_magnitude <- function(parallax, g_mag) {
  distance_pc <- 1 / (parallax / 1000)
  abs_mag <- g_mag - 5 * (log10(distance_pc) - 1)
  return(abs_mag)
}

# Let's import the data
df <- get_gaia_data(vars = "ra, dec, parallax, phot_g_mean_mag, phot_bp_mean_mag, phot_rp_mean_mag", 
                    condition = "parallax > 50")
#data processing
df <- df %>%
  mutate(color_index = phot_bp_mean_mag - phot_rp_mean_mag,
         abs_mag = calculate_absolute_magnitude(parallax, phot_g_mean_mag))
# Filter out any rows with NA values
df <- df %>% filter(!is.na(color_index), !is.na(abs_mag))

# Plot an H-R diagram
ggplot(df, aes(x = color_index, y = abs_mag)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(x = "Color Index (G_BP - G_RP)", y = "Absolute Magnitude (M)", 
       title = "Hertzsprung-Russell Diagram") +
  theme_minimal() +
  scale_y_reverse()  # Reverse the y-axis for magnitude
```
![image](https://github.com/user-attachments/assets/a29b22c9-9ca0-4048-9bf4-8e50847157c7)

## Cosmological Calculation
We can do many things related to cosmology using this. Let’s see a few of them. Before that, I will suggest to run this code.
As we know, there are many cosmological models. You can define these in our package.
``` r
cosmo <- FlatLCDM(0.6774, 0.6911, 0.3089, 8.4e-5)
cosmo
#> $hubble_constant_fact
#> [1] 0.6774
#> 
#> $dark_matter_crit
#> [1] 0.69110000000000005
#> 
#> $matter_crit
#> [1] 0.30890000000000001
#> 
#> $radiation_crit
#> [1] 8.3999999999999995e-05
#> 
#> $type
#> [1] "FlatLCDM"
#> 
#> $h_per_s
#> [1] 2.1949477836373805e-20
```
Now, we can directly use these models and inputs of other functions.
As we know scale factor of our universe `a = 1` today. We can directly use our function to calculate time at any given `a` value.
``` r
t_as_func_a_in_Year(cosmo, 1) # in year
#> [1] 13808979942.748983
```
We can find the age of our universe using this or use the function `age_of_universe_GY()`.
``` r
age_of_universe_GY(cosmo) # our universe age !!!!!
#> [1] 13.808979942748984
```
Our package can find the radius of curvature of our universe. Let’s see:
```r
radius_of_curvature(cosmo)
#> [1] 0
```

Many more functions are coming soon. Wait for the documentation for more!

