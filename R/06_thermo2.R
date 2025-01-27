library(pracma)
zeta_3 <- 1.20205690316
EI <- 13.6  # Hydrogen Binding (eV)
Me <- 5.10999e5  # Mass of electron use the table
zeq <- 3395  # Redshift for matter-radiation equality
H0 <- 1.5e-33
z0 <- 2.725 / (1.160451812e4)
baryon_to_photon_ratio <- 6e-10
nb <- 0.76 * baryon_to_photon_ratio*(2*zeta_3) / (pi^2)  # Baryon number density

# Conversion function from z to T (in eV)
T <- function(z) {
  z0 <- 2.725 / (1.160451812e4)
  return(z0 * (1 + z))  # BigFloat equivalent in R is not needed for this calculation
}

# Pre-factor for the Saha equation
pre_fac_saha <- nb * (2 * pi / Me)^(3/2)

photon_energy_density_fn_T <- function(T,unit="eV"){#T is in eV
  if(unit=="eV"){
    a <- (pi^2/15)*T^4
  }
  else if(unit=="K"){
    a <- (pi^2/15)*(8.6173e-5*T)^4

  }
  else{
    print("error unit can only take eV or K")
    a<-NULL
  }
  return(a)
}

photon_number_density_fn_T <- function(T,unit="eV"){#T is in eV
  if(unit=="eV"){
    a <- (2*zeta_3/pi^2)*T^3
  }
  else if(unit=="K"){
    a <- (2*zeta_3/pi^2)*(8.6173e-5*T)^3

  }
  else{
    print("error unit can only take eV or K")
    a<-NULL
  }
  return(a)
}

photon_energy_density_fn_z <- function(z){
  return((pi^2/15)*T(z)^4)
}

photon_number_density_fn_z <- function(z){
  return((2*zeta_3/pi^2)*T(z)^3)
}

Saha_Xe <- function(z) {
  f <- pre_fac_saha * T(z)^(3/2) * exp(EI / T(z))
  return((-1 + sqrt(1 + 4 * f)) / (2 * f))
}
soln_saha <- function(z) {
  return(Saha_Xe(z) - 0.5)
}
recombination_temp_saha <- function(res_type="Z"){
  z_rec <- fzero(soln_saha, c(1300, 1400))$x
  if(res_type=="Z"){
    result <- z_rec
    print("Red-shift value")
  }
  else{
      result <- T(z_rec)/8.6173e-5
      print("Temp in Kelvin")
    }
  return(result)
}


