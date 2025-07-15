# derivation of R value (universals gas constant) to get the Fc's unit as
# umol.m-2.s-1

# Fc = ((10 * V * P) / (R * S * T)) * slope [--- from licor manual]
#
#                10 * cm^3 * kpa
# umol.m-2.s-1 = ----------------------------------- * (umol.mol-1.s-1)
#                8.3145 m^3.pa.mol^-1.k^-1 * cm^2 * k
#
#                10 * cm * kpa * umol
#              = --------------------------------------------
#                8.3145 * 10^-3 m^3.kpa.mol^-1.k^-1 * k * mol.s
#
#                10 * cm * umol
#              = -------------------------
#                8.3145 * 10^-3 * m^3 * s
#
#               10 * 10^-2 * m * umol       10 * umol
#             = ------------------------- = --------------------------------
#               8.3145 * 10^-3 * m^3 * s    8.3145 * 10^-3 * 10^2 * m^2 * s
#
#               10 * umol                      10 * umol
#             = -------------------------- = -----------------
#               8.3145 * 10^-1 * m^2 * s     0.83145 * m^2 * s
#
# Therefore, the value of R should be 0.83145 (m3.kpa.mol-1.k-1)





# units must be matched
#' @param v total volume in cm3
#' @param p initial gas pressure in kpa
#' @param r universal gas constant 0.83144598 m3.kpa.k-1.mol-1
#' @param s soil surface area within collar in cm2
#' @param t initial gas temperature in C
#' @param slope rate of concentration change in micromol-1.mol-1.s-1 or ppm.s-1
#' @param gas character string specifying name of the gas either co2 or ch4 or n2o
#'
#' @return co2 or ch4/n2o flux in micromol m-2 s-1 for co2 & nanomol m-2 s-1 for ch4

calcFlux <-
  function(v,
           p,
           r = 0.83144598,
           s = 83.7,
           t,
           slope,
           gas = c('co2', 'ch4', 'n2o','h2o')) {
    # Note: in a gas mixture, ppm is equivalent to umol.mol-1
    t <- t + 273.15 # Convert °C to K
    
    if (gas == "co2"|| gas == 'h2o') {
      fc <- ((10 * v * p) / (r * s * t)) * slope # umol.m-2.s-1
    } else if (gas == 'ch4' || gas == 'n2o') {
      fc <-
        (((10 * v * p) / (r * s * t)) * slope) * 1000 # nanomol.m-2.s-1
    } else {
      cat('Unknown gas. Should either be co2, ch4, or n2o')
    }
    return(fc)
  }






ronydata <- read.csv("/Users/ronysmac/Downloads/Fluxester/data/combined_lgr_slope_fall_soil_spring_downsview.csv")
str(ronydata)





co2_flux <- with(ronydata, calcFlux(v = 854.2, p = gasP_torr * 0.1333, s = 83.7, t = gasT_C, slope = co2_dry_slope, gas = "co2"))
ch4_flux <- with(ronydata, calcFlux(v = 854.2, p = gasP_torr * 0.1333, s = 83.7, t = gasT_C, slope = ch4_dry_slope, gas = "ch4"))

n2o_flux <- with(ronydata, calcFlux(v = 854.2, p = cavity_pressure_kpa, s = 83.7, t = cavity_temp_degree_celcius, slope = n2o_ppb_slope/1000, gas = "n2o"))
h2o_flux <- with(ronydata, calcFlux(v = 854.2, p = gasP_torr * 0.1333, s = 83.7, t = gasT_C, slope = h2o_slope, gas = "h2o"))


ronydata$co2_flux <- co2_flux
ronydata$ch4_flux <- ch4_flux
ronydata$n2o_flux <- n2o_flux
ronydata$h2o_flux <- h2o_flux

write.csv(ronydata, file = "/Users/ronysmac/Desktop/phd/Downsview_fieldwork/data/final_flux/downsview_spring_soil_2024.csv", row.names = FALSE)
