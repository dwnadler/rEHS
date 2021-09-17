# Environmental Health and Safety package for R
# Work-in-progress
#
#
#
# clear console:
cat("\014")

# Injury rates ---------------------------------------------------------------
# Incident Rate (ir) as specified by OSHA
# Defined as the number of work-related injuries per 100 full-time workers
# during a one year period
#' @export incident.rate
incident.rate <- function(recordable.cases, total.hours.worked) {
  ir <<- recordable.cases * 200000 / total.hours.worked
  sprintf("Incident rate: %.2f", ir)
}

# DART Rate (Days Away/Restricted or Job Transfer Rate)
# as specified by OSHA
#a mathematical calculation that describes the number of recordable incidents
# per 100 full time employees that resulted in lost or restricted days
# or job transfer due to work related injuries or illnesses.
#' @export dart
dart <- function(dart.incidents, total.hours.worked) {
  result.dart <<- dart.incidents * 200000 / total.hours.worked
  sprintf("DART: %.2f", result.dart)
}

# Severity Rate as specified by OSHA
# a mathematical calculation that describes the number of lost days
# experienced as compared to the number of incidents experienced
#' @export severity.rate
severity.rate <- function(total.days.lost, total.recordable.incidents) {
  sr <<- total.days.lost / total.recordable.incidents
  sprintf("Severity rate: %.2f", sr)
}





# Noise exposure ------------------------------------------------------------
# Permissible Noise Exposure
#' @export T
T <- function(dbA, hours) {
  dB <- 0.2 * (dbA - 90)
  exposure.time <- 8 / (2 ^ dB)
  sprintf("Permissible exposure time: %f hours", exposure.time)
}

# Sound measurement from a distance
#' @export dB.distance
dB.distance <- function(dB0, distance.original, distance.new) {
  dB1 <<- dB0 + 20 * log10(distance.original / distance.new)
  sprintf("Noise level in db at distance %.2f is %.2f", distance.new, dB1)
}


# Thermal stressors ---------------------------------------------------------
# Wind Chill calculation
# Air temperature must be below 70 F
#' @export wind.chill
wind.chill <- function(temp.fahr, wind.speed.mph) {
  if (temp.fahr < 70) {
    temperature <<- 35.74 + (0.6215 * temp.fahr) -
      (35.75 * (wind.speed.mph ^ 0.16)) +
      (0.4275 * (temp.fahr * (wind.speed.mph ^ 0.16)))
    sprintf(
      "Wind chill is %.2f F when wind speed is %.1f and air temperature is %.1f F",
      temperature, wind.speed.mph, temp.fahr)
  }
  else {
    print("Air temperature needs to be below 70 F")
    }
}


# Ergonomics ----------------------------------------------------------------------------------
# Recommended weight limit
#' @export rwl
rwl <- function(horizontal.dist, vertical.dist, distance, angle,
                seconds.between.lifts, grasp, object.weight) {
  # LC load constant not included in function
  LC <- 23 #kg
  # HM horizontal multiplier
  # VM vertical multiplier
  # DM distance multiplier
  # AM assymmetric multiplier
  # FM frequency multiplier
  # CM coupling multiplier

  if (horizontal.dist <= 25) {
    HM <- 1.00
  } else if ( horizontal.dist > 25 & horizontal.dist <= 30 ) {
    HM <- 0.83
  } else if (horizontal.dist > 30 & horizontal.dist <= 40) {
    HM <- 0.63
  } else if (horizontal.dist > 40 & horizontal.dist <= 50) {
    HM <- 0.50
  } else if (horizontal.dist > 50 & horizontal.dist <= 60) {
    HM <- 0.42
  } else print("Must be between 0 and 60 cm.")
  # end of HM statements

  if (vertical.dist <= 30) {
    VM <- 0.78
  } else if ( vertical.dist > 30 & vertical.dist <= 50 ) {
    VM <- 0.87
  } else if (vertical.dist > 50 & vertical.dist <= 70) {
    VM <- 0.93
  } else if (vertical.dist > 70 & vertical.dist <= 100) {
    VM <- 0.99
  } else if (vertical.dist > 100 & vertical.dist <= 150) {
    VM <- 0.93
  } else if (vertical.dist > 150 & vertical.dist <= 175) {
    VM <- 0.78
  } else if (vertical.dist == 175) {
    VM <- 0.70
  } else if (vertical.dist > 175) {
    VM <- 0.00
  } # end of VM statements

  if (distance <= 25) {
    DM <- 1.00
  } else if (distance > 25 & distance <= 40) {
    DM <- 0.93
  } else if (distance > 40 & distance <= 55) {
    DM <- 0.90
  } else if (distance > 55 & distance <=100) {
    DM <- 0.87
  } else if (distance > 100 & distance <= 145) {
    DM <- 0.85
  } else if (distance > 145 & distance <= 175) {
    DM <- 0.85
  } else if (distance == 175) {
    DM <- 0.00
  } # end of DM statements

  if (angle == 90) {
    A <- 0.71
  } else if (angle == 60) {
    A <- 0.81
  } else if (angle == 45) {
    A <- 0.86
  } else if (angle == 30) {
    A <- 0.90
  } else if (angle == 0) {
    A <- 1
  } else print("Angle must be input as 90, 60, 45, 30, or 0 degrees.") # end of angle statements

  if (seconds.between.lifts == 300) {
    FM <- 0.85
  } else if (seconds.between.lifts == 60) {
    FM <- 0.75
  } else if (seconds.between.lifts == 30) {
    FM <- 0.65
  } else if (seconds.between.lifts == 15) {
    FM <- 0.45
  } else if (seconds.between.lifts == 10) {
    FM <- 0.27
  } else if (seconds.between.lifts == 6) {
    FM <- 0.13
  } else print("Time must be entered as 300, 60, 30, 15, 10, or 6 seconds.")
  # end of FM statements

  if (grasp == 1) { # use 1 for good or fair grasps
    CM <- 1.00
  } else if (grasp == 0) { # use 0 for poor grasp
    CM <- 0.90
  } else print("Use 1 for good and fair grasps, or 0 for poor grasp.")
  # end of CM statements

weight.limit <<- LC * HM * VM * DM * FM * A * CM
LI <<- object.weight / weight.limit

sprintf("Weight limit: %.2f and lifting index: %.2f", weight.limit, LI)
} # end of function RWL

#
# Particulates and Gases ---------------------------------------------------
# converting mg/m3 to and from ppm
#' @export mgm3.to.ppm
mgm3.to.ppm <- function(mg.per.cubic.meter, molecular.weight){
  ppm <<- mg.per.cubic.meter * 24.45 / molecular.weight
  sprintf("ppm: %.2f", ppm)
}

#' @export ppm.to.mgm3
ppm.to.mgm3 <- function(ppm, molecular.weight){
  mgm3 <<- ppm * molecular.weight / 24.45
  sprintf("mg/m3: %.2f", mgm3)

}

# minimum air sampling volume
# Limit of quantification is the concentration level above which
# quantitative results may be obtained with a certain degree of confidence
# min.air.volume provides liters
# limit.of.quantification.mg as milligrams
# contaminant.target.concentration in mg/m3
 #' @export min.air.volume
min.air.volume <- function(
  limit.of.quantification.mg, contaminant.target.concentration.mgm3) {
  volume.minimum <<- 1000 *
    limit.of.quantification.mg / contaminant.target.concentration.mgm3
  sprintf("Minimum air volume required: %.4f liters", volume.minimum)
}

# Ventilation ---------------------------------------------------------------
#' @export q.cfm
q.cfm <- function(air.velocity.fpm, area.sf) {
  Q <<- air.velocity.fpm * area.sf
  sprintf("Air flow rate: %.2f fpm", Q)
}

#' @export velocity.fpm
velocity.fpm <- function(velocity.pressure.as.h2o) {
  V <<- 4005 * sqrt(velocity.pressure.as.h2o)
  sprintf("Velocity is %.2f fpm", V)
}

# contaminant generation
#' @export time.interval
time.interval <- function(volume.cf, flow.rate.cfm,
                          contaminant.generation.rate.cfm, concentration.ppm) {
  new.concentration <<- concentration.ppm / 1000000
  a <- volume.cf/flow.rate.cfm
  b <- contaminant.generation.rate.cfm - flow.rate.cfm * new.concentration
  c <- log(b / contaminant.generation.rate.cfm)
  delta.t.min <<- -1 * a * c
  sprintf("Time interval is %.2f minutes for %.1f ppm", delta.t.min, new.concentration * 1000000)
}

# concentration after time frame
#' @export concentration
concentration <- function(contaminant.generation.rate, flow.rate.cfm,
                          timeframe, volume.cf) {
  a <- exp(-1*flow.rate.cfm * timeframe / volume.cf)
  numerator <- contaminant.generation.rate * (1 - a)
  conc <<- numerator * 1000000 / flow.rate.cfm
  sprintf("Concentration: %.2f ppm", conc)
}



cat("\014")



