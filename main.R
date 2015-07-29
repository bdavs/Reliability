#this file contains the equations that run the program.


MTTF_Function <-   #this is the driving function; taken from the AHS paper; it finds the MTTF from a set of variables
  function(Reliability_Investment,C0,Cost_Increment,Amode_fail_rate,Bmode_fail_rate,Bmode_FEF) {
    CV = 1 #no vairation
    W <- C0 / Cost_Increment + CV ^ 2 * Reliability_Investment / Cost_Increment
    inv_lambert <- lambertW(C0 / Cost_Increment * exp(W))
    MTTF <- 1 / (1 / Bmode_fail_rate * ((1 - Bmode_FEF) + (Bmode_FEF / ( 1 + (-C0 + Cost_Increment * inv_lambert) / C0))))
    return(MTTF)
  }
repParts <- function(Ttime,MTTF) #number of repair parts required over the lifecycle of a subsystem
  return(floor(Ttime / MTTF - 0.001))
Cost <- function(MinUnits, initialCost) #cost of all parts of a subsystem
  return(initialCost * (1 + MinUnits))
UnitCost <- function(subsystem1,subsystem2) #cost of a whole unit based on two subsystems
  return(subsystem1 + subsystem2)
UnitInvestment <- function(subsystem1,subsystem2) #total reliability investment of two subsystems
  return(subsystem1 + subsystem2)
NumUnits <- function(Budget,Reliability_Investment,UnitCost) #total number of units able to be purchased based off a fixed budget
  return(floor((Budget - Reliability_Investment) / UnitCost))

Rs <- function(Ttime,MTTF) { #a factor of reliability
  return(exp(-Ttime / MTTF))
}
Ma <- function(Ttime,MTTF) { #a factor of maintainability
  return(exp(-Ttime / (Ttime - MTTF)))
}
A0 <- function(Ttime, MTTF) { #a factor of availability
  return(MTTF / Ttime)
}
AFF <- function(C0, Cs) {  #a factor of affordability
  return(Cs / C0)
} 