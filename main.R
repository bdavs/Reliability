MTTF_Function <- function(Reliability_Investment,C0,Cost_Increment,Amode_fail_rate,Bmode_fail_rate,Bmode_FEF){
  CV = 1
  W <- C0/Cost_Increment + CV ^ 2 * Reliability_Investment/Cost_Increment
  inv_lambert <- lambertW(C0/Cost_Increment * exp(W))
  MTTF <- 1/(1/(Amode_fail_rate)+1/Bmode_fail_rate*((1-Bmode_FEF)+(Bmode_FEF/(1+(-C0 + Cost_Increment * inv_lambert)/C0))))
  return(MTTF)
}
repParts <- function(Ttime,MTTF)
  return(floor(Ttime/MTTF - 0.001))
Cost <- function(MinUnits, initialCost)
  return(initialCost*(1+MinUnits))
UnitCost <- function(subsystem1,subsystem2)
  return(subsystem1+subsystem2)
UnitInvestment <- function(subsystem1,subsystem2)
  return(subsystem1+subsystem2)
NumUnits <- function(Budget,Reliability_Investment,UnitCost)
  return(floor((Budget - Reliability_Investment)/UnitCost))

Rs <- function(Ttime,MTTF){
  return(exp(-Ttime/MTTF))
}
Ma <- function(Ttime,MTTF){
  return(exp(-Ttime/(Ttime - MTTF)))
}
A0 <- function(Ttime, MTTF){
  return(MTTF /Ttime)
}
AFF <- function(C0, Cs){
  return(Cs/C0)
} 