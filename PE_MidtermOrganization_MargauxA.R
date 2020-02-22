# Margaux Armfield
# Getting Organized: Midterm Assignment

# Exponential Growth Model 1 -- closed form solution

# Uses the closed form solution of the exponential model to generate the dynamics of 
# exponential growth from a starting population size of n0. The parms list contains 
# one element called r, which represents the intrinsic growth rate and can take any 
# numeric value. Returns a data frame of population size (n) and corresponding time values 
# from times 0 to max.t.

PE_ExpGrowth1 <- function(n0, parms, max.t){
  time <- 0:max.t
  n = n0*exp(parms$r*time)
  return(data.frame(time, n))
}

# Geometric Growth Model 1 -- closed form solution

# Uses the closed form solution of the geometric model to generate the dynamics of 
# geometric growth from a starting population size of n0. The parms list contains 
# one element called r, which represents the intrinsic growth rate and should be a 
# positive value. Returns a data frame of population size (n) and corresponding time 
# values from times 0 to max.t. 

PE_GeomGrowth1 <- function(n0, parms, max.t){
  time <- 0:max.t
  n = n0*parms$lambda^time
  return(data.frame(time, n))
}

# Exponential Growth Model 2 -- differential equation

# Uses the ode function to solve the differential equation form of the exponential 
# growth model from a starting population size of n0. The parms list contains 
# one element called r, which represents the intrinsic growth rate and can take any 
# numeric value. Returns a data frame of population size (n) and corresponding time values 
# from times 0 to max.t.

PE_ExpGrowth2 <- function(n0, parms, max.t){
  library(deSolve)
  logistic.model.ODE <- function(time, n, parms)
  {
    dn.dt <- parms$r*n
    return(list(c(dn.dt))) #returns a list with one element which is a vector
  }
  #initial varaible is n0, set up as a vector with one, named element
  inits <- c(n = n0)
  p <- list(r = parms$r, K = parms$K)
  times <- 0:max.t # by represents your time step
  
  out <- ode(inits, times, func = logistic.model.ODE, parms = p)
  out <- as.data.frame(out)
  return(out)
}

# Geometric Growth Model 2 -- recursion equation

# Uses a for loop to solve the recursion equation of the geometric model to generate 
# the dynamics of geometric growth from a starting population size of n0. The parms 
# list contains one element called r, which represents the intrinsic growth rate and 
# should be a positive value. Returns a data frame of population size (n) and 
# corresponding time values from times 0 to max.t.

PE_GeomGrowth2 <- function(n0, parms, max.t)
{
  time <- 0:max.t
  n <- c(n0)
  for(i in 1:max.t)
  {
    n[i+1] <- parms$lambda*n[i]
  }
  return(data.frame(time, n))
}

# Logistic growth continuous

# Uses the ODE function in R to model the dynamics logistic continuous growth by using 
# integration to solve the equation analytically. The starting population size is n0.
# The parms list contains two elements: r, which represents the intrinsic growth rate and 
# K, which represents carrying capacity. Both parameters must be positive. Returns a data 
# frame of population size (n) and corresponding time values from times 0 to max.t.

PE_LogisticGrowth_Cont <- function(n0, parms, max.t)
{
  logistic.model.ODE <- function(t, n, parms)
  {
    dn.dt <- parms$r*n*(1-n/parms$K)
    return(list(c(dn.dt))) #returns a list with one element which is a vector
  }
  
  #initial varaible is n0, set up as a vector with one, named element
  inits <- c(n = n0)
  p <- list(r = parms$r, K = parms$K)
  times <- 0:max.t # by represents your time step
  
  out <- ode(inits, times, func = logistic.model.ODE, parms = p)
  out <- as.data.frame(out)
  return(out)
}

#Logistic Growth Discrete

# Uses the recursion equation to model the dynamics of logistic discrete growth using 
# a for loop. The initial population size is n0, and the parms list contains two elements:
# r, which represents the intrinsic growth rate and K, which represents carrying capacity. 
# Both values must always be positive, and K should be greater than n0. Returns a data 
# frame of population size (n) and corresponding time values from times 0 to max.t.

PE_LogisticGrowth_Disc <- function(n0, parms, max.t)
{
  n <- c(n0)
  time <- 0:max.t
  for (i in 1:max.t) {
    n[i+1] <- n[i] + parms$r*n[i]*(1-n[i]/parms$K)
  }
  return(data.frame(time, n))
}

#Environmental Stocasticity

# Uses a random uniform distribution and a for loop to model geometric growth with 
# environmental stocasticity (randomness). The initial population size is n0, and 
# the parms list contains two elements: a min.lambda and a max.lambda, which 
# represent the range that the random lambda value (growth rate) must fall between. 
# Both values must be positive and the min.lambda must be less than or equal to 
# the max.lambda. Returns a data frame of population size (n) and corresponding
# time values from times 0 to max.t.

PE_GeomGrowth_EnvStoc<- function(n0, parms, max.t)
{
  n <- c(n0)
  for(i in 1:max.t)
  {
    n[i+1] <- runif(1,min=parms$min.lambda, max=parms$max.lambda)*n[i]
  }
  time <- 0:max.t
  return(data.frame(time, n))
}

# Demographic Stocasticity

# Uses a random poison distribution and a random binomial distribution to model the dynamics
# of geometric growth with demograhic stocasticity. It uses a for loop to solve the 
# recursion equation. The initial population size is n0. The parms list contains two elements:
# fecundity, which represents mean fecundity of the population, and mortality, the probability
# of mortality at each timestep. Both values must be positive. Returns a data frame of 
# population size (n) and corresponding time values from times 0 to max.t.

PE_GeomGrowth_DemStoc <-function(n0, parms, max.t)
{
  n<-c(n0)
  time <- 0:max.t
  for(i in 1:max.t)
  {
    n[i+1] <- max(0,n[i] + sum(rpois(n[i],parms$fecundity))-rbinom(1,size=n[i],prob=parms$mortality))
  }
  out <- n[length(n)]
  return(data.frame(time, n))
}



