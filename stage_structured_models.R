
PE_PopProjection <- function(n0, n.gen, P){
  n<-matrix(NA,nrow=n.gen+1,ncol=length(n0)) 
  n[1,]<-n0 #set first value of population vector to n0
  for(t in 1:(n.gen))
  {
    n[t+1,]<-P %*% n[t,] #loop through
  }
  return(n)
}

#Projection interval = 1 day
fecundity<-10 #number of eggs produced by an adult per day
P_el<-0.5 #probability that an egg survives and hatches in a given day. 50% survival, egg stage lasts one day
P_ll<-0.6*(1-(1/30)) #probability that a larva survives and doesnt become an adult in a given day. 60% survival, larval stage lasts 30 days
P_la<-0.6*(1/30) #probability that larva survives and transitions to adult
P_aa<-1-1/33 #daily adult survival , adult stage lasts 33 days

P<-matrix(c(0,0,fecundity,P_el,P_ll,0,0,P_la,P_aa),nrow=3,ncol=3,byrow=TRUE) #fill in the matrix values by row
n0<-c(eggs=2000, larvae=0,adults=0)

# create empty matrix (2 dimensional vector)
#each row is one time step and the columns represent the size of each stage class
n.gen<-150
n0<-c(eggs=100,larvae=0,adults=0)
N<-PE_PopProjection(n0=n0,n.gen=n.gen, P=P)
matplot(0:n.gen, N, type='l',col=c("red","green","blue"),lty="solid")
n[91,]/n[92,]



#find the dominant eigen value
lambda <- Re(eigen(P)$value[1]) #growth rate of population
SSD <- eigen(P)$vector[,1]/eigen(P)$vector[1,1] #Stable Stage Distribution
RV <- Re(eigen(t(P))$vector[,1]/eigen(t(P))$vector[1,1]) # left eigen value = repoductive value of each stage
percentage_SSD <- eigen(P)$vectors[,1]/sum(eigen(P)$vectors[,1])*100 #gives you percentages of each group

