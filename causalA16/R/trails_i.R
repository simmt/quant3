library(causalA16)
library(data.table)
set.seed(1)
D<-simulate_potential_outcomes(1000)

randomize<-function(DATA)
{
treated<-sample.int(1000,500)
control<-(1:1000)[treated]
#control<-setdiff(1:1000, treated)
DATA[treated,y:=y1]
DATA[control,y:=y0]
DATA[treated,mean(y)]-D[control, mean(y)]
}

casualsimulate_potential_outcomes

permutation_distribution<-replicate(1000, randomize(D))
mean(permutation_distribution)
sd(permutation_distribution)
hist(permutation_distribution, breaks = 100)




simulate_paired_experiment <- function(n,seed = sample.int(.Machine$integer.max, 1))
{
  stopifnot(n %% 2 == 0) #test run at top of function, stop if not the case that thing inside 'n' is not true
  set.seed(seed)
  DATA<-simulate_potential_outcomes(n)
  #assign some subjects to treatment
  #names var inside the dataset := is defined to be
  #no causality yet, just assigning

    treated<-sample.int(n, n/2)
    control<-(1:n)[treated]
    #control<-setdiff(1:1000, treated)
    DATA[treated,d:=1]
    DATA[control,d:=0]
  DATA[, y:= d * y1 + (1-d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL)]
  attr(DATA, "seed")<-seed
  DATA
}

experiment<-simulate_paired_experiment(1000)
tau_0<-experiment[d==1, mean(y)]-experiment[d==0, mean(y)]
experiment[, .N, .(d, y)]
experiment[, y1:=y]
experiment[, y0=y]
randomize(experiment)

