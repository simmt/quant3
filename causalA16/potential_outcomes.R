#' Simulate a simple experiement
#' <means stop being title>
#' Stimulate potential outcomes and adds a variable
#' assumes SUTVA, defines the observed outcomes, deletes
#' potential otucomes and returns the result
#' @param n number of observations
#' @param seed RNG seed
#' @return data.table
#' @export
#' @import data.table
simulate_potential_outcomes<-function(n,
                                      seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  prob_y0<-.7
  prob_y1<-.3
  DATA<-data.table(
    y0 =  1 * (runif(n) < prob_y0),
    y1 = 1 * (runif(n) < prob_y1))
    attr(DATA, "seed") <- seed
  DATA
}

simulate_simple_experiment <- function(n, prob_treatment,
                                       seed = sample.int(.Machine$integer.max, 1))
  {
  set.seed(seed)
  DATA<-simulate_potential_outcomes(n)
  #assign some subjects to treatment
  #names var inside the dataset := is defined to be
  #no causality yet, just assigning
  DATA[, prob := prob_treatment]
  DATA[, d := 1 * (runif(n) < prob_treatment)]
  DATA[, y:= 1 * d * y1 + (1-d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL)]
  attr(DATA, "seed")<-seed
  DATA
}

simulate_observational_study <- function(n,
                                         seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  DATA<-simulate_potential_outcomes(n)
  #assign some subjects to treatment
  #names var inside the dataset := is defined to be
  #no causality yet, just assigning
  DATA[, prob_d_equals_1 := plogis(-2 + 4 * ( y1 - y0))]
  DATA[, d := 1 * (runif(n) < prob_d_equals_1)]
  DATA[, y:= 1 * d * y1 + (1-d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL,
              prob_d_equals_1 = NULL)]
  attr(DATA, "seed")<-seed
  DATA
}


#' Title
#'
#' details
#' @param DATA
#' @return number
#' @import data.table
#' @export
#'

calc_naive_difference_in_means <- function(DATA){
  DATA[, mean(y[d==1]) - mean(y[d==0])]
  ##logical test, is d equal to one, returns true or false... hence ==
  ##serves to filter...section
}



