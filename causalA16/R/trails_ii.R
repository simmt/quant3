#Figure 4.10
library(causalA16)
library(ggplot2)
library(data.table)
set.seed(8)
data_for_figure_4_1 <- make_data_for_figure_4_1(1000)
calc_average_causal_effect_estimand(data_for_figure_4_1)
calc_naive_difference_in_means(data_for_figure_4_1)
calc_conditional_difference_in_means(data_for_figure_4_1, "s")
calc_conditional_difference_in_means(data_for_figure_4_1, "x")

simulations <- replicate(1000, {
  data_for_figure_4_1 <- make_data_for_figure_4_1(1000)
  c(calc_average_causal_effect_estimand(data_for_figure_4_1),
    calc_naive_difference_in_means(data_for_figure_4_1),
    calc_conditional_difference_in_means(data_for_figure_4_1, "s"),
    calc_conditional_difference_in_means(data_for_figure_4_1, "x"))
})
?replicate
sim_data <- data.table(t(simulations))
setnames(sim_data, c("ACE_estimand",
                     "naive_diff_in_means",
                     "diff_in_means_cond_s",
                     "diff_in_means_cond_x"))
sim_data[, `:=`(
  MAE_diff_in_means = abs(ACE_estimand - naive_diff_in_means),
  MAE_diff_in_means_cond_s = abs(ACE_estimand - diff_in_means_cond_s),
  MAE_diff_in_means_cond_x = abs(ACE_estimand - diff_in_means_cond_x)
)]
#melt turns wide to long... on big vector of values
### . makes list of columns
melt_sim_data <- melt(sim_data[, .(MAE_diff_in_means, MAE_diff_in_means_cond_s,
                                   MAE_diff_in_means_cond_x)])

ggplot(melt_sim_data, aes(y = value, x = variable)) +
  geom_boxplot()

#' Simulate data that corresponds to Figure 4.1 from MgW
#'
#' DETAILS
#' @param n number of observations
#' @param seed RNG seed
#' @return data.table with potential outcomes
#' @import data.table
#' @export
make_data_for_figure_4_1 <- function(n,
                                     seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)


  u <- rbinom(n, 1, prob = .5)
  s <- rbinom(n, 1, prob = plogis(-1 + 2 * u))
  x <- rbinom(n, 1, prob = plogis(1 - 2 * u))
  d <- rbinom(n, 1, prob = plogis(-3  + 6 * s))
  y1 <- rbinom(n, 1, prob = plogis(-1 + 2 * 1 - 2 * x))
  y0 <- rbinom(n, 1, prob = plogis(-1 + 2 * 0 - 2 * x))


  y <- d * y1 + (1 - d) * y0
  DATA <- data.table(y1, y0, y, u, s, x, d)
  attr(DATA, "seed") <- seed
  DATA
}


#' Calculate average causal effect estimand
#'
#' DETAIL
#' @param DATA a dataset with potential outcomes named y0 and y1
#' @return number
#' @import data.table
#' @export
calc_average_causal_effect_estimand <- function(DATA, conditioning_variable)
{
  DATA[, mean(y1 - y0)]
}

#' Calculate the naive difference in means
#'
#' Calculate the naive difference in means
#' @param DATA a dataset, can't have a variable named "Z" in it
#' @param conditioning_variable character object for variable name in DATA;
#' assumed to be binary 0-1
#' @return number
#' @import data.table
#' @export
calc_conditional_difference_in_means <- function(DATA, conditioning_variable)
{
  Z <- DATA[, conditioning_variable, with = FALSE]
  p_Z_1 <- DATA[, mean(Z == 1)]
  p_Z_0 <- DATA[, mean(Z == 0)]
  mean_y_D_1_Z_1 <- DATA[, mean(y[d == 1 & Z == 1])]
  mean_y_D_1_Z_0 <- DATA[, mean(y[d == 1 & Z == 0])]
  mean_y_D_0_Z_1 <- DATA[, mean(y[d == 0 & Z == 1])]
  mean_y_D_0_Z_0 <- DATA[, mean(y[d == 0 & Z == 0])]

  #mean_y_D_1_Z_1 <- DATA[[d == 1 & Z == 1], mean(y)]
  #mean_y_D_1_Z_0 <- DATA[[d == 1 & Z == 0], mean(y)]
  #mean_y_D_0_Z_1 <- DATA[[d == 0 & Z == 1], mean(y)]
  #mean_y_D_0_Z_0 <- DATA[[d == 0 & Z == 0], mean(y)]

  (p_Z_1 * mean_y_D_1_Z_1 + p_Z_0 * mean_y_D_1_Z_0) -
    (p_Z_1 * mean_y_D_0_Z_1 + p_Z_0 * mean_y_D_0_Z_0)
}
