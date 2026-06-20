library(tidyverse)
library(lme4)
library(sjPlot)
library(modelsummary)

#Variables distributions
ggplot(ess11, aes(x = fltdpr)) +
  geom_bar() +
  labs(
    title = "Distribution of Felt Depressed",
    x = "Felt Depressed",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ess11, aes(x = ipcrtiva)) +
  geom_bar() +
  labs(
    title = "Distribution of Importance of Creativity",
    x = "Important to think new ideas and be creative",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ess11, aes(x = impricha)) +
  geom_bar() +
  labs(
    title = "Distribution of Importance of Money",
    x = "Important to be rich, have money and expensive things",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ess11, aes(x = ipfrulea)) +
  geom_bar() +
  labs(
    title = "Distribution of Importance of Rules",
    x = "Important to do what is told and follow rules",
    y = "Count"
  ) +
  theme_minimal()

ggplot(ess11, aes(x = is_dpr)) +
  geom_bar() +
  labs(
    title = "Distribution of Being Depressed",
    x = "Is Depressed",
    y = "Count"
  ) +
  theme_minimal()

#Logistic regression table
modelsummary(
  list(
    "Model 1" = m1,
    "Model 2: Interactions" = m2
  ),
  stars = TRUE,
  statistic = "std.error",
  gof_omit = "R2 Adj.|AIC|BIC|Log.Lik.|F|RMSE|Num.Obs."
)

#MHM table
sjPlot::tab_model(m3)

#Predictive checks plot
ggplot(pred_check, aes(x = y_hat_mean)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = obs_y, linetype = "dashed", linewidth = 1) +
  labs(
    x = "Simulated probabilities of depression",
    y = "Number of simulations",
    title = "Predictive checks for probability of depression"
  )
