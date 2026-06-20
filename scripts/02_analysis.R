library(tidyverse)
library(lme4)

#Inspect variables
summary(ess11$fltdpr)
summary(ess11$ipcrtiva)
summary(ess11$impricha)
summary(ess11$ipfrulea)

table(ess11$cntry)
table(ess11$nobingnd)
table(ess11$fltdpr)

prop.table(table(ess11$is_dpr)) * 100

#Logistic regression 
m1 <- glm(is_dpr ~ z_ipcrtiva + z_impricha + z_ipfrulea + nobingnd,
          data = ess11,
         family = binomial(link = "logit"))

m2 <- glm(is_dpr ~ z_ipcrtiva*nobingnd + z_impricha*nobingnd + 
            z_ipfrulea*nobingnd, data = ess11,
          family = binomial(link = "logit"))

#MHM on fltdpr
m3 <- lmer(fltdpr ~ ipcrtiva + impricha + ipfrulea + nobingnd +
             (1 + ipcrtiva + impricha + ipfrulea + nobingnd | cntry),
           data = ess11)

#Predictive Checks for m2
set.seed(123)

sims <- simulate(m2, nsim = 1000)

obs_y <- mean(ess11$is_dpr)

pred_check <- tibble(
  sim = 1:1000,
  y_hat_mean = map_dbl(sims, mean),
)
