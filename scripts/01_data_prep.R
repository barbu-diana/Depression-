#Download dataset: https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef?tab=0
library(tidyverse)
data <- readxl::read_excel("~/Downloads/ESS11e04_1/ESS11e04_1.xlsx")

ess11 <- data %>%
  select(
    #Controls
    cntry, nobingnd, 
    
    
    fltdpr,
    
    #Predictors
    ipcrtiva, impricha, ipfrulea) %>%
  
  mutate(
    fltdpr = if_else(fltdpr > 4, NA, fltdpr),
    ipcrtiva = if_else(ipcrtiva > 6, NA, ipcrtiva),
    impricha = if_else(impricha > 6, NA, impricha),
    ipfrulea = if_else(ipfrulea > 6, NA, ipfrulea),
    nobingnd = if_else(nobingnd > 3, NA, nobingnd),
  ) %>%
  drop_na() %>%
  mutate(
    #Outcome
    is_dpr = if_else(fltdpr >= 3, 1, 0),
    #Invert scales
    ipcrtiva = 7 - ipcrtiva,
    impricha = 7 - impricha,
    ipfrulea = 7 - ipfrulea,
    
    #Scale variables
    z_ipcrtiva = scale(ipcrtiva)[ ,1],
    z_impricha = scale(impricha)[ ,1],
    z_ipfrulea = scale(ipfrulea)[ ,1]
  )

ess11 <- ess11 %>%
  mutate(nobingnd = if_else(nobingnd == 1, 0, 1))
