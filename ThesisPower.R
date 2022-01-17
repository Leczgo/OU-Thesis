#This file is created to analyze statistical power of
#my thesis. It is also used to determine the number of
#replications for analysis.

#The analysis herein utilizes the R package 'Superpower'
#Methodology utilized is derived from Caldwell & Lakens (2021)

#load 'Superpower' package
library(Superpower)
library(ggplot2)
library(dplyr)
library(tidyr)

#Step 1 - Create Design
#define parameters
n <- 10
# r is set to zero thanks to between-group design
r <- 0
#shared standard deviation
mu <- c(0.5,0.25,0.25,0.0,-0.25,-0.25)
sd <- 1

design_string <- "2b*3b"
label_names <- c(
  "startingposition","nonconcentrated","concentrated",
  "obstacles","none","normal","distanced"
  )

design_result <- ANOVA_design(
  design = design_string,
  n = n,
  mu = mu,
  sd = sd,
  r = r,
  labelnames = label_names
)

#Step 2 - Power Analysis
#set simulation parameters
nsims <- 1000
alpha <- 0.10

sim_result <- ANOVA_power(design_result = design_result,
                          alpha_level = alpha,
                          nsims = nsims)

exact_result <- ANOVA_exact(design_result = design_result,
                            alpha_level = alpha)

#Step 3 - Plot Power

plot_power(design_result = design_result,
           max_n = 100,
           plot = TRUE,
           verbose = TRUE,
           desired_power = 80)
