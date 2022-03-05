#+eval = FALSE
#creating a fractional factorial design
#2^3-1

#load packages
library(AlgDesign)
library(dplyr)
library(tidyr)

#defining the design
#define the levels
levels.design <- c(2,2,2)
#create a full 2^3 factorial design
f.design <- gen.factorial(levels = levels.design)
# create the fractional design from the full design
pf.design <- optFederov(data = f.design,
           nTrials = sum(levels.design),
           approximate = TRUE)
#extract a dataframe of the design
pf.design <- pf.design$design

#extra formatting
#rename columns to reflect factors
pf.factors <- c("startposition",
                "obstaclespresent",
                "populationsize")
pf.design <- pf.design %>% select(-Rep..)
colnames(pf.design) <- pf.factors
#Add extra columns to describe factors
pf.design <- pf.design %>%
  mutate("Scenario" = c("S1","S2","S3","S4"),
         "Starting Position" = 
           ifelse(startposition < 0,"Unconcentrated","Concentrated"),
         "Obstacles" = 
           ifelse(obstaclespresent < 0, "Not-Present","Present"),
         "Population Size" = 
           ifelse(populationsize < 0, "Low", "High"))
  
#create data frame for the design including replications
#define the number of replications
n.reps <- 10
#copy the original design dataframe
design.dummy <- pf.design
#concatenate the dataframe n-1 times
i.reps <- 1
while (i.reps < n.reps) {
  pf.design <- pf.design %>%
    bind_rows(design.dummy)
  i.reps <- i.reps +1
}
#randomize the trials
pf.design <- pf.design[sample(1:nrow(pf.design)),]
#Add trial index
pf.index <- c(1:nrow(pf.design))
pf.design <- pf.design %>% mutate("Trial Index" = pf.index)

#one-time print of design-matrix to .csv file
#write.csv(pf.design, file = "thesis_design.csv")

#one-time quality check of simulation results
#four simulation runs were sampled for errors
#(e.g. incorrect population size, improper distribution, etc.)
#sample taken after 35 simulation runs had been conducted
smpl <- sample(c(1:35),4)
#results are [4,22,14,1], therefore those 4 trials will be checked
smpl <- c(4,22,14,1)