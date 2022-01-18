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

#extra formatting
#Add trial index
pf.index <- c(1:nrow(pf.design))
pf.design <- pf.design %>%
  mutate("Index" = pf.index) %>%
  select(-Rep..)
#rename columns to reflect factors
pf.factors <- c("startposition",
                "obstaclespresent",
                "populationsize","index")
colnames(pf.design) <- pf.factors