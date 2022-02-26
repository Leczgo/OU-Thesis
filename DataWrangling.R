#Wrangle data from simulation output

#Set initial conditions
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lognorm)
library(glue)
#define directory for analysis files
expwd <- "C:/Users/Adam/Desktop/SimThesis/OU-Thesis"
#set directory for simulation data
datawd <- "C:/Users/Adam/Desktop/SimThesis/Pathfinder-Simulation"
setwd(expwd) #sets working directory to analysis folder
dsgmatrix <- read.csv("thesis_design.csv",stringsAsFactors = FALSE) %>% 
  select(-X.1) %>%#import design matrix
  mutate(Trial = paste("Trial",Trial.Index,sep = " "))#add readable trial col

setwd(datawd) #sets working directory to data folder
#import occupant data from sims
occupants <- data.frame() #make an empty data frame to fill
for (imp_val_occ in 1:nrow(dsgmatrix)){#begins loop to read all occupant data
  tri_occ <- dsgmatrix[imp_val_occ,"Trial.Index"]#Pull trial number
  scen_occ <- dsgmatrix[imp_val_occ,"Scenario.Num"]#Pull Scenario number 
  occ_path <- #create file path to read occupant data from appropriate trial
    str_glue("Trial{tri_occ}/T{tri_occ}_S{scen_occ}_occupants.csv")
  temp_occ <- #create a temporary data frame of summarized data
    read.csv(occ_path,stringsAsFactors = FALSE) %>%#read appropriate data
    summarize("Max TET" = max(exit.time.s.),"Min TET" = min(exit.time.s.),
              "Avg TET lognorm" = 
                estimateParmsLognormFromSample(exit.time.s.)[[1]],
              "sd TET lognorm" =
                estimateParmsLognormFromSample(exit.time.s.)[[2]],
              "Avg TET arithmetic" = mean(exit.time.s.),
              "sd TET arithmetic" = sd(exit.time.s.),
              "Max Active Time" = max(active.time.s.),
              "Min Active Time" = min(active.time.s.),
              "Avg Distance lognorm" = 
                estimateParmsLognormFromSample(distance..m.)[[1]],
              "sd Distance lognorm" =
                estimateParmsLognormFromSample(distance..m.)[[2]],
              "Avg Distance arithmetic" = mean(distance..m.),
              "sd Distance arithmetic" = sd(distance..m.)) %>%
    mutate("Trial.Index" = tri_occ)#add trial number
  occupants <- bind_rows(temp_occ,occupants)#bind new data with previous data
}
datamatrix <- #join occupant data with design matrix into single df
  left_join(x = dsgmatrix,y = occupants,by = "Trial.Index")

#import room data
#import initial trial
rooms_select <- c("time.s.","Remaining..Total.","Exited..Total.")#define cols
exited_select <- c("time.s.","Exited..Total.")
remaining_select <- c("time.s.","Remaining..Total.")
rooms <- read.csv("Trial1/T1_S2_rooms.csv",stringsAsFactors = FALSE) %>%
  select(all_of(rooms_select))#pick specific columns
#create data frame of occupants exited over time
exited <- select(rooms,all_of(exited_select))
#create data frame of occupants remaining over time
remaining <- select(rooms,all_of(remaining_select))#select columns
temp_names <- c("Time (s)","Trial 1")
colnames(exited) <- temp_names#rename columns
colnames(remaining) <- temp_names#rename columns
for (imp_val_rooms in 2:nrow(dsgmatrix)) {#begins loop to read room data
  tri_occ <- dsgmatrix[imp_val_rooms,"Trial.Index"]#Pull trial number
  scen_occ <- dsgmatrix[imp_val_rooms,"Scenario.Num"]#Pull Scenario number
  rooms_path <- #create file path to read occupant data from appropriate trial
    str_glue("Trial{tri_occ}/T{tri_occ}_S{scen_occ}_rooms.csv")
  rooms <- read.csv(rooms_path,stringsAsFactors = FALSE) %>%
    #read appropriate data & select appropriate columns
    select(all_of(rooms_select))
  temp_names[2] <- str_glue("Trial {tri_occ}")#modify column names
  temp_exits <- #create temporary df of total exited occupants
    select(rooms,all_of(exited_select))
  temp_remaining <- #create temp df of total remaining occupants
    select(rooms,remaining_select)
  colnames(temp_exits) <- temp_names#renames columns
  colnames(temp_remaining) <- temp_names#renames columns
  exited <- #joins data together
    full_join(exited,temp_exits,by = temp_names[1])
  remaining <- #joins data together
    full_join(remaining,temp_remaining,by = temp_names[1])
}
exited <- arrange(exited,exited[,1])#sort exited df by time column
remaining <- arrange(remaining,remaining[,1])#sort remaining df by time column
for (rowval in 2:nrow(remaining)) {#remove NA values from 'remaining' df
  for (colval in 1:ncol(remaining)) {
    if (is.na(remaining[rowval,colval])) {
      remaining[rowval,colval] <- remaining[rowval - 1,colval]
    }}}
for (rowval in 2:nrow(exited)) {#remove NA values from 'exited' df
  for (colval in 1:ncol(exited)) {
    if (is.na(exited[rowval,colval])) {
      exited[rowval,colval] <- exited[rowval - 1, colval]
    }}}
#import exit data
#create initial data frame
doors_select <- #define selections
  c("time.s.","Exit1.1","Exit1.2","Exit1.3","Exit1.4","Exit1.5")
doors <- read.csv("Trial1/T1_S2_doors.csv",stringsAsFactors = FALSE) %>%
  select(all_of(doors_select))#create master df & select specific cols
exit1 <- select(doors,all_of(doors_select[c(1,2)]))#create df for exit1
exit2 <- select(doors,all_of(doors_select[c(1,3)]))#create df for exit2
exit3 <- select(doors,all_of(doors_select[c(1,4)]))#create df for exit3
exit4 <- select(doors,all_of(doors_select[c(1,5)]))#create df for exit4
exit5 <- select(doors,all_of(doors_select[c(1,6)]))#create df for exit5
temp_names <- c("Time (s)","Trial 1")#define column names
colnames(exit1) <- temp_names#change column names for exit1
colnames(exit2) <- temp_names#change column names for exit2
colnames(exit3) <- temp_names#change column names for exit3
colnames(exit4) <- temp_names#change column names for exit4
colnames(exit5) <- temp_names#change column names for exit5
for (imp_val_doors in 2:nrow(dsgmatrix)) {#bins loop toimport rest of data
  tri_occ <- dsgmatrix[imp_val_doors,"Trial.Index"]#Pull trial number
  scen_occ <- dsgmatrix[imp_val_doors,"Scenario.Num"]#Pull Scenario number
  doors_path <- #create file path to read occupant data from appropriate trial
    str_glue("Trial{tri_occ}/T{tri_occ}_S{scen_occ}_doors.csv")
  doors <- read.csv(doors_path,stringsAsFactors = FALSE) %>%
    #read appropriate data & select appropriate columns
    select(all_of(doors_select))
  temp_names[2] <- str_glue("Trial {tri_occ}")#modify column names
  temp_exit1 <- select(doors,all_of(doors_select[c(1,2)]))#create temp exit 1 df
  temp_exit2 <- select(doors,all_of(doors_select[c(1,3)]))#create temp exit 2 df
  temp_exit3 <- select(doors,all_of(doors_select[c(1,4)]))#create temp exit 3 df
  temp_exit4 <- select(doors,all_of(doors_select[c(1,5)]))#create temp exit 4 df
  temp_exit5 <- select(doors,all_of(doors_select[c(1,6)]))#create temp exit 5 df
  colnames(temp_exit1) <- temp_names#rename columns
  colnames(temp_exit2) <- temp_names#rename columns
  colnames(temp_exit3) <- temp_names#rename columns
  colnames(temp_exit4) <- temp_names#rename columns
  colnames(temp_exit5) <- temp_names#rename columns
  exit1 <- full_join(exit1,temp_exit1,by = temp_names[1])#joins exit1 data
  exit2 <- full_join(exit2,temp_exit2,by = temp_names[1])#joins exit2 data
  exit3 <- full_join(exit3,temp_exit3,by = temp_names[1])#joins exit3 data
  exit4 <- full_join(exit4,temp_exit4,by = temp_names[1])#joins exit4 data
  exit5 <- full_join(exit5,temp_exit5,by = temp_names[1])#joins exit5 data
}
exit1[is.na(exit1)] <- 0 #replace NA with zero values
exit2[is.na(exit2)] <- 0 #replace NA with zero values
exit3[is.na(exit3)] <- 0 #replace NA with zero values
exit4[is.na(exit4)] <- 0 #replace NA with zero values
exit5[is.na(exit5)] <- 0 #replace NA with zero values

exit1 <- arrange(exit1,exit1[,1])#sort exit1 df by time
exit2 <- arrange(exit2,exit2[,1])#sort exit2 df by time
exit3 <- arrange(exit3,exit3[,1])#sort exit3 df by time
exit4 <- arrange(exit4,exit4[,1])#sort exit4 df by time
exit5 <- arrange(exit5,exit5[,1])#sort exit5 df by time

#summarize exit data
exit1_cum <- arrange(exit1,exit1[,1])#create a new df for cumulative count
exit2_cum <- arrange(exit2,exit2[,1])#create a new df for cumulative count
exit3_cum <- arrange(exit3,exit3[,1])#create a new df for cumulative count
exit4_cum <- arrange(exit4,exit4[,1])#create a new df for cumulative count
exit5_cum <- arrange(exit5,exit5[,1])#create a new df for cumulative count

for (cum_exit in (2:nrow(exit1_cum))) {#cumulative sum of all rows for exit1
  exit1_cum[cum_exit,-1] <- exit1_cum[cum_exit,-1] + exit1_cum[cum_exit - 1,-1]}
for (cum_exit in (2:nrow(exit2_cum))) {#cumulative sum of all rows for exit2
  exit2_cum[cum_exit,-1] <- exit2_cum[cum_exit,-1] + exit2_cum[cum_exit - 1,-1]}
for (cum_exit in (2:nrow(exit3_cum))) {#cumulative sum of all rows for exit3
  exit3_cum[cum_exit,-1] <- exit3_cum[cum_exit,-1] + exit3_cum[cum_exit - 1,-1]}
for (cum_exit in (2:nrow(exit4_cum))) {#cumulative sum of all rows for exit2
  exit4_cum[cum_exit,-1] <- exit4_cum[cum_exit,-1] + exit4_cum[cum_exit - 1,-1]}
for (cum_exit in (2:nrow(exit5_cum))) {#cumulative sum of all rows for exit2
  exit5_cum[cum_exit,-1] <- exit5_cum[cum_exit,-1] + exit5_cum[cum_exit - 1,-1]}

#define function to summarize exit data & join to occupants matrix
sum_exit_data <- function (df1,df2,headername) {
  temp_df <- #create temporary df
    gather(df1[,-1],key = Trial,value = temp) %>%#unpivot into single column
    group_by(Trial) %>%#tell formula to aggregate by trial
    summarize("{headername}" := sum(temp)) %>%#calculate total exited per trial
    mutate(Trial.Index = as.integer(#separate trial number from string
      str_sub(Trial,
      start = 7,
      end = str_length(Trial)))) %>%
    select(-Trial)
  datamatrix <<- #join exit data to data matrix df by trial number
    left_join(x = df2,y = temp_df,by = "Trial.Index")
}
sum_exit_data(df1 = exit1, df2 = datamatrix,headername = "Exit1")#execute exit1
sum_exit_data(df1 = exit2, df2 = datamatrix,headername = "Exit2")#execute exit2
sum_exit_data(df1 = exit3, df2 = datamatrix,headername = "Exit3")#execute exit3
sum_exit_data(df1 = exit4, df2 = datamatrix,headername = "Exit4")#execute exit4
sum_exit_data(df1 = exit5, df2 = datamatrix,headername = "Exit5")#execute exit5
datamatrix <- mutate(datamatrix, TotalExit = Exit1 + Exit2 + Exit3
                     + Exit4 + Exit5)
#clean up variables
rm(doors,rooms,temp_names,temp_occ,temp_remaining,tri_occ,scen_occ,doors_path,
   doors_select,exited_select,imp_val_rooms,imp_val_doors,imp_val_occ,
   occ_path,remaining_select,temp_exit1,temp_exit2,temp_exit3,temp_exit4,
   temp_exit5,rooms_select,rooms_path,temp_exits,cum_exit,rowval,colval)

#one-time print of data .csv files
setwd(expwd)
#write.csv(occupants,"occupants.csv")
#write.csv(exit1,"exit1.csv")
#write.csv(exit2,"exit2.csv")
#write.csv(exit3,"exit3.csv")
#write.csv(exit4,"exit4.csv")
#write.csv(exit5,"exit5.csv")
#write.csv(exited,"exited.csv")
#write.csv(remaining,"remaining.csv")
#write.csv(exit1_cum,"exit1_cum.csv")
#write.csv(exit2_cum,"exit2_cum.csv")
#write.csv(exit3_cum,"exit3_cum.csv")
#write.csv(exit4_cum,"exit4_cum.csv")
#write.csv(exit5_cum,"exit5_cum.csv")
#write.csv(datamatrix,"datamatrix.csv")