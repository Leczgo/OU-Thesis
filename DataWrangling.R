#Wrangle data from simulation output

#Set initial conditions
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lognorm)
#define directory for analysis files
expwd <- "C:/Users/Adam/Desktop/SimThesis/OU-Thesis"
#set directory for simulation data
datawd <- "C:/Users/Adam/Desktop/SimThesis/Pathfinder-Simulation"
setwd(expwd) #sets working directory to analysis folder
dsgmatrix <- read.csv("thesis_design.csv",stringsAsFactors = FALSE) %>% 
  select(-X.1) #import design matrix

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
    mutate("Trial" = tri_occ)#add trial number
  occupants <- bind_rows(temp_occ,occupants)#bind new data with previous data
}

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

#clean up variables
rm(doors,rooms,temp_names,temp_occ,temp_remaining,tri_occ,scen_occ,doors_path,
   doors_select,exited_select,imp_val_rooms,imp_val_doors,imp_val_occ,
   occ_path,remaining_select,temp_exit1,temp_exit2,temp_exit3,temp_exit4,
   temp_exit5,rooms_select,rooms_path)

#one-time print of data .csv files