#Analyze data from simulation output

#Set initial conditions
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lognorm)
library(glue)
library(RColorBrewer)
#define directory for analysis files
expwd <- "C:/Users/Adam/Desktop/SimThesis/OU-Thesis"
#set directory for simulation data
datawd <- "C:/Users/Adam/Desktop/SimThesis/Pathfinder-Simulation"
setwd(expwd) #sets working directory to analysis folder
#read data that was previous wrangled and printed into appropriate directory
analysisfiles <- list.files(expwd)#list all files in current directory
analysisfiles <- #filter for only .csv files
  analysisfiles[str_detect(analysisfiles,".csv")]
for (readers in 1:length(analysisfiles)) {
  assign(
    str_remove(analysisfiles[readers],".csv"),
    read.csv(analysisfiles[readers],stringsAsFactors = FALSE))
}

#plot occupants that exited building
exited_plotdata <- exited %>% select(-X) %>% #final wrangling steps
  gather(key = Trial,value = exited,-Time..s.) %>%
  mutate(Trial = str_replace(Trial,"Trial.","Trial ")) %>%
  left_join(y = select(datamatrix,Trial,Scenario.Num),by = "Trial") %>%
  mutate(Scenario = paste("S",Scenario.Num,sep = ""))
exited_plot <- #generate plot
  ggplot(exited_plotdata,mapping = aes(x = Time..s.,y = exited)) +
    geom_line(mapping = aes(colour = Scenario, group = Trial)) +
    labs(title = "Total Occupants Exited In Evacuation",
         y = "Occupants Exited", x = "Time (s)") +
    scale_y_continuous(limits = range(exited_plotdata$exited))
#plot occupants that remained in building
remaining_plotdata <- remaining %>% select(-X) %>% #final wrangling steps
  gather(key = Trial,value = remaining,-Time..s.) %>%
  mutate(Trial = str_replace(Trial,"Trial.","Trial ")) %>%
  left_join(y = select(datamatrix,Trial,Scenario.Num),by = "Trial") %>%
  mutate(Scenario = paste("S",Scenario.Num,sep = ""))
remaining_plot <- #generate plot
  ggplot(remaining_plotdata,mapping = aes(x = Time..s.,y = remaining)) +
    geom_line(mapping = aes(colour = Scenario,group = Trial)) +
    labs(title = "Total Occupants Remaining In Evacuation",
         y = "Occupants Remaining", x = "Time (s)") +
    scale_y_continuous(limits = range(remaining_plotdata$remaining))
