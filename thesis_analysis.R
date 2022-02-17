#Analyze data from simulation output

#Set initial conditions
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lognorm)
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

#plot exit data by exit
exit1_cum_plot_data <- exit1_cum %>% select(-X) %>%#df of flattened exit1 data
  gather(key = Trial,value = numexited,-Time..s.) %>% 
  mutate(exit = "Exit 1",Trial = str_replace(Trial,"Trial.","Trial "))
exit2_cum_plot_data <- exit2_cum %>% select(-X) %>%#df of flattened exit2 data
  gather(key = Trial,value = numexited,-Time..s.) %>% 
  mutate(exit = "Exit 2",Trial = str_replace(Trial,"Trial.","Trial "))
exit3_cum_plot_data <- exit3_cum %>% select(-X) %>%#df of flattened exit3 data
  gather(key = Trial,value = numexited,-Time..s.) %>% 
  mutate(exit = "Exit 3",Trial = str_replace(Trial,"Trial.","Trial "))
exit4_cum_plot_data <- exit4_cum %>% select(-X) %>%#df of flattened exit4 data
  gather(key = Trial,value = numexited,-Time..s.) %>% 
  mutate(exit = "Exit 4",Trial = str_replace(Trial,"Trial.","Trial "))
exit5_cum_plot_data <- exit5_cum %>% select(-X) %>%#df of flattened exit5 data
  gather(key = Trial,value = numexited,-Time..s.) %>% 
  mutate(exit = "Exit 5",Trial = str_replace(Trial,"Trial.","Trial "))
all_exit_cum_data <- rbind(exit1_cum_plot_data,exit2_cum_plot_data,
                           exit3_cum_plot_data,exit4_cum_plot_data,
                           exit5_cum_plot_data) %>% #append exit data frames
  left_join(y = select(datamatrix,Trial,Scenario),by = "Trial")
all_exit_cum_plot <- #generate plot
  ggplot(all_exit_cum_data, mapping = aes(x = Time..s.,y = numexited)) +
  geom_line(mapping = aes(colour = Scenario,group = Trial)) + 
  facet_wrap(facets = ~exit) +
  labs(title = "Total Occupants Exited by Exit",
       y = "Occupants Exited",
       x = "Time (s)")

#plot proportion of occupants that exited by trial
prop_exits_plot_data <- datamatrix %>%#final data wrangling
  select(Scenario,Trial.Index,Exit1,Exit2,Exit3,Exit4,Exit5) %>%
  gather(key = exit,value = numexited,Exit1,Exit2,Exit3,Exit4,Exit5)
prop_exited_plot <- #generate plot
  ggplot(prop_exits_plot_data) +
  geom_col(mapping = aes(x = Trial.Index,y = numexited,fill = exit),
           position = "fill") +
  facet_wrap(facets = ~Scenario) +
  labs(title = "Proportion of Agents choosing each Exit",
       x = "Trial Number",y = "Proporition of Agents",fill = "Exit")

#plot TET
TET_plot_data <- #pick data
  select(datamatrix,Trial,Scenario,Trial.Index,
         Max.TET,Avg.TET.arithmetic,sd.TET.arithmetic)
TET_plot <- #generate plot
  ggplot(TET_plot_data,mapping = aes(x = Trial.Index, colour = Scenario)) + 
  geom_segment(mapping = aes(y = Max.TET,xend = Trial.Index+1,yend = Max.TET)) +
  geom_point(mapping = aes(y = Avg.TET.arithmetic,size = sd.TET.arithmetic)) +
  labs(title = "Average and Maximum Evacuation Times",
       x = "Trial Number",y = "Time(s)",size = "Standard Deviation")
#TET_hist <- #plot TET histogram
  #ggplot (TET_plot_data,mapping = aes(x = Max.TET)) +
  #geom_histogram(binwidth = 15) +
  #geom_density()
  #scale_x_continuous(limits = c(min(Max.TET)-25,max(Max.TET)+25)) + 
  #labs(title = "Histogram of Total Evacuation Time",
       #x = "Time (s)",y = "Proportion")

#plot distance traveled
dist_plot_data <- #pick data
  select(datamatrix,Trial,Scenario,Trial.Index,Avg.Distance.arithmetic,
         sd.Distance.arithmetic)
dist_plot <- #generate plot
  ggplot(dist_plot_data,mapping = aes(x = Trial.Index,colour = Scenario)) +
  geom_point(mapping = aes(y = Avg.Distance.arithmetic,
                           size = sd.Distance.arithmetic)) +
  labs(title = "Average Distance Traveled by Evacuating Agents",
       x = "Trial Number",
       y = "Distance (m)",size = "Standard Deviation")

#start ANOVA
anova_df <- #make analysis df
  datamatrix %>%
  select(startposition,obstaclespresent,populationsize,Max.TET) %>%
  mutate(startposition = as.factor(startposition),
         obstaclespresent = as.factor(obstaclespresent),
         populationsize = as.factor(populationsize))
TET_anova <- 
  #aov(Max.TET ~ startposition * obstaclespresent * populationsize,
      #data = anova_df)
  #lm(Max.TET ~ startposition * obstaclespresent * populationsize,
     #data = anova_df)
summary(TET_anova)
anova(TET_anova)
