#Analyze data from simulation output

#Set initial conditions
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lognorm)
library(RColorBrewer)
library(ggpubr)
library(knitr)
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
exited_labeled <- exited_plotdata %>% filter(Trial == "Trial 32") %>%
  filter(Time..s. == 250) #filter for suspect trial
exited_plot <- #generate plot
  ggplot(exited_plotdata,mapping = aes(x = Time..s.,y = exited)) +
    geom_line(mapping = aes(colour = Scenario, group = Trial)) +
    geom_label(exited_labeled,mapping = aes(label = Trial)) +
    labs(title = "Total Occupants Exited In Evacuation",
         y = "Occupants Exited", x = "Time (s)") +
    scale_y_continuous(limits = range(exited_plotdata$exited))

#plot occupants that remained in building
remaining_plotdata <- remaining %>% select(-X) %>% #final wrangling steps
  gather(key = Trial,value = remaining,-Time..s.) %>%
  mutate(Trial = str_replace(Trial,"Trial.","Trial ")) %>%
  left_join(y = select(datamatrix,Trial,Scenario.Num),by = "Trial") %>%
  mutate(Scenario = paste("S",Scenario.Num,sep = ""))
remaining_labeled <- remaining_plotdata %>% #filters for suspected trial
  filter(Trial == "Trial 32") %>% filter(Time..s. == 250)
remaining_plot <- #generate plot
  ggplot(remaining_plotdata,mapping = aes(x = Time..s.,y = remaining)) +
    geom_line(mapping = aes(colour = Scenario,group = Trial)) +
    geom_label(data = remaining_labeled,mapping = aes(label = Trial)) +
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
all_exit_labeled <- all_exit_cum_data %>% #filter for suspected trial
  filter(Trial == "Trial 32") %>% filter(Time..s. == 250)
all_exit_cum_plot <- #generate plot
  ggplot(all_exit_cum_data, mapping = aes(x = Time..s.,y = numexited)) +
  geom_line(mapping = aes(colour = Scenario,group = Trial)) +
  geom_label(data = all_exit_labeled,mapping = aes(label = Trial)) +
  facet_wrap(facets = ~exit) + #add facets for multiple plots
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
       x = "Trial Number",y = "Time (s)",size = "Standard Deviation")
TET_hist <- #plot TET histogram
  ggplot (TET_plot_data,mapping = aes(x = Max.TET)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(facets = ~Scenario) +
  labs(title = "Histogram of Total Evacuation Time",
       x = "Time (s)",y = "Count")
TET_box <- #plot TET boxplot
  ggplot(TET_plot_data,mapping = aes(x = Scenario, y = Max.TET)) +
  geom_boxplot() +
  labs(title = "Box Plot of Total Evacuation Time",
       x = "Simulation Scenario",y = "Time (s)")
#plot Avg Distance
dist_plot_data <- #pick data
  select(datamatrix,Trial,Scenario,Trial.Index,
         Avg.Distance.arithmetic,sd.Distance.arithmetic)
dist_plot <- #generate plot
  ggplot(Avg_dist_plot_data,mapping = aes(x = Trial.Index, colour = Scenario)) +
  geom_point(mapping = aes(y = Avg.Distance.arithmetic,
                           size = sd.Distance.arithmetic)) +
  labs(title = "Average and Maximum Travel Distance",
       x = "Trial Number",y = "Distance (m)",size = "Standard Deviation")
dist_hist <- #plot TET histogram
  ggplot (dist_plot_data,mapping = aes(x = Avg.Distance.arithmetic)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(facets = ~Scenario) +
  labs(title = "Histogram of Average Travel Distance",
       x = "Distance (m)",y = "Count")
dist_box <- #plot TET boxplot
  ggplot(dist_plot_data,mapping = aes(x = Scenario,
                                      y = Avg.Distance.arithmetic)) +
  geom_boxplot() +
  labs(title = "Box Plot of Average Travel Distance",
       x = "Simulation Scenario",y = "Distance (m)")

#start ANOVA of TET
outsub <- "Outlier(s) Removed"
anova_df <- #make analysis df
  datamatrix %>%
  select(startposition,obstaclespresent,populationsize,Max.TET,
         Avg.Distance.arithmetic,Trial.Index) %>%
  mutate(startposition = as.factor(startposition),
         obstaclespresent = as.factor(obstaclespresent),
         populationsize = as.factor(populationsize)) %>%
  filter(Trial.Index != 32) #remove Trial 32, which is an outlier
TET_anova <- #generate anova model
  aov(Max.TET ~ startposition * obstaclespresent * populationsize,
      data = anova_df)
TET_residual_data <- #draw out residuals & fitted values
  data.frame(TET_anova$residuals,TET_anova$fitted.values) %>%
  mutate(Trial.Index = c(1:31,33:40)) %>% 
  left_join(select(datamatrix,Trial.Index,Scenario),by = "Trial.Index")
TET_residual_plot <- #generate residual plot
  ggplot(TET_residual_data,
         mapping = aes(x = TET_anova.fitted.values,y = TET_anova.residuals)) +
  geom_point(mapping = aes(colour = Scenario)) +
  labs(title = "Plot of Residuals of TET Values",
       x = "Fitted TET Values (s)",
       y = "Residuals",
       subtitle = outsub
       )
TET_qq_plot_residuals <- ggqqplot(TET_anova$residuals) +
  labs(title = "Q-Q Plot of TET Residuals",
       subtitle = outsub)#generate plot to test normality
TET_qq_plot_effects <- ggqqplot(TET_anova$effects) +
  labs(title = "Q-Q Plot of TET Effects",
       subtitle = outsub)#generate plot to test normality
TET_population_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = populationsize,y = Max.TET)) +
    stat_summary(fun = mean,geom = "line",
                 aes(color = "red",group = 1),show.legend = FALSE) +
    stat_summary(fun = mean,geom = "point",
                 aes(color = "red",group = 1),show.legend = FALSE) +
    labs(title = "Main Effects Plot for TET against Population Size",
         x = "Population Factor Level", y = "Response: TET (s)",
         subtitle = outsub)
TET_obstacles_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = obstaclespresent,y = Max.TET)) +
  stat_summary(fun = mean,geom = "line",
               aes(color = "red",group = 1),show.legend = FALSE) +
  stat_summary(fun = mean,geom = "point",
               aes(color = "red",group = 1),show.legend = FALSE) +
  labs(title = "Main Effects Plot for TET against the Presence of Obstacles",
       x = "Obstacle Factor Level", y = "Response: TET (s)",
       subtitle = outsub)
TET_position_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = startposition,y = Max.TET)) +
  stat_summary(fun = mean,geom = "line",
               aes(color = "red",group = 1),show.legend = FALSE) +
  stat_summary(fun = mean,geom = "point",
               aes(color = "red",group = 1),show.legend = FALSE) +
  labs(title = "Main Effects Plot for TET against the Starting Position
       of Agents",
       x = "Position Factor Level", y = "Response: TET (s)",
       subtitle = outsub)

#start ANOVA for avg distance traveled
anova_df <- #remove another outlier that exits for avg distance
  anova_df %>% filter(Trial.Index != 6)
distance_anova <- aov(Avg.Distance.arithmetic ~ populationsize *
                        obstaclespresent * startposition, data = anova_df)
distance_residual_data <- #collect residuals from ANOVA output
  data.frame(distance_anova$residuals,distance_anova$fitted.values) %>%
  mutate(Trial.Index = c(1:5,7:31,33:40)) %>%
  left_join(select(datamatrix,Scenario,Trial.Index),by = "Trial.Index")
distance_residual_plot <- #generate plot of residuals vs. fitted values
  ggplot(data = distance_residual_data,
         mapping = aes(x = distance_anova.fitted.values,
                       y = distance_anova.residuals)) +
    geom_point(mapping = aes(colour = Scenario)) +
    labs(title = "Plot of Residuals of Average Distance Traveled",
         x = "Fitted Distance Values (m)",y = "Residuals",
         subtitle = outsub)
distance_qq_plot_residuals <- #generate qq plot of residuals
  ggqqplot(distance_anova$residuals) +
    labs(title = "Q-Q Plot of Average Distance Residuals",
         subtitle = outsub)
distance_qq_plot_effects <- #generate qq plot of effects
  ggqqplot(distance_anova$effects) + 
    labs(title = "Q-Q Plot of Average Distance Effects",
         subtitle = outsub)
distance_population_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = populationsize,
                                y = Avg.Distance.arithmetic)) +
  stat_summary(fun = mean,geom = "line",
               aes(color = "red",group = 1),show.legend = FALSE) +
  stat_summary(fun = mean,geom = "point",
               aes(color = "red",group = 1),show.legend = FALSE) +
  labs(title = "Main Effects Plot for Average Distance Traveled
       against the Population Size",
       x = "Population Factor Level", y = "Response: TET (s)",
       subtitle = outsub)
distance_obstacles_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = obstaclespresent,
                                y = Avg.Distance.arithmetic)) +
  stat_summary(fun = mean,geom = "line",
               aes(color = "red",group = 1),show.legend = FALSE) +
  stat_summary(fun = mean,geom = "point",
               aes(color = "red",group = 1),show.legend = FALSE) +
  labs(title = "Main Effects Plot for Average Distance Traveled against the
       Presence of Obstacles",
       x = "Obstacle Factor Level", y = "Response: TET (s)",
       subtitle = outsub)
distance_position_me_plot <- #generate main effects plot 
  ggplot(anova_df,mapping = aes(x = startposition,
                                y = Avg.Distance.arithmetic)) +
  stat_summary(fun = mean,geom = "line",
               aes(color = "red",group = 1),show.legend = FALSE) +
  stat_summary(fun = mean,geom = "point",
               aes(color = "red",group = 1),show.legend = FALSE) +
  labs(title = "Main Effects Plot for Average Distance Traveled against
       the Starting Position of Agents",
       x = "Position Factor Level", y = "Response: TET (s)",
       subtitle = outsub)

#take a sample of two from each scenario group for analysis within Pathfinder
#sample(datamatrix[datamatrix$Scenario == "S4","Trial.Index"],2)
#S1 = 18 and 30,S2 = 29 and 24,S3 = 40 and 2,S4 = 31 and 8
#Trial 32 will also be included since it is such a large outlier