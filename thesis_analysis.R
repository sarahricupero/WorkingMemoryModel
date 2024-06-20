###Thesis Analysis Script
#
## Sarah Ricupero, skr5576@psu.edu
## Thesis project
## 

# Starts with importing of data output by model
# Conducts t-tests
# Creates plots

# Import necessary libraries
library(tidyverse)

# Import data
# behav_log stores stimuli for each trial, with set ignore/respond cues
# task_log provides states and associated times for each trial
# plotting_data gives rts for each run for each trial
# summary_data gives avg rt and sd for each trial
load("~/Documents/Thesis/Model files/behav_log_run2.Rda")
load("~/Documents/Thesis/Model files/task_log_run2.Rda")
load("~/Documents/Thesis/Model files/plotting_data_run2.Rda")
load("~/Documents/Thesis/Model files/summary_data_run2.Rda")

#Pull out a vector that names trials as novel/learned
task_log$cond <- 0
for(i in 1:72){
  if(stim_order[i] == 1 | stim_order[i] == 2){
    #Cond is 1 for learned trials
    task_log[task_log$trial == i,]$cond <- 1
  }else{
    #Cond is 0 for novel trials
    task_log[task_log$trial == i, ]$cond <- 0
  }
}

trial_conds <- vector('numeric', length=72)
for(i in 1:72){
  sub <- task_log[task_log$trial == i, ]
  trial_conds[[i]] <- sub$cond[[1]]
}

trial_conds[trial_conds == 0] <- "novel"
trial_conds[trial_conds == 1] <- "learned"

summary_data$cond <- trial_conds

mean(summary_data$avg_rt, na.rm = TRUE)
mean(summary_data$sd, na.rm = TRUE)

mean(summary_data$avg_rt[summary_data$cond == "learned"], na.rm = TRUE)
mean(summary_data$sd[summary_data$cond == "learned"], na.rm = TRUE)

mean(summary_data$avg_rt[summary_data$cond == "novel"], na.rm = TRUE)
mean(summary_data$sd[summary_data$cond == "novel"], na.rm = TRUE)

t.test(avg_rt ~ cond, data = summary_data)

plotting_dataMelt <- melt(plotting_data, na.rm=TRUE, id.vars = c("trial"))

ggplot() + geom_point(data=plotting_dataMelt, aes(trial, value)) + labs(x = "Trial", y = "Reaction Time (s)") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 18, face = "bold",color = "black"),
        axis.text.y = element_text(size = 18, face = "bold",color = "black"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text( size = 18, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  ggtitle("Model Data (n = 20)") + scale_fill_discrete(name = "Condition")

ggplot(summary_data, aes(x=trial, y=avg_rt)) +  labs(x = "Trial", y = "Reaction Time (s)") +
  geom_point()+ 
  geom_errorbar(aes(ymin=avg_rt-sd, ymax=avg_rt+sd), width=.2, 
                position=position_dodge(0.05)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 18, face = "bold",color = "black"),
        axis.text.y = element_text(size = 18, face = "bold",color = "black"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text( size = 18, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  ggtitle("Averaged Model Data (n = 20)") + scale_fill_discrete(name = "Condition")
