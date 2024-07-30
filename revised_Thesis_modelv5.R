
### An ACT-R-like model of a sequence learning task

## Sarah Ricupero, skr5576@psu.edu
## Thesis project
## v2 doubles the task length to incorporate the second run in the scanner
## v3 had everything written so that only the number of runs needs to be changed.
## v4 makes final model adjustments and does desired number of runs (150)
## v5 takes out values with retrieval probability less than 0.25 and changes presentation time for learned chunks.

# Task is written as a function that takes in sequences being used, 
# timing of sequences, and number of runs
# basically takes in the details of the task

# Import necessary libraries
library(tidyverse)

#Define sequences used in task (learned_stim and novel_stim), with indices corresponding to four defined gabor_stimuli
learned_stim <- rbind(c(3,1,4,2), c(2,4,3,1))
novel_stim <- rbind(c(4,3,1,2), c(1,2,3,4), c(4,2,1,3), c(1,4,2,3), c(1,3,4,2), c(4,1,3,2), c(4,2,3,1), c(1,3,2,4), c(1,2,4,3), c(4,1,2,3), c(1,4,3,2), c(4,3,2,1))

#Create a vector (stim_order) that contains the stimulus order, where 1 and 2 are learned and 3-14 are novel
stim_order <- c(1,3,2,4,2,1,1,2,5,1,6,2,1,7,2,2,1,8,1,2,9,10,2,1,11,2,1,2,1,12,2,13,1,1,2,14,1,3,2,4,2,1,1,2,5,1,6,2,1,7,2,2,1,8,1,2,9,10,2,1,11,2,1,2,1,12,2,13,1,1,2,14)

# Define four gabor stimuli being used, and define press_key function
gabor_stimuli <- c('horizontal', 'vertical', 'rightDiagnol', 'leftDiagnol')
press_key <- function(key){
  return(key)
}

# Initialize keys used for responses
response_keys <- c('d', 'f', 'j', 'k')

# Define mapping of key press to stimuli
key_mapping <- data.frame(stimulus = gabor_stimuli, key = response_keys)

# Initialize behavior log for task, including trial number, stimulus (1-4), start time of trial & cue
behavior_log <- data.frame(trial = seq(1,72,1), stim1 = vector("numeric", length = 72),
                           stim2 = vector("numeric", length = 72), stim3 = vector("numeric", length = 72), 
                           stim4 = vector("numeric", length = 72), trialStartTime = vector("numeric", length = 72), 
                           cue = vector("character", length = 72) )

#Define function to lay out trials of the task
gaborSequenceTask <- function(learned_stim, novel_stim, stim_order, gabor_stimuli){
  #reset clock
  clock <- 0
  
  # define cue; ignore or respond
  response_cue <- c('ignore', 'respond')
  
  #Bind the learned stimuli above the novel
  all_stim <- rbind(learned_stim, novel_stim)
  
  # Create vector with recall cue on 3/4 of trials
  cue_zeros <- sample(x = 1:72, size  = 18)
  cue_nonzero <- seq(1:72)
  cue_nonzero <- cue_nonzero[!(cue_nonzero %in% cue_zeros)]
  behavior_log$cue[cue_zeros] <- response_cue[1]
  behavior_log$cue[cue_nonzero] <- response_cue[2]
  
  #Present stimuli in a sequence
  
  for(i in seq_along(stim_order)){
    curr_trial <- vector("character", length = 4)
    behavior_log$trialStartTime[i] <- clock
    for (j in 1:4){
      curr_trial[j] <- gabor_stimuli[all_stim[stim_order[i],j]]
    }
    behavior_log[i, 2:5] <- curr_trial
    clock <- clock + 21.6
    
    
  }
   
   return(behavior_log)
}

#Define ACT-R functions for activation, recall probability, and retrieval time
baseLevelActivation <- function(presentation, chunk_time){
  #set value for decay
  decay <- 0.5
  
  bLA <- log(presentation/(1-decay)) - decay*log(chunk_time)
  return(bLA)
}

recall_probability <- function(baseLevelActivation){
  threshold <- 0
  rP <- 1/(1 + exp(threshold - baseLevelActivation))
  return(rP)
}

retrieval_time <- function(baseLevelActivation){
  latency_factor <- 0.4
  time <- latency_factor*exp(-1*baseLevelActivation)
  return(time)
}

# Use the gaborSequenceTask function to create trials
# This contains a sample function, the trials assigned ignore & respond will change every time
behavior_log <- gaborSequenceTask(learned_stim, novel_stim, stim_order, gabor_stimuli)

# Transform stimuli presented into working memory chunks.
# Each chunk contains 2 items, WM capacity is 2 chunks
stim_behav <- behavior_log[c(2:6)]

# Define a dataframe to store chunk information; called chunk_data
chunk_data <- data.frame(chunk1=vector("character", length = 72), chunk2=vector("character", length = 72),
                         time1=vector("numeric", length = 72), time2=vector("numeric", length = 72))
# Populate chunk_data using stim_behav data frame
for(i in 1:nrow(stim_behav)){
  chunk_data$chunk1[i] <- paste(stim_behav[i,1], stim_behav[i,2])
  chunk_data$time1[i] <- stim_behav$trialStartTime[i] + 2.4
  
  chunk_data$chunk2[i] <- paste(stim_behav[i,3], stim_behav[i,4])
  chunk_data$time2[i] <- stim_behav$trialStartTime[i] + 7.2
}

chunk1_data <- chunk_data[c(1,3)]
colnames(chunk1_data) <- c("chunk", "time")
chunk2_data <- chunk_data[c(2,4)]
colnames(chunk2_data) <- c("chunk", "time")
#Stack the chunks and the times, so that we have 2 columns
chunk_list <- rbind(chunk1_data, chunk2_data)
chunk_list <- chunk_list[order(chunk_list$time),]
#temp <- data.frame(chunk = ids, id = c(1:12))
#dplyr::left_join(chunk_list, temp, by = chunk)

#Create id variable for each chunk
ids <- unique(chunk_list$chunk)
for(i in 1:length(ids)){
  chunk_list$id[chunk_list$chunk == ids[i]] <- i
}

## Run through the task using the behavior log, keep a log of values and outputs
# trial refers back to behavior_log
# time refers to current time in task
# state refers to current state of task (start, encode-chunk, encode-cue, respond)
# attend refers to a boolean where 0 is nothing in attention and 1 is something in attention
# encode_time refers to the time at which a chunk is encoded (presentation time of second item)
# presentation (when encode-chunk) refers to number of times chunk has been presented
# chunk_time (when encode-chunk) refers to time since first presentation of chunk
create_tasklog <- function(behavior_log, chunk_list){
  task_log <- data.frame(trial=vector("numeric", length = 288), time=vector("numeric", length = 288), state=vector("character", length = 288), 
                         attend=vector("numeric", length = 288), encode_time=vector("numeric", length=288), presentation=vector("numeric", length = 288), 
                         chunk_time=vector("numeric", length = 288), wm_1=vector("character", length = 288), wm_2=vector("character", length = 288), cond=vector("numeric", length=288))
  # Each stimulus sets the time it was first seen, which is used to calculate chunk_time at any point
  #chunk_time <-   vector("numeric", length = 12)
  # We define a chunk as two consecutive items
  #chunk <- vector("character", length=2)
  # Add chunk to working memory, this is limited by (set memory span of 2 chunks & time)
  #wm_storage <- vector("list", length = 2)
  count <- 1
  for(i in 1:nrow(behavior_log)){
    #Set the trial's start time
    current_time <- behavior_log$trialStartTime[[i]]
    
    # Iterate through start, chunk1(stim1, stim2), chunk2(stim3, stim4), response
    for(j in 1:4){
      task_log$trial[[count]] <- behavior_log$trial[[i]]
      
      #Iterate through the trial where the step you're at matters
      if(j == 1){
        task_log$time[[count]] <- current_time
        task_log$state[[count]] <- "start"
        task_log$attend[[count]] <- 0
        task_log$state[[count]] <- "fixation"
      }else if(j == 2 | j == 3){
        task_log$attend[[count]] <- 1
        task_log$state[[count]] <- "encode_chunk"
        #Build a chunk
        if(j == 2){
          #Add time from first stim presentation (2.4s) 
          current_time <- current_time + 2.4
          task_log$time[[count]] <- current_time
          task_log$wm_1[[count]] <- paste(behavior_log[i,2], behavior_log[i,3])
        }else if(j == 3){
          #Add time from stim2 and stim3 (2.4 + 2.4)
          current_time <- current_time + 4.8
          task_log$time[[count]] <- current_time
          task_log$wm_2[[count]] <- paste(behavior_log[i,4], behavior_log[i,5])
          task_log$encode_time[[count]] <- behavior_log$trialStartTime[[i]] + 9.6
        }
        
      }else if(j == 4){
        #Add time from stim4
        current_time <- current_time + 7.2
        task_log$time[[count]] <- current_time 
        task_log$attend[[count]] <- 1
        if(behavior_log$cue[[i]] == "respond"){
          task_log$state[[count]] <- "respond"
        }else{
          task_log$state[[count]] <- "ignore"
        }
        #Add time from response cue & time given to respond (this isn't actually used)
        current_time <- current_time + 7.2
      }
      count <- count + 1
    }
  }

  #Label the chunks with their id
  for(i in 1:nrow(task_log)){
    if(task_log$state[[i]] == "encode_chunk"){
      chunk <- paste0(task_log$wm_1[[i]], task_log$wm_2[[i]])
      task_log$id[[i]] <- chunk_list$id[chunk_list$chunk == chunk][[1]]
    }else{task_log$id[[i]] <- 0 }
  }
  task_log$id <- as.numeric(task_log$id)
  
  #update presentation and chunk time based on task; these will only be updated at the respond state
  #presentation: number of times stimulus has been presented
  #chunk_time: time since first presentation
  # create vector to hold a list of chunks seen
  chunk_history <- vector("numeric", length=144)
  chunk_iterate <- 1
  for(i in 1:nrow(task_log)){
    if(task_log$state[[i]] == "respond" | task_log$state[[i]] == "ignore"){
      #Pull id from last two chunks presented
      curr_chunk1 <- task_log[[i-2,11]]
      curr_chunk2 <- task_log[[i-1,11]]
      
      chunk_history[chunk_iterate] <- curr_chunk1
      chunk_history[chunk_iterate+1] <- curr_chunk2
      chunk_iterate <- chunk_iterate + 2
      
      task_log$presentation[[i-2]] <- length(which(chunk_history == curr_chunk1))
      task_log$presentation[[i-1]] <-length(which(chunk_history == curr_chunk2))
      # This will be the time that the response is asked for, and will be used to find time since first presentation
      task_log$chunk_time[[i-2]] <- task_log$time[i]
      task_log$chunk_time[[i-1]] <- task_log$time[i]
      
    }
  } 
  
  # We need to calculate the time since first presentation, using the chunk_time listed. 
  # To do this, create a list of unique chunks with their time of first presentation
  chunk_list <- chunk_list[order(chunk_list$time),]
  unique_chunk_list <- chunk_list[!duplicated(chunk_list$id), ]
  unique_chunk_list <- unique_chunk_list[order(unique_chunk_list$id), ]
  return(task_log)
}

task_log <- create_tasklog(behavior_log, chunk_list)
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
# Add pre learning for chunks in learned sequences, each learned sequence presented 12 times each
# 3 1 rightDiag horizontal , 4 2 leftDiag vertical, 2 4 vertical leftDiag, 3 1 rightDiag horizontal
# Add 24 to rightDiag horizontal, 12 to other 2 chunks
task_log[task_log$id == 1, ]$presentation <- task_log[task_log$id == 1,]$presentation + 24
task_log[task_log$id == 5, ]$presentation <- task_log[task_log$id == 5,]$presentation + 12
task_log[task_log$id == 3, ]$presentation <- task_log[task_log$id == 3,]$presentation + 12

#Pull out a vector that names trials as novel/learned
trial_conds <- vector('numeric', length=72)
for(i in 1:72){
  sub <- task_log[task_log$trial == i, ]
  trial_conds[[i]] <- sub$cond[[1]]
}

#Populate a dataframe with 10 columns of error values to pull from 
num_encode <- nrow(task_log[task_log$state == "encode_chunk", ])

error_values_l <- data.frame(trial=seq(1, 144, 1))
error_values_n <- data.frame(trial=seq(1, 144, 1))
##################################################
# Set number of times you want to run your model #
##################################################
num_runs <- 150
# break up lower error for novel trials, higher error for learned
ind_error_n <- runif(num_runs, 0.1,0.3)
ind_error_l <- runif(num_runs, 0.2, 0.6)

#Create separate error data frames for novel and learned.
for(i in 1:num_runs){
  col_num <- 1 + i
  error_values_l[col_num] <- rnorm(144, mean=0, (pi)^2/3) * (ind_error_l[i])^2
  colnames(error_values_l)[col_num] <- paste0("error",i)
  error_values_n[col_num] <- rnorm(144, mean=0, (pi)^2/3) * (ind_error_n[i])^2
  colnames(error_values_n)[col_num] <- paste0("error",i)
}

# When calculating the chunk_time in our equations, we will subtract the time from recall in task_log from the time in unique_chunk_list
unique_chunk_list <- chunk_list[!duplicated(chunk_list$chunk),]

# The chunks seen in the training session are adjusted so that their initial view time is 0s.
# Change so that the learned chunks have an earlier time.
unique_chunk_list$time[c(1,3,5)] <- -600

create_modelvalues <- function(task_log, num_encode, error_values){
  model_values <- data.frame(trial=vector("numeric", length=num_encode), bLactivation=vector("numeric", length=num_encode),
                              recall_prob=vector("numeric", length=num_encode), rt=vector("numeric", length=num_encode))
  iter_val <- 1
  for(i in 1:nrow(task_log)){
    if(task_log[[i,3]] == "encode_chunk"){
      current_response <- task_log[i, ]
      model_values$trial[[iter_val]] <- current_response[[1]]
      if(task_log[[i,10]] == 1){
        curr_error <- error_values_l[iter_val, runif(1, 1, 10)]
      }else{
        curr_error <- error_values_n[iter_val, runif(1, 1, 10)]
      }
      curr_chunktime <-  task_log[[i,7]] + 12 - unique_chunk_list[[current_response$id,2]] 
      bl_act <- baseLevelActivation(current_response$presentation,curr_chunktime)
      model_values$bLactivation[[iter_val]] <- bl_act
      act <- bl_act + curr_error
      model_values$recall_prob[[iter_val]] <- recall_probability(act)
      # Using KLM, .28 seconds per keystroke
      model_values$rt[[iter_val]] <- retrieval_time(act) + .28*2
      #move to next response
      iter_val <- iter_val + 1
    } else{}
  }
  return(model_values)
}

data_list <- list()
for(i in 1:num_runs){
  curr_vals <- create_modelvalues(task_log, num_encode, error_values)
  data_list[[i]] <- curr_vals
}

##############################
# Create a run with no error #
##############################
base_values <- data.frame(trial=vector("numeric", length=num_encode), bLactivation=vector("numeric", length=num_encode),
                           recall_prob=vector("numeric", length=num_encode), rt=vector("numeric", length=num_encode))
iter_val <- 1
for(i in 1:nrow(task_log)){
  if(task_log[[i,3]] == "encode_chunk"){
    current_response <- task_log[i, ]
    base_values$trial[[iter_val]] <- current_response[[1]]
    curr_error <- 0 
    curr_chunktime <-  task_log[[i,7]] + 12 - unique_chunk_list[[current_response$id,2]] 
    bl_act <- baseLevelActivation(current_response$presentation,curr_chunktime)
    base_values$bLactivation[[iter_val]] <- bl_act
    act <- bl_act + curr_error
    base_values$recall_prob[[iter_val]] <- recall_probability(act)
    # Using KLM, .28 seconds per keystroke
    base_values$rt[[iter_val]] <- retrieval_time(act) + .28*2
    #move to next response
    iter_val <- iter_val + 1
  } else{}
}

trial_conds <- vector('numeric', length=72)
for(i in 1:72){
  sub <- task_log[task_log$trial == i, ]
  trial_conds[[i]] <- sub$cond[[1]]
}

create_meanrt <- function(model_values){
  mean_rt <- vector("numeric", length = 72)
  for(i in 1:72){
    mean_rt[[i]] <- mean(model_values$rt[model_values$trial == i])
  }
  return(mean_rt)
}

#If probability of recall is below 0.25, set RT to NA
base_values$rt[base_values$recall_prob < 0.25] <- NA

base_mean_rt <- create_meanrt(base_values)
base_mean_rt <- base_mean_rt/2
trial_conds[trial_conds == 0] <- "novel"
trial_conds[trial_conds == 1] <- "learned"
base_rt <- data.frame(trial=c(1:72), rt=base_mean_rt, cond <- trial_conds)
colnames(base_rt)[[3]] <- c("cond")
base_activation <- dplyr::left_join(base_values,base_rt[c(1,3)], join_by(trial))

summary(lm(trial ~ rt, data=base_rt))

t.test(rt ~ cond, data = base_rt)

ggplot() + geom_point(data=base_rt, aes(trial,rt, colour = cond), size = 2.5)  + labs(x = "Trial", y = "Reaction Time (s)") +
  #geom_smooth(data = base_rt, aes(x=trial, y=rt, colour = cond), method = lm, SE = FALSE)+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 18, face = "bold",color = "black"),
        axis.text.y = element_text(size = 18, face = "bold",color = "black"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text( size = 18, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  ggtitle("Model Reaction Times with No Noise") + scale_colour_manual(values = c("gray", "black"))

#Create plot of activation values across trials
ggplot() + geom_point(data=base_activation, aes(trial,bLactivation, colour = cond), size = 2.5)  + labs(x = "Trial", y = "Base level Activation") +
  #geom_smooth(data = base_rt, aes(x=trial, y=rt, colour = cond), method = lm, SE = FALSE)+
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 18, face = "bold",color = "black"),
        axis.text.y = element_text(size = 18, face = "bold",color = "black"),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text( size = 18, face = "bold"),
        legend.text = element_text(size = 13, face = "bold")) + 
  ggtitle("Base Model Activation over trials") + scale_colour_manual(values = c("gray", "black"))


###################################################################################
#Make data_list more readable, so it can be saved out and used for further analyses.
for(i in 1:length(data_list)){
  run_num <- rep(i,144)
  data_list[[i]][[5]] <- run_num
}
data_list_frame <- do.call("rbind", data_list)
colnames(data_list_frame)[[5]] <- c("run") 

values_removed <- data_list_frame[data_list_frame$recall_prob < 0.25, ]
#set all rts to NA if the Recall Probability is lower than 0.25
data_list_frame$rt[data_list_frame$recall_prob < 0.25] <- NA

#This dataframe hold all runs
mean_rt_full <- data_list_frame[c(1,4,5)]
#Remove trials with NaN values
#na_trials <- mean_rt_full[mean_rt_full$rt == NaN, ]
#for(i in 1:num_runs){
  #Subset by run
  sub <- mean_rt_full[mean_rt_full$run == i,]
  sub_na <- na_trials[na_trials$run == i, ]
  
  #Set trials in sub_na  to 0 in mean_rt_full
  mean_rt_full[mean_rt_full$run == i & mean_rt_full$trial %in% sub_na$trial,2] <- 0
}
#mean_rt_full$rt <- as.numeric(mean_rt_full$rt)
#Calculate average values
trials <- c(1:72)
for(i in 1:num_runs){
  run_values <- mean_rt_full[mean_rt_full$run == i,]
  run <- vector("numeric", length = 72)
  for(j in 1:72){
    run[j] <-  mean(run_values[run_values$trial == j,2])
  }
  trials <- cbind(trials,run)
}

#Divide by 2 for average single responsedf
plotting_data <- data.frame(trials)
plotting_data <- plotting_data[-c(1)]
plotting_data <- plotting_data/2
  
# remove outliers (over 7sec) - unnecessary with changes made
#is.na(plotting_data) <- plotting_data > 7
### NEED TO REMOVE  VALUES BEFORE PLOTTING *****
plotting_data$trial <- seq(1,72,1)

# Summarize data with means and sds for each trial.
summary_data <- data.frame(trial=seq(1,72,1), avg_rt=rowMeans(plotting_data[1:num_runs], na.rm=TRUE), sd=apply(plotting_data[1:num_runs], 1, sd, na.rm=TRUE))


#Save out data generated with this run
# run7 is the basic noise model for 150 runs
save(behavior_log, file="revised_behav_log.Rda")
save(task_log, file="revised_task_log.Rda")
save(values_removed, file="over25prob_revNoiseRun150v2.Rda")
save(data_list_frame, file="full_data_revNoiseRun150v2.Rda")
save(plotting_data, file="plotting_data_revNoiseRun150v2.Rda")
save(summary_data, file="summary_data_revNoiseRun150v2.Rda")