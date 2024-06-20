#Libraries
library(ggplot2)
library(reshape2)


# Analysis of Kalm & Norris behavioral data
first_df <- read.delim("Documents/Thesis/task-gabor_meas-rt_first.tsv", header=FALSE)
last_df <-  read.delim("Documents/Thesis/task-gabor_meas-rt_last.tsv", header=FALSE)
rtd_df <- read.delim("Documents/Thesis/task-gabor_meas-rt_d.tsv", header=FALSE)

#Add trial number 
first_df$trial <- seq(1,72,1)
last_df$trial <- seq(1,72,1)
rtd_df$trial <- seq(1,72,1)

#Analysis of first_df times
new_seq <- first_df[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72), ]
old_seq <- first_df[-c(new_seq$trial), ]


avg_new <- rowMeans(new_seq[c(1:22)],na.rm = TRUE)
avg_old <- rowMeans(old_seq[c(1:22)], na.rm = TRUE)

plot(as.numeric(names(avg_new)), unname(avg_new))

plot(as.numeric(names(avg_old)), unname(avg_old))

#try with last
new_seq <- last_df[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72), ]
old_seq <- last_df[-c(new_seq$trial), ]


avg_new <- rowMeans(new_seq[c(1:22)],na.rm = TRUE)
avg_old <- rowMeans(old_seq[c(1:22)], na.rm = TRUE)

plot(as.numeric(names(avg_new)), unname(avg_new))

plot(as.numeric(names(avg_old)), unname(avg_old))

#Do with d
new_fullseq <- rtd_df[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72),]
old_fullseq <- rtd_df[-c(new_fullseq$trial), ]

#remove NA rows
n_fullseq <- new_fullseq[-c(2,6,10,14,18,22),]
o_fulleq <- old_fullseq[-c(2,7,10,15,18,23,26,31,34,39,42,47), ]

#Divide by trial for new
by_trials <- melt(n_fullseq, id.vars="trial")
old_by_trials <- melt(o_fulleq, id.vars="trial")

avg_new <- rowMeans(new_fullseq[c(1:22)],na.rm = TRUE)
avg_old <- rowMeans(old_fullseq[c(1:22)], na.rm = TRUE)

new_seq <- data.frame(trial=as.numeric(names(avg_new)), rt =unname(avg_new))
old_seq <- data.frame(trial=as.numeric(names(avg_old)), rt =unname(avg_old))

summary(new_seq$rt)
mean(new_seq$rt, na.rm = TRUE)
sd(new_seq$rt, na.rm = TRUE)

summary(old_seq$rt)
mean(old_seq$rt, na.rm = TRUE)
sd(old_seq$rt, na.rm = TRUE)

full_seq <- rbind(new_seq, old_seq)
summary(full_seq$rt)
mean(full_seq$rt, na.rm = TRUE)
sd(full_seq$rt, na.rm = TRUE)


ggplot() + 
  geom_point(data=new_seq, aes(x=trial, y=rt),size = 4, color = "blue") +
  geom_point(data=old_seq, aes(x=trial, y=rt),size = 4, color = "red") +
  geom_point(data=by_trials, aes(x=trial, y=value), size = 2, color="black") +
  geom_point(data=old_by_trials, aes(x=trial, y=value), size = 2, color="grey")

ggplot() +
  geom_point(data=new_seq, aes(x=trial, y=rt),size = 4, color = "blue") +
  geom_point(data=by_trials, aes(x=trial, y=value), size = 2, color="black")

ggplot() +
  geom_point(data=old_seq, aes(x=trial, y=rt),size = 4, color = "red") +
  geom_point(data=old_by_trials, aes(x=trial, y=value), size = 2, color="grey")

avg_early <- mean(old_seq[(old_seq$trial <= 36), 2], na.rm=TRUE)
avg_late <- mean(old_seq[(old_seq$trial >= 37), 2], na.rm=TRUE)

