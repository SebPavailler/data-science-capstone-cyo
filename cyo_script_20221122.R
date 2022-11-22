########################
# LOAD RELEVANT PACKAGES
########################

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(rjson)) install.packages("rjson", 
                                     repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", 
                                         repos = "http://cran.us.r-project.org")



##############################
# DOWNLOAD AND OPEN DATA FILE
##############################

dl <- tempfile()
download.file(
  "https://raw.githubusercontent.com/SebPavailler/data-science-capstone-cyo/main/Activities.json"
  ,dl)

# opens data file - JSON format
tmp_dat <- fromJSON(file=dl)



######################
# DATASET PREPARATION
######################


summary(tmp_dat) # see data structure

summary(tmp_dat[[1]]) # show first element of the list

activities_list <- tmp_dat[[1]]$summarizedActivitiesExport # store the list of activities
rm(tmp_dat)

head(summary(activities_list)) # preview list

head(activities_list[[1]]) # preview the first activity

# number of features for each activity
#-------------------------------------
# extract number of features for each activity
list_lengths <- sapply(activities_list, function(x){length(x)}) 
# histogram plot
hist(list_lengths, xlab='Number of features', ylab='Number of activities', main=NULL)

# transform into a dataframe
#----------------------------
# unlist the features and build a dataframe
activities_df <- bind_rows(sapply(activities_list, function(x){
  as_tibble_row(unlist(x))
}))

head(activities_df) # preview of the dataframe


# Build a clean dataframe
#-------------------------
activities <- activities_df %>% 
  # select relevant features
  select(activityId, activityType, startTimeLocal, duration, distance, elevationGain) %>% 
  # change class/name/unit
  mutate(activity_id = as.numeric(activityId), 
         activity_type = ifelse(activityType=="swimming", "lap_swimming", 
                                ifelse(activityType=="cross_country_skiing_ws",
                                       "cross_country_skiing", activityType)), 
         start_time = as.numeric(startTimeLocal)/1000, 
         duration = as.numeric(duration)/1000, 
         distance = as.numeric(distance)/100, 
         elevation_gain = as.numeric(elevationGain)/100) %>% 
  # select transformed features
  select(activity_id, activity_type, start_time, distance, elevation_gain, duration) %>% 
  filter(activity_type %in% c("cycling", 
                              "running", 
                              "open_water_swimming", 
                              "trail_running", 
                              "cross_country_skiing", 
                              "lap_swimming"),
         duration > 600,
         elevation_gain <5000 | is.na(elevation_gain)) %>% # data quality filters
  mutate(activity_type=as.factor(activity_type))

head(activities) # preview the new dataframe


# Save 10% of dataset as a validation set
#----------------------------------------
set.seed(91, sample.kind="Rounding")
valid_index <- createDataPartition(y = activities$duration, 
                                   times = 1, p = 0.1, list = FALSE)
cyo <- activities[-valid_index,] # cyo set
validation <- activities[valid_index,] # validation set
rm(valid_index)


###############
# RMSE FUNCTION
###############

RMSE <- function(true_values, predicted_values){
  sqrt(mean((true_values - predicted_values)^2))
}


####################
# TRAIN AND TEST SET
####################

# build test set as 20% of cyo set
set.seed(2020, sample.kind="Rounding")
test_index <- createDataPartition(y = cyo$duration, times = 1, p = 0.2, list = FALSE)
train_set <- cyo[-test_index,]
test_set <- cyo[test_index,]
rm(test_index)


###############
# BASELINE MODEL
################


mu <- mean(train_set$duration) # average duration for all activities

baseline_rmse <- RMSE(test_set$duration, mu) # calculate RMSE
print(paste("Baseline RMSE is",round(baseline_rmse,0), "seconds")) # print results


#####################
# MODELS DEVELOPMENT
#####################


# Effect of sport
#----------------

# plot effect of sport on duration
train_set %>% 
  ggplot(aes(activity_type, duration)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=30, hjust=1),
        axis.title.x = element_blank())


# Build model 1
# sport bias
b_k <- train_set %>% 
  group_by(activity_type) %>% 
  summarize(b_k = mean(duration - mu))

# make predictions on test set
pred_1 <- test_set %>% 
  left_join(b_k, by='activity_type') %>% 
  mutate(pred = mu + b_k) %>% 
  pull(pred)

model_1_rmse <- RMSE(test_set$duration, pred_1) # calculate RMSE
print(paste("RMSE with model #1 is",round(model_1_rmse,0),"seconds")) # print results


# Effect of distance
#-------------------

# Plot effect of distance on duration
train_set %>% 
  ggplot(aes(distance/1000, duration/60)) + #convert distance to km and duration to min
  geom_point() +
  theme_bw() +
  xlab("Distance (km)") +
  ylab("Duration (min)")

# Build model 2
# fit a linear model
fit_dist <- train_set %>% 
  lm(duration ~ distance, data=.)

# make predictions on the test set
pred_2 <- predict(fit_dist, test_set)

model_2_rmse <- RMSE(test_set$duration, pred_2) # calculate RMSE
print(paste("RMSE with model #2 is",round(model_2_rmse,0),"seconds")) # print results


# Interaction sport/distance
#---------------------------

# Plot the interaction distance/sport
train_set %>% 
  ggplot(aes(distance/1000, 
             duration/60, #convert distance to km and duration to min
             color=activity_type)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + # add a linear tred
  theme_bw() +
  xlab("Distance (km)") +
  ylab("Duration (min)") +
  labs(color='Sport')  


# Build model 3
# extract a vector of sports
sports <- train_set %>% 
  select(activity_type) %>% 
  mutate(activity_type=as.character(activity_type)) %>% 
  distinct(.) %>% 
  pull(activity_type) 

# fit linear models for each activity type
fit_d_k <- sapply(sports, function(x){
  fit <- train_set %>% 
    filter(activity_type==x) %>% 
    lm(duration ~ distance, data=.)
  c(fit$coefficients[1], fit$coefficients[2]) # extract coefficients
})

# store the coefficients in a tibble
b_d_k <- tibble(activity_type = colnames(fit_d_k),
                p_k=fit_d_k[1,],
                m_k=fit_d_k[2,])

# make predictions on the test set
pred_3 <- test_set %>% 
  left_join(b_d_k, by='activity_type') %>% 
  mutate(pred = p_k+m_k*distance) %>% 
  pull(pred)

model_3_rmse <- RMSE(test_set$duration, pred_3) # calculate RMSE
print(paste("RMSE with model #3 is",round(model_3_rmse,0),"seconds")) # print results


# Effect of date/time
#--------------------

# Plot time of the day vs residuals
train_set %>% 
  left_join(b_d_k, by='activity_type') %>% 
  mutate(residuals = duration - (p_k+m_k*distance), # residuals from model #3
         start_hour = hour(round_date(as_datetime(start_time)
                                      ,unit="hour"))) %>% # extract hour from the timecode
  ggplot(aes(start_hour, residuals/60, group=start_hour)) + 
  geom_boxplot()+
  xlab("Start time (hour)") +
  ylab("Residuals (min)") +
  theme_bw()

# Plot month of the year vs residuals
train_set %>% 
  left_join(b_d_k, by='activity_type') %>% 
  mutate(residuals = duration - (p_k+m_k*distance), # residuals from model #3
         start_month = month(round_date(as_datetime(start_time),
                                        unit="month"))) %>% # extract month as 1 to 12
  ggplot(aes(start_month, residuals/60, group=start_month)) + 
  geom_boxplot() + 
  xlab("Month (1 to 12)") +
  ylab("Residuals (min)") +
  theme_bw()


# Effect of elevation gain
# -------------------------

# plot elevation gain vs distance
train_set %>% 
  ggplot(aes(elevation_gain, distance/1000)) + 
  geom_point() +
  xlab("Positive elevation (m)") +
  ylab("Distance (km)") +
  theme_bw()

# plot normalized elevation gained vs distance
train_set %>% 
  mutate(elevation_norm = elevation_gain/distance) %>% 
  ggplot(aes(elevation_norm*100, distance/1000)) + 
  geom_point() +
  xlab("Normalized elevation (%)") +
  ylab("Distance (km)") +
  theme_bw()

# plot normalized elevation gain vs residuals
train_set %>% 
  left_join(b_d_k, by='activity_type') %>% 
  mutate(residuals = duration - (p_k+m_k*distance)) %>%
  mutate(elevation_norm = elevation_gain/distance) %>% 
  ggplot(aes(elevation_norm*100, residuals/60)) + 
  geom_point() +
  xlab("Normalized elevation (%)") +
  ylab("Residuals (min)") +
  theme_bw()

# add a threshold line to previous plot
train_set %>% 
  left_join(b_d_k, by='activity_type') %>% 
  mutate(residuals = duration - (p_k+m_k*distance)) %>%
  mutate(elevation_norm = elevation_gain/distance) %>% 
  ggplot(aes(elevation_norm*100, residuals/60)) + 
  geom_point() +
  xlab("Normalized elevation (%)") +
  ylab("Residuals (min)") +
  theme_bw() +
  geom_vline(xintercept = 4.5, color='red', size = 2)

# Build model 4
taus <-  seq(0.02,0.07,0.005) # sequence of taus (thresholds for elevation gain)

rmses <- sapply(taus, function(t){ # apply function to calculate elevation bias to the sequence of taus
  b_tau <- train_set %>% 
    left_join(b_d_k, by='activity_type') %>% 
    mutate(elevation_norm = elevation_gain/distance) %>% 
    filter(elevation_norm > t) %>% # conditional effect, tau applies here
    summarize(b_tau = mean(duration - (p_k+m_k*distance))) %>% 
    pull(b_tau)
  
  # make predictions on the test set
  pred_4 <- test_set %>% 
    left_join(b_d_k, by='activity_type') %>% 
    mutate(elevation_norm = elevation_gain/distance,
           pred = ifelse(elevation_norm < t | is.na(elevation_norm),
                         p_k + m_k*distance,
                         p_k + m_k*distance + b_tau)) %>% 
    pull(pred)
  
  return(RMSE(test_set$duration, pred_4)) # return rmse
})

tibble(thresholds=taus, rmses=rmses) %>% # plot RMSEs vs taus
  ggplot(aes(thresholds, rmses)) +
  geom_point()+
  theme_bw() 

# features of model 4
tau <- taus[which.min(rmses)] # optimal tau
model_4_rmse <- min(rmses) # minimum RMSE
print(paste("Optimal tau is", tau)) # print tau
print(paste("RMSE with model #4 is",round(model_4_rmse,0),"seconds")) # print RMSE


###################
# FINAL VALIDATION
###################

# Apply models on the validation set

# Baseline model - append _f suffix for 'final'
# Simple mean
mu_f <- mean(cyo$duration)
baseline_rmse_f <- RMSE(validation$duration, mu_f)


# Model 1 - append _f suffix for 'final'
# Sport bias
b_k_f <- cyo %>% 
  group_by(activity_type) %>% 
  summarize(b_k_f = mean(duration - mu_f))

pred_1_f <- validation %>% 
  left_join(b_k_f, by='activity_type') %>% 
  mutate(pred = mu + b_k_f) %>% 
  pull(pred)

model_1_rmse_f <- RMSE(validation$duration, pred_1_f)

# Model 2 - append _f suffix for 'final'
# simple linear regression
fit_dist_f <- cyo %>% 
  lm(duration ~ distance, data=.)

pred_2_f <- predict(fit_dist_f, validation)

model_2_rmse_f <- RMSE(validation$duration, pred_2_f)

# Model 3 - append _f suffix for 'final'
# linear regressions for each sport
fit_d_k_f <- sapply(sports, function(x){
  fit <- cyo %>% 
    filter(activity_type==x) %>% 
    lm(duration ~ distance, data=.)
  c(fit$coefficients[1], fit$coefficients[2])
})

b_d_k_f <- tibble(activity_type = colnames(fit_d_k_f),
                  p_k_f=fit_d_k_f[1,],
                  m_k_f=fit_d_k_f[2,])

pred_3_f <- validation %>% 
  left_join(b_d_k_f, by='activity_type') %>% 
  mutate(pred = p_k_f+m_k_f*distance) %>% 
  pull(pred)

model_3_rmse_f <- RMSE(validation$duration, pred_3_f)

# Model 4 - append _f suffix for 'final'
# add a conditional elevation effect
b_tau_f <- cyo %>% 
  left_join(b_d_k_f, by='activity_type') %>% 
  mutate(elevation_norm = elevation_gain/distance) %>% 
  filter(elevation_norm > tau) %>% 
  summarize(b_tau_f = mean(duration - (p_k_f+m_k_f*distance))) %>% 
  pull(b_tau_f)

pred_4_f <- validation %>% 
  left_join(b_d_k_f, by='activity_type') %>% 
  mutate(elevation_norm = elevation_gain/distance,
         pred = ifelse(elevation_norm < tau | is.na(elevation_norm),
                       p_k_f + m_k_f*distance,
                       p_k_f + m_k_f*distance + b_tau_f)) %>% 
  pull(pred)

model_4_rmse_f <- RMSE(validation$duration, pred_4_f)

# Results in a table
rmse_results <- tibble(model = c("Baseline", "Model 1", "Model 2", "Model 3", "Model 4"),
                       method=c("Simply the mean", 
                                "Sport effect", 
                                "Simple distance linear regression", 
                                "Distance linear regressions by sport",
                                "Distance by sport + elevation conditional effect"),
                       RMSE = c(round(baseline_rmse_f,0),
                                round(model_1_rmse_f,0),
                                round(model_2_rmse_f,0),
                                round(model_3_rmse_f,0),
                                round(model_4_rmse_f,0)))

rmse_results %>% knitr::kable()


############
# DISCUSSION
############

# Size of the dataset
#--------------------

# plot activity count by sport for train_set
p1 <- train_set %>% 
  group_by(activity_type) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(activity_type, count)) +
  geom_col()+
  geom_text(aes(label=count), vjust=-.3)+
  theme_bw() +
  ggtitle("Train set") +
  ylim(c(0,260))+
  ylab(element_blank())+
  theme(axis.text.x = element_text(angle=40, hjust=1),
        axis.title.x = element_blank())

# plot activity count by sport for test_set
p2 <- test_set %>% 
  group_by(activity_type) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(activity_type, count)) +
  geom_col()+
  geom_text(aes(label=count), vjust=-.3)+
  theme_bw() +
  ggtitle("Test set")+
  ylim(c(0,260))+
  ylab(element_blank())+
  theme(axis.text.x = element_text(angle=40, hjust=1),
        axis.title.x = element_blank())

# plots next to each other
grid.arrange(p1,p2, nrow=1, left="Number of activities")