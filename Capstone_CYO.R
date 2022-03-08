#============================================================================
#Download and read in CSV file
#============================================================================
#Install and attach required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(rpart)
library(randomForest)

#Horse colic data set
#From the UCI Machine Learning Repository
#http://archive.ics.uci.edu/ml/datasets/Horse+Colic
#CSV file downloaded to GitHub repository


#read csv file from working directory into a data frame df
df <- read_csv("horse.csv")

#view data
head(df)

dim(df)
sapply(df,class)

#============================================================================
#save data frame with number of NAs per column
col_nas <- as.data.frame(colSums(is.na(df)))
col_nas <- col_nas %>%
  mutate(p = row.names(col_nas)) %>%
  setNames(c("parameters", "count_NAs"))

#replace row names with numbers
row.names(col_nas) <- 1:nrow(col_nas)

#order to see where the most NAs are
col_nas <- col_nas %>%
  arrange(desc(parameters))

#view top 20
head(col_nas, 20)


#============================================================================
#remove columns / parameters that will be of no use to us
#Change all columns to numeric values
#Replace NAs with appropriate values

#save to new dataframe
df2 <- df %>%
#cp_data indicates if pathology data is present (yes/no) / not relevant because no pathology data is included
#remove column
  select(-cp_data) %>%
#replace lesion 1, 2 and 3 with one column, there were no NAs
  mutate(lesion = ifelse(lesion_1 >0 | lesion_2 > 0 | lesion_3 > 0, 1 ,0)) %>%
  select(-lesion_1, -lesion_2, -lesion_3) %>%
#Changed as per column description file, no NAs
  mutate(surgery2 = ifelse(surgery == "yes" ,1, 0)) %>%
  select(-surgery) %>%
#Changed as per column description file, no NAs
  mutate(age2 = ifelse(age == "adult" ,1, 2)) %>%
  select(-age) %>%
#Changed as per column description file, NAs replaced below
  mutate(temp_extrem = ifelse(temp_of_extremities == "normal" ,1 ,
                              ifelse(temp_of_extremities == "warm" ,2,
                                     ifelse(temp_of_extremities == "cool" ,3,
                                            ifelse(temp_of_extremities == "cold" ,4,0))))) %>%
  select(-temp_of_extremities) %>%
#Changed as per column description file, NAs replaced below
  mutate(periph_pulse = ifelse(peripheral_pulse == "normal" ,1 ,
                              ifelse(peripheral_pulse == "increased" ,2,
                                     ifelse(peripheral_pulse == "reduced" ,3,
                                            ifelse(peripheral_pulse == "absent" ,4,0))))) %>%
  select(-peripheral_pulse) %>%
#Changed as per column description file, NAs replaced below
  mutate(mucous_membrane2 = ifelse(mucous_membrane == "normal_pink" ,1 ,
                               ifelse(mucous_membrane == "bright_pink" ,2,
                                      ifelse(mucous_membrane == "pale_pink" ,3,
                                             ifelse(mucous_membrane == "pale_cyanotic" ,4,
                                                    ifelse(mucous_membrane == "bright_red" ,5,
                                                           ifelse(mucous_membrane == "dark_cyanotic" ,6 ,0))))))) %>%
  select(-mucous_membrane) %>%
#Changed as per column description file, NAs replaced below
  mutate(cap_refill_time = ifelse(capillary_refill_time == "less_3_sec" ,1, 2)) %>%
  select(-capillary_refill_time) %>%
#Changed as per column description file, NAs replaced below
  mutate(pain2 = ifelse(pain == "alert" ,1 ,
                                   ifelse(pain == "depressed" ,2,
                                          ifelse(pain == "mild_pain" ,3,
                                                 ifelse(pain == "severe_pain" ,4,
                                                        ifelse(pain == "extreme_pain" ,5, 0)))))) %>%
  select(-pain) %>%
#Changed as per column description file, NAs replaced below
  mutate(peristalsis2 = ifelse(peristalsis == "hypermotile" ,1 ,
                               ifelse(peristalsis == "normal" ,2,
                                      ifelse(peristalsis == "hypomotile" ,3,
                                             ifelse(peristalsis == "absent" ,4,0))))) %>%
  select(-peristalsis) %>%
#Changed as per column description file, NAs replaced below
  mutate(abdom_distention = ifelse(abdominal_distention == "none" ,1 ,
                               ifelse(abdominal_distention == "slight" ,2,
                                      ifelse(abdominal_distention == "moderate" ,3,
                                             ifelse(abdominal_distention == "severe" ,4,0))))) %>%
  select(-abdominal_distention) %>%
#Changed as per column description file, NAs replaced below
  mutate(nasogast_tube = ifelse(nasogastric_tube == "none" ,1 ,
                                   ifelse(nasogastric_tube == "slight" ,2,
                                          ifelse(nasogastric_tube == "significant" ,3,0)))) %>%
  select(-nasogastric_tube) %>%
#Changed as per column description file, NAs replaced below
  mutate(nasogast_reflux = ifelse(nasogastric_reflux == "none" ,1 ,
                                ifelse(nasogastric_reflux == "less_1_liter" ,2,
                                       ifelse(nasogastric_reflux == "more_1_liter" ,3,0)))) %>%
  select(-nasogastric_reflux) %>%
#Changed as per column description file, NAs replaced below
  mutate(rectal_exam_feces2 = ifelse(rectal_exam_feces == "normal" ,1 ,
                               ifelse(rectal_exam_feces == "increased" ,2,
                                      ifelse(rectal_exam_feces == "decreased" ,3,
                                             ifelse(rectal_exam_feces == "absent" ,4,0))))) %>%
  select(-rectal_exam_feces) %>%
#Changed as per column description file, NAs replaced below
  mutate(abdomen2 = ifelse(abdomen == "normal" ,1 ,
                        ifelse(abdomen == "other" ,2,
                               ifelse(abdomen == "firm" ,3,
                                      ifelse(abdomen == "distend_small" ,4,
                                             ifelse(abdomen == "distend_large" ,5, 0)))))) %>%
  select(-abdomen) %>%
#Changed as per column description file, NAs replaced below
  mutate(abdomo_appear = ifelse(abdomo_appearance == "clear" ,1 ,
                                  ifelse(abdomo_appearance == "cloudy" ,2,
                                         ifelse(abdomo_appearance == "serosanguious" ,3,0)))) %>%
  select(-abdomo_appearance) %>%
#Changed to include only 2 outcomes 1 = lived or 0 = euthanised or died
  mutate(outcome2 = ifelse(outcome == "lived" ,1 ,
                                ifelse(outcome == "died" ,0,
                                       ifelse(outcome == "euthanized" ,0,0)))) %>%
  select(-outcome) %>%
  mutate(surg_lesion = ifelse(surgical_lesion == "yes" ,1, 0)) %>%
  select(-surgical_lesion)

dim(df2)
sapply(df2,class)

#============================================================================
#replace NAs/0s from process above

#Temp of extremities, replace NAs with mean
  df2$temp_extrem[is.na(df2$temp_extrem)] <- mean(df2$temp_extrem,na.rm=TRUE)
#Peripheral pulse, replace NAs with mean
  df2$periph_pulse[is.na(df2$periph_pulse)] <- mean(df2$periph_pulse,na.rm=TRUE)
#Mucous membranes, replace NAs with mean
  df2$mucous_membrane2[is.na(df2$mucous_membrane2)] <- mean(df2$mucous_membrane2,na.rm=TRUE)
#Capillary refill time, replace NAs with mean
  df2$cap_refill_time[is.na(df2$cap_refill_time)] <- mean(df2$cap_refill_time,na.rm=TRUE)
#Pain level, replace NAs with mean
  df2$pain2[is.na(df2$pain2)] <- mean(df2$pain2,na.rm=TRUE)
#Peristalsis activity in gut, replace NAs with mean
  df2$peristalsis2[is.na(df2$peristalsis2)] <- mean(df2$peristalsis2,na.rm=TRUE)
#Abdominal distension, replace NAs with mean
  df2$abdom_distention[is.na(df2$abdom_distention)] <- mean(df2$abdom_distention,na.rm=TRUE)
#Gas coming out of nasogastric tube, replace NAs with mean
  df2$nasogast_tube[is.na(df2$nasogast_tube)] <- mean(df2$nasogast_tube,na.rm=TRUE)
#Nasogastric reflux, replace NAs with mean
  df2$nasogast_reflux[is.na(df2$nasogast_reflux)] <- mean(df2$nasogast_reflux,na.rm=TRUE)
#rectal examination - feces, replace NAs with mean
  df2$rectal_exam_feces2[is.na(df2$rectal_exam_feces2)] <- mean(df2$rectal_exam_feces2,na.rm=TRUE)
#abdomen, replace NAs with mean
  df2$abdomen2[is.na(df2$abdomen2)] <- mean(df2$abdomen2,na.rm=TRUE)
#abdomen, replace NAs with mean
  df2$abdomo_appear[is.na(df2$abdomo_appear)] <- mean(df2$abdomo_appear,na.rm=TRUE)
#PH of nasogastric reflux, replace NAs with mean
  df2$nasogastric_reflux_ph[is.na(df2$nasogastric_reflux_ph)] <- mean(df2$nasogastric_reflux_ph,na.rm=TRUE)
#abdomcentesis total protein, replace NAs with mean
  df2$abdomo_protein[is.na(df2$abdomo_protein)] <- mean(df2$abdomo_protein,na.rm=TRUE)
#rectal temperature, replace NAs with mean
  df2$rectal_temp[is.na(df2$rectal_temp)] <- mean(df2$rectal_temp,na.rm=TRUE)
#respiratory rate, replace NAs with mean
  df2$respiratory_rate[is.na(df2$respiratory_rate)] <- mean(df2$respiratory_rate,na.rm=TRUE) 
#total protein, replace NAs with mean
  df2$total_protein[is.na(df2$total_protein)] <- mean(df2$total_protein,na.rm=TRUE) 
#packed cell volume, replace NAs with mean
  df2$packed_cell_volume[is.na(df2$packed_cell_volume)] <- mean(df2$packed_cell_volume,na.rm=TRUE) 
#Pulse, replace NAs with mean
  df2$pulse[is.na(df2$pulse)] <- mean(df2$pulse,na.rm=TRUE) 
#============================================================================
#check again for NAs
#save data frame with number of NAs per column
col_nas2 <- as.data.frame(colSums(is.na(df2)))
col_nas2 <- col_nas2 %>%
  mutate(p = row.names(col_nas2)) %>%
  setNames(c("parameters", "count_NAs"))

#replace row names with numbers
row.names(col_nas2) <- 1:nrow(col_nas2)

#order to see where the most NAs are
col_nas2 <- col_nas2 %>%
  arrange(desc(parameters))

#view top 20
head(col_nas2, 20)

#============================================================================
#how does each parameter correlate with each of the others
cor(df2)
heatmap(cor(df2))

#============================================================================
#Visualise data
#distribution of predictors , stratified by outcome 

#boxplots
df2 %>% gather(params, val, -outcome2) %>%
  ggplot(aes(factor(outcome2), val, fill = outcome2)) + 
  geom_boxplot() +
  facet_wrap(~params, scales = "free", ncol = 6)

#geom count
df2 %>% gather(params, val, -outcome2) %>%
  ggplot(aes(factor(outcome2), val, fill = outcome2)) + 
  geom_count() +
  facet_wrap(~params, scales = "free", ncol = 6)

#violin plot - use this one
df2 %>% gather(params, val, -outcome2) %>%
  ggplot(aes(factor(outcome2), val, fill = outcome2)) + 
  geom_violin() +
  scale_y_discrete() +
  facet_wrap(~params, scales = "free", ncol = 6)

#bar count plot
df2 %>% gather(params, val, -outcome2) %>%
  ggplot(aes(factor(outcome2), val, fill = outcome2)) + 
  geom_col() +
  facet_wrap(~params, scales = "free", ncol = 6)


#============================================================================

#remove parameters that wont be useful
df3 <- df2 %>%
  select(-hospital_number, -age2, -nasogast_tube, -peristalsis2, -surgery2) %>%
  select(-rectal_temp, -respiratory_rate, -rectal_exam_feces2)

#remove parameters that wont be useful
df3 <- df2 %>%
  select(outcome2, abdom_distention, temp_extrem, mucous_membrane2 )
#total_protein, surgery2
#============================================================================

#create an index and data partition from the full dataframe to give us a training and a test dataset
set.seed(1)
test_index <- createDataPartition(df2$outcome2, times = 1, p = 0.5, list = FALSE)
test_set <- df2[test_index, ]
train_set <- df2[-test_index, ]

#duplicate to test removal of parameters
set.seed(1)
test_index <- createDataPartition(df3$outcome2, times = 1, p = 0.5, list = FALSE)
test_set3 <- df3[test_index, ]
train_set3 <- df3[-test_index, ]

#============================================================================

#linear regression function
lm_func <- function(df_train, df_test){
  fit <- df_train %>%
    lm(outcome2 ~ ., data = .)
  p_hat <- predict(fit, newdata = df_test)
  y_hat <- factor(ifelse(p_hat > 0.5, 1, 2))
  mean(y_hat == df_test$outcome2)
}

lm_func(train_set, test_set)
lm_func(train_set3, test_set3)

#Guess an outcome and test the overall accuracy against the test set
y_hat_guess <- sample(c(0, 1), length(test_index), replace = TRUE)
calc_acc <- mean(y_hat_guess == test_set$outcome2)
calc_acc
accuracy_results <- tibble(method = "Guess", Accur = calc_acc)

#confusion matrix
table(predicted = y_hat_guess, actual = test_set$outcome2)
#Accuracy by outcome
test_set %>% 
  mutate(y_hat = y_hat_guess) %>%
  group_by(outcome2) %>% 
  summarize(accuracy = mean(y_hat_guess == outcome2))



cm <- confusionMatrix(data = factor(y_hat_guess), reference = factor(test_set$outcome2))
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]


#linear regression
fit_lr <- train_set %>%
  lm(outcome2 ~ ., data = .)
p_hat_lr <- predict(fit_lr, newdata = test_set)
y_hat_lr <- ifelse(p_hat_lr > 0.5, 1, 0) %>% factor()
calc_acc <- mean(y_hat_lr == test_set$outcome2)
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "Linear regression", Accur = calc_acc))

#logistic regression
glm_fit <- train_set %>%
  glm(outcome2 ~ ., data = ., family = "binomial")
p_hat_glm <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_glm <- ifelse(p_hat_glm >0.5, 1, 0) %>% factor()
calc_acc <- confusionMatrix(y_hat_glm, factor(test_set$outcome2))$overall[["Accuracy"]]
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "Logistic regression", Accur = calc_acc))
#cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

#curse of dimensionality - multiple predictors
#K Nearest Neighbour
knn_fit <- knn3(outcome2 ~ ., data = train_set, k = 17)
y_hat_knn <- predict(knn_fit, test_set, type = "prob")
y_hat_knn1 <- ifelse(y_hat_knn[,1] > 0.5,1 ,0)
calc_acc <- confusionMatrix(factor(y_hat_knn1), factor(test_set$outcome2))$overall["Accuracy"]
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "KNN", Accur = calc_acc))
#cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

ks <- seq(1, 50, 1)
accuracy <- map_df(ks, function(k){
  fit <- knn3(outcome2 ~ ., data = train_set, k = k)
  
  y_hat <- predict(fit, train_set, type = "prob")
  y_hat <- ifelse(y_hat[,1] > 0.5,1 ,0) %>% factor()
  cm_train <- confusionMatrix(y_hat, factor(train_set$outcome2))
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test_set, type = "prob")
  y_hat <- ifelse(y_hat[,1] > 0.5,1 ,0) %>% factor()
  cm_test <- confusionMatrix(y_hat, factor(test_set$outcome2))
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k = k, train = train_error, test = test_error)
})

accuracy %>%
  ggplot(aes(k, train)) +
  geom_line()

#Regression trees
fit_rt <- rpart(outcome2 ~ ., data = df2)
plot(fit_rt, margin = 0.1)
text(fit_rt, cex = 0.75)

#Classification/decision tree
train_rpart <- train(factor(outcome2) ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)
plot(train_rpart)
y_hat_rpart <- predict(train_rpart, test_set)
calc_acc <- confusionMatrix(y_hat_rpart, factor(test_set$outcome2))$overall[["Accuracy"]]
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "Classification tree", Accur = calc_acc))


#random forest
fit_rf <- randomForest(outcome2 ~ ., data = train_set, nodesize = 9)
#rafalib::mypar()
#plot(fit_rf)
y_hat_rf <- predict(fit_rf, newdata = test_set) 
y_hat_rf <- ifelse(y_hat_rf > 0.5,1 ,0) %>% factor()
calc_acc <- confusionMatrix(y_hat_rf, factor(test_set$outcome2))$overall[["Accuracy"]]
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "Random forest", Accur = calc_acc))


nodesize <- seq(1 ,25, 1)
acc <- sapply(nodesize, function(ns){
  train(factor(outcome2) ~ ., method = "rf", data = train_set,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)
which.max(acc)

accuracy_results
#ensemble prediction
y_hat_lr
y_hat_glm
y_hat_knn1 #factor
y_hat_rpart
y_hat_rf

y_hat_ens <- as.data.frame(as.integer(y_hat_lr))
y_hat_ens["y_hat_glm"] <- as.integer(y_hat_glm)
y_hat_ens["y_hat_knn1"] <- as.integer(factor(y_hat_knn1))
y_hat_ens["y_hat_rpart"] <- as.integer(y_hat_rpart)
y_hat_ens["y_hat_rf"] <- as.integer(y_hat_rf)
y_hat_ens$y_hat_ens <- apply(y_hat_ens,1,mean)
y_hat_ens$y_hat_ens <- ifelse(y_hat_ens$y_hat_ens > 1.5,2 ,1)  
y_hat_ens$y_hat_ens <- ifelse(y_hat_ens$y_hat_ens == 1,0 ,1) 
y_hat_ens <- factor(y_hat_ens$y_hat_ens) 
calc_acc <- mean(y_hat_ens == test_set$outcome2)
calc_acc
accuracy_results <- accuracy_results %>% add_row(tibble_row(method = "Ensemble", Accur = calc_acc))
accuracy_results
