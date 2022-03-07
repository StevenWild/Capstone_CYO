#============================================================================
#Download and read in CSV file
#============================================================================
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)

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
col_nas <- as.data.frame(colSums(is.na(df))) %>%
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
#Rreplace NAs with appropriate values

#save to new dataframe
df2 <- df %>%
  #cp_data indicates if pathology data is present (yes/no) / not relevant because no pathology data is included
  select(-cp_data) %>%
  #replace lesion 1, 2 and 3 with one column
  mutate(lesion = ifelse(lesion_1 >0 | lesion_2 > 0 | lesion_3 > 0, 1 ,0)) %>%
  select(-lesion_1, -lesion_2, -lesion_3) %>%

  mutate(surgery2 = ifelse(surgery == "yes" ,1, 0)) %>%
  select(-surgery) %>%
  mutate(age2 = ifelse(age == "adult" ,1, 2)) %>%
  select(-age) %>%
  mutate(temp_extrem = ifelse(temp_of_extremities == "normal" ,1 ,
                              ifelse(temp_of_extremities == "warm" ,2,
                                     ifelse(temp_of_extremities == "cool" ,3,
                                            ifelse(temp_of_extremities == "cold" ,4,0))))) %>%
  select(-temp_of_extremities) %>%
  mutate(periph_pulse = ifelse(peripheral_pulse == "normal" ,1 ,
                              ifelse(peripheral_pulse == "increased" ,2,
                                     ifelse(peripheral_pulse == "reduced" ,3,
                                            ifelse(peripheral_pulse == "absent" ,4,0))))) %>%
  select(-peripheral_pulse) %>%
  mutate(mucous_membrane2 = ifelse(mucous_membrane == "normal_pink" ,1 ,
                               ifelse(mucous_membrane == "bright_pink" ,2,
                                      ifelse(mucous_membrane == "pale_pink" ,3,
                                             ifelse(mucous_membrane == "pale_cyanotic" ,4,
                                                    ifelse(mucous_membrane == "bright_red" ,5,
                                                           ifelse(mucous_membrane == "dark_cyanotic" ,6 ,0))))))) %>%
  select(-mucous_membrane) %>%
  mutate(cap_refill_time = ifelse(capillary_refill_time == "less_3_sec" ,1, 2)) %>%
  select(-capillary_refill_time) %>%
  mutate(pain2 = ifelse(pain == "alert" ,1 ,
                                   ifelse(pain == "depressed" ,2,
                                          ifelse(pain == "mild_pain" ,3,
                                                 ifelse(pain == "severe_pain" ,4,
                                                        ifelse(pain == "extreme_pain" ,5, 0)))))) %>%
  select(-pain) %>%
  mutate(peristalsis2 = ifelse(peristalsis == "hypermotile" ,1 ,
                               ifelse(peristalsis == "normal" ,2,
                                      ifelse(peristalsis == "hypomotile" ,3,
                                             ifelse(peristalsis == "absent" ,4,0))))) %>%
  select(-peristalsis) %>%
  mutate(abdom_distention = ifelse(abdominal_distention == "none" ,1 ,
                               ifelse(abdominal_distention == "slight" ,2,
                                      ifelse(abdominal_distention == "moderate" ,3,
                                             ifelse(abdominal_distention == "severe" ,4,0))))) %>%
  select(-abdominal_distention) %>%
  mutate(nasogast_tube = ifelse(nasogastric_tube == "none" ,1 ,
                                   ifelse(nasogastric_tube == "slight" ,2,
                                          ifelse(nasogastric_tube == "significant" ,3,0)))) %>%
  select(-nasogastric_tube) %>%
  mutate(nasogast_reflux = ifelse(nasogastric_reflux == "none" ,1 ,
                                ifelse(nasogastric_reflux == "less_1_liter" ,2,
                                       ifelse(nasogastric_reflux == "more_1_liter" ,3,0)))) %>%
  select(-nasogastric_reflux) %>%
  mutate(rectal_exam_feces2 = ifelse(rectal_exam_feces == "normal" ,1 ,
                               ifelse(rectal_exam_feces == "increased" ,2,
                                      ifelse(rectal_exam_feces == "decreased" ,3,
                                             ifelse(rectal_exam_feces == "absent" ,4,0))))) %>%
  select(-rectal_exam_feces) %>%
  mutate(abdomen2 = ifelse(abdomen == "normal" ,1 ,
                        ifelse(abdomen == "other" ,2,
                               ifelse(abdomen == "firm" ,3,
                                      ifelse(abdomen == "distend_small" ,4,
                                             ifelse(abdomen == "distend_large" ,5, 0)))))) %>%
  select(-abdomen) %>%
  mutate(abdomo_appear = ifelse(abdomo_appearance == "clear" ,1 ,
                                  ifelse(abdomo_appearance == "cloudy" ,2,
                                         ifelse(abdomo_appearance == "serosanguious" ,3,0)))) %>%
  select(-abdomo_appearance) %>%
  mutate(outcome2 = ifelse(outcome == "lived" ,1 ,
                                ifelse(outcome == "died" ,2,
                                       ifelse(outcome == "euthanized" ,3,0)))) %>%
  select(-outcome) %>%
  mutate(surg_lesion = ifelse(surgical_lesion == "yes" ,1, 0)) %>%
  select(-surgical_lesion)

dim(df2)
sapply(df2,class)

#============================================================================
#check again for NAs
#save data frame with number of NAs per column
col_nas <- as.data.frame(colSums(is.na(df2))) %>%
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
sns.heatmap(dfTrain.corr(),cmap='magma_r',annot=True)
cor(df)
d2 <- df %>%
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

#============================================================================