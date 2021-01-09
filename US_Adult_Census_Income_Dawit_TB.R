#
#This is R code written for EDX capstone project (2nd part - CYO)
#The purpose of the project is to predict if individual's income would exceed 50K/year, based on the individual's Census data.

#Install the required packages if they needed 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

#load the required libraries. 
library(tidyverse)
library(caret)
library(dplyr)

#Read the data-set from my repository in github US Adult Census Income
url <- "https://raw.githubusercontent.com/dtsadik/EDX_CYO_Project/d886694086edb3b6110a971f28709f01004c29b5/adult.csv"
adult<-read_csv(url)


#Explore adult data set
adult %>% as_tibble()    #it has 32,561 observations and 15 variables

#check if n/a or "?" character exists. 
colSums(is.na(adult))
colSums(adult == "?")

#remove columns with zero (or almost zero) variablilities, as they don't add much to the predications.
adult<-adult[-nearZeroVar(adult)]

#remove records that has still "?"
adult<-adult[rowSums(adult == "?")==0,]

#Exclude variables that are not needed for various reasons.
adult<-adult%>% select(sex, race, education.num, workclass, occupation, age, marital.status, hours.per.week, income)

#explore the modified and cleaned data-set
adult %>% as_tibble()  #it has now 30,708 observations and 9 variables


#Analyze the variables

##income
all<-length(adult$income)  #total number of the observations
gt50<- adult %>% filter(income==">50K") %>%  count()  #total number of 50k plus incomes
gt50Perc<-mean(adult$income==">50K") #percentage of 50k plus incomes in the data-set

##sex 
#group sex and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(sex_group=sex) %>% summarize(percentage_in_data_set=n()/all)

#group sex and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(sex_group=sex) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "sex_group") %>% select (sex_group, percentage_in_data_set, percentage_in_gt50K) 

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


## race 
#group race and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(race_group=race) %>% summarize(percentage_in_data_set=n()/all)

#group race and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(race_group=race) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "race_group") %>% 
  select (race_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


## education.num 
#group education.num and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(education.num_group=education.num) %>% summarize(percentage_in_data_set=n()/all)

#group education.num and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(education.num_group=education.num) %>% summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "education.num_group") %>% 
  select (education.num_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


## workclass
#group work classes and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(workclass_group=workclass) %>% summarize(percentage_in_data_set=n()/all)

#group work classes and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(workclass_group=workclass) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "workclass_group") %>% 
  select (workclass_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


## occupation 
#group occupation and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(occupation_group=occupation) %>% summarize(percentage_in_data_set=n()/all)

#group occupation and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(occupation_group=occupation) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "occupation_group") %>% 
  select (occupation_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


##age
#group age in 10 years intervals, and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(age_group=round(age/10)*10) %>% summarize(percentage_in_data_set=n()/all)

#group age in 10 years interval, and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(age_group=round(age/10)*10) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "age_group") %>%  
  select (age_group, percentage_in_data_set, percentage_in_gt50K ) 

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


##marital.status
#group marital.status and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(marital.status_group= marital.status) %>% summarize(percentage_in_data_set=n()/all)

#group marital.status and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(marital.status_group=marital.status) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "marital.status_group") %>% 
  select (marital.status_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()


## hours.per.week 
#group hours.per.week and get the proportion of each group in the data-set
tmp1<-adult %>% group_by(hours.per.week_group=round(hours.per.week/10)*10) %>% summarize(percentage_in_data_set=n()/all)

#group hours.per.week and get the proportion of each group in the ">50K" income group; and join it with the above result.
tmp2<-adult %>% filter(income==">50K") %>% group_by(hours.per.week_group=round(hours.per.week/10)*10) %>%  summarize(percentage_in_gt50K=n()/gt50) %>% 
  left_join(tmp1, by = "hours.per.week_group") %>% 
  select (hours.per.week_group, percentage_in_data_set, percentage_in_gt50K) %>% arrange(desc(percentage_in_data_set))

#print out the result.
tmp2 %>% mutate(gt50K_over_data_set=percentage_in_gt50K/percentage_in_data_set) %>% arrange(desc(gt50K_over_data_set)) %>% knitr::kable()

## Model development

# Represent the ">50K" income group by "1" and the "<=50" income group by "0". 
# Also change the variable type to factor.
adult$income = as.factor(ifelse(adult$income=='>50K',1,0))

#split 90/10 the data-set into adult_train & adult_test data sets.
set.seed(1, sample.kind="Rounding") 
adult_test_index <- as.vector(createDataPartition(adult$income, times = 1,
                                                  p = 0.1, list = FALSE))
adult_train <- adult[-adult_test_index,]
adult_test <- adult[adult_test_index,]


#glm model
train_glm <- train(income ~ ., method="glm", data=adult_train)
pred_glm <- predict(train_glm, adult_test, "raw")
cm_glm<-confusionMatrix(pred_glm, factor(adult_test$income))
cm_glm$overall[["Accuracy"]]


#knn model
#see what parameter can be tuned in knn.
modelLookup("knn")

# use 5-fold cross-validation to tune k.
control <- trainControl(method = "cv", number = 5, p = .9)
train_knn <- train(income ~ ., method = "knn", data = adult_train, 
                   tuneGrid = data.frame(k = c(5,7,9,11,13)),
                   trControl = control)

# get the best k
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

pred_knn <- predict(train_knn, adult_test, type="raw")
cm_knn <- confusionMatrix(pred_knn, factor(adult_test$income)) 
cm_knn$overall[["Accuracy"]]


##Classification Trees 

# see what parameters can be tuned in rpart.
modelLookup("rpart") 

#cross-validate/tune cp
train_rpart <-  train(income ~ ., data = adult_train,
                      method = "rpart",
                      tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)))

#get the best cp
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune

pred_rpart <- predict(train_rpart, adult_test, type="raw")
cm_rpart <- confusionMatrix(pred_rpart, factor(adult_test$income))
cm_rpart$overall[["Accuracy"]]


## Ensembles
# Use the predictions from the previous three models and predict like what the 
# majority predicted (i.e. 2 out 3).

# change the prediction outputs to numeric as we will need to do some calculation
# on them. 
pred_glm_num<-as.numeric(as.character(pred_glm))
pred_knn_num<-as.numeric(as.character(pred_knn))
pred_rpart_num<-as.numeric(as.character(pred_rpart))

# get the ensembles based on the three predictions - predict like the majority.
# if two models predicts 1 (i.e. sum of three prediction is more than 1), then predict 1.
pred_ensembles<- 
ifelse((pred_glm_num + pred_knn_num + pred_rpart_num) > 1, 1, 0) %>% factor(levels = levels(adult_test$income)) 
cm_ensembles<-confusionMatrix(pred_ensembles, factor(adult_test$income))
cm_ensembles$overall[["Accuracy"]]

#Compare the results from the four models.
CM_accuracy <- 
    data_frame(method ="glm", accuracy = cm_glm$overall[["Accuracy"]]) %>%
  bind_rows(data_frame(method="knn", accuracy = cm_knn$overall[["Accuracy"]] )) %>%
  bind_rows(data_frame(method="rpart", accuracy = cm_rpart$overall[["Accuracy"]])) %>%
  bind_rows(data_frame(method="ensembles", accuracy = cm_ensembles$overall[["Accuracy"]]))

# Print out the result from the four models
CM_accuracy %>% knitr::kable()