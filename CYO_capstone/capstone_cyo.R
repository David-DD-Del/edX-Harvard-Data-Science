#######
#Setup#
#######

# Check for and install required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(gridExtra)
library(corrplot)

# Original file is from https://www.kaggle.com/datasets/sagarbanik/phishing-url-binary-datatset
# Assign url to download dataset
url <- "https://raw.githubusercontent.com/David-DD-Del/edX-Harvard-Data-Science/refs/heads/main/CYO_capstone/Dataset.csv"

# Assign name of datset file
phish_csv <- "Dataset.csv"

# Check for file and download it
if(!file.exists(phish_csv))
  download.file(url, phish_csv)

# Read CSV file
phish <- read.csv(phish_csv)


##################
#Data Exploration#
##################

# According to the description from the original site, a label 1 means legitimate, 
# and 0 for phished

# Get an overall quick view of the data
str(phish)

# Check of any na values
any(is.na(phish))

# Get unique count and range of columns
feelers <- lapply((1:ncol(phish)), function(n){
  uniq_count <- length(unique(phish[,n]))
  range <- range(phish[,n])
  return(c(uniq_count = uniq_count, range_min = range[1], range_max = range[2]))
})

# Get Column Names
Columns <- colnames(phish)

# Assign phish column names to feelers
names(feelers) <- Columns

# Check out the columns unsure about
feelers$at_the_rate[1:3]
feelers$protocol[1:3]
feelers$protocol_count[1:3]
feelers$web_traffic[1:3]

# Count -1 values in date columns
sum(phish$whois_regDate == -1)
sum(phish$whois_expDate == -1)
sum(phish$whois_updatedDate == -1)

# Count amount of date columns that are all -1
neg_ones <- phish %>% filter(whois_regDate == -1 & whois_expDate == -1 & whois_updatedDate) %>% 
  mutate(count = n())
neg_ones$count[1]

# Check other parameters of all -1 date columns
head(neg_ones)

# Remove negative values
phish_clean <- phish %>% 
  filter(whois_regDate > -1 & whois_expDate > -1 & whois_updatedDate & -1)

# Put in column definitions based on observed data and column names
Definitions <- c("Registration Date", "Expiration Date", "Last Updated Date", 
                 "Count of dots", "Length of URL", "Count of digits", 
                 "Count of special characters", "Count of hyphens", 
                 "Count of double slashes", "Count of single slashes", "Count of @ symbol", 
                 "Begins with a protocol", "Count of protocols", 
                 "A threshold of web traffic", "If it is a legitimate URL"
)

# Table of columns and their definitions
colnames_definitions <- data.frame(Columns, Definitions) 
colnames_definitions %>% kable()


####################
#Visual Exploration#
####################



# Histogram of label column
phish_clean %>% ggplot(aes(label)) +
  geom_histogram(bins = 2, color = "white") +
  scale_x_continuous(breaks = c(0, 1)) +
  ggtitle("Phised vs Legitimate") 

# Find the exact proportion
sum(phish_clean$label == 0) / length(phish_clean$label)

# Count of dots stratified by label
phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(dot_count, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~label) + 
  ggtitle("Count of Dots", subtitle = "Phished vs Legitimate")

# Count of dots stratified by label scaled y
phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(dot_count, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~label) + 
  scale_y_log10() +
  labs(title = "Count of Dots", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

# URL length stratified by label scaled y 
phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(url_len, fill = label)) +
  geom_histogram(binwidth = 50, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "URL Length", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

# Digit_count and special_count stratified by label scaled y
p1 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(digit_count, fill = label)) +
  geom_histogram(bins = 15, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Digits", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p2 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(special_count, fill = label)) +
  geom_histogram(binwidth = 10, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Special Characters", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

grid.arrange(p1, p2, ncol = 2)

# Rest of the count columns stratified by label scaled y
p1 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(hyphen_count, fill = label)) +
  geom_histogram(binwidth = 3, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Hyphens", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p2 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(double_slash, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Double Slashs", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

# Separating the grid so it won't get squished by the pdf
grid.arrange(p1, p2, ncol = 2)

p3 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(single_slash, fill = label)) +
  geom_histogram(binwidth = 3, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Single Slashs", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p4 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(at_the_rate, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of @ Characters", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

# Separating the grid so it won't get squished by the pdf
grid.arrange(p3, p4, ncol = 2)

p5 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(protocol_count, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Count of Protocols", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p5

# Binary columns stratified by label
p1 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(protocol, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_x_continuous(breaks = c(0, 1)) +
  facet_wrap(~label) + 
  labs(title = "Begins with Protocol", subtitle = "Phished vs Legitimate")

p2 <- phish_clean %>% mutate(label = factor(label)) %>% 
  ggplot(aes(web_traffic, fill = label)) +
  geom_histogram(binwidth = 1, color = "white") + 
  scale_x_continuous(breaks = c(0, 1)) +
  facet_wrap(~label) + 
  labs(title = "Decent Web Traffic", subtitle = "Phished vs Legitimate")

grid.arrange(p1, p2, ncol = 2)

# Date columns stratified by label scaled y
p1 <- phish_clean %>% mutate(label = factor(label)) %>% 
  filter(whois_regDate > 0) %>% 
  ggplot(aes(whois_regDate, fill = label)) +
  geom_histogram(bins = 10, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Registration Date", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p2 <- phish_clean %>% mutate(label = factor(label)) %>% 
  filter(whois_expDate > 0) %>%
  ggplot(aes(whois_expDate, fill = label)) +
  geom_histogram(bins = 10, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Expiration Date", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

# Separating the grid so it won't get squished by the pdf
grid.arrange(p1, p2, ncol = 2)

p3 <- phish_clean %>% mutate(label = factor(label)) %>% 
  filter(whois_updatedDate > 0) %>%
  ggplot(aes(whois_updatedDate, fill = label)) +
  geom_histogram(bins = 20, color = "white") + 
  scale_y_log10() +
  facet_wrap(~label) + 
  labs(title = "Updated Date", subtitle = "Phished vs Legitimate", 
       y = "count (logarithmic scaled)")

p3

# Correlation graph
cors <- phish %>% cor()

corrplot(cors, method = "square", outline = T, tl.col = "black")


################################
# Construct train and test set #
################################

# Created train, test with a 80% to 20% split
set.seed(5)
phish_test_index <- createDataPartition(y = phish_clean$label, times = 1, p = .2, list = F)

phish_test <- phish_clean[phish_test_index,]
phish_train <- phish_clean[-phish_test_index,]

propor_train <- sum(phish_train$label == 0) / length(phish_train$label)
propor_test <- sum(phish_test$label == 0) / length(phish_test$label)

# Change label data type
phish_train <- phish_train %>% mutate(label = factor(label))
phish_test <- phish_test %>% mutate(label = factor(label))

##############################
#Constructing the Base Models#
##############################

# Base models
# SVM model
svm_train <- train(label ~ ., data = phish_train, method = "svmLinear")
svm_predict <- predict(svm_train, newdata = phish_test)

# DT model
dt_train <- train(label ~ ., data = phish_train, method = "rpart")
dt_predict <- predict(svm_train, newdata = phish_test)

# SVM base results
svm_confu_matr <- confusionMatrix(svm_predict, phish_test$label)
svm_confu_matr

# DT base results
dt_confu_matr <- confusionMatrix(dt_predict, phish_test$label)
dt_confu_matr

# Get new data2
url <- "https://raw.githubusercontent.com/David-DD-Del/edX-Harvard-Data-Science/refs/heads/main/CYO_capstone/data2.csv"

malicio_csv <- "data2.csv"

if(!file.exists(malicio_csv))
  download.file(url, malicio_csv)

malicio <- read.csv(malicio_csv)

# Match malicio with phish columns that I have access to
#Quick look at data
head(malicio)

# make the phish parameters that I can into malicio
malicio <- malicio %>% mutate(dot_count = str_count(url, "\\."), url_len = str_count(url, "."), 
                  digit_count = str_count(url, "[0-9]"), special_count = str_count(url, "[^[:alnum:]]"), 
                  hyphen_count = str_count(url, "-"), double_slash = str_count(url, "//"), 
                  single_slash = str_count(url, "/"), at_the_rate = str_count(url, "@"), 
                  protocol = ifelse(str_starts(url, "http"), 1, 0), label = ifelse(label == "bad", 0, 1)) %>% 
  mutate(label = factor(label))
# remove columns you no longer need
malicio <- malicio %>% select(-url)


# Remove columns from phish that malicio can't obtain
phish_train <- phish_train %>% select(-c(whois_regDate, whois_expDate, whois_updatedDate, 
                                         protocol_count, web_traffic))
phish_test <- phish_test %>% select(-c(whois_regDate, whois_expDate, whois_updatedDate, 
                                       protocol_count, web_traffic))

# Retesting svm on phish with less columns
phish_short <- train(label ~ ., data = phish_train, method = "svmLinear")
p_short_pred <- predict(phish_short, newdata = phish_test)
svm_short <- confusionMatrix(p_short_pred, phish_test$label)
svm_short

# Test SVM on outside data
pred_outside <- predict(phish_short, newdata = malicio)
svm_outside <- confusionMatrix(pred_outside, malicio$label)
svm_outside

# Retest dt on phish with less columns
dt_short_train <- train(label ~ ., data = phish_train, method = "rpart")
dt_short_pred <- predict(phish_short, newdata = phish_test)
dt_short <- confusionMatrix(p_short_pred, phish_test$label)
dt_short

# Test DT on outside data
dt_pred_outside <- predict(phish_short, newdata = malicio)
dt_outside <- confusionMatrix(pred_outside, malicio$label)
dt_outside

#############
#  Results  #
#############


# Table of models
Methods <- c("Default train() with rpart method(DT)","Default train() with svmLinear method(SVM)", "Default DT on Short Phis", "Default SVM on Short Phis", "Default DT on Outside", "Default on SVM on Outside")  

all_models <- data.frame(Methods, Accuracy = c(dt_confu_matr$overall["Accuracy"],  
                                               svm_confu_matr$overall["Accuracy"], 
                                               dt_short$overall["Accuracy"],
                                               svm_short$overall["Accuracy"], 
                                               dt_outside$overall["Accuracy"], 
                                               svm_outside$overall["Accuracy"])  
)  

all_models %>% kable()















#Steps I took before the data changed for some reason, it was off by 1 prediction
# then looped to get it to perfect, I tested these best results number = 25 p = .9
# on the outside data but it didn't improve it, showing that this data has reached its  
# limit of being useful to improve the model


# Look for the perfect cv settings for svm
#samples <- seq(26, 30, by = 1)
#svm_accus <- sapply(samples, function(samp){
#  print(samp)
#  control <- trainControl(method = "cv", number = samp, p = 0.9)
#  svm_train_loop <-train(label ~ ., data = phish_train, method = "svmLinear", 
#                         trControl = control) 
#  svm_pred_loop <- predict(svm_train_loop, newdata = phish_test)
#  svm_accu_loop <- confusionMatrix(svm_pred_loop, phish_test$label)
#  return(svm_accu_loop$overall["Accuracy"])
#})

# Plot of different sample sizes on svm accuracies
#cv_plot_svm <- data.frame(Sample = samples, Accuracy = svm_accus)
#cv_plot_svm %>% ggplot(aes(Sample, Accuracy)) +
#  geom_point() + 
#  ggtitle("Accuracies of SVM with Different Bootstrap Samples")

#svm_best_cv <- c(samples[which.max(svm_accus)], 0.9)


# Look for the perfect cv settings for dt
#samples <- seq(26, 30, by = 1)
#dt_accus <- sapply(samples, function(samp){
#  print(samp)
#  control <- trainControl(method = "cv", number = samp, p = 0.9)
#  dt_train_loop <-train(label ~ ., data = phish_train, method = "rpart", 
#                         trControl = control) 
#  dt_pred_loop <- predict(dt_train_loop, newdata = phish_test)
#  dt_accu_loop <- confusionMatrix(dt_pred_loop, phish_test$label)
#  return(dt_accu_loop$overall["Accuracy"])
#})

# Plot of different sample sizes on dt accuracies
#cv_plot_dt <- data.frame(Sample = samples, Accuracy = dt_accus)
#cv_plot_dt %>% ggplot(aes(Sample, Accuracy)) +
#  geom_point() + 
#  ggtitle("Accuracies of DT with Different Bootstrap Samples")

#dt_best_cv <- c(samples[which.max(dt_accus)], 0.9)




# Getting the results for the final svm and dt
# Final svm model
#svm_tr_final <- train(label ~ ., data = phish_train, method = "svmLinear", 
#                      trControl = trainControl(method = "cv", number = svm_best_cv[1], 
#                                               p = svm_best_cv[2]))

# Final SVM predictions
#svm_pred_final <- predict(svm_tr_final, newdata = phish_test) 
# FInal SVM results
#svm_final <- confusionMatrix(svm_pred_final, phish_test$label)
#svm_final$overall["Accuracy"]

# Final Dt model
#dt_tr_final <- train(label ~ ., data = phish_train, method = "rpart", 
#                      trControl = trainControl(method = "cv", number = dt_best_cv[1], 
#                                               p = dt_best_cv[2]))
# Final Dt predictions
#dt_pred_final <- predict(dt_tr_final, newdata = phish_test) 
# FInal dt results
#dt_final <- confusionMatrix(dt_pred_final, phish_test$label)
#dt_final$overall["Accuracy"] 

# Table of models
#Methods <- c("Default train() with rpart method(DT)","Default train() with svmLinear method(SVM)",  
#             "Best Cross Validation Settings on train() with rpart", "Best Cross Validation Settings on train() with svmLinear")  
  
#all_models <- data.frame(Methods, Accuracy = c(dt_confu_matr$overall["Accuracy"],  
#                                               svm_confu_matr$overall["Accuracy"],  
#                                               dt_final$overall["Accuracy"],  
#                                               svm_final$overall["Accuracy"])  
#)  
#  
#all_models %>% kable()
