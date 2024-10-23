##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

########################################################################
####Code above is provided by edx class and is to not be altered########
########################################################################
# Note using R 4.4.1
# Add additional package
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(knitr)
library(gridExtra)
library(corrplot)
# Exploration
# Get Column Names
Columns <- colnames(edx)

# Manually get column definitions from the help section on movielens
Definitions <- c("Unique ID for the user.", 
                 "Unique ID for the movie.",
                 "A rating between 0 and 5, for the movie.", 
                 "Date and time the rating was given.", 
                 "Movie title (not unique).",
                 "Genres associated with the movie."
)

# Table of edx variables and their definitions
colnames_definitions <- data.frame(Columns, Definitions)

colnames_definitions %>% kable()

# See what each row represents
head(edx)

# size of the edx
dim(edx)

# Overall View of the data
str(edx)

# Explore the range of the rows and columns
unique(edx$rating)
# Find the unique count for the columns
Count <- sapply((1:ncol(edx)), function(n){
  length(unique(edx[,n]))
})

# Present a table of the unique counts
count_data <- data.frame(Columns, Count)

count_data %>% kable()

# The method for creating the partitions were taken from the given code
# creating the edx and final_holdout_test datasets

# Partition the edx dataset for refining and avoid overtraining
set.seed(5)
edx_test_index <- createDataPartition(y = edx$rating, times = 1,
                                      p = 0.1, list = FALSE)
tmp <- edx[edx_test_index,]
edx_train <- edx[-edx_test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- tmp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from edx_test set back into edx_train set
removed <- anti_join(tmp, edx_test)
edx_train <- rbind(edx_train, removed)

# Check for any na
any(is.na(edx_train))
any(is.na(edx_test))

# Graphical exploration
# Histogram of ratings
r1 <- edx_train %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.5, color = "white") +
  ggtitle("Ratings", subtitle = "0.5 Increments")

r2 <- edx_train %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth = 1, color = "white") +
  ggtitle("Ratings", subtitle = "Whole Increments")

grid.arrange(r1, r2, ncol = 2)

# Histogram of movies
movie_counts <- edx_train %>% 
  group_by(movieId) %>%
  summarise(Movies = n())

h1 <- movie_counts %>% ggplot(aes(Movies)) + 
  geom_histogram(bins = 30, color = "white") + 
  ggtitle("Ratings per Movie")

# Histogram of users
user_counts <- edx_train %>%
  group_by(userId) %>%
  summarise(Users = n())

h2 <- user_counts %>% ggplot(aes(Users)) +
  geom_histogram(bins = 30, color = "white") +
  ggtitle("Ratings per User")

# Histogram of genres
genres_counts <- edx_train %>%
  group_by(genres) %>%
  summarise(Genres = n())

h3 <- genres_counts %>% ggplot(aes(Genres)) +
  geom_histogram(bins = 30, color = "white") +
  ggtitle("Ratings per Genres")

grid.arrange(h1, h2, h3, ncol = 3)

# Scaled histogram of movies
l1 <- movie_counts %>% ggplot(aes(Movies)) + 
  geom_histogram(bins = 20, color = "white") + 
  scale_x_log10() + 
  ggtitle("Ratings per Movie")

# Scaled histogram of users
l2 <- user_counts %>% ggplot(aes(Users)) +
  geom_histogram(bins = 20, color = "white") +
  scale_x_log10() + 
  ggtitle("Ratings per User")

# Scaled histogram of genres
l3 <- genres_counts %>% ggplot(aes(Genres)) +
  geom_histogram(bins = 15, color = "white") + 
  scale_x_log10() + 
  ggtitle("Ratings per Genres")

grid.arrange(l1, l2, l3, ncol = 3)

# Number the genres to get the correlation
edx_train <- edx_train %>% mutate(genres_n = as.numeric(factor(genres)))

# Compute correlation of numeric columns
cors <- edx_train %>% select(-c(title,genres)) %>% cor()
# Plot the correlation using the corrplot package
corrplot(cors, method = "square", outline = T, tl.col = "black")

# Check p-value of the move correlated predictors
cor.test(edx$movieId, edx$timestamp)


# Following the method from "Building the Recommendation System"
# from class, the code follows very close from building the rmse to 
# getting the predictions for the movie + user effects model 

# Create the rmse function
rmse <- function(true_rating, predicted_rating) {
  sqrt(mean((true_rating - predicted_rating)^2))
}

# Create mu_hat
mu_hat <- mean(edx_train$rating)

# naive rmse
naive_rmse <- rmse(edx_test$rating, mu_hat)
naive_rmse

# Create the bi_hat values (movie effects)
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(bi_hat = mean(rating - mu_hat))

# Prediction values based on movie effects model on edx_test
pred_movie <- edx_test %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  mutate(preds = mu_hat + bi_hat) %>%
  pull(preds)

# RMSE for movie effects model
movie_rmse <- rmse(edx_test$rating, pred_movie)
movie_rmse

# Create the bu_hat values (user effects)
user_avgs <- edx_train %>% 
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>% 
  summarise(bu_hat = mean(rating - mu_hat - bi_hat))

# Prediction values based on movie effects + user effects model on edx_test
pred_m_u <- edx_test %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>% 
  mutate(preds = mu_hat + bi_hat + bu_hat) %>% 
  mutate(preds = preds) %>%
  pull(preds)

# RMSE for movie + user effects model
m_u_rmse <- rmse(edx_test$rating, pred_m_u)
m_u_rmse

# Methods definitions for base models
Methods <- c("Just the average", 
             "Movie Effects Model",
             "Movie + User Effects Model"
)

# Table of methods and their rmse
base_models <- data.frame(Methods, RMSE = c(naive_rmse, movie_rmse, 
                                            m_u_rmse))
# Table of base models
base_models %>% kable()

# Testing Clamping
# Make a function to clamp to 0.5-5 
clamp <- (function(preds){
  p_clamp <- sapply(preds, function(p){
    if(p < 0.5) {
      return(0.5)
    } else if (p > 5 ) {
      return(5)
    } else {
      return(p)
    }
  })
  return(p_clamp)
})

# Testing clamp on earlier predictions
# Clamping the predictions from movie effects model
movie_rmse_c <- rmse(edx_test$rating, clamp(pred_movie))
movie_rmse_c
movie_rmse # unclamped rmse of movie effects model

# Clamping the predictions from movie + user effects model
m_u_rmse_c <- rmse(edx_test$rating, clamp(pred_m_u))
m_u_rmse_c
m_u_rmse # unclamped rmse of movie + user effects model

# Testing rounding to the nearest 0.5 on earlier predictions
# Rounding the predictions from movie effects model
movie_rmse_r <- rmse(edx_test$rating, round(pred_movie * 2) / 2)
movie_rmse_r
movie_rmse # non rounded rmse of movie effects model

# Rounding the predictions from movie + user effects model
m_u_rmse_r <- rmse(edx_test$rating, round(pred_m_u * 2) / 2)
m_u_rmse_r
m_u_rmse # non rounded rmse of movie + user effects model

# Methods definitions for clamping and rounding models
Methods <- c("Movie Effects Model", 
             "Movie Effects Model Clamped", 
             "Movie Effects Model Rounded", 
             "Movie + User Effects Model", 
             "Movie + User Effects Model Clamped", 
             "Movie + User Effects Model Rounded"
)

# Table of methods and their rmse
clamp_round_models <- data.frame(Methods, RMSE = c(movie_rmse, movie_rmse_c, 
                                                   movie_rmse_r, m_u_rmse, m_u_rmse_c, 
                                                   m_u_rmse_r))
# Table of clamp and rounding models
clamp_round_models %>% kable()

# Applying the same method from earlier to add another effect to the model

# Create the bg_hat values (genres effects)
genre_avg <- edx_train %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>% 
  group_by(genres) %>%
  summarise(bg_hat = mean(rating - mu_hat - bi_hat - bu_hat))

# Prediction values based on movie + user + genres effect model on edx_test
pred_m_u_g <- edx_test %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avg, by = "genres") %>%
  mutate(preds = mu_hat + bi_hat + bu_hat + bg_hat) %>%
  mutate(preds = clamp(preds)) %>%
  pull(preds)

# RMSE for movie + user + genres effects model
m_u_g_rmse <- rmse(edx_test$rating, pred_m_u_g)
m_u_g_rmse
m_u_rmse_c # earlier best model

# Following the method from "Regularization"
# from class, the code follows very close from regularizing
# bi_h and bu_h and using the same method to apply it to bg_h

# Lambdas to test on regularizing 
lambdas <- seq(0, 10, 0.5)

# Regularization of movies on full effects model
movie_rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(edx_train$rating)
  # Regularized
  bi_h <- edx_train %>% 
    group_by(movieId) %>%
    summarise(bi_h = sum(rating - mu_hat) / (n() + l))
  
  bu_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    group_by(userId) %>% 
    summarise(bu_h = mean(rating - mu_hat - bi_h))
  
  bg_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    left_join(bu_h, by = "userId") %>% 
    group_by(genres) %>%
    summarise(bg_h = mean(rating - mu_hat - bi_h - bu_h))
  
  pred_r <- edx_test %>% 
    left_join(bi_h, by = "movieId") %>% 
    left_join(bu_h, by = "userId") %>% 
    left_join(bg_h, by = "genres") %>% 
    mutate(pred = mu_hat + bi_h + bu_h + bg_h) %>% 
    mutate(pred = clamp(pred)) %>%
    pull(pred)
  
  return(rmse(edx_test$rating, pred_r))
})

# Plot to show purpose of testing different lambdas
qplot(lambdas, movie_rmses)

# Best lambda for regularized movies on full effects model
movie_lambda <- lambdas[which.min(movie_rmses)]
movie_lambda

# RMSE for regularized movies on full effects model
movie_reg <- min(movie_rmses)
movie_reg
m_u_g_rmse # earlier best model

# Regularization of movies and users on full effects model
m_u_rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(edx_train$rating)
  # Regularized movies
  bi_h <- edx_train %>% 
    group_by(movieId) %>%
    summarise(bi_h = sum(rating - mu_hat) / (n() + l))
  # Regularized  users
  bu_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    group_by(userId) %>% 
    summarise(bu_h = sum(rating - mu_hat - bi_h) / (n() + l))
  
  bg_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    left_join(bu_h, by = "userId") %>% 
    group_by(genres) %>%
    summarise(bg_h = mean(rating - mu_hat - bi_h - bu_h))
  
  pred_r <- edx_test %>% 
    left_join(bi_h, by = "movieId") %>% 
    left_join(bu_h, by = "userId") %>% 
    left_join(bg_h, by = "genres") %>% 
    mutate(pred = mu_hat + bi_h + bu_h + bg_h) %>% 
    mutate(pred = clamp(pred)) %>%
    pull(pred)
  
  return(rmse(edx_test$rating, pred_r))
})

# Best lambda for full effects model when regularizing movies and users
m_u_lambda <- lambdas[which.min(m_u_rmses)]
m_u_lambda

# RMSE for full effects model when regularizing movies and users
m_u_reg <- min(m_u_rmses)
m_u_reg
movie_reg # earlier best model

# Regularization of movies, users, and genres on full effects model
m_u_g_rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(edx_train$rating)
  # Regularized movies
  bi_h <- edx_train %>% 
    group_by(movieId) %>%
    summarise(bi_h = sum(rating - mu_hat) / (n() + l))
  # Regularized users
  bu_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    group_by(userId) %>% 
    summarise(bu_h = sum(rating - mu_hat - bi_h) / (n() + l))
  # Regularized genres
  bg_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    left_join(bu_h, by = "userId") %>% 
    group_by(genres) %>%
    summarise(bg_h = sum(rating - mu_hat - bi_h - bu_h) / (n() + l))
  
  pred_r <- edx_test %>% 
    left_join(bi_h, by = "movieId") %>% 
    left_join(bu_h, by = "userId") %>% 
    left_join(bg_h, by = "genres") %>% 
    mutate(pred = mu_hat + bi_h + bu_h + bg_h) %>% 
    mutate(pred = clamp(pred)) %>%
    pull(pred)
  
  return(rmse(edx_test$rating, pred_r))
})

# Best lambda for model when regularizing movies, users, and genres
m_u_g_lambda <- lambdas[which.min(movie_rmses)]
m_u_g_lambda

# RMSE for full effects model when regularizing movies, users, and genres
m_u_g_reg <- min(m_u_g_rmses)
m_u_g_reg
m_u_reg # earlier best model

# Try to fine tune lambda for a better result on our best model
lambdas_fine <- seq(4.1, 4.9, 0.1)
m_u_rmses <- sapply(lambdas_fine, function(l){
  
  mu_hat <- mean(edx_train$rating)
  # REgularized movies
  bi_h <- edx_train %>% 
    group_by(movieId) %>%
    summarise(bi_h = sum(rating - mu_hat) / (n() + l))
  # Regularized users
  bu_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    group_by(userId) %>% 
    summarise(bu_h = sum(rating - mu_hat - bi_h) / (n() + l))
  
  bg_h <- edx_train %>% 
    left_join(bi_h, by = "movieId") %>%
    left_join(bu_h, by = "userId") %>% 
    group_by(genres) %>%
    summarise(bg_h = mean(rating - mu_hat - bi_h - bu_h))
  
  pred_r <- edx_test %>% 
    left_join(bi_h, by = "movieId") %>% 
    left_join(bu_h, by = "userId") %>% 
    left_join(bg_h, by = "genres") %>% 
    mutate(pred = mu_hat + bi_h + bu_h + bg_h) %>% 
    mutate(pred = clamp(pred)) %>%
    pull(pred)
  
  return(rmse(edx_test$rating, pred_r))
})

# Find out if a better lambda was found
m_u_lambda_fine <- lambdas_fine[which.min(m_u_rmses)]
m_u_lambda_fine
m_u_lambda # earlier best lambda

# Methods definitions for Regularization
Methods <- c("Full Effects Model", 
             "Regularized Movie on Full Effects Model", 
             "Regularized Movie + User on Full Effects Model ", 
             "Regularized Movie + User + Genres Model" 
)

# Table of methods and their rmses
regularized_models <- data.frame(Methods, RMSE = c(m_u_g_rmse, movie_reg, m_u_reg, m_u_g_reg))
# Table of estimate models
regularized_models %>% kable()

# Create our final model to get predictions from final_holdout_test
lambda_final <- m_u_lambda_fine
mu_hat <- mean(edx_train$rating)
# Regularized
bi_h <- edx_train %>% 
  group_by(movieId) %>%
  summarise(bi_h = sum(rating - mu_hat) / (n() + lambda_final))
# Regularized
bu_h <- edx_train %>% 
  left_join(bi_h, by = "movieId") %>%
  group_by(userId) %>% 
  summarise(bu_h = sum(rating - mu_hat - bi_h) / (n() + lambda_final))

bg_h <- edx_train %>% 
  left_join(bi_h, by = "movieId") %>%
  left_join(bu_h, by = "userId") %>% 
  group_by(genres) %>%
  summarise(bg_h = mean(rating - mu_hat - bi_h - bu_h))

# Get predictions of the final model on final_holdout_test
final_pred <- final_holdout_test %>% 
  left_join(bi_h, by = "movieId") %>%
  left_join(bu_h, by = "userId") %>% 
  left_join(bg_h, by = "genres") %>%
  mutate(pred = mu_hat + bi_h + bu_h + bg_h) %>% 
  mutate(clamp(pred)) %>%
  pull(pred)

# RMSE for our final model on final_holdout_test
final_rmse <- rmse(final_holdout_test$rating, final_pred)
final_rmse

# Methods definitions for all models
Methods <- c("Movie Effects Model", 
             "Movie Effects Model Clamped", 
             "Movie Effects Model Rounded", 
             "Movie + User Effects Model", 
             "Movie + User Effects Model Clamped", 
             "Movie + User Effects Model Rounded", 
             "Full Effects Model", 
             "Regularized Movie on Full Effects Model", 
             "Regularized Movie + User on Full Effects Model", 
             "Regularized Movie + User + Genres Model", 
             "Best Model on final_holdout_test"
)

# Table of methods and their rmse
all_models <- data.frame(Methods, RMSE = c(movie_rmse, movie_rmse_c, movie_rmse_r, 
                                           m_u_rmse, m_u_rmse_c, m_u_rmse_r, 
                                           m_u_g_rmse, movie_reg, m_u_reg, 
                                           m_u_g_reg, final_rmse))
# Table of all models
all_models %>% kable()
