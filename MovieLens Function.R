predictMovieLens <- function (train, test){
  #SET UP
  # function to create a test set and training set of 5000 entries total to reduce run time while testing
  createSet <- function (df, n){
    samp <- sample_n(df, n)
    test_index <- createDataPartition(y = samp$rating, times = 1, p = 0.2, 
                                      list = FALSE)
    train_set <- samp[-test_index,]
    test_set <- samp[test_index,]
    
    test_set <- test_set %>% 
      semi_join(train_set, by = "movieId") %>%
      semi_join(train_set, by = "userId")
    return(list(train_set, test_set))
  }
  # add release date columns
  train <- train %>% mutate(releasedate=as.numeric(sub('.*(\\d{4}).*', '\\1', train$title)))
  test <- test %>% mutate(releasedate=as.numeric(sub('.*(\\d{4}).*', '\\1', test$title)))
  # create test and training set
  set <- createSet(train, nrow(train))
  train_set <- as.data.frame(set[1])
  test_set <- as.data.frame(set[2])
  
  lambdas <- seq(0, 10, 0.25)
  
  regularization <- function(l, train_set, test_set){
    #NAIVE RMSE 
    mu <- mean(train_set$rating)
    
    #MOVIE BIAS  
    b_i <- train_set %>% 
      group_by(movieId) %>% 
      summarize(b_i = sum(rating - mu) / (n() + l))
    # NAs to 0
    b_i[is.na(b_i)] <- 0
    # add movie avgs to train set for future use
    train_set <- train_set %>% left_join(b_i, by='movieId')
    
    
    #MOVIE + USER BIAS 
    b_u <- train_set %>% 
      group_by(userId) %>%
      summarize(b_u = sum(rating - mu - b_i) / (n() + l))
    # NAs to 0
    b_u[is.na(b_u)] <- 0
    # add user biases to train_set for future use
    train_set <- train_set %>% left_join(b_u, by='userId')
    
    
    #MOVIE + USER + YEAR BIAS  (overall, not by individual)
    b_y <- train_set %>%
      group_by(releasedate) %>% 
      summarize(b_y = sum(rating - mu - b_i - b_u) / (n() + l))
    # NAs to 0
    b_y[is.na(b_y)] <- 0
    train_set <- train_set %>% left_join(b_y, by='releasedate')
    
    #MOVIE + USER + YEAR + GENRE BIAS (by genre combinations, may result in overtraining)
    b_g <- train_set %>%
      group_by(genres) %>%
      summarize (b_g = sum(rating - mu - b_i - b_y)/(n() + l))
    # NAs to 0
    b_g[is.na(b_g)] <- 0
    # add genre biases to train_set for future use
    train_set <- train_set %>% left_join(b_g, by='genres')
    
    #PREDICTED RATINGS
    predicted_ratings <- test_set %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_y, by = "releasedate") %>%
      left_join(b_g, by = "genres") %>%
      mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
      pull(pred)
    
    return(RMSE(predicted_ratings, test_set$rating))
  }
  # apply above function to find ideal lambda
  rmses <- sapply(lambdas, regularization, train_set = train_set, test_set = test_set)
  # find ideal lambda (one with smalled RMSE)
  lambda <- lambdas[which.min(rmses)]
  # apply function to given train and test sets (edx and validation, respectively) with the given lambda to predict values and find RMSE
  regularization(lambda, train, test)
}

predictMovieLens(edx, validation)