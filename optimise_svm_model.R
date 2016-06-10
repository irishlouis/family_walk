# remove control device & bin steps
model.data <- summary[device_id != "TAS1E31150005"][,steps := cut(steps, 5),]

# delete slowest bin
model.data <- model.data[steps != "(-0.014,2.8]"]

require(caret)

set.seed(456456)

s <- createDataPartition(model.data$device_id, p = 0.6, list = FALSE)
training <- model.data[s][,':='(
  device_id = as.factor(device_id),
  steps = as.factor(steps),
  epoch_id = NULL)]
testing <- model.data[-s][,':='(
  steps = as.factor(steps),
  device_id = as.factor(device_id),
  epoch_id = NULL)]

# define model training control
my_control <- trainControl(
  method='repeatedcv',
  repeats=5,
  search="random",
  savePredictions=TRUE,
  classProbs=TRUE
)


# train list of models using control spec'd above
set.seed(456798)
model_svm <- train(device_id ~ ., 
                        data=training, 
                        trControl=my_control,
                        metric = "Kappa",
                        method ='svmRadial',
                        tuneLength = 10)


# look at validation results for each 
confusionMatrix(predict(model_svm, testing), testing$device_id)

###############################################
#
# feature selection using GA

# new simpler ctrl func
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
## Use this function to optimize the model. The two parameters are 
## evaluated on the log scale given their range and scope. 
svm_fit_bayes <- function(logC, logSigma) {
  ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- train(device_id ~ ., data = training,
                 method = "svmRadial",
                 metric = "Kappa",
                 trControl = ctrl,
                 tuneGrid = data.frame(C = exp(logC), sigma = exp(logSigma)))
  )
  ## The function wants to _maximize_ the outcome so we return 
  ## the negative of the resampled RMSE value. `Pred` can be used
  ## to return predicted values but we'll avoid that and use zero
  list(Score = getTrainPerf(mod)[, "TrainKappa"], Pred = 0)
}

lower_bounds <- c(logC = -5, logSigma = -9)
upper_bounds <- c(logC = 20, logSigma = -0.75)
bounds <- list(
  logC = c(lower_bounds[1], upper_bounds[1]),
  logSigma = c(lower_bounds[2], upper_bounds[2]))

## Create a grid of values as the input into the BO code
initial_grid <- model_svm$results[, c("C", "sigma", "Kappa")]
initial_grid$C <- log(initial_grid$C)
initial_grid$sigma <- log(initial_grid$sigma)

names(initial_grid) <- c("logC", "logSigma", "Value")

library(rBayesianOptimization)

set.seed(8606)
ba_search <- BayesianOptimization1(FUN = svm_fit_bayes, 
                                  bounds = bounds,
                                  init_grid_dt = initial_grid, 
                                  init_points = 5, 
                                  n_iter = 50,
                                  acq = "ucb", 
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)

#'  Best Parameters Found: 
#'  Round = 55	logC = 4.0622	logSigma = -2.5745	Value = 0.9380 

set.seed(456798)
final_svm <- train(device_id ~ ., 
                      data = training,
                      method = "svmRadial",
                      tuneGrid = data.frame(C = exp(ba_search$Best_Par["logC"]), 
                                            sigma = exp(ba_search$Best_Par["logSigma"])),
                      metric = "Kappa",
                      trControl = ctrl)

confusionMatrix(predict(final_svm, testing), testing$device_id)
confusionMatrix(predict(model_svm, testing), testing$device_id)

compare_models(final_svm, model_svm)





###########
# modified function
## GP <- GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ], Y = Value_Vec[Rounds_Unique]) # , ...) '...' causes error


BayesianOptimization1 <- function (FUN, bounds, init_points, n_iter, acq = "ucb", kappa = 2.576, 
                                   eps = 0, verbose = TRUE, ...) 
{
  DT_bounds <- data.table(Parameter = names(bounds), Lower = sapply(bounds, 
                                                                    magrittr::extract2, 1), Upper = sapply(bounds, magrittr::extract2, 
                                                                                                           2), Type = sapply(bounds, class))
  DT_history <- data.table(matrix(-Inf, nrow = init_points + 
                                    n_iter, ncol = length(bounds) + 2)) %>% setnames(., old = names(.), 
                                                                                     new = c("Round", names(bounds), "Value"))
  Pred_list <- vector(mode = "list", length = init_points + 
                        n_iter)
  for (i in 1:init_points) {
    Sys.sleep(time = 1)
    set.seed(as.numeric(Sys.time()))
    This_Par <- Matrix_runif(n = 1, lower = DT_bounds[, Lower], 
                             upper = DT_bounds[, Upper]) %>% as.vector(.) %>% 
      magrittr::inset(., DT_bounds[, Type] == "integer", 
                      round(magrittr::extract(., DT_bounds[, Type] == 
                                                "integer"))) %>% magrittr::set_names(., DT_bounds[, 
                                                                                                  Parameter])
    This_Log <- utils::capture.output({
      This_Time <- system.time({
        This_Score_Pred <- do.call(what = FUN, args = as.list(This_Par))
      })
    })
    data.table::set(DT_history, i = as.integer(i), j = names(DT_history), 
                    value = as.list(c(Round = i, This_Par, Value = This_Score_Pred$Score)))
    Pred_list[[i]] <- This_Score_Pred$Pred
    if (verbose == TRUE) {
      paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"], 
                                                      trim = FALSE, digits = 0, nsmall = 2), format(DT_history[i, 
                                                                                                               "Round", with = FALSE], trim = FALSE, digits = 0, 
                                                                                                    nsmall = 0), format(DT_history[i, -"Round", with = FALSE], 
                                                                                                                        trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", 
            collapse = "\t") %>% cat(., "\n")
    }
  }
  for (j in (init_points + 1):(init_points + n_iter)) {
    Par_Mat <- Min_Max_Scale_Mat(as.matrix(DT_history[1:(j - 
                                                           1), DT_bounds[, Parameter], with = FALSE]), lower = DT_bounds[, 
                                                                                                                         Lower], upper = DT_bounds[, Upper])
    Rounds_Unique <- setdiff(1:(j - 1), which(duplicated(Par_Mat) == 
                                                TRUE))
    Value_Vec <- DT_history[1:(j - 1), Value]
    GP <- GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ], Y = Value_Vec[Rounds_Unique]) # , ...)
    Next_Par <- Utility_Max(DT_bounds, GP, acq = acq, y_max = max(DT_history[, 
                                                                             Value]), kappa = kappa, eps = eps) %>% Min_Max_Inverse_Scale_Vec(., 
                                                                                                                                              lower = DT_bounds[, Lower], upper = DT_bounds[, Upper]) %>% 
      magrittr::inset(., DT_bounds[, Type] == "integer", 
                      round(magrittr::extract(., DT_bounds[, Type] == 
                                                "integer"))) %>% magrittr::set_names(., DT_bounds[, 
                                                                                                  Parameter])
    This_Log <- utils::capture.output({
      This_Time <- system.time({
        Next_Score_Pred <- do.call(what = FUN, args = as.list(Next_Par))
      })
    })
    data.table::set(DT_history, i = as.integer(j), j = names(DT_history), 
                    value = as.list(c(Round = j, Next_Par, Value = Next_Score_Pred$Score)))
    Pred_list[[j]] <- Next_Score_Pred$Pred
    if (verbose == TRUE) {
      paste(c("elapsed", names(DT_history)), c(format(This_Time["elapsed"], 
                                                      trim = FALSE, digits = 0, nsmall = 2), format(DT_history[j, 
                                                                                                               "Round", with = FALSE], trim = FALSE, digits = 0, 
                                                                                                    nsmall = 0), format(DT_history[j, -"Round", with = FALSE], 
                                                                                                                        trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", 
            collapse = "\t") %>% cat(., "\n")
    }
  }
  Best_Par <- as.numeric(DT_history[which.max(Value), DT_bounds[, 
                                                                Parameter], with = FALSE]) %>% magrittr::set_names(., 
                                                                                                                   DT_bounds[, Parameter])
  Best_Value <- max(DT_history[, Value])
  Pred_DT <- data.table::as.data.table(Pred_list)
  Result <- list(Best_Par = Best_Par, Best_Value = Best_Value, 
                 History = DT_history, Pred = Pred_DT)
  cat("\n Best Parameters Found: \n")
  paste(names(DT_history), c(format(DT_history[which.max(Value), 
                                               "Round", with = FALSE], trim = FALSE, digits = 0, nsmall = 0), 
                             format(DT_history[which.max(Value), -"Round", with = FALSE], 
                                    trim = FALSE, digits = 0, nsmall = 4)), sep = " = ", 
        collapse = "\t") %>% cat(., "\n")
  return(Result)
}


