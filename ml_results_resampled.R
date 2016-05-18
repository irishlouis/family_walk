# run with multiple splits to see if performance holds for new data/different subject

checkModelPerf <- function(seed=1){
  set.seed(seed)
  # partition data for building model
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
  my_control <- caret::trainControl(
    method='repeatedcv',
    number=3,
    savePredictions=TRUE,
    classProbs=TRUE  
  )
  
  # train list of models using control spec'd above
  model_list <- caretList(device_id ~ ., 
                          data=training, 
                          trControl=my_control,
                          methodList=c('gbm','xgbTree', 'rf', 'svmRadial'))
  
  # look at validation results for each 
  return(lapply(model_list, 
         function(x) confusionMatrix(predict(x, testing), testing$device_id)$overall[2]) %>% unlist)
}

set.seed(123123)
# set how many runs to do
n_resamples <- 30
seeds <- round(rep(runif(n_resamples, 1, 10000000)), 0)

# generate models using 30 different seeds to split training / testing and store Kappa values
resample.results <- do.call(rbind, lapply(seeds, function(s) {print(which(seeds ==s)); return(checkModelPerf(s))}))

# summary of kappa values for each model method for the 30 runs
summary(resample.results)

require(scales)

# plot resultant Kappa values
ggplot(melt(resample.results, value.name = "kappa") %>% 
         mutate(Var2 = str_replace(Var2, ".Kappa", "")),
       aes(x=Var2, y=kappa)) + 
  geom_violin() +
  geom_boxplot(fill = "grey", alpha = 0.25) + 
  geom_point(alpha = 0.25) +
  theme_bw() +
  labs(title = "Kappa Results from Models - 30 Data Partitions",
       subtitle = "The evaluation results of models show some variation in performance depending on the data split.
In general models are showing strong predictive power.",
       caption = "Grey box represents IQR with Median\nViolin plot shows distribution",
       x="",
       y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
         panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = percent)
         
  

