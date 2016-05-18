# remove control device & bin steps
model.data <- summary[device_id != "TAS1E31150005"][,steps := cut(steps, 5),]

# splits of steps
table(model.data$steps)

# delete slowest bin
model.data <- model.data[steps != "(-0.014,2.8]"]

# distribution of steps per device
table(model.data[,.(steps, device_id),])

require(caret)
require(caretEnsemble)

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
lapply(model_list, 
       function(x) confusionMatrix(predict(x, testing), testing$device_id))

# encouraging results
