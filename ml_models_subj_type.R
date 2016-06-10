# remove control device & bin steps
model.data2 <- summary[device_id != "TAS1E31150005"][,steps := cut(steps, 5),]

# splits of steps
table(model.data2$steps)

# delete slowest bin
model.data2 <- model.data2[steps != "(-0.014,2.8]"]

# distribution of steps per device
table(model.data2[,.(steps, device_id),])

model.data2[,subj_type:=ifelse(model.data2$device_id == "TAS1E31150059", "dog", "human"),]

require(caret)
require(caretEnsemble)

set.seed(456456)

s <- createDataPartition(model.data2$subj_type, p = 0.6, list = FALSE)
training <- model.data2[s][,':='(
  device_id = NULL,
  subj_type = as.factor(subj_type),
  steps = as.factor(steps),
  epoch_id = NULL)]
testing <- model.data2[-s][,':='(
  device_id = NULL,
  steps = as.factor(steps),
  subj_type = as.factor(subj_type),
  epoch_id = NULL)]

# define model training control
my_control <- trainControl(
  method='repeatedcv',
  number=3,
  savePredictions=TRUE,
  classProbs=TRUE  
)

# train list of models using control spec'd above
model_list <- caretList(subj_type ~ ., 
                        data=training, 
                        trControl=my_control,
                        methodList=c('gbm','xgbTree', 'rf', 'svmRadial'))

# look at validation results for each 
lapply(model_list, 
       function(x) confusionMatrix(predict(x, testing), testing$subj_type))

# very encouraging results