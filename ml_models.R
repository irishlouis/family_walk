# remove control device
model.data <- summary[device_id != "TAS1E31150005"]

require(caret)
require(caretEnsemble)

# partition data
seeds <- c(456)#, 465, 783, 315, 97851, 951)

for (s in seeds) {

set.seed(s)

s <- createDataPartition(model.data$device_id, p = 0.6, list = FALSE)
training <- model.data[s][,':='(
 # device_id = as.factor(device_id),
  epoch_id = NULL)]
testing <- model.data[-s][,':='(
  device_id = as.factor(device_id),
  epoch_id = NULL)]

# define model training control
my_control <- trainControl(
  method='repeatedcv',
  number=3,
  savePredictions=TRUE,
  classProbs=TRUE  
)


set.seed(s * 3)
# train list of models using control spec'd above
model_list <- caretList(device_id ~ ., 
                        data=training, 
                        trControl=my_control,
                        methodList=c('xgbTree', 'rf', 'svmRadial'))

print(lapply(model_list, 
       function(x) confusionMatrix(predict(x, testing), testing$device_id)))
}

