prepare_data <- function(dataset) {
    
  return dataset_prepared
}

save_prepare_data <- function(dataset) {
  
  write dataset to csv file
}
  
build_model <- function(dataset) {
  built model
  save model
}

predict <- function(dataset) {
  read model
  prepare new dataset
  predict using dataset
  return predictions
}


#
dataset <- read.csv("CustData2.csv")

training_data <- prepare_data(dataset)

save_prepare_data(training_data)

build_model(training_data)

prediction_dataset <- read.csv("Predict.csv")

prediction_dataset <- prepare_data(prediction_dataset)

predict(prediction_dataset)