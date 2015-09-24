# Code for the Coursera Machine Learning Course Project
# 9/22/2015

library(ggplot2)
library(gridExtra)
library(plyr) # Has to be before dplyr
library(dplyr)
library(reshape2)
library(caret)

# Load the data
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")


# Remove rows that have NA
training_na <- training[complete.cases(training),]

# A lot of columns in testing data set are 100% NA
# -> remove them from both training and testing
nb_test <- nrow(testing)
all_na <-  which(apply(testing, 2, function(x) sum(is.na(x))==nb_test))
testing2 <- testing[,-all_na]
training2 <- training[,-all_na]

# also remove the first column which is just an index
testing2 <- testing2[,-1]
training2 <- training2[,-1]

# eliminate columns with factors with only 1 level
# in either training or testing
to_rm = c()
# Exclude the last column which is different for train & test
for (colmn in colnames(training2[,-ncol(training2)])) {
	if (class(training2[,colmn]) == "factor") {
		if ((nlevels(testing2[,colmn]) < 2) | (nlevels(training2[,colmn]) < 2)) {
			col_nb <- grep(colmn,colnames(testing2)) # convert to column index
			to_rm <- c(to_rm, col_nb) # add to list of "to remove"
		}
	}
}
training2 <- training2[,-to_rm]
testing2 <- testing2[,-to_rm]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
set.seed(4053)
gbmFit <- train(classe ~ ., data = training2, method = "gbm",
                 trControl = fitControl, verbose = FALSE)
varImp(gbmFit)


trellis.par.set(caretTheme())
plot(gbmFit, metric = "Accuracy") # or Kappa - little difference

results <- predict(gbmFit, newdata = testing)


# Write out the answers to the files

# Function to write the answers 
# Argument x is the list of 20 answers in order
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Make sure the files that get written out have one character each with your
# prediction for the corresponding problem ID.
pml_write_files(results)