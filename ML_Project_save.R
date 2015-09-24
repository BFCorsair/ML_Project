# Code for the Coursera Machine Learning Course Project
# 9/22/2015

library(ggplot2)
library(gridExtra)
library(plyr) # Has to be before dplyr
library(dplyr)
library(reshape2)
library(caret)


# Function to write the answers 
# Argument x is the list of 20 answers in order
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}



# Load the data
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

## 10-fold CV
fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 1)
set.seed(825)
gbm_start <- Sys.time()
gbmFit1 <- train(classe ~ ., data = training, method = "gbm", trControl = fitControl,
	verbose = FALSE) # This last option is actually onefor gbm() that passes through
gbm_end <- Sys.time()
gbmFit1

# varImp(gbmFit1)
# gbm variable importance

#   only 20 most important variables shown (out of 6952)

#                                  Overall
# X                              1.000e+02
# avg_roll_dumbbell              4.473e+00
# cvtd_timestamp02/12/2011 13:34 1.007e+00
# cvtd_timestamp30/11/2011 17:11 4.776e-01
# avg_pitch_dumbbell             2.802e-01
# var_accel_dumbbell             2.036e-01
# avg_roll_forearm               1.424e-01
# max_picth_dumbbell             3.612e-02
# max_roll_dumbbell              1.362e-02
# accel_forearm_y                3.037e-03
# amplitude_roll_arm             2.698e-03
# var_accel_forearm              2.698e-03
# min_roll_forearm               2.668e-03
# var_accel_arm                  2.510e-03
# min_roll_belt                  2.052e-03
# yaw_arm                        1.937e-03
# accel_forearm_z                1.485e-03
# magnet_dumbbell_x              1.160e-03
# amplitude_pitch_dumbbell       1.084e-03
# stddev_yaw_arm                 8.359e-04


# Remove rows that have NA
training_na <- training[complete.cases(training),]
# Find out which columns have rows w/ division by 0: #DIV/0!
div_0 <- which(apply(training_na, 2, function(x) any(grepl("#DIV/0!", x))))
# Among the columns that have DIV/0, remove the ones that are only 2 factors
to_rm = c()
for (colmn in div_0) {if (nlevels(training_na[,colmn]) < 3) {to_rm <- c(to_rm, colmn)} }
training2 <- training_na[,-to_rm]
# For each column in div_0, eliminate the rows that have DIV/0
# Recompute the columns that have DIV/0
div_0 <- which(apply(training2, 2, function(x) any(grepl("#DIV/0!", x))))
# Iterate over each of these columns to remove all rows that have DIV/0
training3 <- training2
for (colmn in div_0) {
	# Check if there are still DIV/0 after iterations on previous columns
	if (any(grepl("#DIV/0!", training3[,colmn]))) {
		training3 <- training3[-grep("#DIV/0!", training3[,colmn]),]
	}
}
fitControl3 <- trainControl(method = "repeatedcv", number = 10,repeats = 1)
gbm_start <- Sys.time()
gbmFit3 <- train(classe ~ ., data = training3, method = "gbm", trControl = fitControl3,
	verbose = FALSE) # This last option is actually onefor gbm() that passes through
gbm_duration <- Sys.time() - gbm_start
varImp(gbmFit3)

# --------
# repeats = 10

#                                  Overall
# X                              100.00000
# avg_roll_dumbbell                9.12990
# min_roll_forearm                 3.74972
# avg_roll_forearm                 1.65028
# pitch_belt                       1.12143
# var_accel_dumbbell               0.84081
# amplitude_roll_belt              0.83807
# yaw_dumbbell                     0.54251
# cvtd_timestamp05/12/2011 14:23   0.41204
# max_picth_arm                    0.28594
# raw_timestamp_part_1             0.22657
# max_roll_dumbbell                0.15789
# stddev_roll_arm                  0.13753
# avg_pitch_forearm                0.11638
# max_picth_belt                   0.11348
# avg_yaw_dumbbell                 0.09883
# raw_timestamp_part_2             0.08830
# roll_forearm                     0.04845
# roll_dumbbell                    0.04694
# max_yaw_forearm-1.6              0.04511

# ***
# *** ACCURACY RESULTS SHOWS THAT REPEATS =1 IS SUFFICIENT
# ***
# gbmFit3
# Stochastic Gradient Boosting 

# 217 samples
# 153 predictors
#   5 classes: 'A', 'B', 'C', 'D', 'E' 

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 197, 196, 196, 196, 193, 195, ... 
# Resampling results across tuning parameters:

#   interaction.depth  n.trees  Accuracy   Kappa      Accuracy SD  Kappa SD  
#   1                   50      0.9899849  0.9873799  0.02105679   0.02651068
#   1                  100      0.9895087  0.9867816  0.02136422   0.02689531
#   1                  150      0.9895087  0.9867816  0.02136422   0.02689531
#   2                   50      0.9894849  0.9867615  0.02255609   0.02836773
#   2                  100      0.9909156  0.9885657  0.02043453   0.02570144
#   2                  150      0.9913702  0.9891386  0.01905502   0.02396554
#   3                   50      0.9908526  0.9884663  0.01840583   0.02320828
#   3                  100      0.9908742  0.9884932  0.01946481   0.02453653
#   3                  150      0.9904394  0.9879469  0.01974251   0.02488349

# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning parameter 'n.minobsinnode'
#  was held constant at a value of 10
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 150, interaction.dept

# --------
# repeats = 1
# X                             100.0000
# avg_roll_dumbbell              12.6028
# min_roll_forearm                3.7816
# var_accel_dumbbell              1.3216
# avg_roll_forearm                0.8032
# avg_pitch_forearm               0.7546
# max_yaw_forearm-1.6             0.5548
# kurtosis_roll_arm-1.67764       0.0000
# kurtosis_picth_belt-1.721378    0.0000
# max_yaw_dumbbell#DIV/0!         0.0000
# min_yaw_belt-1.5                0.0000
# kurtosis_picth_forearm-1.2010   0.0000
# skewness_roll_belt0.005478      0.0000
# kurtosis_picth_belt-0.424625    0.0000
# skewness_roll_dumbbell0.5023    0.0000
# kurtosis_roll_arm-0.70821       0.0000
# skewness_roll_arm-0.62000       0.0000
# skewness_pitch_forearm-1.0117   0.0000
# kurtosis_picth_forearm-1.5831   0.0000
# skewness_roll_arm-0.20774       0.0000

# ------------
# fitControl2 <- trainControl(method = "repeatedcv", number = 10,repeats = 1)
# gbm_start <- Sys.time()
# gbmFit2 <- train(classe ~ ., data = training_na, method = "gbm", trControl = fitControl2,
# 	verbose = FALSE) # This last option is actually onefor gbm() that passes through
# gbm_duration <- Sys.time() - gbm_start
# gbmFit2
# varImp(gbmFit2)

# gbm variable importance

#   only 20 most important variables shown (out of 6952)

#                                Overall
# X                             100.0000
# avg_roll_dumbbell               4.0045
# min_roll_forearm                2.6427
# var_accel_dumbbell              0.8078
# avg_pitch_dumbbell              0.5058
# roll_dumbbell                   0.1359
# magnet_dumbbell_y               0.1047
# kurtosis_roll_dumbbell1.0996    0.0000
# skewness_yaw_arm1.66571         0.0000
# kurtosis_picth_dumbbell1.6385   0.0000
# kurtosis_yaw_arm-0.62736        0.0000
# skewness_roll_arm-0.28659       0.0000
# max_yaw_belt21.0                0.0000
# skewness_roll_belt1.518172      0.0000
# min_yaw_forearm1.7              0.0000
# skewness_pitch_forearm0.8258    0.0000
# skewness_roll_dumbbell0.2248    0.0000
# kurtosis_picth_arm0.47390       0.0000
# skewness_pitch_forearm-0.9148   0.0000
# kurtosis_picth_forearm-0.3883   0.0000

# ----------


# Write out the answers to the files
# Make sure the files that get written out have one character each with your
# prediction for the corresponding problem ID.
# pml_write_files(answers)