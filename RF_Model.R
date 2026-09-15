#ALL CAL RANDOM FOREST SHRUBLAND BIOMASS MODEL
#  Code developed with CoPilot AI and some older code.

# INSTALL PACKAGES
install.packages("raster", type = "source")
install.packages("randomForest", repos='http://cran.us.r-project.org')
install.packages("gdal", repos='http://cran.us.r-project.org')
install.packages("tools", repos='http://cran.us.r-project.org')
install.packages("http://cran.us.r-project.org/bin/windows/contrib/3.5/rlang_0.4.11.zip", repos='http://cran.us.r-project.org') 
install.packages("ggplot2", repos='http://cran.us.r-project.org')
install.packages('lifecycle',repos='http://cran.us.r-project.org')
install.packages("caret", repos='http://cran.us.r-project.org')
install.packages("magrittr", repos='http://cran.us.r-project.org', dependencies = TRUE)
install.packages("Rcpp", repos='http://cran.us.r-project.org', dependencies = TRUE)
install.packages("olsrr",  repos='http://cran.us.r-project.org', dependencies = TRUE)
install.packages("readxl",  repos='http://cran.us.r-project.org', dependencies = TRUE)
install.packages("tidyverse")
install.packages(c("randomForest", "ranger", "caret", "e1071"))
install.packages("doParallel")
install.packages("outForest")




library(randomForest)
library(ranger)
library(caret)   # for cross-validation and tuning
library("readxl")
library(olsrr)
library("magrittr")
library("fs")
library(doParallel)
library(outForest)



set.seed(1114)

# Create a parallel cluster using all but one core
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# SET THE WORKING DIRECTORY
setwd("C:/Users/cschrader/Box/CalFire_April 2025/Biomass_models_R")

#SEE THE Data_Explore.R FILE FOR DATA EXTRACTION, SELECTION.

sheet2 <- sheetOut #sheet2 is set to the outlier corrected data

# FIND THE BEST MTRY VALUE USING tuneRF ON THE TRAINING DATASET
# tuneRF(x, y, mtryStart, ntreeTry=50, stepFactor=2, improve=0.05,
#        trace=TRUE, plot=TRUE, doBest=FALSE, ...)

set.seed(1114)

tRF <- tuneRF(sheet2[,-1], sheet2$logN_BM,
              stepFactor = 0.5,
              plot = TRUE,  #OOB error as a function of mtry
              ntreeTry = 500,
              mtryStart=1, 
              improve = 0.01, 
              trace = TRUE,  #real-time progress
              allowParallel = TRUE)

optimal_mtry <- tRF[tRF[,2]== min(tRF[,2])]
print(optimal_mtry)


#SPLIT DATA IN TO TRAIN AND TEST DATASETS. BUT DONT NEED TO 
# DO THIS WITH RF OOB AND K-FOLD CV VALIDATION
trainIndex <- createDataPartition(sheet2$logN_BM, p = 0.8, list = FALSE)
train_data <- sheet2[trainIndex, ]
test_data <- sheet2[-trainIndex, ]


# #CHECK FOR OUTLIERS
out <- outForest(sheet2, .-POINT_Y~., num.trees = 3000,  max_prop_outliers = .05, max.depth = 6, mtry = 3)# mtry = 2,
outliers(out)
df <- head(Data(out))
plot(out)
print(out)
summary(out)
sheetOut <- Data(out)
outliers <-outliers(out)

set.seed(26608)
#FIT RANDOM FOREST MODEL
rf_model <- randomForest(
  logN_BM ~ .,, 
  data = sheet2, 
  ntree = 2000,        #RULE OF THUMB, 10X NUMBER OF FEATURES
  mtry = 3,          #floor(sqrt(ncol(train_data) - 1)), OR P/3. 3 sELECTED ABOVE WITH tuneRF
  importance = TRUE,
  allowParallel = TRUE)

varImpPlot(rf_model, sort=TRUE, main="Feature Importance Plot", type=1)  # quick visualization
# Predict on test set
#predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model
#RMSE <- sqrt(mean((predictions - test_data$logN_BM)^2))
#print(paste0("Root Mean Squared Error (RMSE): ", RMSE))


print(rf_model)
# Out-of-bag (OOB) MSE/R^2 shown in summary

# FEATURE IMPORTANCE
varImpPlot(rf_model, sort=TRUE, main="Feature Importance Plot", type=1)  # quick visualization
#import
out.importance <- round(importance(rf_model), 2)
#print(out.importance)
#var_import <- importance(rf_model, type=1)

# PLOT
library(ggplot2)
#imp_data <- as.data.frame(var_import)
#imp_data$Variable <- rownames(imp_data)

# ggplot(imp_data, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   coord_flip() +
#   labs(title = "Variable Importance by Mean Decrease Accuracy", x = "Variables", y = "% Increase in MSE")

#K-FOLD CROSS VALIDATION
train.control <- trainControl(method = "cv", number = 10)

# TRAIN THE K-FOLD MODEL  
model <- train(logN_BM ~ ., 
               data = sheet2, method = "rf",
               trControl = train.control,
               allowParallel = TRUE)
print(model)




# VALIDATION REF:  http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#loading-required-r-packages

#PREDICT THE TEST DATA AND COMPUTE THE R2, RMSE, AND MAE -- VALIDATION APPROACH.
predictions <- rf_model %>% predict(test_data)
data.frame( R2 = R2(predictions, test_data$logN_BM),
            RMSE = RMSE(predictions, test_data$logN_BM), # ROOT MEAN SQ ERROR
            MAE = MAE(predictions, test_data$logN_BM))  # MEAN ABSOLUTE ERROR
cat("R-SQ - % variance explained:", cor(predictions, test_data$logN_BM)^2)
write.csv(test_data, file = "test + data.csv")
write.csv(predictions, file = "predict_test.csv")



#REGRESSION, PLOTS
plot(predictions, test_data$logN_BM)
regression <-lm(predictions~ test_data$logN_BM, data=test_data)



regression2 <-lm(predictions~ test_data$logN_BM, data=test_data)
summary(regression)
abline(regression)
plot(regression)   #RESIDUAL PLOTS, Q-Q, ETC  -- OUTLIER DETECT BUT ONLY FOR LINEAR MODEL

# SAVE THE MODEL TO FILE
save(rf_model, file = "AllCal_Shrub_RF.RData")

################ STOP CLUSTER #################################
# Stop the cluster after computation
stopCluster(cl)
registerDoSEQ()  # Return to sequential execution

