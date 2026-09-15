# DATA EXPLORATION AND FEATURE REDUCTION
# Basic Exploration – str() and summary() give an overview of data types and distributions.
# Near-Zero Variance Removal – Eliminates features with almost no variability.
# Missing Value Check – Identifies columns with missing data.
# Correlation Analysis – Uses corrplot to visualize relationships between numeric features.
# High Correlation Removal – Drops features above a correlation threshold (default 0.85).
# Recursive Feature Elimination (RFE) – Uses cross-validation to select the most predictive features.

# INSTALL PACKAGES
install.packages("raster", type = "source")
install.packages("randomForest", repos='http://cran.us.r-project.org')
install.packages("rgdal", repos='http://cran.us.r-project.org')
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
install.packages("VSURF")
install.packages("RTools")


library(randomForest)
library(ranger)
library(caret)   # for cross-validation and tuning
library("readxl")
library(olsrr)
library("magrittr")
library("fs")
library("ggplot2")
library(ggcorrplot)
library(dplyr)
library(doParallel)
library(VSURF)



set.seed(42)

# Create a parallel cluster using all but one core
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# SET THE WORKING DIRECTORY
setwd("C:/Users/cschrader/Box/CalFire_April 2025/Biomass_models_R")


# GET THE SHEET WITH THE PLOTS C:\Users\Charlie\Box\CalFire_April 2025
#read_excel(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)
sheet <- read_excel("C:/Users/cschrader/Box/CalFire_April 2025/Plot Data/Plots/All_plots/Updated/All_plots_11Sept26_PREDICTORS.xlsx", sheet = "PLOTS", na = "#N/A", col_names=TRUE, col_types=NULL, skip=0)

# PREDICTOR NAMES IN THE SHEET MUST MATCH THE RASTER NAMES (SEE RASTERPATHS R FILE)

# NOW MAKE A NEW DATA FRAME WITH SELECT COLUMNS 
sheetALL <-subset(sheet, select = c('logN_BM',  
                                    'DJF_SPEI', 'MAM_SPEI', 'SPEI12_YR', 
                                    'POINT_Y', 'EVI', 'EVI2', 'GCC',
                                    'GVI', 'MSAVI', 'MSI', 'MSI2', 'NBR', 'NBR2', 'NDVI', 'NDWI', 
                                    'SAVI', 'SVI', 'TCA', 'TCB', 'TCG', 'TCW', 'Band1', 'Band2', 
                                    'Band3', 'Band4', 'Band5', 'Band6',  'ppt_annual',
                                    'ppt_bienni', 'cmd_annual', 'cmd_bienni',  
                                    'tsf', 'Mtemp_warm', 'MAT', 
                                    'Mtemp_cold', 'Mtemp_wet', 'Mtemp_dry', 
                                    'DEM', 'Slope', 
                                    'SWness_TRA', 'CMD', 'CMD_at', 'CMD_sm', 'CMD_sp', 'CMD_wt', 
                                    'dist2coast', 'ruggedness', 'Topo_wet', 
                                    'Topo_Pos1', 'Topo_Pos2', 'Total_N_De' )) #'ahm',

                                      # 'fveg_class', 'POINT_x','RCM_sh_ht','RCM_sh_cv','Outlier_flag'

# NOW MAKE A DATAFRAME WITH THE FEATURES COMMON TO ALL VSURF RUNS
sheetVSURF3 <-subset(sheet, select = c('logN_BM',  
                                      'POINT_Y','GCC','Band2','Band5','TCW','CMD_wt','Band4',
                                      'SPEI12_YR', 'CMD_sm','cmd_annual', 'MAT',
                                      'Mtemp_warm', 'DJF_SPEI', 'Band6','Topo_Pos2','ppt_annual',
                                      'ahm','Total_N_De','EVI','tsf'))
                                      #'Total_N_De','EVI',tsf))  TOP THREE FEATURES FROM HC RFE NOT IN VSURF

# NOW MAKE A DATAFRAME WITH THE FEATURES COMMON HIGH COLLINEARTY FILTER AND RFE
sheetHCRFE <-subset(sheet, select = c('logN_BM', 
                                      'DJF_SPEI','MAM_SPEI', 'SPEI12_YR','POINT_Y', 
                                      'EVI','TCW','Band4','ppt_annual','ahm', 
                                      'tsf', 'Mtemp_warm', 'MAT', 'DEM','Slope', 
                                      'CMD_sm','dist2coast', 'ruggedness', 'Topo_wet','Total_N_De'))

# REMOVE FROM sheetVSURF3 ABOVE THOSE IDENTIFIED AS HC AND THE BOTTOM 3 FROM RFE
sheetVSURF4 <-subset(sheet, select = c('logN_BM',  
                                       'POINT_Y','TCW','Band4',
                                       'SPEI12_YR', 'CMD_sm', 'MAT',
                                       'Mtemp_warm', 'DJF_SPEI','Topo_Pos2','ppt_annual',
                                       'ahm','Total_N_De','EVI','tsf'))
                                        #'cmd_annual','Band5','Band2','GCC','CMD_wt', 'Band6'
                                        #'
# ADD CMD ANNUAL AND BAND 5 BACK INTO  VSURF4, REMOVE TOPO BECAUSE NOT IMPORTANT 
sheetVSURF5 <-subset(sheet, select = c('logN_BM',  
                                       'POINT_Y','TCW','Band4',
                                       'SPEI12_YR', 'CMD_sm', 'MAT',
                                       'Mtemp_warm', 'DJF_SPEI','ppt_annual', 'CMD_wt',
                                       'Total_N_De','EVI','tsf','cmd_annual','Band5'))
                                        #,'Band2','GCC','CMD_wt', 'Band6','Topo_Pos2','ahm'
# REMOVED AHM BECAUSE RASTERS ARE A MESS...ADDED CMD_WT B/C HIGHEST CORR WITH AHM. 
# MIGHT BE THAT THE AHM DATA GOT MESSED UP IN THE DOWNSCALE WITH CLIMATENA

                                    
# FILTER OR NOT
sheet1A<-sheetALL[sheetALL$Outlier_flag == 0,]    # REMOVE THE OUTLIERS IDENTIFIED IN FIRST RF RUN, THEY ARE FLAGGED IN EXCEL INPUT
sheet2 <- sheet1A %>% select(-Outlier_flag) # REMOVE THE OUTLIERS FIELD
#sheet2 <- sheet1A[!is.na(sheet1A$RCM_sh_cv), ] # REMOVE ALL SHRUB COVER THAT IS NA

sheet2 <- sheetVSURF5 #sheetOut# sheetVSURF5 # NO FILTERsheetVSURF5

#BASIC STRUCTURE AND SUMMARY
str(sheet2)
summary(sheet2)

#CHECK FOR ZERO VARIANCE FEATURES
nzv <- nearZeroVar(sheet2, saveMetrics = TRUE)
print(nzv)

#  REMOVED THESE FEATURES BECAUSE NEAR ZERO VARIANCE
#ASP                             32.288889      3.562945   FALSE  TRUE
#ASP_Eastness                    32.288889      3.562945   FALSE  TRUE
#ASP_Northness                   32.288889      3.562945   FALSE  TRUE
# ASP","ASP_Eastness","ASP_Northness",

ToCSV_fun <- function(id,df) {
  filename <- paste0(id, format(Sys.time(), "%y%m%d%H%M%S"), ".csv")
  write.csv(df, filename, row.names=FALSE)
}
# CREATE A HISTOGRAM OF THE DEPENDENT VAR (log BIOMASS)
# Bar plot of the binned y_reg values
ggplot(sheet2, aes(x=logN_BM)) + 
  geom_bar(fill='skyblue', color='black') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Bar Plot of log biomass') +
  xlab('log biomasss') +
  ylab('Frequency')

# Basic histogram
ggplot(sheet2, aes(x=logN_BM)) + geom_histogram()
# Change the width of bins
ggplot(sheet2, aes(x=logN_BM)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(sheet2, aes(x=logN_BM)) + 
  ggtitle('Bar Plot of log biomass') +
  xlab('log biomasss') +
  ylab('Frequency')+
  geom_histogram(fill='skyblue', color='black')


# CORRELATION MATRIX
corr_matrix <- cor(sheet2, use = "pairwise.complete.obs", method = "pearson") #USE ONLY VALUES WHERE THERE ARE NO NAs

#     WRITE THE MATRIX TO A CSV IN THE WORKSPACE
ToCSV_fun("corr_matrix_",corr_matrix)
filename <- paste0("corr_matrix_", format(Sys.time(), "%y%m%d%H%M%S"), ".csv")
write.csv(corr_matrix, filename, row.names=FALSE)

ggcorrplot(
  corr_matrix,
  method = "circle",       # Shape of the correlation representation ("circle" or "square")
  type = "lower",          # Show only lower triangle
  lab = TRUE,              # Display correlation coefficients
  lab_size = 3,            # Font size for coefficients
  colors = c("red", "white", "blue"), # Color gradient
  title = "Correlation Matrix with Coefficients",
  ggtheme = ggplot2::theme_minimal()
)

# REMOVE HIGHLY CORRELATED FEATURES
high_corr <- findCorrelation(corr_matrix, cutoff = 0.85) # Adjust cutoff as needed
data_reduced_high_corr <- sheet2[, -high_corr]
#     OUTPUT
cat("Removed features due to high correlation:\n")
print(names(sheet2)[high_corr])
ToCSV_fun("Removed_corr_features_",high_corr)
cat("Retained features:\n")
print(names(data_reduced_high_corr))
ToCSV_fun("Retained_corr_features_",data_reduced_high_corr)

# CORR WITH LOG_BM
corr_logBM<-cor(sheet2$logN_BM, sheet2[2:53])
print(corr_logBM)
#out <- (names(sheet2)[high_corr])
ToCSV_fun("corr_logN_BM_",corr_logBM)

# FeatureTerminatorR 
# https://cran.r-project.org/web/packages/FeatureTerminatoR/FeatureTerminatoR.pdf
# mutlicol_terminator(df, x_cols, y_cols, alter_df = TRUE, cor_sig = 0.9)
# COULDNT GET THIS TO WORK...


# FSELECTOR
# COULD NOT GET THIS TO INSTALL
#weights <- information.gain(log_BM ~ ., sheet2)
#print(weights)

#****************PCA*****************************************
#PCA - 13 COMPONENTS WHICH CAPTURES OVER 90% OF THE VARIATION
#  NOT USING PCA TO REDUCE DIMENSIONALITY, BECAUSE WOULD HAVE TO MAKE
#  RASTERS OF THE COMPONENTS TO MAKE A MAP. 
data_scaled <- scale(sheet2)
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE, rank.= 13)
summary(pca_result)
scores <- pca_result$x

#Correlation matrix of the principal components
cor_matrix <- cor(scores)

print(cor_matrix)

# PCA SCREE PLOT
fviz_eig <- function(pca) {
  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  df <- data.frame(PC = paste0("PC", 1:length(var_explained)),
                   Variance = var_explained)
  ggplot(df, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(group = 1), color = "red") +
    geom_point(color = "red") +
    ylab("Proportion of Variance Explained") +
    theme_minimal()
}
fviz_eig(pca_result)

# 5. Identify variable importance for first few PCs
loadings <- pca_result$rotation  # loadings matrix
abs_loadings <- abs(loadings)

# Select top variables contributing to first 2 PCs  
# THIS THROWS A CLOSURE ERROR
# top_vars <- function(loadings, pc = 1, top_n = 5) {
#   sort(abs(loadings[, pc]), decreasing = TRUE)[1:top_n]
# }
# SELECT THE TOP 10 VARS FROM THE FIRST 2 PCS 
top_vars1<-sort(abs(loadings[, 1]), decreasing = TRUE)[1:15]
top_vars2<-sort(abs(loadings[, 2]), decreasing = TRUE)[1:15]
top_vars3<-sort(abs(loadings[, 3]), decreasing = TRUE)[1:15]

cat("Top 10 variables for PC1:\n")
print(top_vars1) #(loadings, pc = 1, top_n = 5))

cat("Top 10 variables for PC2:\n")
print(top_vars2)#(loadings, pc = 2, top_n = 5))

cat("Top 10 variables for PC3:\n")
print(top_vars3)#(loadings, pc = 2, top_n = 5))

# 6. Keep only top contributing variables (example: top 5 from PC1 & PC2)
selected_vars <- unique(c(names(top_vars1),
                          names(top_vars2),
                          names(top_vars3)))
#data_selected <- data[, selected_vars]

cat("\nSelected variables:\n")
print(selected_vars)

#***************END PCA******************************************

# Recursive Feature Elimination (RFE) ALSO SEE RFE CODE IN RF_Model.R
control <- rfeControl(functions = rfFuncs,  # Use Random Forest functions
                      method = "cv",       # Cross-validation "repeatedcv
                      repeats = 5,
                      number = 10, # number of folds
                      allowParallel = TRUE)   # Enable parallelism

rfeResults <- rfe(sheet2[, -1],  # Features only, removes the first column which is biomass
                  sheet2$logN_BM,              # Target variable
                  sizes = c(1:30),              # Number of features to try
                  rfeControl = control)



print(rfeResults)

# Final reduced dataset
final_data <- sheet2[, predictors(rfeResults)]
predictors(rfeResults)
out <- predictors(rfeResults)
ToCSV_fun("rfe_predictors_", out)
#print(final_data)
#77777777777777777777END RFE((((((((((((((((((((((()))))))))))))))))))))))

test1 <-sheet2[,2:53]
test2 <-sheet2[,1]

#@@@@@@@@@@@@@@@@@@@@@@@VSURF@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Run VSURF feature selection
# ntree = number of trees in the random forest (increase for better stability)
# parallel = TRUE can be used for faster computation if you have multiple cores

vsurf_result <- VSURF(sheet2$logN_BM ~., data = sheet2[,2:53],  # DEFAULT VALUES ARE ON THE RIGHT
                          
                      parallel = TRUE,                   #FAlSE
                      mtry = 17,                         #p/3 for regression, SQRT(p) for classification
                      ntree = 2000,                      # 2000
                      ntree_thres = 500,                # 500
                      ntree_interp = 100,               # 100
                      ntree_pred = 100,                 # 100          
                      nfor.thres = 20,                   # 20
                      nmin = 1,                          # 1
                      nfor.interp = 10,                  # 10
                      nsd = 1,                           # 1
                      nfor.pred = 10,                    # 10
                      nmj = 1,                           # 1
                      ncores = detectCores() - 1,         # detectCores() - 1
                      clusterType = "PSOCK",             # PSOCK
                      RFimplem = "randomForest",        # "randomForest"
                      na.action = na.omit)              # OMIT ANY ROWS WITH AN NA...ANYWHERE.

# View results
print(vsurf_result)
print(vsurf_result$mean.perf)

# Selected variables after each step
cat("Variables after thresholding step:\n")
print(vsurf_result$varselect.thres)
ToCSV_fun("vsurf_varselect_thres_DEFAULT_", colnames(sheet2)[vsurf_result$varselect.thres])

cat("\nVariables after interpretation step:\n")
print(vsurf_result$varselect.interp)
ToCSV_fun("vsurf_varselect_interp_DEFAULT_", colnames(sheet2)[vsurf_result$varselect.interp])

cat("\nVariables after prediction step:\n")
print(vsurf_result$varselect.pred)
ToCSV_fun("vsurf_varselect_pred_DEFAULT_", colnames(sheet2)[vsurf_result$varselect.pred])

# Plot the VSURF process
plot(vsurf_result,var.names = TRUE)
plot(vsurf_result$varselect.pred)

# VAR IMPORTANCE
var_importance <- vsurf_result$imp.mean.dec
importance_df <- data.frame(
  Variable = colnames((sheet2)[vsurf_result$varselect.thres]),
  Importance = var_importance
)
ToCSV_fun("var_importDF_DEFAULT", importance_df)
print(vsurf_result$imp.mean.dec)
print(names(vsurf_result$imp.mean.dec))
print(vsurf_result$varselect.pred)
# Summary
summary(vsurf_result)

################ STOP CLUSTER #################################
# Stop the cluster after computation
stopCluster(cl)
registerDoSEQ()  # Return to sequential execution