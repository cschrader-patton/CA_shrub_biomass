
# AI CODE ON MASKING RASTERS BEFORE PREDICTION
#
#https://rspatial.github.io/raster/reference/predict.html
#
#
# Load required package
library(terra)

# --- Step 1: Load predictor rasters ---
# Replace with your actual raster file paths
predictors <- rast(c("var1.tif", "var2.tif", "var3.tif"))

# --- Step 2: Load mask layer ---
# This can be a shapefile or a raster mask
mask_shape <- vect("mask_boundary.shp")  # Vector mask
# OR: mask_raster <- rast("mask_raster.tif")  # Raster mask

# --- Step 3: Crop and mask predictors ---
# Crop to bounding box of mask to reduce processing
predictors_crop <- crop(predictors, mask_shape)

# Mask out areas outside the shape
predictors_masked <- mask(predictors_crop, mask_shape)

# --- Step 4: Load your trained model ---
# Example: a random forest model
load("trained_model.rda")  # This should load an object like 'rf_model'

# --- Step 5: Predict only on masked area ---
predicted <- predict(predictors_masked, rf_model, na.rm = TRUE)

# --- Step 6: Save the result ---
writeRaster(predicted, "prediction_masked.tif", overwrite = TRUE)

cat("Prediction completed and saved as prediction_masked.tif\n")