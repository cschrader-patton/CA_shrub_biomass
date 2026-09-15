#SUMMARIZE BIOMASS RASTER BY ECOREGION ZONE

library(terra)
# Load raster and vector
r <- rast("G:\\OneDrive - USDA\\WWETAC\\All_Cal_veg_biomass\\Biomass\\CA_ShrubHerb_Biomass_WWETAC_UCD_2024_v1.0_LZW.tif")       
zones <- vect("C:\\Users\\cschrader\\Box\\CalFire_April 2025\\Spatial Data\\EcoRegions\\FCAT_Regions2023_1.shp") 

# COMPUTE MEAN BIOMASS PER ZONE
zonal_stats <- zonal(r, zones, fun = "mean", na.rm = TRUE)
print(zonal_stats)