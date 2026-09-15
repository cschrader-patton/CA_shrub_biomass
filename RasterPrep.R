# FILENAMES AND PATHS TO RASTER DATA
#  NAMES MUST MATCH THE PREDICTOR VAR NAMES IN THE MODEL, AND THE SPREADSHEET!!
#  BELOW ARE THE RASTER CALLS FOR WHEN USING WORKSTATIONS..

install.packages("rgdal", dependencies = TRUE)
library(terra)
#library("rgdal")
library(olsrr)
library("fs")
library(parallel)


# Detect available cores (leave 1 free for OS)
#n_cores <- max(1, detectCores() - 1)
#print(n_cores)
# Create a parallel cluster
#cl <- makeCluster(n_cores)

#*************************RASTER FILE PATHS**********************************************************

MAT <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CA_BIO1_MAT.tif"
Total_N_De <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Total_Nitrogen_Derposition_CA.tif"
MTemp_warm <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CA_10_Mean_temp_warmest_qtr.tif"
CMD_wt <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CMD_wt.tif"
CMD_sm <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CMD_sm.tif"
Point_Y  <-  "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Point_Y.tif"

Snap <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/CA_SnapRaster_30m.tif")

Mask <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Shrub_Masks/fveg_22_MODSHRUB_Mask.tif"

#YEAR-SPECIFIC LAYERS 
# ***************************************************************************************************************
EVI <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synth_2024_EVI.tif"
Band4 <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synthetic_2024_Summer_Band4.tif"
Band5 <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synthetic_2024_Summer_Band5.tif"
TCW <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synth_2024_TCW.tif"
ppt_annual <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/WGS84_PPT_2024.tif"
tsf <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/TSF2024.tif"
cmd_annual <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/WGS84_CMD_2024.tif"
SPEI12_YR <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/SPEI12_2024.tif" 
DJF_SPEI <- "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/SPEI03_CA_30m_Winter_DJF_2000_2025.tif"


###################################BATCH RASTER PREP#################################################################################

list <- c(MAT,Total_N_De,MTemp_warm,CMD_wt,CMD_sm,EVI,Band4,Band5,TCW,ppt_annual,tsf,cmd_annual,SPEI12_YR,DJF_SPEI,Point_Y)
list <- c(Mask)


# EXPORT THE FUNCTION TO THE CLUSTER
#clusterExport(cl, varlist = c("resample_fun"))

terraOptions( tempdir = "d:/Temp", threads=20,  memfrac=0.5, verbose=TRUE)
terraOptions()
mem_info(TCW,n=1,print=TRUE)

#LOOP THRU THE ABOVE LIST AND MATCH (PROJECT) THEM TO THE SAME PARAMETERS AS SNAP
#  NOTE: LIST NEEDS TO BE JUST THE PATHFILENAME TO THE RASTER, NOT A RASTER. 
for (val in list)
{
  print("Processing...")
  print(val)
  print("...")
  filepath <- dirname(val)
  print(filepath)
  filename <- basename(val)
  print(filename)
  filenameNoext<-gsub(".tif$","", filename)
  print(filenameNoext)
  OutFile <- paste(filepath,"/FINAL/",filename, sep="")               # CREATE A NEW FILE NAME WITH THE SAME NAME IN A TEMP FOLDER
  print("Output file.. ")
  print(OutFile)
  valRaster <- rast(val) #raster(val)  # MAKE A RASTER OBJECT FROM THE CURRENT OBJECT IN THE LOOP..IF ITS NOT ALREADY A RASTER
  TorF <- compareGeom(valRaster, Snap, stopOnError = FALSE)
  #TorF <- as(Snap, 'BasicRaster') == as(valRaster, 'BasicRaster')   # CHECK FOR RASTER MATCHING valRaster
  print(" Does the raster match up to the template?")  
  print(TorF)                                                       # CHECK FOR RASTER MATCHING: IF TRUE THEY ARE MATCHED 
  #print(" Are there NaNs in the raster?")
  #print(table(!is.na(valRaster)))                                 # ARE THERE NaNs IN THE RASTER?
  #print(Naan)
  summary(valRaster)
  print("Getting rid of NaNs")
  #GET RID OF NANS
  valRaster[is.na(valRaster)] <- 0
  PRJ <- crs(Snap)
  #print(" Reprojecting...")
  Temp <- project(valRaster, PRJ, method='near', threads=TRUE)    # PROJECT THE RASTER FROM WGS TO CA ALBERS...NOT NECESSARY IF ALREADY IN ALBERS
  print(" Resampling...")
  Temp2 <- terra::resample(Temp, Snap, method='near', threads=TRUE)   # RESAMPLE THE RASTER TO MATCH THE OTHERS
  #Temp2 <- resample(Temp, Snap, method='ngb')              
  print(" writing file...")
  writeRaster(Temp2, "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/fveg_22_MODSHRUB_Mask.tif", overwrite=TRUE)                                       # WRITE THE RASTER TO A NEW FILE
}

MAT <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CA_BIO1_MAT.tif")
Total_N_De <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Total_Nitrogen_Derposition_CA.tif")
TorF <- compareGeom(Total_N_De, Snap, stopOnError = FALSE)
print(TorF)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*************************RASTER PATHS**********************************************************
#  NOTE THESE ARE RASTER OBJECTS, NOT JUST PATHS

MAT <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CA_BIO1_MAT.tif")
Total_N_De <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Total_Nitrogen_Derposition_CA.tif")
Mtemp_warm <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CA_10_Mean_temp_warmest_qtr.tif")
CMD_wt <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CMD_wt.tif")
CMD_sm <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/CMD_sm.tif")
POINT_Y  <-  rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Point_Y.tif")

Snap <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/CA_SnapRaster_30m.tif")

Mask <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Shrub_Masks/fveg_22_MODSHRUB_Mask.tif")

#YEAR-SPECIFIC LAYERS 
# ***************************************************************************************************************
EVI <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synth_2024_EVI.tif")
Band4 <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synthetic_2024_Summer_Band4.tif")
Band5 <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synthetic_2024_Summer_Band5.tif")
TCW <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/Synth_2024_TCW.tif")
ppt_annual <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/WGS84_PPT_2024.tif")
tsf <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/TSF2024.tif")
cmd_annual <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/WGS84_CMD_2024.tif")
SPEI12_YR <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/SPEI12_2024.tif") 
DJF_SPEI <- rast("G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass_predictors/R_ready/Clipped/FINAL/SPEI03_CA_30m_Winter_DJF_2000_2025.tif")

#MASK, CREATE RASTERBRICK. IN TERRA NO RASTERBRICKS, INSTEAD USE MULTIPLE SPATRASTER LAYERS
SpatRast <- c(MAT,Total_N_De,Mtemp_warm,CMD_wt,CMD_sm,EVI,Band4,Band5,TCW,ppt_annual,tsf,cmd_annual,SPEI12_YR,DJF_SPEI,POINT_Y)

#ADD THE NAMES OF THE LAYERS
names(SpatRastMasked) <- c("MAT","Total_N_De","Mtemp_warm","CMD_wt","CMD_sm","EVI","Band4","Band5","TCW","ppt_annual","tsf","cmd_annual","SPEI12_YR","DJF_SPEI","POINT_Y")


#MASK
SpatRastMasked <- mask(SpatRast, Mask, inverse=FALSE, maskvalues=0, updatevalue=NA)


#https://rdrr.io/cran/terra/man/mask.html


##############  PREDICT     ###########################
# SET THE OUTPUT NAME
OutputBiomass = "G:/OneDrive - USDA/WWETAC/All_Cal_veg_biomass/Biomass/CA_ShrubHerb_Biomass_2024_v1.0_logBM2.tif"


## RUN PREDICTION
#  TAKES ABOUT 10 HOURS
rp1 <- terra::predict(SpatRastMasked, rf_model,na.rm = TRUE, fun=predict,  cores=25, cpkgs="randomForest", filename=OutputBiomass,overwrite = TRUE)


# RUN THE PREDICTION
predict(SpatRastMasked, rf_model, filename = OutputBiomass, fun=predict, format = 'GTiff', progress = "text", datatype = 'FLT4S', ext=NULL, const=NULL, index=1, na.rm = TRUE, inf.rm = TRUE, overwrite = TRUE) 

terra::writeRaster(rp1, OutputBiomass, overwrite=TRUE)

print(rf_model)







#********************************BONEYARD******************************************************************************************************************
#myWGSRasterr <- raster("G:/OneDrive - USDA/WWETAC/SoCal_Biomass/GeoPhys_Climate_tifs/PRISM/Annual_PPT_Aug31_L4/PPT_2024_DRAFT.tif")

#GET THE WGS RASTER THAT WAS BUILT IN ARCGIS. THIS WOULD BE DIRECTLY FROM THE CLIMATENA ASC FILES WHICH ARE WGS
#  OTHER FILES LIKE THE BIANNUAL PPT 
myWGSRaster <-  raster("G:/OneDrive - USDA/WWETAC/SoCal_Biomass/GeoPhys_Climate_tifs/PRISM/BiAnnual_PPT_Aug31_L4/PPT_23_24_DRAFT.tif")

# RASTER PREP FOR THE LANDSAT NDVI
#myWGSRaster <-  raster("H:/Landsat_SoCal_L4/NDVI_SoCal_LIV/reprojected/L8_NDVI_YR_2024_July_Aug_LIV.tif")

#GET RID OF NANS
myWGSRaster[is.na(myWGSRaster)] <- 0

#MAKE A VAR WITH THE ALBERS PROJECTION
myPRJ <-"+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs"

#DEFINE PROJECTION IF NECESSARY
projection(myWGSRaster)<- ("+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs")

# REPROJECT FROM WGS TO ALBERS
myNewRaster <- projectRaster(myWGSRaster,crs=myPRJ, res=30,method='ngb')  # THIS COULD TAKE A LONG TIME..

#RESAMPLE TO MATCH THE OTHER RASTERS
myResample <- resample(myNewRaster, L4aspE, method='ngb')  # USING ASPE AS A TEMPLATE

#...NOW WRITE THE NEW RASTER TO A FILE
writeRaster(myResample ,"G:/OneDrive - USDA/WWETAC/SoCal_Biomass/GeoPhys_Climate_tifs/PRISM/BiAnnual_PPT_Aug31_L4/PPT_23_24.tif", overwrite = TRUE) 

#writeRaster(myResample ,"H:/Landsat_SoCal_L4/NDVI_SoCal_LIV/reprojected/R_ready/L8_NDVI_YR_2024_July_Aug_S275_Albers30m.tif", overwrite = TRUE) 

writeRaster(myResample, "G:/OneDrive - USDA/WWETAC/SoCal_Biomass/GeoPhys_Climate_tifs/PRISM/Annual_PPT_Aug31_L4/PPT_2024.tif", overwrite = TRUE) 

#COMPARE RASTERS TO CHECK FOR EXTENT MATCHING. IF TRUE THEY DONT MATCH
#  SHOULD BE FALSE (!=)  ## NDVI10  TSF10 AGLBM10
#  EVI, CWD, PPT AND AET DID NOT MATCH THE EXTENT OF THE OTHER RASTERS
as(L4aspE, 'BasicRaster') != as(NDVI24, 'BasicRaster')

#NDlist <-  list.files("G:/OneDrive - USDA/WWETAC/SoCal_Biomass/GeoPhys_Climate_tifs/L4/OpenLandMap_soils/","//.tif$", full.names = TRUE, recursive = FALSE ) #" 2.tif$ //.tif$
