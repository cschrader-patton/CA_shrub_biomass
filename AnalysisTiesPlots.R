library(tidyverse)
library(terra)
library(openxlsx)

CRSproject<-"epsg:3310"

########### BLM #############
BLM<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/BLM_AIM/California/LMFCal.shp")
BLM<-project(BLM, CRSproject)
BLMsp<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/BLM_AIM/California/LMFSpecies_CA.csv")
BLMspDict<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/BLM_AIM/California/SpeciesTable.csv")

# VegCoverField<-"TotalFolia"
TreeCoverField<-c("FH_NoxTree", "FH_NonNoxT")
ShrubCoverField<-c("FH_NonNoxS", "FH_NoxShru", "FH_NoxSubS", "FH_Sagebru", "FH_NonNo_3", "FH_NonNo_4")

DF<-data.frame(BLM)
DF_shrub<-DF %>% 
  mutate(TreeCov=rowSums(across(all_of(TreeCoverField))), 
         ShrubCov=rowSums(across(all_of(ShrubCoverField))), 
         Project='BLM', 
         SuProject="LFM",
         DateVisiteD=as.Date(DateVisite),
         Year=year(DateVisiteD)) %>%
  filter(
    (TotalFolia >20 & TreeCov <10 & ShrubCov >10) |
      (TotalFolia <20 & TreeCov <10 & ShrubCov >2)) %>%
  select(Project='Project', "PrimaryKey", Date="DateVisiteD", Year="Year", VegCov="TotalFolia", TreeCov="TreeCov", 
         ShrCov="ShrubCov", ShrHeight="Hgt_Shrub_", SuProject)%>%
  filter(ShrHeight>0) %>% 
  filter (ShrCov<=100)

MaxCoverSp <- BLMsp %>% left_join(BLMspDict %>% filter (SpeciesState=="CA") %>% select (SpeciesCode, ScientificName), by = c("Species" = "SpeciesCode")) %>% 
  group_by(PrimaryKey) %>%
  mutate(
    has_X = "Shrub" %in% GrowthHa_1  # I want to prioritize the maximum in the shurb group
  ) %>%
  filter(ifelse(has_X, GrowthHa_1 == "Shrub", TRUE)) %>%
  slice_max(AH_Species, n = 1, with_ties = T) %>%
  ungroup()

result <- MaxCoverSp %>%
  filter(PrimaryKey %in% DF_shrub$PrimaryKey)

ties_count <- result %>%
  group_by(PrimaryKey) %>%
  filter(n() > 1) %>%
  distinct(PrimaryKey) %>%
  nrow()

ties_count

# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (PrimaryKey, Species, ScientificName, AH_Species), by = "PrimaryKey") %>% rename(DominantSp=Species, SciName=ScientificName, SpCover=AH_Species)
# 
# BLM_shrub<-BLM[BLM$PrimaryKey %in% DF_shrub$PrimaryKey,]
# BLM_shrub_points <- as.data.frame(BLM_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, BLM_shrub_points[,c('PrimaryKey','x', 'y')], by = "PrimaryKey")
# BLM_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
# writeVector(BLM_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/BLM_shrub.shp', overwrite=TRUE)
# 
########### Landfire #############
rm(list = ls())
CRSproject<-"epsg:3310"
Landfire<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/LandfireAD/PointsXYCal.shp")
Landfire<-project(Landfire, CRSproject)
DF<-data.frame(Landfire)

Stands<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/LandfireAD/LFstands.csv")
Visits<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/LandfireAD/dtVisits.csv")
LFSpecies<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/LandfireAD/LFSpecies.csv")

DFAll<-inner_join(DF, Stands, by="EventID")
DFAll2<-inner_join(DFAll, Visits, by="EventID")

VegCoverField<-c("SourceTreeCov", "SourceShrubCov", "SourceHerbCov")
# TreeCoverField<-c("SourceTreeCov")
# ShrubCoverField<-c("SourceShrubCov")
# ShrubHeight<-c("SourceShrubHgt")

DF_shrub<-DFAll2 %>% 
  mutate(VegCov=rowSums(across(all_of(VegCoverField))), 
         Project='Landfire', 
         ShrHeight=ifelse(is.na(SourceShrubHgt),LFShrubHgt, SourceShrubHgt)*100,
         DateVisiteD=make_date(YYYY, MM, DD)) %>%
  filter(LocMeth=="G" & Protocol != "VegBank") %>%
  filter(
    (VegCov >20 & SourceTreeCov <10 & SourceShrubCov >10) |
      (VegCov <20 & SourceTreeCov <10 & SourceShrubCov >2)) %>%
  select(Project='Project', PrimaryKey="EventID", Date="DateVisiteD", Year="YYYY", VegCov="VegCov", TreeCov="SourceTreeCov", 
         ShrCov="SourceShrubCov", ShrHeight=ShrHeight, SuProject="Protocol") %>%
  filter(ShrHeight>0)

MaxCoverSp <- LFSpecies %>% 
  group_by(EventID) %>%
  mutate(
    has_X = "S" %in% Lifeform  # I want to prioritize the maximum in the shurb group
  ) %>%
  filter(ifelse(has_X, Lifeform == "S", TRUE)) %>%
  slice_max(LFAbsCov, n = 1, with_ties = T
            ) %>%
  ungroup()

ties_count <- MaxCoverSp %>%
  group_by(EventID) %>%
  filter(n() > 1) %>%
  distinct(EventID) %>%
  nrow()

ties_count

# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (EventID, Item, SciName, LFAbsCov), by = c("PrimaryKey"="EventID")) %>% rename(DominantSp=Item, SciName=SciName, SpCover=LFAbsCov)
# 
# Landfire_shrub<-Landfire[Landfire$EventID %in% DF_shrub$PrimaryKey,]
# Landfire_shrub_points <- as.data.frame(Landfire_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, Landfire_shrub_points[,c('EventID','x', 'y')], by = c("PrimaryKey" = "EventID"))
# Landfire_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
# writeVector(Landfire_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Landfire_shrub.shp', overwrite=TRUE)

########### VegCamp #############
rm(list = ls())
CRSproject<-"epsg:3310"
VegCamp<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/VegCamp/ds1020/ds1020.gdb")
VegCamp<-project(VegCamp, CRSproject)
DF<-data.frame(VegCamp)

VCSpecies<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/VegCamp/SurveyPlants.csv")

DF_shrub<-DF %>% 
  mutate(
    Shrub_ht = recode(Shrub_ht_txt,
                      "<.5m" = 25,
                      ".5-1m" = 75,
                      ".5-2m" = 75,
                      "1-2m" = 150,
                      "2-5m" = 350,
                      "5-10m" = 750,
                      "5-15m" = 750,
                      "10-15m" = 1250,
                      "15-20m" = 1750,
                      "15-35m" = 2750,
                      "20-35m" = 2750, 
                      "N/A" = NA_real_,
                      "Not recorded"= NA_real_,
                      .default = as.numeric(Shrub_ht_txt)),
    Project='VegCamp', 
    DateVisiteD=as.Date(SurveyDate),
    Year=year(DateVisiteD)) %>%
  filter(Shrub_ht>0) %>%
  filter(SurveyType %in% c("Rapid Assessment", "Releve")) %>%
  filter(
    (TotalVegCov >20 & TreeCov <10 & ShrubCov >10) |
      (TotalVegCov <20 & TreeCov <10 & ShrubCov >2)) %>%
  select(Project='Project', PrimaryKey="SurveyID", Date="DateVisiteD", Year="Year", VegCov="TotalVegCov", TreeCov="TreeCov", 
         ShrCov="ShrubCov", ShrHeight=Shrub_ht, SuProject="SurveyType")

MaxCoverSp <- VCSpecies %>% 
  group_by(SurveyID) %>%
  mutate(
    has_X = "Shrub" %in% Stratum  # I want to prioritize the maximum in the shurb group
  ) %>%
  filter(ifelse(has_X, Stratum == "Shrub", TRUE)) %>%
  slice_max(Species_co, n = 1, with_ties = T) %>%
  ungroup()

result <- MaxCoverSp %>%
  filter(SurveyID %in% DF_shrub$PrimaryKey)

ties_count <- result %>%
  group_by(SurveyID) %>%
  filter(n() > 1) %>%
  distinct(SurveyID) %>%
  nrow()

ties_count

# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (SurveyID, CodeSpecie, Species_na, Species_co), by = c("PrimaryKey"="SurveyID")) %>% rename(DominantSp=CodeSpecie, SciName=Species_na, SpCover=Species_co)
# 
# VegCamp_shrub<-VegCamp[VegCamp$SurveyID %in% DF_shrub$PrimaryKey,]
# VegCamp_shrub_points <- as.data.frame(VegCamp_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, VegCamp_shrub_points[,c('SurveyID','x', 'y')], by = c("PrimaryKey" = "SurveyID"))
# VegCamp_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
# writeVector(VegCamp_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/VegCamp_shrub.shp', overwrite=TRUE)
# 


########### Slaton #############
rm(list = ls())
CRSproject<-"epsg:3310"
Slaton<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Slaton/ecoplots_Slaton_20241101_public_tbl/plot_locations_Albers_public20241101.shp")
Slaton<-project(Slaton, CRSproject)
DF<-data.frame(Slaton)
DF_points <- as.data.frame(Slaton, geom = 'XY')
DF_points_unique<-DF_points %>% distinct(plot, .keep_all = TRUE)

Plots<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Slaton/1_site_descr.csv")
PlotsXY<-inner_join(Plots, DF_points_unique[,c('plot','x', 'y')], by = c("PlotOrig" = "plot"))
SiteData<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Slaton/2_site_data.csv")
PlotsXY<-inner_join(PlotsXY, SiteData, by = c("Plot"))

Species<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Slaton/4_shrubs.csv")
SpeciesDict<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Slaton/10_sp_names.csv")
Species_Plot<-Species %>% 
  group_by(Plot) %>%
  summarise(w_mean = weighted.mean(Avg_Ht, Cover, na.rm = TRUE))

DFAll<-inner_join(PlotsXY, Species_Plot, by="Plot")

VegCoverField<-c("Sat_cov_con", "Sat_cov_hdwd", "Sat_cov_shrub", "Sat_cov_forb", "Sat_cov_gram")
TreeCoverField<-c("Sat_cov_con", "Sat_cov_hdwd")

DF_shrub<-DFAll %>% 
  mutate(VegCov=rowSums(across(all_of(VegCoverField))), 
         TreeCov=rowSums(across(all_of(TreeCoverField))), 
         Project='Slaton', 
         DateVisiteD=as.Date(Date, "%m/%d/%Y"),
         Year=year(DateVisiteD),
         SuProject="Slaton")%>%
  filter(
    (VegCov >20 & TreeCov <10 & Sat_cov_shrub >10) |
      (VegCov <20 & TreeCov <10 & Sat_cov_shrub >2)) %>%
  select(Project='Project', PrimaryKey="Plot", Date="DateVisiteD", Year="Year", VegCov="VegCov", TreeCov="TreeCov", 
         ShrCov="Sat_cov_shrub", ShrHeight="w_mean", SuProject="SuProject") %>%
  filter(!is.na(ShrHeight))

MaxCoverSp <- Species %>% left_join(SpeciesDict %>% distinct(code, .keep_all = TRUE) %>% select (code, species, growth.form), by = c("Species" = "code")) %>% 
  group_by(Plot) %>%
  mutate(
    has_X = "shrub" %in% growth.form  # I want to prioritize the maximum in the shurb group
  ) %>%
  filter(ifelse(has_X, growth.form == "shrub", TRUE)) %>%
  slice_max(Cover, n = 1, with_ties = T) %>%
  ungroup()


result <- MaxCoverSp %>%
  filter(Plot %in% DF_shrub$PrimaryKey)

ties_count <- result %>%
  group_by(Plot) %>%
  filter(n() > 1) %>%
  distinct(Plot) %>%
  nrow()

ties_count

# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (Plot, Species, species, Cover), by = c("PrimaryKey"="Plot")) %>% rename(DominantSp=Species, SciName=species, SpCover=Cover)
# 
# pointsSlaton <- vect(PlotsXY, geom = c("x", "y"), crs = CRSproject)
# Slaton_shrub<-pointsSlaton[pointsSlaton$Plot %in% DF_shrub$PrimaryKey,]
# Slaton_shrub_points <- as.data.frame(Slaton_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, Slaton_shrub_points[,c('Plot','x', 'y')], by = c("PrimaryKey" = "Plot"))
# Slaton_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
# writeVector(Slaton_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Slaton_shrub.shp', overwrite=TRUE)
# 



########### SAMO #############
rm(list = ls())
CRSproject<-"epsg:3310"
SAMO<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/SantaMonica/samo_geodata/samogeodata.gdb", layer='SAMO_Vegetation_Plots')
SAMO<-project(SAMO, CRSproject)
DF<-data.frame(SAMO)
## No height cover for this dataset

########### SoCal #############
rm(list = ls())
CRSproject<-"epsg:3310"
SoCal<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/SoCal_20-21 field campaign/SoCalPlots.shp")
SoCal<-terra::project(SoCal, CRSproject)
DF<-data.frame(SoCal)

PlotDate<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/SoCal_20-21 field campaign/Biomass Data Final/plot data.csv")
DF<-inner_join(DF, PlotDate[,c("Plot.ID", "Date")],  by = c("Plot_ID" = "Plot.ID"))
SpeciesDict<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/SoCal_20-21 field campaign/Biomass Data Final/Species_List.csv")

dir<-"C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/SoCal_20-21 field campaign/Biomass Data Final/raw shrub data.xlsx"
plots<-getSheetNames(dir)
plot<-"5-mc-2013-2"
AllPlots<-c()
for (plot in plots){
  adults<-read.xlsx(dir,plot,sep.names=" ",cols=(1:7))
  small<- read.xlsx(dir,plot,sep.names=" ",cols=(8:24))
  adults.mod<-adults %>% rowwise() %>%
    mutate(`Canopy (Avg.)` = mean(c(`Canopy 1 (m)`,`Canopy 2 (m)`), trim=0,na.rm=T),
           `Area (m2)` = (`Canopy (Avg.)`/2)^2*pi,
           `Area in transect` = `Area (m2)`*`Cover (%)`/100,
           "CoverSp"=(`Area in transect` /90)*100
    ) %>% rename(Height="Height (m)") %>% select(Species, Height, CoverSp)
  
  if (!any(is.na(small$Count))){
    small.mod<-small %>% rowwise() %>% mutate(
      `Avg Diam 1`= mean(c(`Diameter 1 (1)`,`Diameter 1 (2)`), trim=0,na.rm=T),
      `Avg Diam 2`= mean(c(`Diameter 2 (1)`,`Diameter 2 (2)`), trim=0,na.rm=T),
      `Avg Diam 3`= mean(c(`Diameter 3 (1)`,`Diameter 3 (2)`), trim=0,na.rm=T),
      `Area 1` = (`Avg Diam 1`/2)^2*pi,
      `Area 2` = (`Avg Diam 2`/2)^2*pi,
      `Area 3` = (`Avg Diam 3`/2)^2*pi
    ) %>% mutate(
      "Height"= rowMeans(across(c("Height 1","Height 2", "Height 3")), na.rm=T),
      "AvegCoverSp"= rowMeans(across(c(`Area 1`, `Area 2`, `Area 3`)), na.rm = TRUE)
    ) %>% mutate(
      "CoverSp"=(AvegCoverSp/90)*100
    ) %>% select(Species, Height, CoverSp, Count) %>%
      uncount(Count)
  } else (small.mod<-c())
  
  PlotSpecies<-rbind(adults.mod, small.mod)
  AllPlotsPlot<-data.frame(cbind(Plot=plot, PlotSpecies))
  AllPlots<-rbind(AllPlots, AllPlotsPlot)
}

Species_Plot<-AllPlots %>%
  group_by(Plot) %>%
  summarise(w_mean_height = weighted.mean(Height, CoverSp, na.rm = TRUE),
            sumCover=sum(CoverSp)) %>%
  mutate(
    CoverSpTrunc=ifelse(sumCover>100,100,sumCover),
    Plot=toupper(Plot)
  ) %>% rename(CoverSp=CoverSpTrunc)


DFAll<-inner_join(DF, Species_Plot,  by = c("Plot_ID" = "Plot"))

DF_shrub<-DFAll %>%
  mutate(VegCov=c('unknown'),
         Project='SoCal',
         ShrHeight=w_mean_height*100,
         DateVisiteD=as.Date(Date, "%m/%d/%Y"),
         TreeCov=c('unknown')) %>%
  select(Project='Project', PrimaryKey="Plot_ID", Date="DateVisiteD", Year="Field_year", VegCov="VegCov", TreeCov="TreeCov",
         ShrCov="CoverSp", ShrHeight=ShrHeight, SuProject='Project')


MaxCoverSp <- AllPlots %>%
  group_by(Plot, Species) %>%
  summarise(SumCoverSp=sum(CoverSp)) %>%
  left_join(SpeciesDict %>% select (Field.Code, Genus, Species) %>% 
              mutate(full_name = str_c(Genus, Species, sep = " "),
                     Field.Code=str_to_upper(Field.Code)), by = c("Species" = "Field.Code")) %>% 
  slice_max(SumCoverSp, n = 1, with_ties = T) %>%
  ungroup() %>% mutate(Plot=str_to_upper(Plot), CoverSpTrunc=ifelse(SumCoverSp>100,100,SumCoverSp))


result <- MaxCoverSp %>%
  filter(Plot %in% DF_shrub$PrimaryKey)

ties_count <- result %>%
  group_by(Plot) %>%
  filter(n() > 1) %>%
  distinct(Plot) %>%
  nrow()

ties_count


# 
# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (Plot, Species, full_name, CoverSpTrunc), by = c("PrimaryKey"="Plot")) %>% 
#   rename(DominantSp=Species, SciName=full_name, SpCover=CoverSpTrunc)
# 
# 
# SoCal_shrub<-SoCal[SoCal$Plot_ID %in% DF_shrub$PrimaryKey,]
# SoCal_shrub_points <- terra::as.data.frame(SoCal_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, SoCal_shrub_points[,c('Plot_ID','x', 'y')], by = c("PrimaryKey" = "Plot_ID"))
# SoCal_shrub_fin <- terra::vect(joined, geom = c("x", "y"), crs = CRSproject)
# terra::writeVector(SoCal_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/SoCal_shrub.shp', overwrite=TRUE)
# 



# ########### Chaparral #############
# rm(list = ls())
# CRSproject<-"epsg:3310"
# Chap<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/Chapparal plots 2017-18/Chaparral_Database.shp")
# Chap<-terra::project(Chap, CRSproject)
# DF<-data.frame(Chap)
# 
# #remove all plots in which name there is a 'postgoat' string
# DF <- DF[!grepl("postgoat", DF$PLOT), ]
# 
# VegCoverField<-c("LTreeCov", "LShrubCov", "TOTHbCov")
# 
# DF_shrub<-DF %>% 
#   mutate(VegCov=rowSums(across(all_of(VegCoverField))), 
#          Project='Chaparral', 
#          DateVisiteD="unkonwn", 
#          ShrHeight=as.numeric(ShrubHt_cm),
#          Year=as.numeric(substr(Site_Year, nchar(Site_Year)-3, nchar(Site_Year)))) %>%
#   filter(Site_Year != "NMD_2017") %>%
#   filter(
#     (VegCov >20 & LTreeCov <10 & LShrubCov >10) |
#       (VegCov <20 & LTreeCov <10 & LShrubCov >2)) %>%
#   select(Project='Project', PrimaryKey="PLOT", Date="DateVisiteD", Year="Year", VegCov="VegCov", TreeCov="LTreeCov", 
#          ShrCov="LShrubCov", ShrHeight="ShrHeight", SuProject="Project")
# 
# 
# Chap_shrub<-Chap[Chap$PLOT %in% DF_shrub$PrimaryKey,]
# Chap_shrub_points <- as.data.frame(Chap_shrub, geom = 'XY')
# joined <- inner_join(DF_shrub, Chap_shrub_points[,c('PLOT','x', 'y')], by = c("PrimaryKey" = "PLOT"))
# Chap_shrub_fin <- terra::vect(joined, geom = c("x", "y"), crs = CRSproject)
# terra::writeVector(Chap_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Chap_shrub.shp', overwrite=TRUE)
# 
# 
