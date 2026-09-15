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

PerSpBLM <- BLMsp %>% left_join(BLMspDict %>% filter (SpeciesState=="CA") %>% select (SpeciesCode, ScientificName), by = c("Species" = "SpeciesCode"))

DF_shrubSP<- DF_shrub %>% select (Project, PrimaryKey) %>%
  left_join(PerSpBLM %>% select (PrimaryKey, Species, ScientificName, GrowthHa_1, AH_Species, Hgt_Specie), by = "PrimaryKey") %>% 
  rename(Species=Species, SciName=ScientificName, Lifeform=GrowthHa_1, Cover=AH_Species, Height=Hgt_Specie)

BLM_shrub<-BLM[BLM$PrimaryKey %in% DF_shrubSP$PrimaryKey,]
BLM_shrub_points <- as.data.frame(BLM_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, BLM_shrub_points[,c('PrimaryKey','x', 'y')], by = "PrimaryKey")
BLM_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(BLM_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/BLM_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(DF_shrubSP, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/BLM_shrub_sps.csv')

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
         DateVisiteD=make_date(YYYY, MM, DD)) %>%
  filter(LocMeth=="G" & Protocol != "VegBank") %>%
  filter(
    (VegCov >20 & SourceTreeCov <10 & SourceShrubCov >10) |
      (VegCov <20 & SourceTreeCov <10 & SourceShrubCov >2)) %>%
  select(Project='Project', PrimaryKey="EventID", Date="DateVisiteD", Year="YYYY", VegCov="VegCov", TreeCov="SourceTreeCov", 
         ShrCov="SourceShrubCov", SuProject="Protocol") 

DF_shrubSP<- DF_shrub %>% select (Project, PrimaryKey) %>%
  left_join(LFSpecies %>% select (EventID, Item, SciName, Lifeform, LFAbsCov, LFHgt) %>% mutate(LFHgt=LFHgt*100), by = c("PrimaryKey"="EventID")) %>% 
  rename(Species=Item, SciName=SciName, Lifeform=Lifeform, Cover=LFAbsCov, Height=LFHgt) %>% filter(Height>0)


Landfire_shrub<-Landfire[Landfire$EventID %in% DF_shrubSP$PrimaryKey,]
Landfire_shrub_points <- as.data.frame(Landfire_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, Landfire_shrub_points[,c('EventID','x', 'y')], by = c("PrimaryKey" = "EventID"))
Landfire_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(Landfire_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Landfire_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(DF_shrubSP, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Landfire_shrub_sps.csv')

########### VegCamp #############
# rm(list = ls())
# CRSproject<-"epsg:3310"
# VegCamp<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/VegCamp/ds1020/ds1020.gdb")
# VegCamp<-project(VegCamp, CRSproject)
# DF<-data.frame(VegCamp)
# 
# VCSpecies<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/VegCamp/SurveyPlants.csv")
# 
# DF_shrub<-DF %>% 
#   mutate(
#     Shrub_ht = recode(Shrub_ht_txt,
#                       "<.5m" = 25,
#                       ".5-1m" = 75,
#                       ".5-2m" = 75,
#                       "1-2m" = 150,
#                       "2-5m" = 350,
#                       "5-10m" = 750,
#                       "5-15m" = 750,
#                       "10-15m" = 1250,
#                       "15-20m" = 1750,
#                       "15-35m" = 2750,
#                       "20-35m" = 2750, 
#                       "N/A" = NA_real_,
#                       "Not recorded"= NA_real_,
#                       .default = as.numeric(Shrub_ht_txt)),
#     Project='VegCamp', 
#     DateVisiteD=as.Date(SurveyDate),
#     Year=year(DateVisiteD)) %>%
#   filter(Shrub_ht>0) %>%
#   filter(SurveyType %in% c("Rapid Assessment", "Releve")) %>%
#   filter(
#     (TotalVegCov >20 & TreeCov <10 & ShrubCov >10) |
#       (TotalVegCov <20 & TreeCov <10 & ShrubCov >2)) %>%
#   select(Project='Project', PrimaryKey="SurveyID", Date="DateVisiteD", Year="Year", VegCov="TotalVegCov", TreeCov="TreeCov", 
#          ShrCov="ShrubCov", ShrHeight=Shrub_ht, SuProject="SurveyType")
# 
# MaxCoverSp <- VCSpecies %>% 
#   group_by(SurveyID) %>%
#   mutate(
#     has_X = "Shrub" %in% Stratum  # I want to prioritize the maximum in the shurb group
#   ) %>%
#   filter(ifelse(has_X, Stratum == "Shrub", TRUE)) %>%
#   slice_max(Species_co, n = 1, with_ties = FALSE) %>%
#   ungroup()
# 
# DF_shrub <- DF_shrub %>% 
#   left_join(MaxCoverSp %>% select (SurveyID, CodeSpecie, Species_na), by = c("PrimaryKey"="SurveyID")) %>% rename(DominantSp=CodeSpecie, SciName=Species_na)
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
         ShrCov="Sat_cov_shrub", ShrHeight="w_mean", SuProject="SuProject") 

PerSp <- Species %>% left_join(SpeciesDict %>% distinct(code, .keep_all = TRUE) %>% select (code, species, growth.form), by = c("Species" = "code")) 

DF_shrubSP<- DF_shrub %>% select (Project, PrimaryKey) %>%
  left_join(PerSp %>% select (Plot, Species, species, growth.form, Cover, Avg_Ht), by = c("PrimaryKey"="Plot")) %>% 
  rename(Species=Species, SciName=species, Lifeform=growth.form, Cover=Cover, Height=Avg_Ht) %>% filter(Height>0)

pointsSlaton <- vect(PlotsXY, geom = c("x", "y"), crs = CRSproject)
Slaton_shrub<-pointsSlaton[pointsSlaton$Plot %in% DF_shrubSP$PrimaryKey,]
Slaton_shrub_points <- as.data.frame(Slaton_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, Slaton_shrub_points[,c('Plot','x', 'y')], by = c("PrimaryKey" = "Plot"))
Slaton_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(Slaton_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Slaton_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(DF_shrubSP, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Slaton_shrub_sps.csv')





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
# plot<-"2-mc-2013-25"
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
         DateVisiteD=as.Date(Date, "%m/%d/%Y"),
         TreeCov=c('unknown')) %>%
  select(Project='Project', PrimaryKey="Plot_ID", Date="DateVisiteD", Year="Field_year", VegCov="VegCov", TreeCov="TreeCov",
         ShrCov="CoverSp", SuProject='Project')


PerSp <- AllPlots %>% group_by(Plot, Species) %>%
  summarise(w_mean_height = weighted.mean(Height, CoverSp, na.rm = TRUE),
            sumCover=sum(CoverSp)) %>% mutate(Plot=str_to_upper(Plot)) %>%
  left_join(SpeciesDict %>% mutate(full_name = str_c(Genus, Species, sep = " "),
                                   Field.Code=str_to_upper(Field.Code),
                                   Lifeform="shrub")
                        %>% select (Field.Code, full_name, Lifeform), by = c("Species" = "Field.Code"))

DF_shrubSP<- DF_shrub %>% select (Project, PrimaryKey) %>%
  left_join(PerSp %>% select (Plot, Species, full_name, Lifeform, sumCover, w_mean_height), by = c("PrimaryKey"="Plot")) %>% 
  rename(Species=Species, SciName=full_name, Lifeform=Lifeform, Cover=sumCover, Height=w_mean_height) %>% filter(Height>0)

SoCal_shrub<-SoCal[SoCal$Plot_ID %in% DF_shrubSP$PrimaryKey,]
SoCal_shrub_points <- terra::as.data.frame(SoCal_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, SoCal_shrub_points[,c('Plot_ID','x', 'y')], by = c("PrimaryKey" = "Plot_ID"))
SoCal_shrub_fin <- terra::vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(SoCal_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/SoCal_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(DF_shrubSP, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Socal_shrub_sps.csv')





########### Chaparral #############
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





########### National Park Service ###########
LAVOPlots<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312921/LAVO_tPlotEvents.csv")
LAVOPlotsSp<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312921/LAVO_tPlotEventSpecies.csv")
LAVOPlotsSpDict<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312921/LAVO_tSpecies.csv")
LAVOPlots<-LAVOPlots[-c(114,228,285),]# Discard the plot wiht incorrect coordinates or date:

PlotsXY<-LAVOPlots[, c("Plot_Event", "Field_X", "Field_Y")]
Coord<- vect(PlotsXY, geom = c("Field_X", "Field_Y"), crs = "epsg:26910")
Coord<-project(Coord, CRSproject)
# writeVector(Coord, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/NPS_LAVO_shrub_plotsv0.shp', overwrite=TRUE)

LAVOPlots$S1_HgtN<-ifelse(LAVOPlots$S1_Hgt=="-0.5 m", "0.5",
                           ifelse(LAVOPlots$S1_Hgt=="1 m", "1",
                                  ifelse(LAVOPlots$S1_Hgt=="2 m", "2",
                                         ifelse(LAVOPlots$S1_Hgt=="5 m", "5",
                                                ifelse(LAVOPlots$S1_Hgt=="10 m", "10","20")))))
LAVOPlots$S1_HgtN<-as.numeric(LAVOPlots$S1_HgtN)
LAVOPlots$S2_HgtN<-ifelse(LAVOPlots$S2_Hgt=="-0.5 m", "0.5",
                         ifelse(LAVOPlots$S2_Hgt=="1 m", "1",
                                ifelse(LAVOPlots$S2_Hgt=="2 m", "2",
                                       ifelse(LAVOPlots$S2_Hgt=="5 m", "5",
                                              ifelse(LAVOPlots$S2_Hgt=="10 m", "10","20")))))
LAVOPlots$S2_HgtN<-as.numeric(LAVOPlots$S2_HgtN)
LAVOPlots$S3_HgtN<-ifelse(LAVOPlots$S3_Hgt=="-0.5 m", "0.5",
                         ifelse(LAVOPlots$S3_Hgt=="1 m", "1",
                                ifelse(LAVOPlots$S3_Hgt=="2 m", "2",
                                       ifelse(LAVOPlots$S3_Hgt=="5 m", "5",
                                              ifelse(LAVOPlots$S3_Hgt=="10 m", "10","20")))))
LAVOPlots$S3_HgtN<-as.numeric(LAVOPlots$S3_HgtN)


VegCoverField<-c("T1_Cover", "T2_Cover", "T3_Cover", "S1_Cover", "S2_Cover", "S3_Cover", "H_Cover", "N_Cover")
TreeCoverField<-c("T1_Cover", "T2_Cover", "T3_Cover")
ShrubCoverField<-c("S1_Cover", "S2_Cover", "S3_Cover")

DF_shrub<-LAVOPlots %>% 
  mutate(VegCov=rowSums(across(all_of(VegCoverField)), na.rm=T), 
         TreeCov=rowSums(across(all_of(TreeCoverField)), na.rm=T), 
         ShrubCov=rowSums(across(all_of(ShrubCoverField)), na.rm=T), 
         Project='NPS_LAVO', 
         DateVisiteD=as.Date(substr(LAVOPlots$Event_Date, 1, 10), "%Y-%m-%d"),
         Year=year(DateVisiteD),
         SuProject="LAVO")%>%
  filter(
    (VegCov >20 & TreeCov <10 & ShrubCov >10) |
      (VegCov <20 & TreeCov <10 & ShrubCov >2)) %>%
  mutate(ShrubHeight=(replace_na(S1_HgtN, 0)*replace_na(S1_Cover, 0)+replace_na(S2_HgtN, 0)*replace_na(S2_Cover, 0)+replace_na(S3_HgtN, 0)*replace_na(S3_Cover, 0))/ShrubCov) %>%
  select(Project='Project', PrimaryKey="Plot_Event", Date="DateVisiteD", Year="Year", VegCov="VegCov", TreeCov="TreeCov", 
         ShrCov="ShrubCov", ShrHeight="ShrubHeight", SuProject="SuProject") 

HeightTable<-LAVOPlots %>% select(Plot_Event,T1_Hgt, T2_Hgt, T3_Hgt, S1_Hgt, S2_Hgt, S3_Hgt, H_Hgt) %>% pivot_longer(!Plot_Event, names_to = "Class", values_to = "Height")
HeightTable$Class<-ifelse(HeightTable$Class=="S1_Hgt", "S1",
                               ifelse(HeightTable$Class=="S2_Hgt", "S2",
                                      ifelse(HeightTable$Class=="S3_Hgt", "S3",
                                             ifelse(HeightTable$Class=="T1_Hgt", "T1",
                                                    ifelse(HeightTable$Class=="T2_Hgt", "T2",
                                                           ifelse(HeightTable$Class=="T3_Hgt", "T3","H"))))))
HeightTable$Height<-ifelse(HeightTable$Height=="-0.5 m", "0.5",
                          ifelse(HeightTable$Height=="1 m", "1",
                                 ifelse(HeightTable$Height=="2 m", "2",
                                        ifelse(HeightTable$Height=="5 m", "5",
                                               ifelse(HeightTable$Height=="10 m", "10","20")))))
HeightTable$GrowthForm<-ifelse(HeightTable$Class %in% c("S1","S2","S3"), "Shrub",
                          ifelse(HeightTable$Class %in% c("T1","T2","T3"), "Tree",
                                 "Herb"))
HeightTable$Height<-as.numeric(HeightTable$Height)


CoverTable<-LAVOPlots %>% select(Plot_Event,T1_Cover, T2_Cover, T3_Cover, S1_Cover, S2_Cover, S3_Cover, H_Cover) %>% pivot_longer(!Plot_Event, names_to = "Class", values_to = "RelCover")
CoverTable$Class<-ifelse(CoverTable$Class=="S1_Cover", "S1",
                          ifelse(CoverTable$Class=="S2_Cover", "S2",
                                 ifelse(CoverTable$Class=="S3_Cover", "S3",
                                        ifelse(CoverTable$Class=="T1_Cover", "T1",
                                               ifelse(CoverTable$Class=="T2_Cover", "T2",
                                                      ifelse(CoverTable$Class=="T3_Cover", "T3","H"))))))
CoverTable$RelCover<-as.numeric(CoverTable$RelCover)

PerSp <- DF_shrub %>% 
  inner_join(LAVOPlotsSp %>% 
               select(Plot_Event, Stratum, Spp_Code, Real_Cover) %>%
               left_join(LAVOPlotsSpDict %>% select(Spp_Code, Field_Name), by = c("Spp_Code" = "Spp_Code")), 
             by = c("PrimaryKey" = "Plot_Event")) %>% 
  left_join(CoverTable, by = c("PrimaryKey" = "Plot_Event", "Stratum"="Class")) %>%
  mutate(AbsCover=Real_Cover*(RelCover/100)) %>%
  left_join(HeightTable, by = c("PrimaryKey" = "Plot_Event", "Stratum"="Class")) %>%
  rename(Species=Spp_Code, SciName=Field_Name, Lifeform=GrowthForm, Cover=AbsCover, Height=Height) %>% filter(Cover>0) %>% 
  select(Project, PrimaryKey, Species, SciName, Lifeform, Cover, Height)

LAVO_shrub<-Coord[Coord$Plot_Event %in% PerSp$PrimaryKey,]
LAVO_shrub_points <- as.data.frame(LAVO_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, LAVO_shrub_points[,c('Plot_Event','x', 'y')], by = c("PrimaryKey" = "Plot_Event"))
LAVO_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(LAVO_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/LAVO_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(PerSp, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/LAVO_shrub_sps.csv')


## REDWOOD NP
REDWPlots<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312923/REDW_REDW_PLOTS_v4_BE_tPlotEvents.csv")
REDWPlotsSp<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312923/REDW_REDW_PLOTS_v4_BE_tPlotEventSpecies.csv")
REDWPlotsSpDict<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/NPS/records-2312923/REDW_REDW_PLOTS_v4_BE_tSpecies.csv")

PlotsXY<-REDWPlots[, c("Plot_Event", "Field_X", "Field_Y")]
Coord<- vect(PlotsXY, geom = c("Field_X", "Field_Y"), crs = "epsg:26910")
Coord<-project(Coord, CRSproject)
# writeVector(Coord, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/NPS_REDW_shrub_plotsv0.shp', overwrite=TRUE)


VegCoverField<-c("T1_Cover", "T2_Cover", "T3_Cover", "S1_Cover", "S2_Cover", "S3_Cover", "H_Cover", "N_Cover")
TreeCoverField<-c("T1_Cover", "T2_Cover", "T3_Cover")
ShrubCoverField<-c("S1_Cover", "S2_Cover", "S3_Cover")

DF_shrub<-REDWPlots %>% 
  mutate(VegCov=rowSums(across(all_of(VegCoverField)), na.rm=T), 
         TreeCov=rowSums(across(all_of(TreeCoverField)), na.rm=T), 
         ShrubCov=rowSums(across(all_of(ShrubCoverField)), na.rm=T), 
         Project='NPS_REDW', 
         DateVisiteD=as.Date(substr(REDWPlots$Event_Date, 1, 10), "%Y-%m-%d"),
         Year=year(DateVisiteD),
         SuProject="REDW")%>%
  filter(
    (VegCov >20 & TreeCov <10 & ShrubCov >10) |
      (VegCov <20 & TreeCov <10 & ShrubCov >2)) %>%
  mutate(ShrubHeight=(replace_na(S1_Hgt, 0)*replace_na(S1_Cover, 0)+replace_na(S2_Hgt, 0)*replace_na(S2_Cover, 0)+replace_na(S3_Hgt, 0)*replace_na(S3_Cover, 0))/ShrubCov) %>%
  select(Project='Project', PrimaryKey="Plot_Event", Date="DateVisiteD", Year="Year", VegCov="VegCov", TreeCov="TreeCov", 
         ShrCov="ShrubCov", ShrHeight="ShrubHeight", SuProject="SuProject") 

HeightTable<-REDWPlots %>% select(Plot_Event,T1_Hgt, T2_Hgt, T3_Hgt, S1_Hgt, S2_Hgt, S3_Hgt, H_Hgt) %>% pivot_longer(!Plot_Event, names_to = "Class", values_to = "Height")
HeightTable$Class<-ifelse(HeightTable$Class=="S1_Hgt", "S1",
                          ifelse(HeightTable$Class=="S2_Hgt", "S2",
                                 ifelse(HeightTable$Class=="S3_Hgt", "S3",
                                        ifelse(HeightTable$Class=="T1_Hgt", "T1",
                                               ifelse(HeightTable$Class=="T2_Hgt", "T2",
                                                      ifelse(HeightTable$Class=="T3_Hgt", "T3","H"))))))
HeightTable$GrowthForm<-ifelse(HeightTable$Class %in% c("S1","S2","S3"), "Shrub",
                               ifelse(HeightTable$Class %in% c("T1","T2","T3"), "Tree",
                                      "Herb"))
HeightTable$Height<-as.numeric(HeightTable$Height)
HeightTable<-HeightTable[!is.na(HeightTable$Height),]

CoverTable<-REDWPlots %>% select(Plot_Event,T1_Cover, T2_Cover, T3_Cover, S1_Cover, S2_Cover, S3_Cover, H_Cover) %>% pivot_longer(!Plot_Event, names_to = "Class", values_to = "RelCover")
CoverTable$Class<-ifelse(CoverTable$Class=="S1_Cover", "S1",
                         ifelse(CoverTable$Class=="S2_Cover", "S2",
                                ifelse(CoverTable$Class=="S3_Cover", "S3",
                                       ifelse(CoverTable$Class=="T1_Cover", "T1",
                                              ifelse(CoverTable$Class=="T2_Cover", "T2",
                                                     ifelse(CoverTable$Class=="T3_Cover", "T3","H"))))))
CoverTable$RelCover<-as.numeric(CoverTable$RelCover)

PerSp <- DF_shrub %>% 
  inner_join(REDWPlotsSp %>% 
               select(Plot_Event, Stratum, Spp_Code, Real_Cover) %>%
               left_join(REDWPlotsSpDict %>% select(Spp_Code, Field_Name), by = c("Spp_Code" = "Spp_Code")), 
             by = c("PrimaryKey" = "Plot_Event")) %>% 
  left_join(CoverTable, by = c("PrimaryKey" = "Plot_Event", "Stratum"="Class")) %>%
  mutate(AbsCover=Real_Cover*(RelCover/100)) %>%
  left_join(HeightTable, by = c("PrimaryKey" = "Plot_Event", "Stratum"="Class")) %>%
  rename(Species=Spp_Code, SciName=Field_Name, Lifeform=GrowthForm, Cover=AbsCover, Height=Height) %>% filter(Cover>0) %>% 
  select(Project, PrimaryKey, Species, SciName, Lifeform, Cover, Height)


REDW_shrub<-Coord[Coord$Plot_Event %in% PerSp$PrimaryKey,]
REDW_shrub_points <- as.data.frame(REDW_shrub, geom = 'XY')
joined <- inner_join(DF_shrub, REDW_shrub_points[,c('Plot_Event','x', 'y')], by = c("PrimaryKey" = "Plot_Event"))
REDW_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(REDW_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/REDW_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(PerSp, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/REDW_shrub_sps.csv')


########### Region5 ###########
R5PlotsXY<-terra::vect("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/R5_EcoPlot_2005/shapefile/shapefile/e3co_merge1.shp")
R5Plots<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/R5_EcoPlot_2005/eco_site.csv")
R5PlotsSP<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/R5_EcoPlot_2005/eco_flor.csv")
R5Plots_Visit<-read.csv("C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Plots/R5_EcoPlot_2005/eco_samp.csv")
R5PlotsXY_DF<-data.frame(R5PlotsXY)
R5PlotsSP_Hgt<-R5PlotsSP %>% filter(HEIGHT_FT>0)
R5Plots_Visit_Date<-R5Plots_Visit %>% filter(nchar(DATE)>0)
R5Plots_Visit_Date$DATE_g<-as.Date(R5Plots_Visit_Date$DATE, format="%m/%d/%Y")
R5Plots_Visit_Date$Year<-year(R5Plots_Visit_Date$DATE_g)

R5PlotDB<-R5Plots %>% 
  filter(NEW_SITE_ID %in% R5PlotsXY_DF$NEWSITEI)  %>% 
  left_join(R5Plots_Visit_Date, select(NEW_SITE_ID, DATE_g, Year, TOT_VEG_PCT, TOT_TREE_PCT, SHRUB_PCT, FORB_PCT, GRAM_PCT), 
            by = c("NEW_SITE_ID"="NEW_SITE_ID")) %>% 
  filter((TOT_VEG_PCT >20 & TOT_TREE_PCT <10 & SHRUB_PCT >10) |
      (TOT_VEG_PCT <20 & TOT_TREE_PCT <10 & SHRUB_PCT >2)) %>%
  filter(NEW_SITE_ID %in% R5PlotsSP_Hgt$NEW_SITE_ID) %>%
    mutate(Project="R5Plots") %>%
  select(Project='Project', PrimaryKey="NEW_SITE_ID", Date="DATE_g", Year="Year", VegCov="TOT_VEG_PCT", TreeCov="TOT_TREE_PCT", 
         ShrCov="SHRUB_PCT", SuProject="Project") 

## 12 plot from 1995
R5Species<-R5PlotsSP_Hgt %>% filter(NEW_SITE_ID %in% R5PlotDB$PrimaryKey) %>% 
  mutate(Height=HEIGHT_FT*0.3048,
         Project="R5Plots") %>%
  filter(LAYER_TYPE %in% c("S", "F", "G")) %>%
  select(Project, NEW_SITE_ID, J_SCI_NAME, SPECIES, LAYER_TYPE, COVER, Height) %>%
  rename(PrimaryKey=NEW_SITE_ID, Species=SPECIES, SciName=J_SCI_NAME, Lifeform=LAYER_TYPE, Cover=COVER, Height=Height)

Species_PlotH<-R5Species %>% 
  group_by(PrimaryKey) %>%
  summarise(ShrHeight = weighted.mean(Height, Cover, na.rm = TRUE))

R5PlotDB<-R5PlotDB %>% inner_join(Species_PlotH, by=c("PrimaryKey")) %>% relocate(ShrHeight, .after = ShrCov)

R5_shrub<-R5PlotsXY[R5PlotsXY$NEWSITEI %in% R5PlotDB$PrimaryKey,]
R5_shrub_points <- as.data.frame(R5_shrub, geom = 'XY')
joined <- inner_join(R5PlotDB, R5_shrub_points[,c('NEWSITEI','x', 'y')], by = c("PrimaryKey" = "NEWSITEI"))
R5_shrub_fin <- vect(joined, geom = c("x", "y"), crs = CRSproject)
writeVector(R5_shrub_fin, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/R5_shrub_plotsSPs.shp', overwrite=TRUE)
write.csv(R5Species, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/R5_shrub_sps.csv')





# ###############ALL TOGETHER#################
# rm(list = ls())
# CRSproject<-"epsg:3310"
BLM<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/BLM_shrub_plotsSPs.shp')
BLMSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/BLM_shrub_sps.csv')
Landfire<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Landfire_shrub_plotsSPs.shp')
LandfireSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Landfire_shrub_sps.csv')
# VegCamp<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/VegCamp_shrub.shp')
Slaton<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Slaton_shrub_plotsSPs.shp')
SlatonSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Slaton_shrub_sps.csv')
Slaton$PrimaryKey<-as.character(Slaton$PrimaryKey)
SoCal<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/SoCal_shrub_plotsSPs.shp')
SoCalSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/Socal_shrub_sps.csv')
# Chap<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Chap_shrub.shp')
LAVO<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/LAVO_shrub_plotsSPs.shp')
LAVOSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/LAVO_shrub_sps.csv')
REDW<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/REDW_shrub_plotsSPs.shp')
REDWSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/REDW_shrub_sps.csv')
R5<-terra::vect('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/R5_shrub_plotsSPs.shp')
R5SP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/R5_shrub_sps.csv')


# 
TestAll<-rbind(BLM, Landfire, Slaton, SoCal, LAVO, REDW, R5)
terra::writeVector(TestAll, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/AllDB_shrubSP.shp', overwrite=TRUE)
DFAll<-data.frame(TestAll)
table(DFAll$Year)
# hist(DFAll$ShrHeight)
TestAllSP<-rbind(BLMSP, LandfireSP, SlatonSP, SoCalSP, LAVOSP,REDWSP, R5SP)
write.csv(TestAllSP, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/AllDB_SPs.csv')

TestAllSP<-read.csv('C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/AllDB_SPs.csv')

ListSp<-TestAllSP %>% distinct(SciName, .keep_all = TRUE) %>% select(SciName, Lifeform)
write.csv(ListSp, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/ListSPs.csv')

AmountShrub<- TestAllSP %>%
  filter(Lifeform %in% c("Shrub", "Succulent", "SubShrub", "Vine", "", "S", "shrub", "forb/subshrub", "subshrub",
                         "shrub/forb", "forb/shrub", "subshrub/forb","shrub/tree","cactus", "tree/shrub", "shrub or forb",     
                          "forb or shrub","shrub, cactus", NA, "shrub, tree", "tree, shrub","shrub/forb/subshrub",
                         "shrub/succulent","vine/shrub","forb, subshrub")) %>%
  group_by(SciName) %>%
  summarise(Occurrences=n(),
            meanCover=mean(Cover), 
            meanHeight=mean(Height))
write.csv(AmountShrub, 'C:/Users/anduane/Box/CalFire_April 2025/Plot Data/Analysis/outputs/Level2/ListSPsamountsHeight.csv')
    
hist(AmountShrub$meanHeight)
