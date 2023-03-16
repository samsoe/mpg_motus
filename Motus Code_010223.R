# William Blake - Motus
# 01/02/23 
###################################################################################################
#04/05/2021
## Installing Packages
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("motus","lubridate","maps","tidyverse","rworldmap","ggmap","DBI", "RSQLite") # "motus" use the update provided above
ipak(packages)

###################################################################################################
#Set New Directory
mac<-"~/Desktop/Motus IWC R/sql database"
setwd(mac)
###################################################################################################

Sys.setenv(TZ = "UTC")
#Download data for the first time

###################################################################################################
## IF NO INTERNET
# df.goodtags <- readRDS("./df.receiver_goodtags_110419.RDS")
#IF INTERNET OR MEMORY ISSUE YOU CAN TRY 
#motusLogout().
###################################################################################################
## IF INTERNET ACCESS

#By Project:
#proj.num <- 386 # SCBI pipits
#proj.num <- 450 # STGR
proj.num <- 213 # IWC birds
#src1 <- tagme(proj.num, new=T, update=T, forceMeta = T) # On 10/22: force meta to amend big changes in projects 213, 226, and 352.
src1 <- tagme(proj.num, update=T,  forceMeta = T, skipNodes = TRUE)

tbl.alltags1 <-tbl(src1,"alltags")
df.alltags1 <- tbl.alltags1 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

############################

# Filter to include only tags since 9/18/18 deployment at 20:27 UTC
df.goodtags  <-df.alltags1  %>%
  filter(runLen > 1) %>%         ## On 10/28/20 to read CTT tags do not include filter=freqsd < 0.1 & 
  ## May want to run analyses with runlength of 2 instead
  filter(ts >= ymd_hms("2018-09-18 20:27:00"))
#saveRDS(df.goodtags, "./goodtags213_011822.RDS")
#View(df.goodtags)
#df.goodtags <- readRDS("./goodtags_010521.RDS")
###################################################################################################
###################################################################################################

#Select specific columns and reorder column display:
df.selecttags<-df.goodtags%>% ##df.alltags1
  select(ts, freqsd, runLen, recvProjName,motusTagID,mfgID,speciesEN,
         markerNumber,tagDeployID, tagProjID, tagProjName,tagDeployStart, tagDepLat,tagDepLon,sig,
         runLen,recvProjID,recvDeployName,recvSiteName,recvDeployLat,recvDeployLon,
         port, antBearing,
         tagType, codeSet, tagModel,pulseLen, tagLifespan, tagDeployID)
df.selecttags<-df.selecttags %>%
  mutate(tagDeployStart = as_datetime(tagDeployStart, tz = "UTC", origin = "1970-01-01"))
#View(df.selecttags)


## Save to RDS (better than CSV)
#saveRDS(df.selecttags, "./selecttags213_010223.RDS")
#df.selecttags <-readRDS("./selecttags213_010223.RDS")

########################################################
########################################################

#Sorting data example:

Species.detect <-df.selecttags   %>%
  filter(speciesEN=="Common Poorwill")

#Save to CSV example:
write.csv(Species.detect, "./COPO_010223.csv")


###################################################################################################
###################################################################################################
### BY RECEIVERS:

proj.num1 <- "SG-6F3BRPI3EA63" # Indian Ridge  (twice- now non active)
src1 <- tagme(proj.num1, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags1 <- tbl(src1, "alltags") # virtual table
df.alltags1 <- tbl.alltags1 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num2 = "SG-04EBRPI3F784" # BreunerLab # Batmobile Cliffs # Miller Ridge twice (non-active?)
src2 <- tagme(proj.num2, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags2 <- tbl(src2, "alltags") # virtual table
df.alltags2 <- tbl.alltags2 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num3 = "SG-8550RPI3768F" # Miller Ridge # Miller Creek # Pump Slough (active at Miller)
src3 <- tagme(proj.num3, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags3 <- tbl(src3, "alltags") # virtual table
df.alltags3 <- tbl.alltags3 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num4 = "SG-AE14RPI346CE" # Teller # Also empty deployment in the IWC BAT receivers (active at Teller)
src4 <- tagme(proj.num4, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags4 <- tbl(src4, "alltags") # virtual table
df.alltags4 <- tbl.alltags4 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num5 = "SG-2091RPI3BB4A" # South Baldy Ridge (non-active?)
src5 <- tagme(proj.num5, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags5 <- tbl(src5, "alltags") # virtual table
df.alltags5 <- tbl.alltags5 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num6 = "SG-E5B7RPI30AB4" # Lee Metcalf (active)
src6 <- tagme(proj.num6, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags6 <- tbl(src6, "alltags") # virtual table
df.alltags6 <- tbl.alltags6 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num7 = "SG-5F6ERPI30C91" # UMBELmobile (non-active?)
src7 <- tagme(proj.num7, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags7 <- tbl(src7, "alltags") # virtual table
df.alltags7 <- tbl.alltags7 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num8 = "SG-DD7DRPI3F777" # Lake Petite (active)
src8 <- tagme(proj.num8, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags8 <- tbl(src8, "alltags") # virtual table
df.alltags8 <- tbl.alltags8 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num9 = "SG-393CRPI3946E" # Lost Trail, Navopatia (Now active for ISLA MASCORIT Motus project)
src9 <- tagme(proj.num9, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags9 <- tbl(src9, "alltags") # virtual table
df.alltags9 <- tbl.alltags9 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num10 = "SG-F43CRPI363B4" # Willow Mtn # UMBELmobile in Woodchuck for 2020-2021 (active)
src10 <- tagme(proj.num10, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags10 <- tbl(src10, "alltags") # virtual table
df.alltags10 <- tbl.alltags10 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num11 = "SG-FEBBRPI3AFBA" # Barb # Ginger_KBK # Sula (active at Barbs)
src11 <- tagme(proj.num11, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags11 <- tbl(src11, "alltags") # virtual table
df.alltags11 <- tbl.alltags11 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num12 = "SG-3180RPI3AE9D" # Batmobile Floodplains # Woodchuck Breunermobile (active at woodchuck)
src12 <- tagme(proj.num12, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags12 <- tbl(src12, "alltags") # virtual table
df.alltags12 <- tbl.alltags12 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num13 = "SG-C08ERPI38376" # Birds Of Prey Center # Bobs (non-active)
src13 <- tagme(proj.num13, new=F,forceMeta = T, update = TRUE, skipNodes = TRUE)
tbl.alltags13 <- tbl(src13, "alltags") # virtual table
df.alltags13 <- tbl.alltags13 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

######################## CTT
proj.num14 <- "CTT-5031194D3168" # IndianRidge3-MPG-MT (active)
sql.motus14 <- tagme(proj.num14, new = F,  update=TRUE, forceMeta =T, skipNodes = TRUE)
tbl.alltags14 <- tbl(sql.motus14, "alltags")
df.alltags14 <- tbl.alltags14 %>% 
  collect() %>% 
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num15 <- "CTT-39B99EDBFFFC" #Lucky Peak (seasonally non-active)
src15 <- tagme(proj.num15, new=F, rename = T , update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags15 <- tbl(src15, "alltags") # virtual table
df.alltags15 <- tbl.alltags15 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num16 <- "CTT-2EE41E27E49C" # The Roost (active)
src16 <- tagme(proj.num16, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags16 <- tbl(src16, "alltags") # virtual table ## "alltags_fast"
df.alltags16 <- tbl.alltags16 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num17 <- "CTT-87C77C759BBF" # South Baldy Ridge (active)
src17 <- tagme(proj.num17, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags17 <- tbl(src17, "alltags") # virtual table
df.alltags17 <- tbl.alltags17 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num18 <- "CTT-D53C7198C4F0" #Deer Flat NWR (active)
src18 <- tagme(proj.num18, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags18 <- tbl(src18, "alltags") # virtual table
df.alltags18 <- tbl.alltags18 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num19 <- "CTT-912EAEBB71F5" # Camas NWR1 (non-active and sent back)
src19 <- tagme(proj.num19, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags19 <- tbl(src19, "alltags") # virtual table
df.alltags19 <- tbl.alltags19 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num20 <- "CTT-F5FA9A2D5D83" # Sterling WMA (active)
src20 <- tagme(proj.num20, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags20 <- tbl(src20, "alltags") # virtual table
df.alltags20 <- tbl.alltags20 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num21 <- "CTT-26E533CA5563" # Minidoka NWR (active)
src21 <- tagme(proj.num21, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags21 <- tbl(src21, "alltags") # virtual table
df.alltags21 <- tbl.alltags21 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num22 <- "CTT-40F188AEB096" # Fort Boise WMA (active)
src22 <- tagme(proj.num22, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags22 <- tbl(src22, "alltags") # virtual table
df.alltags22 <- tbl.alltags22 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num23 <- "CTT-3C9873F4B04E" # Billingsley WMA and Kuna1 (active at Billingsley)
src23 <- tagme(proj.num23, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags23 <- tbl(src23, "alltags") # virtual table
df.alltags23 <- tbl.alltags23 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num24 <- "CTT-CC506A073F27" # Kuna2 (active)
src24 <- tagme(proj.num24, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags24 <- tbl(src24, "alltags") # virtual table
df.alltags24 <- tbl.alltags24 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num25 <- "CTT-1B51CADBC2EF" #Birds Of Prey2 (active)
src25 <- tagme(proj.num25, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags25 <- tbl(src25, "alltags") # virtual table
df.alltags25 <- tbl.alltags25 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num26 <- "CTT-F2B981B73FE3" # Niagara Springs WMA # Birds of Prey Center1  (active at Niagara)
src26 <- tagme(proj.num26, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags26 <- tbl(src26, "alltags") # virtual table
df.alltags26 <- tbl.alltags26 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num27 <- "CTT-AE44767B1CC3" # Warm Springs ARCO (active)
src27 <- tagme(proj.num27, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags27 <- tbl(src27, "alltags") # virtual table
df.alltags27 <- tbl.alltags27 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num28 <- "CTT-E91F83776412" # Vesper Meadow KBO  (active)
src28 <- tagme(proj.num28, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags28 <- tbl(src28, "alltags") # virtual table
df.alltags28 <- tbl.alltags28 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num29 <- "CTT-090A15C0E6EB" # Dry Fork East APR - SCBI (active)
src29 <- tagme(proj.num29, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags29 <- tbl(src29, "alltags") # virtual table
df.alltags29 <- tbl.alltags29 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num30 <- "CTT-D7D11D3641CD" # Apricot Lane Farms (active)
src30 <- tagme(proj.num30, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags30 <- tbl(src30, "alltags") # virtual table
df.alltags30 <- tbl.alltags30 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num31 <-  "CTT-7645B4C32B21" # Camas NWR2 (active)
src31 <- tagme(proj.num31, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags31 <- tbl(src31, "alltags") # virtual table
df.alltags31 <- tbl.alltags31 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num32 <-  "CTT-B5A78CE7F3B3" # Rock Creek FVLT (active)
src32 <- tagme(proj.num32, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags32 <- tbl(src32, "alltags") # virtual table
df.alltags32 <- tbl.alltags32 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

############################
############################
#As of Jan 31, 2022 --> Adding all missing receivers:
proj.num33 <- "CTT-8844392FDE05" # Rogue River
src33 <- tagme(proj.num33, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags33 <- tbl(src33, "alltags") # virtual table
df.alltags33 <- tbl.alltags33 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num34 <- "CTT-5CB1ED08C1E8" # Manley Ranch
src34 <- tagme(proj.num34, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags34 <- tbl(src34, "alltags") # virtual table
df.alltags34 <- tbl.alltags34 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num35 <- "CTT-1B9C5A3B9610" # Miller Stolen
src35 <- tagme(proj.num35, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags35 <- tbl(src35, "alltags") # virtual table
df.alltags35 <- tbl.alltags35 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num36 <- "CTT-37B65B47070F" # McCauley Butte
src36 <- tagme(proj.num3, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags36 <- tbl(src36, "alltags") # virtual table
df.alltags36 <- tbl.alltags36 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num37 <- "CTT-5C686D803E87" # Salmon IDFG HQ
src37 <- tagme(proj.num37, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags37 <- tbl(src37, "alltags") # virtual table
df.alltags37 <- tbl.alltags37 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num38 <- "CTT-B5A78CE7F3B3" # Rock Creek Confluence
src38 <- tagme(proj.num38, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags38 <- tbl(src38, "alltags") # virtual table
df.alltags38 <- tbl.alltags38 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num39 <- "CTT-DF5063612CF1" # Lee Metcalf CTT
src39 <- tagme(proj.num39, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags39 <- tbl(src39, "alltags") # virtual table
df.alltags39 <- tbl.alltags39 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num40 <- "CTT-0AB294E68254" # Lazy Heart inactive
src40 <- tagme(proj.num40, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags40 <- tbl(src40, "alltags") # virtual table
df.alltags40 <- tbl.alltags40 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num41 <- "CTT-B90D83FBFD06" # Lazy Heart new
src41 <- tagme(proj.num41, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags41 <- tbl(src41, "alltags") # virtual table
df.alltags41 <- tbl.alltags41 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num42 <- "CTT-A80E9FC841B1" # Teller CTT
src42 <- tagme(proj.num42, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags42 <- tbl(src42, "alltags") # virtual table
df.alltags42 <- tbl.alltags42 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num43 <- "CTT-E960B6663B74" # Peterson's Ranch
src43 <- tagme(proj.num43, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags43 <- tbl(src43, "alltags") # virtual table
df.alltags43 <- tbl.alltags43 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num44 <- "CTT-9E705FDB557B" # Kalsta Ranch
src44 <- tagme(proj.num44, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags44 <- tbl(src44, "alltags") # virtual table
df.alltags44 <- tbl.alltags44 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num45 <- "CTT-76C89FA52A3E" # Market Lake WMA
src45 <- tagme(proj.num45, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags45 <- tbl(src45, "alltags") # virtual table
df.alltags45 <- tbl.alltags45 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num46 <- "CTT-D5914A844938" # Deer Parks WMU
src46 <- tagme(proj.num46, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags46 <- tbl(src46, "alltags") # virtual table
df.alltags46 <- tbl.alltags46 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num47 <- "SG-393CRPI3946E" # Navopatia / Isla Mascorit (old soon)
src47 <- tagme(proj.num47, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags47 <- tbl(src47, "alltags") # virtual table
df.alltags47 <- tbl.alltags47 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num48 <- "CTT-D5814733BF65" # Malheur NWR - HQ Shop
src48 <- tagme(proj.num48, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags48 <- tbl(src48, "alltags") # virtual table
df.alltags48 <- tbl.alltags48 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

#Malheur didn't work because no connextion
proj.num49 <- "CTT-8B9CDA2CE7EC" # Malheur NWR - Boca Lake
src49 <- tagme(proj.num49, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags49 <- tbl(src49, "alltags") # virtual table
df.alltags49 <- tbl.alltags49 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num50 <- "CTT-302881B0CE9C" # McNary NWR
src50 <- tagme(proj.num50, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags50 <- tbl(src50, "alltags") # virtual table
df.alltags50 <- tbl.alltags50 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num51 <- "CTT-F209FE92D5A0" # APR - Dry Fork
src51 <- tagme(proj.num51, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags51 <- tbl(src51, "alltags") # virtual table
df.alltags51 <- tbl.alltags51 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

proj.num52 <- "CTT-B1C537DCCD5E" # APR - White Rok Mailbox
src52 <- tagme(proj.num52, new=F, update=T, forceMeta =T, skipNodes = TRUE)
tbl.alltags52 <- tbl(src52, "alltags") # virtual table
df.alltags52 <- tbl.alltags52 %>%
  collect() %>%
  as.data.frame() %>%     # for all fields in the df (data frame)
  mutate(ts = as_datetime(ts, tz = "UTC", origin = "1970-01-01"))

############################

# Bind all receiver rows:
df.alltags <- bind_rows(df.alltags1,df.alltags2,df.alltags3,df.alltags4,df.alltags5,df.alltags6,df.alltags7,
                        df.alltags8,df.alltags9,df.alltags10,df.alltags11,df.alltags12, df.alltags13, df.alltags14,
                        df.alltags15,df.alltags16,df.alltags17,df.alltags18,df.alltags19,df.alltags20,df.alltags21,
                        df.alltags22,df.alltags23,df.alltags24,df.alltags25,df.alltags26,df.alltags27,df.alltags28,
                        df.alltags29,df.alltags30,df.alltags31)

####
#View(df.alltags)
#saveRDS(df.alltags, "./df.alltags_allprojects_040521_skipnodes.RDS")
#saveRDS(df.alltags, "./df.alltags_allreceivers_012921.RDS")
#df.alltags <- readRDS("./df.alltags_allreceivers_012921.RDS")
