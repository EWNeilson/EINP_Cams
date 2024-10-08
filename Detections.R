
options(warn = 1)
library(terra)
library(tidyverse)


#############################
## TAG DATA
# tags = read.csv("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/Data/PhotoTags/EINP Bison 2020_Tags_20240610/EINP_Bison_2020_tags20240610_173159UTC.csv")
tags = read.csv("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/Data/PhotoTags/EINP Bison 2020-July2023_Tags_202411/EINP_Bison_2020-July2023_tags20241001_151902UTC.csv")

# DATE AND TIME
tags$DT = strptime(tags$image_date_time ,"%Y-%m-%d %H:%M:%S", tz="MST")
# hist(tags$DT,breaks= 1000)
# max(tags$DT)

yearQuery = strptime("2021" , format = "%Y", tz = "MST") 
tags%>%
  filter(DT<yearQuery)%>%
  ggplot(aes( as.POSIXct(DT))) +
  geom_histogram()

# LOCATIONS
tags$Loc = toupper(tags$ï..location)
tags$Loc = gsub("_", "", tags$Loc)

rename = T
while(rename){
  renames = which(!is.na(as.numeric(substr(tags$Loc,nchar(tags$Loc),nchar(tags$Loc)))))
  if(length(renames) != 0){
    tags$Loc[renames] = substr(tags$Loc[renames],1,nchar(tags$Loc[renames])-1)
  }
  else{
    rename = F
  }
}

# table(tags$Loc)
# View(tags[which(tags$Loc == "NG1C"),])
# hist(tags$DT[which(tags$Loc == "PBT5C")], breaks = 100)
# table(tags$species_common_name[which(tags$Loc == "NF4C")])



################################
## DEPLOYMENT INTERVALS
camMeta = read.csv("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/Data/SiteData/June2024/einp-bison-fire-csv/form-2__deployments.csv")

# # DATE AND TIME
# camMeta$StartDate = unlist(lapply(strsplit(camMeta$created_at,"T"), `[[`, 1))
# scrapTime = unlist(lapply(strsplit(camMeta$created_at,"T"), `[[`, 2))
# camMeta$StartTime = unlist(lapply(strsplit(scrapTime,"[.]"), `[[`, 1))
# camMeta$Start = as.POSIXct(strptime(paste(camMeta$StartDate,camMeta$StartTime,sep= " "), "%Y-%m-%d %H:%M:%S", tz="MST"))
camMeta$Start = as.POSIXct(strptime(paste(camMeta$X17_Deployment_Date,camMeta$X18_Deployment_Time,sep= " "), "%d/%m/%Y %H:%M:%S", tz="MST"))

# LOCATIONS
camMeta$Loc = toupper(camMeta$X14_Site_ID)
camMeta$Loc = gsub(" ", "", camMeta$Loc)

# View(camMeta[which(camMeta$Loc == "PNG3"),])
# table(camMeta$Loc)
# length(unique(camMeta$Loc))

## NOT CHANGING ENTRY MISTAKES IN EPICOLLECT - DO IT HERE
camMeta$Loc[which(camMeta$ï..ec5_uuid == "5d7dc089-37e1-451a-a432-515eb8893628")] = "NG4"   ## double checked the entry that was entered into epicollect for NG4 (epicollect website)

camMeta$Loc[which(camMeta$Loc == "PNG3")] = "PBG3"
camMeta$Loc[which(camMeta$Loc == "PBT5P")] = "PBT5"

camMeta$SiteType = NA
camMeta$SiteType[which(camMeta$X15_Exposure_Type == "Exp")] = "P"
camMeta$SiteType[which(camMeta$X15_Exposure_Type == "Exc")] = "C"

camMeta$Loc = paste0(camMeta$Loc,camMeta$SiteType)


###########
## TAGS FIXES

## 1. NF2P the dates are238months ahead
hist(tags$DT[which(tags$Loc == "NF2P")], breaks = 100)
nf2pStart = min(camMeta$Start[which(camMeta$Loc == "NF2P")])
nf2pPhotoStart = min(tags$DT[which(tags$Loc == "NF2P")])
nf2pTimeDiff = nf2pStart - nf2pPhotoStart
tags$DT[which(tags$Loc == "NF2P")] = tags$DT[which(tags$Loc == "NF2P")] + nf2pTimeDiff
hist(tags$DT[which(tags$Loc == "NF2P")], breaks = 100)

## 2. PBG3_C has photos from ng3p - I am checking to see which ones here F:\CFS\EINP\Photos\NG3_P\P_1\103RECNX
# View(tags[which(tags$DT == strptime("2021-02-17 05:28:50" ,"%Y-%m-%d %H:%M:%S", tz="MST")),])
## THe above query is the first photo from that upload.
## Before cleaning the above query returned two identical photos.The correct on is NG3_P.
# table(tags$ï..location[which(tags$equipment_serial =="HLPXEM06042519")])
## this table call reveals the 1095 photos in BPBG#_C that should be deleted (they are in NG3P)
preDelete = nrow(tags)
tags = tags[-which(tags$equipment_serial =="HLPXEM06042519" & tags$ï..location == "PBG3_C2"),]
preDelete - nrow(tags)  ## should == 1095

hist(tags$DT,breaks= 1000)


# COMPARE META AND TAGS
table(camMeta$Loc)
table(tags$Loc)
length(unique(camMeta$Loc))
table(camMeta$Loc %in% tags$Loc)
table(tags$Loc %in% camMeta$Loc)




#########################
# GET DEPLOYMENT INTERVALS
options(warn = 1)
camMeta$End = as.POSIXct(NA)
MissingPhotos = NA
for (i in 1:nrow(camMeta)){
  
  loc = camMeta$Loc[i]
  start= camMeta$Start[i]
  
  camStartDates = camMeta$Start[which(camMeta$Loc == loc)]
  camTagDates = tags$DT[which(tags$Loc == loc)]
  
  camMeta$End[i] = NA
  
  if(max(camStartDates) == start){
    print("Deployment not finished yet")
  }else{
    end = min(camStartDates[which(camStartDates>start)])
    end = strptime(end ,"%Y-%m-%d", tz="MST")
    print(paste0("Serviced on ",end))
    options(warn = 2)
    realEnd = try(max(camTagDates[which(camTagDates > start & camTagDates < end)]))
    
    if(class(realEnd)[1] != "try-error"){
      print(paste0("finished on ",realEnd))
      options(warn = 1)
      camMeta$End[i] = as.POSIXct(as.character(realEnd))
    }else{
      print(paste0("photos missing for ",loc))
      MissingPhotos = c(MissingPhotos, loc)}
  }
    
  print("")
}
options(warn = 1)

View(camMeta[which(camMeta$Loc == "NF2P"),])

## PLOT INTERVALS
CamDates = camMeta %>%
  select(Loc,Start,End)%>%
  filter(complete.cases(.))

ggplot(CamDates,aes(y=Loc, yend = Loc, x=Start, xend=End)) + 
  geom_segment()  

## GET CAMERA DURATIONS TO CALCULATE EFFORT
CamEffort = CamDates %>%
  mutate(effort = difftime(End, Start))%>%
  group_by(Loc)%>%
  summarize(te = sum(effort))
CamEffort$Loc = unlist(lapply(gsub("PB", "B", CamEffort$Loc), `[[`, 1))



##########
## CALCULATE SITE COMBINATIONS INTERVALS
SiteDates = camMeta %>%
  select(Loc,Start,End)%>%
  filter(complete.cases(.))
SiteDates$Loc = unlist(lapply(gsub("PB", "B", SiteDates$Loc), `[[`, 1))
SiteDates$Loc = unlist(lapply(substr(SiteDates$Loc,1,3), `[[`, 1))

## add in burn pre-post intervals
SiteDates1 = data.frame()  #empty on purpose
for (i in 1:nrow(SiteDates)){
  
  cRow = SiteDates[i,]
  cRow$AfterBurn = 0
  
  if( cRow$Start < burnDate & cRow$End > burnDate){
    print("splitting deployment by 2022 burn")
    preRow = cRow; postRow = cRow
    preRow$End = burnDate
    postRow$Start = burnDate
    postRow$AfterBurn = 1
    SiteDates1 = rbind(SiteDates1, postRow)
    SiteDates1 = rbind(SiteDates1, preRow)}
  else if (cRow$Start > burnDate){
    cRow$AfterBurn = 1
    SiteDates1 = rbind(SiteDates1, cRow)}
  else{
    SiteDates1 = rbind(SiteDates1, cRow)}
}
SiteDates = SiteDates1; rm(SiteDates1)

## combine sub sites    
SiteDates$SD = NA
for (s in 1:nrow(SiteDates)){
  stInd = rownames(SiteDates[s,])
  
  if(is.na(SiteDates$SD[s])){
    for (i in 1:nrow(SiteDates)){
      ovInd = rownames(SiteDates[i,])
      
      if(stInd != ovInd & SiteDates$Loc[s] == SiteDates$Loc[i]){
        if(SiteDates$Start[i] > SiteDates$Start[s] & SiteDates$Start[i] < SiteDates$End[s] |
           SiteDates$Start[i] < SiteDates$Start[s] & SiteDates$End[i] > SiteDates$Start[s] ){
          print(paste0(stInd, " overlapping interval with ", ovInd))
          SiteDates$SD[i] = stInd
        }
      }
    }
  }
}
## fill in the NAs on SD
for (s in 1:nrow(SiteDates)){
  stInd = rownames(SiteDates[s,])
  if(is.na(SiteDates$SD[s])){
    SiteDates$SD[s] = stInd
  }
}

## get the total effort  
SiteDatesRed = SiteDates %>%
  group_by(SD,AfterBurn)%>%
  summarize(Site = unique(Loc),
            Start = min(Start),
            End = max(End))
ggplot(SiteDatesRed,aes(y=Site, yend = Site, x=Start, xend=End)) +  #,col=AfterBurn
  geom_segment()  

## GET COMBINED CAMERA DURATIONS TO CALCULATE EFFORT
CamEffort = SiteDatesRed %>%
  mutate(effort = difftime(End, Start))%>%
  group_by(AfterBurn,Site)%>%
  summarize(te = sum(effort))

table(CamEffort$AfterBurn)

CamEffort %>%
  group_by(AfterBurn)%>%
  summarize(meanEffort = mean(te))




###########
## CHECK DEPLOYMENT INTEVALS WHEN INCLUDING UNTAGGED PHOTOS
upload_meta = read.csv("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Restoration/Camera_Restoration_SharedDrive/CardFormatting/EINP_Uploads.csv")
upload_meta = upload_meta %>%
  #filter(Uploaded == "N")%>%
  select(Site,StartDate,EndDate)
upload_meta$Start = strptime(upload_meta$StartDate ,"%d-%b-%y", tz="MST") ## 29-Jul-20
upload_meta$End = strptime(upload_meta$EndDate,"%d-%b-%y", tz="MST")
upload_meta = upload_meta %>%
  select(Site,Start,End)%>%
  rename(Loc = Site)

CamDate1 = rbind(CamDates, upload_meta)

ggplot(CamDate1,aes(y=Loc, yend = Loc, x=Start, xend=End)) + 
  geom_segment()  

