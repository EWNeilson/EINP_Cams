





#####################


## Seasons
winter = c(12,01,02,03) 
spring = c(04,05,06) 
summer = c(07,08,09) 
fall = c(10, 11) 

tags$Month = month(tags$DT)
tags$Season = ifelse(tags$Month %in% winter, "w", ifelse(tags$Month %in% spring, "c", ifelse(tags$Month %in% summer, "s", "f")))
tags$Year = year(tags$DT)
tags$MonthYear = format(as.Date(tags$DT), "%Y-%m")

tags$Loc = unlist(lapply(gsub("PB", "B", tags$Loc), `[[`, 1))
tags$Site = unlist(lapply(substr(tags$Loc,1,3), `[[`, 1))


View(tags %>% filter(Site == "BG3" & MonthYear == "2022-03"))

View(tags %>% filter(DT == strptime("2022-06-03 09:51:49" ,"%Y-%m-%d %H:%M:%S", tz="MST")))

############################# 
## find independent events ## 
############################# 


## SPLIT LOCATIONINTO SITES AND TYPES
# foo = tags$Loc[50]
# site = substr(foo,nchar(foo),nchar(foo))
# sType = substr(foo,1,nchar(foo)-1)
# tags$Eco = unlist(lapply(substr(tags$Loc,1,nchar(tags$Loc)-1), `[[`, 1))

cam = tags
cam$Loc = unlist(lapply(gsub("PB", "B", cam$Loc), `[[`, 1))
table(cam$Loc)
cam$Eco = unlist(lapply(substr(cam$Loc,2,2), `[[`, 1))
cam$Burn = unlist(lapply(substr(cam$Loc,1,1), `[[`, 1))
cam$Type = unlist(lapply(substr(cam$Loc,4,4), `[[`, 1))
cam$Site = unlist(lapply(substr(cam$Loc,1,3), `[[`, 1))

## getting pre and post burns
burnDate = strptime("2022-05-01" ,"%Y-%m-%d", tz="MST")
cam$AfterBurn = ifelse(cam$DT < burnDate, 0,1)
cam$MonthYear = format(as.Date(cam$DT), "%Y-%m")


sort(unique(cam$MonthYear))
table(cam$Eco)
table(cam$Burn)
table(cam$Type)

# filter to only species detections
spec = unique(tags$species_common_name)
specRed = c("Elk (wapiti)","White-tailed Deer","Foxes",
            "Bison","Red Squirrel","Human","Domestic Dog",
            "Coyote","Northern Flying Squirrel" ,"Moose",
            "Mink", "Black Bear","Weasels and Ermine",
            "Fisher","Least Weasel", "Long-tailed Weasel",
            "Rabbits and hares","Snowshoe Hare",
            "Wolves, Coyotes and Allies","Deer Mouse",
            "Domestic Cow","Mule Deer","Gray Squirrel",
            "Deer","Vehicle", "Black Bear","Voles, Mice and Allies",
            "Red Fox")             
  

## new columns for finding independent events  
cam = cam %>%
  mutate(lab = paste(Site, species_common_name,age_class,sex_class,sep = "_"),event = NA) %>%
  filter(species_common_name %in% specRed)
unique(cam$species_common_name)
unique(cam$Site)

cam = cam[order(cam$Loc,cam$DT), ]

## event parameters
int = 30  ## cutoff for independent event in minutes

## new dataframe for tracking event information
Labs = data.frame(lab=unique(cam$lab),
                  count=0,
                  eventCurrent=min(cam$DT,na.rm=T))

for(k in 1:nrow(cam)){
  if(!is.na(cam$lab[k])){
    print(k)
    if( difftime(cam$DT[k], Labs$eventCurrent[which(Labs$lab == cam$lab[k])] , units = "min") > int ){
      ## new event
      Labs$count[which(Labs$lab == cam$lab[k])] = Labs$count[which(Labs$lab == cam$lab[k])] + 1
      Labs$eventCurrent[which(Labs$lab == cam$lab[k])] = cam$DT[k]
    }
    cam$event[k] = paste( cam$lab[k], Labs$count[which(Labs$lab == cam$lab[k])], sep = "_")  ## this always works, because if there is no new even, Labs$count is not updated
  }
}

### EVENTS AMONG SITE CAMS
SubSiteCams = cam


###################
## SUMMARIES AND FIGURES 

options(warn = 1)
pd <- position_dodge(0.3) 


##
CamSum = cam %>%
  filter(species_common_name %in% c("Bison"))%>%   # "Elk (wapiti)", 
  group_by(species_common_name,Eco, Burn,Site,AfterBurn)%>%
  summarize(EventCount = length(unique(event)))

CamSum = left_join(CamSum, CamEffort, by = c("Site","AfterBurn"))
CamSum = CamSum %>%  mutate(EventRate = EventCount / as.numeric(as.character(te)))


png(file="C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/MS/Figures/BisonPrePost.png",width = 1200, height = 1000, units = "px")
print(
  CamSum %>%
    filter(Eco != "F")%>%
    group_by(Burn,Eco,AfterBurn)%>%
    summarize(
      RateN    = length(EventRate),
      MeanRate = mean(EventRate),
      SDRate   = sd(EventRate),
      SERate   = SDRate / sqrt(RateN))%>%
    ggplot(aes(x=as.factor(AfterBurn), y=MeanRate, colour=Burn, group=Burn)) + 
    geom_errorbar(aes(ymin=MeanRate-SERate, ymax=MeanRate+SERate), width=.3, position=pd) +
    geom_point(position=pd, size=5)+
    ylab("Event Frequency / Day")+ xlab("BeforeAfter Prescribed Burn")+
    facet_wrap(Eco~.,ncol=3)+
    theme_bw(base_size=30)+ theme(legend.title = element_blank())
  )


## MONTLY EVENTS
# minMY = as.Date(paste0(min(format(SiteDates$Start, "%Y-%m")),"-01"))
# maxMY = as.Date(paste0(max(format(SiteDates$End, "%Y-%m")),"-01"))
# monthYear = as.Date(seq(minMY,maxMY,by="month"))
# monthYear = format(seq(minMY,maxMY,by="month"), "%Y-%m")

# png(file="C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Permafrost/MS/MS/FiguresJuly2023/Weighted/HR_Preds_Feb2024.png",width = 1200, height = 1000, units = "px")
# print(

MonthSum = cam %>%
  mutate(CamMonthYear = paste0(Site,MonthYear))%>%
  group_by(species_common_name,CamMonthYear)%>%  
  summarize(EventRate = length(unique(event))) %>%
  ungroup %>%
  complete(CamMonthYear, species_common_name)%>%
  filter(species_common_name %in% c("Bison"))%>% ## keep in all the species up to here to ensure we have as many MonthYears as possible
  mutate(
    Burn = unlist(lapply(substr(CamMonthYear,1,1), `[[`, 1)),
    Eco = unlist(lapply(substr(CamMonthYear,2,2), `[[`, 1)),
    MonthYear = unlist(lapply(substr(CamMonthYear,4,10), `[[`, 1)))%>%
  mutate(MonthYear = as.Date(paste0(MonthYear,"-01")),
         EventRate = replace_na(EventRate,0))%>%
  filter(Eco != "F")%>%
  group_by(Burn,Eco,MonthYear)%>%
  summarize(
    RateN    = length(EventRate),
    MeanRate = mean(EventRate),
    SDRate   = sd(EventRate),
    SERate   = SDRate / sqrt(RateN))

MonthSum %>%
  ggplot(aes(x=MonthYear, y=MeanRate, colour=Burn, group=Burn)) + 
  geom_errorbar(aes(ymin=MeanRate-SERate, ymax=MeanRate+SERate), width=.3, position=pd) +
  geom_point(position=pd, size=5)+
  # geom_line(position=pd, size=3)+
  ylab("Event Frequency / Day")+ xlab("BeforeAfter Prescribed Burn")+
  facet_wrap(.~Eco,ncol=1)+
  theme_bw(base_size=30)+ theme(legend.title = element_blank())



######################
## LANDSCAPE

dep <- terra::vect("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/Analysis/Data/Spatial/Vectors/EINP_DEP_2018.shp")

## SITES
sites = terra::vect("C:/Users/erneilso/OneDrive - NRCan RNCan/Projects/Bison/Analysis/Data/Spatial/Sites/FromField/SitesDec5_26912.shp")
sites$DEP = terra::extract(dep,sites)["EP_NAME"]
table(sites$DEP)
