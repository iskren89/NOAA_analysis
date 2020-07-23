---
title: "Economic and health costs of severe climate events in the U.S."
author: "Iskren Blagov"
date: "23/07/2020"
output: html_document
---

**Synopsis**  
We analyse the NOAA Storm Database and assess the economic and public health impact of severe weather events in the United States. We take a look at which types of events (as indicated in the EVTYPE variable in the dataset) lead to the highest number of injuries and fatalities. We also explore which types of events lead to the biggest economic impact - as measured by estimates of property and crop damage. The report is aimed at guiding public policy on preparation and resource allocation for potential severe weather events.  
**Data Processing**  
We use R to access and load the NOAA Storm Database. We use the dplyr R package to analyse the data and the knitr R package to present our findings. As we are interested in the types of events that lead to the highest economic and healt costs, we group the data by event type (as indicated by the EVTYPE variable in the original dataset.)
```{r message=FALSE}
library(dplyr)
library(knitr)
library("RColorBrewer")
coul<-brewer.pal(5,"Set2")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "noaa_database.csv.bz2")
dat <- read.csv("noaa_database.csv.bz2")
dat$EVTYPE<-as.factor(dat$EVTYPE)
by_event<-group_by(dat,EVTYPE)
```
We summarise the INJURIES and FATALITIES data from the grouped dataset.
```{r message=FALSE}
injured <- by_event %>% summarise(sum=sum(INJURIES)) %>% arrange(desc(sum))
dead <- by_event %>% summarise(sum=sum(FATALITIES)) %>% arrange(desc(sum))
```
We also process the property and crop damage data from the grouped dataset. The original data is presented in a way that does not allow analysis - e.g. 10 million in property damage is presented as 10 in the "PROPDMG" column and M in the "PROPDMGEXP" column. We process the property and crop damage numbers to convert 10M into 10 million. We sum the property and crop damage to assess the total economic damage for each event type.
```{r message=FALSE}
prop_dmg <- by_event %>% filter(PROPDMGEXP=="B"|PROPDMGEXP=="M"|PROPDMGEXP=="K") %>% 
    mutate (PROPDAMAGE = case_when(PROPDMGEXP=="B" ~ PROPDMG*10^9,PROPDMGEXP=="M" ~ PROPDMG*10^6,PROPDMGEXP=="K" ~ PROPDMG*10^3)) %>% 
    summarise(sum=sum(PROPDAMAGE)) %>%
    arrange(desc(sum))
crop_dmg <- by_event %>% filter(CROPDMGEXP=="B"|CROPDMGEXP=="M"|CROPDMGEXP=="K") %>% 
    mutate (CROPDAMAGE = case_when(CROPDMGEXP=="B" ~ CROPDMG*10^9,CROPDMGEXP=="M" ~ CROPDMG*10^6,CROPDMGEXP=="K" ~ CROPDMG*10^3)) %>% 
    summarise(sum=sum(CROPDAMAGE)) %>%
    arrange(desc(sum))
total_dmg <- merge(prop_dmg,crop_dmg,by.x="EVTYPE",by.y="EVTYPE",all.x=TRUE,all.y=TRUE)
total_dmg[is.na(total_dmg)] = 0
total_dmg$total <- total_dmg[,2]+total_dmg[,3]
total_dmg <- arrange(total_dmg,desc(total))
```
**Results**  
First we assess which types of severe weather events lead to the highest number of injuries and fatalities. We see that tornadoes are responsible for the highest number of injuries in the U.S. - with more than 90 thousand people injured in the period between 1950 and 2011. Other types of events that cause a lot of injuries are TSTM-winds, floods, excessive heat and lightnings - each with 5000 to 7000 cases in the period between 1950 and 2011.
```{r warning = FALSE}
injured[1:5,]
```
We also see that tornadoes are responsible for the highest number of fatalities in the U.S. - with 5633 cases in the period between 1950 and 2011, while excessive heat caused almost 2000 fatalities in the same period. Other types of events that cause a lof of fatalities are flash floods, heat, and lightnings, which caused between 800 and 1000 fatalities in the years between 1950 and 2011.
```{r warning = FALSE}
dead[1:5,]
```
We see that floods caused the highest economic damage (property and crop damage) amongst the types of severe weather events in the NOAA database - floods caused more than 150 billion in property and crop damage for the years 1950-2011. Other types of events that caused enormous economic damage are hurricanes/typhoons (72 billion), tornadoes (57 billion) and storm surges (43 billion).
```{r}
total_dmg[1:4,]
barplot(total_dmg[1:4,4], col=coul, main=c("Total property and crop damage 1950-2011"))
legend("topright",legend=total_dmg[1:4,1], fill=coul)
```
