library(dplyr)
library(leaflet)
library(tidyr)
library(readr)
library(sqldf)
library(ggplot2)  ##plotting
library(scales)    ##tuning plots
library(stringr)
library(anytime)   ##fixing time and dates
library(rgdal)      ##maps


##A. INTRO-read csv

crime<- read_csv("crimes-in-chicago/Chicago_Crimes_2012_to_2017.csv")
View(crime)

##remove missing geographical locations
data <- crime %>% drop_na(Latitude) %>% drop_na(Longitude) 
View(data)


##remove space from column names
names(data) = gsub(" ", "", names(data))
names(data)

data1 = sqldf("Select PrimaryType , 
                  count(*) as frequency 
                  from data group by PrimaryType")


##B. plot crime type against frequency to find the highest type of crime
           ##factoring data to plot
PrimaryType <- factor(data1$PrimaryType, levels = data1$PrimaryType)

           ## Draw plot
data1<- sqldf("Select * 
                  from data1
                  order by frequency DESC")


ggplot(data1, aes(x=reorder(PrimaryType, -frequency), y=frequency)) + 
  geom_bar(stat="identity", width=0.8, fill="#191970") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Frequency vs Crime Type") + scale_y_continuous(labels=comma)+
  theme(axis.text.x=element_text(angle=65, vjust=0.6),
        text = element_text(size=6))

##c. crime percentage PIE chart
        ##C.1. for theft
data2 <- sqldf("Select PrimaryType, Arrest, count(Arrest) as afrequency 
                  from data 
                  where PrimaryType='THEFT'
                  group by Arrest")
View(data2)
         ##converting to percentage and adding to original dataframe

percentage <- data2$afrequency / sum(data2$afrequency) * 100
data2<-cbind(data2,percentage)
View(data2)
          ##convert arrest logical to character
str(data2)
data2$Arrest <- as.character(data2$Arrest)

data2$percentage <-round(data2$percentage, digits = 0)


mycols <- c("#0073C2FF", "#EFC000FF")

         ##plot
ggplot(data2, aes(x = "", y = percentage, fill = Arrest)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes( label =percentage), color = "white")+
  scale_fill_manual(values = mycols) + 
  labs(title="Arrests percentage for theft")
  theme_void()
  
  
            ##C.2. crime percentage pie chart
  data4 <- sqldf("Select PrimaryType, Arrest, count(Arrest) as nfrequency 
                  from data 
                  where PrimaryType='NARCOTICS'
                  group by Arrest")
  View(data4)
  ##converting to percentage and adding to original dataframe
  
  percentage2 <- data4$nfrequency / sum(data4$nfrequency) * 100
  data4<-cbind(data4,percentage2)
  View(data4)
  ##convert arrest logical to character
  str(data4)
  data4$Arrest <- as.character(data4$Arrest)
  
  data4$percentage2 <-round(data4$percentage2, digits = 0)
  
  
  mycols <- c("#0073C2FF", "#EFC000FF")
  
  ggplot(data2, aes(x = "", y = percentage2, fill = Arrest)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes( label =percentage), color = "white")+
    scale_fill_manual(values = mycols) + 
    labs(title="Arrests percentage for narcotics")
  theme_void()
  
           ##C.3. crime battery

  ##crime percentage pie chart
  data5 <- sqldf("Select PrimaryType, Arrest, count(Arrest) as nfrequency 
                  from data 
                  where PrimaryType='BATTERY'
                  group by Arrest")
  View(data5)
  ##converting to percentage and adding to original dataframe
  
  percentage3 <- data5$nfrequency / sum(data5$nfrequency) * 100
  data5<-cbind(data5,percentage3)
  View(data5)
  ##convert arrest logical to character
  str(data5)
  data5$Arrest <- as.character(data5$Arrest)
  
  data5$percentage3 <-round(data5$percentage3, digits = 0)
  
  
  mycols <- c("#0073C2FF", "#EFC000FF")
  
  ggplot(data5, aes(x = "", y = percentage3, fill = Arrest)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes( label =percentage3), color = "white")+
    scale_fill_manual(values = mycols) + 
    labs(title="Arrests percentage for battery")
  theme_void()
  
  
  
        ##C.4. Criminal damage arrest percentage PIE CHART
        
  data6 <- sqldf("Select PrimaryType, Arrest, count(Arrest) as nfrequency 
                  from data 
                  where PrimaryType='CRIMINAL DAMAGE'
                  group by Arrest")
  View(data6)
       ##converting to percentage and adding to original dataframe
  
  percentage4 <- data6$nfrequency / sum(data6$nfrequency) * 100
  data6<-cbind(data6,percentage4)
  View(data6)
       ##convert arrest logical to character
  str(data6)
  data6$Arrest <- as.character(data6$Arrest)
  
  data6$percentage4 <-round(data6$percentage4, digits = 0)
  
  
  mycols <- c("#0073C2FF", "#EFC000FF")
  
  ggplot(data6, aes(x = "", y = percentage4, fill = Arrest)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes( label =percentage4), color = "white")+
    scale_fill_manual(values = mycols) + 
    labs(title="Arrests percentage for criminal damage")
  theme_void()
  
  
  
  
  ##D. PERFORMING TIME SERIES ANALYSIS
  
          ##fix date format of main data
  data$Date<-as.Date(data$Date, "%m/%d/%Y %H:%M:%S")
  t<-format(as.Date(data$Date), "%Y-%m")
  data<-cbind(data,t)   ##create date with month and year view
         ##change time from factor to Date before sql query 
str(data$t)
data$t<-anydate(data$t)
View(data$t)

         ##D.1. Theft Frequency based on Month and Year
theft <- sqldf ("Select  t, count(*) AS tfreq
                  from data 
                  where PrimaryType='THEFT'
                  group by t
                  order by t ASC")

         ##plotting general theft trend with years passing by
ggplot(theft, aes(x=theft$t,y=theft$tfreq)) + geom_line(col="#000080") +
  scale_x_date(date_labels = "%b-%Y",breaks = pretty(theft$t, n = 10)) + xlab("") + ylab("Monthly Theft")+theme_grey()


            ##D.2. Narcotics Frequency based on Month and Year
narcotics <- sqldf ("Select  t, count(*) AS tfreq
                  from data 
                  where PrimaryType='NARCOTICS'
                  group by t
                  order by t ASC")

          ##plotting general narcotics trend with years passing by
ggplot(narcotics, aes(x=narcotics$t,y=narcotics$tfreq)) + geom_line(col="#A9A9A9") +
  scale_x_date(date_labels = "%b-%Y",breaks = pretty(narcotics$t, n = 10)) + xlab("") + ylab("Monthly Narcotics")+theme_grey()


          ##D.3. Battery Frequency based on Month and Year
battery <- sqldf ("Select  t, count(*) AS tfreq
                  from data 
                  where PrimaryType='BATTERY'
                  group by t
                  order by t ASC")

          ##plotting general battery trend with years passing by
ggplot(battery, aes(x=battery$t,y=battery$tfreq)) + geom_line(col="#8B008B") +
  scale_x_date(date_labels = "%b-%Y",breaks = pretty(battery$t, n = 10)) + xlab("") + ylab("Monthly Battery")+theme_grey()


            ##D.4. Criminal Damage Frequency based on Month and Year
criminaldamage <- sqldf ("Select  t, count(*) AS tfreq
                  from data 
                  where PrimaryType='CRIMINAL DAMAGE'
                  group by t
                  order by t ASC")

         ##plotting general criminal damage trend with years passing by
ggplot(criminaldamage, aes(x=criminaldamage$t,y=criminaldamage$tfreq)) + geom_line(col="#000000") +
  scale_x_date(date_labels = "%b-%Y",breaks = pretty(criminaldamage$t, n = 10)) + xlab("") + ylab("Monthly Criminal Damage")+theme_grey()



##E.BAR CHART For each Top 4 Crimes Chosen find the top 5 locations
            
              ##E.1. theft top locations
theftloc = sqldf("Select LocationDescription as theftlocation, 
                  count(*) as frequency 
                  from data 
                  where PrimaryType='THEFT'
                   group by LocationDescription
                   order by frequency DESC
                limit 5")
ggplot(theftloc,aes(theftlocation,frequency))+geom_bar(stat='identity',fill="blue")


              ##E.2. narcotics top locations
narcoticsloc = sqldf("Select LocationDescription as narcoticslocation, 
                  count(*) as frequency 
                  from data 
                  where PrimaryType='NARCOTICS'
                   group by LocationDescription
                   order by frequency DESC
                limit 5")

ggplot(narcoticsloc,aes(narcoticslocation,frequency))+geom_bar(stat='identity',fill="blue")

            ##E.3. battery top locations
batteryloc = sqldf("Select LocationDescription as battery, 
                  count(*) as frequency 
                  from data 
                  where PrimaryType='BATTERY'
                   group by LocationDescription
                   order by frequency DESC
                limit 5")
ggplot(batteryloc,aes(battery,frequency))+geom_bar(stat='identity')

                ##E.4. criminal damage top locations
criminaldamageloc = sqldf("Select LocationDescription as criminaldamage, 
                  count(*) as frequency 
                  from data 
                  where PrimaryType='CRIMINAL DAMAGE'
                   group by LocationDescription
                   order by frequency DESC
                limit 5")
ggplot(criminaldamageloc,aes(criminaldamage,frequency))+geom_bar(stat='identity')



##F.Leaflet mapping points 

##F.0.adding districts of police

bnd <- readOGR("pd.geojson")
m<-leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>% 
  addPolygons(data=bnd, color="#690000", weight=1,)+
  addCircleMarkers(data = bnd, popup = popupTable(bnd))
m


##F.1. highest theft areas

maptheft = sqldf("Select Latitude, Longitude 
                  from data 
                  where Year=2016 and 
                  PrimaryType='THEFT' 
                  and LocationDescription='STREET' and 
                  Arrest=1")
m %>% addCircleMarkers(lng=maptheft$Longitude,lat=maptheft$Latitude,radius=0)

##F.2. highest criminal damages areas
mapcd = sqldf("Select Latitude, Longitude 
                  from data 
                  where Year=2016 and 
                  PrimaryType='CRIMINAL DAMAGE' 
                  and LocationDescription='STREET' and 
                  Arrest=1")
m %>% addCircleMarkers(lng=mapcd$Longitude,lat=mapcd$Latitude,radius=0,col="black")

##F.3. highest narcotics areas
mapnarc = sqldf("Select Latitude, Longitude 
                  from data 
                  where Year=2016 and 
                  PrimaryType='NARCOTICS' 
                  and LocationDescription='SIDEWALK' and 
                  Arrest=1")
m %>% addCircleMarkers(lng=mapnarc$Longitude,lat=mapnarc$Latitude,radius=0,col="purple")


##F.4. highest battery areas
mapBAT = sqldf("Select Latitude, Longitude 
                  from data 
                  where Year=2016 and 
                  PrimaryType='BATTERY' 
                  and LocationDescription='APARTMENT' and 
                  Arrest=1")
m %>% addCircleMarkers(lng=mapBAT$Longitude,lat=mapBAT$Latitude,radius=0,col="red")
