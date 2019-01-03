#Greg Kelly

library(readr)
library(lubridate)
library(tidyverse)
library(readxl)
library(plyr)




#gather the daily data for the years 2007 to 2016 to compare against the HIPE dataset
#setwd('..')
setwd('2007_2016 data for HIPE Analysis/')

#Balbriggan -------
files = dir(pattern = "Balbriggan")
list = lapply(files, read_csv)
Balbriggan = do.call(merge, list)

colnames(Balbriggan) <- paste("Balbriggan", colnames(Balbriggan), sep = "_")
colnames(Balbriggan)[1] = "Date"
is.Date(Balbriggan$Date)

#count nas
#map(Balbriggan, ~sum(is.na(.)))

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Balbriggan = Balbriggan[!duplicated(Balbriggan$Date),]


#Ballyfermot -------
files = dir(pattern = "Ballyfermot")
list = lapply(files, read_csv)
Ballyfermot = do.call(merge, list)

colnames(Ballyfermot) <- paste("Ballyfermot", colnames(Ballyfermot), sep = "_")
colnames(Ballyfermot)[1] = "Date"
is.Date(Ballyfermot$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Ballyfermot = Ballyfermot[!duplicated(Ballyfermot$Date),]


#count nas
#map(Ballyfermot, ~sum(is.na(.)))



#Blanchardstown -------
files = dir(pattern = "Blanchardstown")
list = lapply(files, read_csv)
Blanchardstown = do.call(merge, list)

colnames(Blanchardstown) <- paste("Blanchardstown", colnames(Blanchardstown), sep = "_")
colnames(Blanchardstown)[1] = "Date"
is.Date(Blanchardstown$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Blanchardstown = Blanchardstown[!duplicated(Blanchardstown$Date),]
#count nas
#map(Blanchardstown, ~sum(is.na(.)))




#Clonskeagh -------
files = dir(pattern = "Clonskeagh")
list = lapply(files, read_csv)
Clonskeagh = do.call(rbind, list)

colnames(Clonskeagh) <- paste("Clonskeagh", colnames(Clonskeagh), sep = "_")
colnames(Clonskeagh)[1] = "Date"
is.Date(Clonskeagh$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Clonskeagh = Clonskeagh[!duplicated(Clonskeagh$Date),]

#count nas
#map(Clonskeagh, ~sum(is.na(.)))


#Coleraine St -------
files = dir(pattern = "ColeraineSt")
list = lapply(files, read_csv)
ColeraineSt = do.call(merge, list)

colnames(ColeraineSt) <- paste("ColeraineSt", colnames(ColeraineSt), sep = "_")
colnames(ColeraineSt)[1] = "Date"
is.Date(ColeraineSt$Date)


#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
ColeraineSt = ColeraineSt[!duplicated(ColeraineSt$Date),]

#count nas
#map(ColeraineSt, ~sum(is.na(.)))


#Davitt Road -------
files = dir(pattern = "DavittRd")
list = lapply(files, read_csv)
DavittRd = do.call(rbind, list)

colnames(DavittRd) <- paste("DavittRd", colnames(DavittRd), sep = "_")
colnames(DavittRd)[1] = "Date"
is.Date(DavittRd$Date) 

DavittRd$Date = as.Date(DavittRd$Date, format = "%Y.%m.%d")

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
DavittRd = DavittRd[!duplicated(DavittRd$Date),]

#count nas
#map(DavittRd, ~sum(is.na(.)))


#DunLaoighaire -------
files = dir(pattern = "DunLaoighaire")
list = lapply(files, read_csv)
DunLaoighaire = do.call(merge, list)

colnames(DunLaoighaire) <- paste("DunLaoighaire", colnames(DunLaoighaire), sep = "_")
colnames(DunLaoighaire)[1] = "Date"
is.Date(DunLaoighaire$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
DunLaoighaire = DunLaoighaire[!duplicated(DunLaoighaire$Date),]

#count nas
#map(DunLaoighaire, ~sum(is.na(.)))


#Finglas -------
files = dir(pattern = "Finglas")
list = lapply(files, read_csv)
Finglas = do.call(rbind, list)

colnames(Finglas) <- paste("Finglas", colnames(Finglas), sep = "_")
colnames(Finglas)[1] = "Date"
is.Date(Finglas$Date)
Finglas$Date = as.Date(Finglas$Date, format = "%Y.%m.%d")

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Finglas = Finglas[!duplicated(Finglas$Date),]


#count nas
#map(Finglas, ~sum(is.na(.)))


#Knocklyon -------
files = dir(pattern = "Knocklyon")
list = lapply(files, read_csv)
Knocklyon = do.call(rbind, list)

colnames(Knocklyon) <- paste("Knocklyon", colnames(Knocklyon), sep = "_")
colnames(Knocklyon)[1] = "Date"
is.Date(Knocklyon$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Knocklyon = Knocklyon[!duplicated(Knocklyon$Date),]

#count nas
#map(Knocklyon, ~sum(is.na(.)))


#Marino -------
files = dir(pattern = "Marino")
list = lapply(files, read_csv)
Marino = do.call(rbind, list)

colnames(Marino) <- paste("Marino", colnames(Marino), sep = "_")
colnames(Marino)[1] = "Date"
is.Date(Marino$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Marino = Marino[!duplicated(Marino$Date),]

#count nas
#map(Marino, ~sum(is.na(.)))


#Phoenix Park -------
files = dir(pattern = "PhoenixPark")
list = lapply(files, read_csv)
PhoenixPark = do.call(rbind, list)

colnames(PhoenixPark) <- paste("PhoenixPark", colnames(PhoenixPark), sep = "_")
colnames(PhoenixPark)[1] = "Date"
is.Date(PhoenixPark$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
PhoenixPark = PhoenixPark[!duplicated(PhoenixPark$Date),]

#count nas
#map(PhoenixPark, ~sum(is.na(.)))



#Rathmines-------
files = dir(pattern = "*Rathmines(.*)daily")
list = lapply(files, read_csv)
Rathmines = do.call(merge, list[2:3])

colnames(Rathmines) <- paste("Rathmines", colnames(Rathmines), sep = "_")
colnames(Rathmines)[1] = "Date"
is.Date(Rathmines$Date)


#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Rathmines = Rathmines[!duplicated(Rathmines$Date),]

#count nas
#map(Rathmines, ~sum(is.na(.)))
#TODO fix the benzene file in the other script


#Ringsend -------
files = dir(pattern = "Ringsend")
list = lapply(files, read_csv)
Ringsend = do.call(merge, list)

colnames(Ringsend) <- paste("Ringsend", colnames(Ringsend), sep = "_")
colnames(Ringsend)[1] = "Date"
is.Date(Ringsend$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Ringsend = Ringsend[!duplicated(Ringsend$Date),]

#count nas
#map(Ringsend, ~sum(is.na(.)))




#Rosemount -------
#monthly
# files = dir(pattern = "Rosemount")
# list = lapply(files, read_csv)
# Rosemount = do.call(rbind, list)
# 
# colnames(Rosemount) <- paste("Rosemount", colnames(Rosemount), sep = "_")
# colnames(Rosemount)[1] = "Date"
# 
# #count nas
# #map(Rosemount, ~sum(is.na(.)))



#StAnnesPark -------
files = dir(pattern = "StAnnesPark")
list = lapply(files, read_csv)
StAnnesPark = do.call(merge, list)

colnames(StAnnesPark) <- paste("StAnnesPark", colnames(StAnnesPark), sep = "_")
colnames(StAnnesPark)[1] = "Date"
is.Date(StAnnesPark$Date)


#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
StAnnesPark = StAnnesPark[!duplicated(StAnnesPark$Date),]

#count nas
#map(StAnnesPark, ~sum(is.na(.)))




#Swords -------
files = dir(pattern = "Swords")
list = lapply(files, read_csv)
Swords = do.call(rbind, list)

colnames(Swords) <- paste("Swords", colnames(Swords), sep = "_")
colnames(Swords)[1] = "Date"
is.Date(Swords$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Swords = Swords[!duplicated(Swords$Date),]

#count nas
#map(Swords, ~sum(is.na(.)))


#Tallaght -------
files = dir(pattern = "Tallaght")
list = lapply(files, read_csv)
Tallaght = do.call(merge, list)

colnames(Tallaght) <- paste("Tallaght", colnames(Tallaght), sep = "_")
colnames(Tallaght)[1] = "Date"
is.Date(Tallaght$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
Tallaght = Tallaght[!duplicated(Tallaght$Date),]


#count nas
#map(Tallaght, ~sum(is.na(.)))


#WinetavernSt -------
files = dir(pattern = "WinetavernSt(.*)daily")
list = lapply(files, read_csv)
WinetavernSt = do.call(merge, list)

colnames(WinetavernSt) <- paste("WinetavernSt", colnames(WinetavernSt), sep = "_")
colnames(WinetavernSt)[1] = "Date"
is.Date(WinetavernSt$Date)

#remove the mulitples of the 1st of the years. a lot of the datasets atarted and ended with Jan 1st
WinetavernSt = WinetavernSt[!duplicated(WinetavernSt$Date),]

#count nas
#map(WinetavernSt, ~sum(is.na(.)))




#Dublin -------
#Dublin_Data merge everything
Dublin_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Balbriggan, Ballyfermot, Blanchardstown, Clonskeagh, ColeraineSt, DavittRd, DunLaoighaire, Finglas,
                                                                     Knocklyon, Marino, PhoenixPark, Rathmines, Ringsend, StAnnesPark, Swords, Tallaght, WinetavernSt))

#check if there are any duplicate rows
Dublin_daily = unique(Dublin_daily)

#sort columns alphabetically
Dublin_daily = Dublin_daily[ , order(names(Dublin_daily))]

#Move the date to the first column
Date = Dublin_daily$Date
Dublin_daily$Date = NULL
Dublin_daily = cbind(Date, Dublin_daily)

#another way to merge the data-full join
#test = list(Balbriggan, Ballyfermot, Blanchardstown, Clonskeagh, ColeraineSt, DavittRd, DunLaoighaire, Finglas,
#     Knocklyon, Marino, PhoenixPark, Rathmines, Ringsend, StAnnesPark, Swords, Tallaght, WinetavernSt) %>% reduce(full_join, by = "Date")


#count nas
#map(Dublin_2007_2016, ~sum(is.na(.)))


#split the data into individual years for visual analysis
Dublin_1996 = subset(Dublin_daily, Date >= "1996-01-01" & Date < "1997-01-01")
Dublin_1996 = unique( Dublin_1996 )

Dublin_1997 = subset(Dublin_daily, Date >= "1997-01-01" & Date < "1998-01-01")
Dublin_1997 = unique(Dublin_1997)

Dublin_1998 = subset(Dublin_daily, Date >= "1998-01-01" & Date < "1999-01-01")
Dublin_1998 = unique( Dublin_1998 )

Dublin_1999 = subset(Dublin_daily, Date >= "1999-01-01" & Date < "2000-01-01")
Dublin_1999 = unique( Dublin_1999 )

Dublin_2000 = subset(Dublin_daily, Date >= "2000-01-01" & Date < "2001-01-01")
Dublin_2000 = unique( Dublin_2000 )

Dublin_2001 = subset(Dublin_daily, Date >= "2001-01-01" & Date < "2002-01-01")
Dublin_2001 = unique( Dublin_2001 )

Dublin_2002 = subset(Dublin_daily, Date >= "2002-01-01" & Date < "2003-01-01")
Dublin_2002 = unique( Dublin_2002 )

Dublin_2003 = subset(Dublin_daily, Date >= "2003-01-01" & Date < "2004-01-01")
Dublin_2003 = unique( Dublin_2003 )

Dublin_2004 = subset(Dublin_daily, Date >= "2004-01-01" & Date < "2005-01-01")
Dublin_2004 = unique( Dublin_2004 )

Dublin_2005 = subset(Dublin_daily, Date >= "2005-01-01" & Date < "2006-01-01")
Dublin_2005 = unique( Dublin_2005 )

Dublin_2006 = subset(Dublin_daily, Date >= "2006-01-01" & Date < "2007-01-01")
Dublin_2006 = unique( Dublin_2006 )


#available HIPE data for these years
Dublin_2007 = subset(Dublin_daily, Date >= "2007-01-01" & Date < "2008-01-01")
Dublin_2007 = unique( Dublin_2007 )

Dublin_2008 = subset(Dublin_daily, Date >= "2008-01-01" & Date < "2009-01-01")
Dublin_2008 = unique(Dublin_2008)

Dublin_2009 = subset(Dublin_daily, Date >= "2009-01-01" & Date < "2010-01-01")
Dublin_2009 = unique(Dublin_2009)

Dublin_2010 = subset(Dublin_daily, Date >= "2010-01-01" & Date < "2011-01-01")
Dublin_2010 = unique(Dublin_2010)

Dublin_2011 = subset(Dublin_daily, Date >= "2011-01-01" & Date < "2012-01-01")
Dublin_2011 = unique(Dublin_2011)

Dublin_2012 = subset(Dublin_daily, Date >= "2012-01-01" & Date < "2013-01-01")
Dublin_2012 = unique(Dublin_2012)

Dublin_2013 = subset(Dublin_daily, Date >= "2013-01-01" & Date < "2014-01-01")
Dublin_2013 = unique(Dublin_2013)

#2013 has duplicate rows that the unique function did not clean up so removing the rows manaully
Dublin_2013 = Dublin_2013[-c(2:8), ]

Dublin_2014 = subset(Dublin_daily, Date >= "2014-01-01" & Date < "2015-01-01")
Dublin_2014 = unique(Dublin_2014)

Dublin_2015 = subset(Dublin_daily, Date >= "2015-01-01" & Date < "2016-01-01")
Dublin_2015 = unique(Dublin_2015)

Dublin_2016 = subset(Dublin_daily, Date >= "2016-01-01" & Date < "2017-01-01")
Dublin_2016 = unique(Dublin_2016)


#save over dublin daily data file with new fixed cleaner data
Dublin_daily = rbind(Dublin_1996, Dublin_1997, Dublin_1998, Dublin_2001, Dublin_2002, 
                     Dublin_2003, Dublin_2004, Dublin_2005, Dublin_2006, Dublin_2007, Dublin_2008, Dublin_2009, 
                     Dublin_2010, Dublin_2011, Dublin_2012, Dublin_2013, Dublin_2014, Dublin_2015, Dublin_2016)

#save the gathered data
write_csv(Dublin_daily, "../Analysis/Dublin_daily_all_data.csv")


#get min, max and mean of all the data
df = Dublin_daily

#make all columns numeric except Date
df[, 2:169] = sapply(df[,  2:169], as.numeric)

#summary(df)
#colnames(df)
#dim((df))

# #make the values below 0 NAs. this is faulty data caused my uncalibrated sensors
df[df < 0] = NA

#change the column name of Rathmines_PM25 to Rathmines_PM2.5 for averages
colnames(df)[which( colnames(df)=="Rathmines_PM25" )] = "Rathmines_PM2.5"


#determine the mean of the different type of pollutants in Dublin across the different regions
df$Benzene_mean <- rowMeans(df[,grep("Benzene",names(df))], na.rm = TRUE)
df$CO_mean <- rowMeans(df[,grep("CO_Mean",names(df))], na.rm = TRUE)
df$CO_min <- rowMeans(df[,grep("CO_Min",names(df))], na.rm = TRUE)
df$NO_mean <- rowMeans(df[,grep("NO_Mean",names(df))], na.rm = TRUE)
df$NO_min <- rowMeans(df[,grep("NO_Min",names(df))], na.rm = TRUE)
df$NO2_mean <- rowMeans(df[,grep("NO2_Mean",names(df))], na.rm = TRUE)
df$NO2_min <- rowMeans(df[,grep("NO2_Min",names(df))], na.rm = TRUE)
df$NOx_mean <- rowMeans(df[,grep("NOx_Mean",names(df))], na.rm = TRUE)
df$NOx_min <- rowMeans(df[,grep("NOx_Min",names(df))], na.rm = TRUE)
df$SO2_mean <- rowMeans(df[,grep("SO2_Mean",names(df))], na.rm = TRUE)
df$SO2_min <- rowMeans(df[,grep("SO2_Min",names(df))], na.rm = TRUE)
df$PM10 <- rowMeans(df[,grep("PM10",names(df))], na.rm = TRUE)
df$PM25 <- rowMeans(df[,grep("PM2.5",names(df))], na.rm = TRUE)
df$ozone_mean <- rowMeans(df[,grep("ozone_Mean",names(df))], na.rm = TRUE)
df$ozone_min <- rowMeans(df[,grep("ozone_Min",names(df))], na.rm = TRUE)
df$Toluene <- rowMeans(df[,grep("Toluene",names(df))], na.rm = TRUE)


#determine the Max Values across Dublin for pollutants with this data
Max = df[,grep("CO_Max",names(df))]
df$CO_max = apply(Max, 1, max, na.rm = TRUE)

Max = df[,grep("NO_Max",names(df))]
df$NO_max <- rowMeans(df[,grep("NO_Max",names(df))], na.rm = TRUE)

Max = df[,grep("NO2_Max",names(df))]
df$NO2_max <- rowMeans(df[,grep("NO2_Max",names(df))], na.rm = TRUE)

Max = df[,grep("NOx_Max",names(df))]
df$NOx_max <- rowMeans(df[,grep("NOx_Max",names(df))], na.rm = TRUE)

Max = df[,grep("SO2_Max",names(df))]
df$SO2_max <- rowMeans(df[,grep("SO2_Max",names(df))], na.rm = TRUE)

Max = df[,grep("ozone_Max",names(df))]
df$ozone_max <- rowMeans(df[,grep("ozone_Max",names(df))], na.rm = TRUE)

#remove different area data keeping the min, max, mean values
df2 = df[, -c(2:169)]


#save the data
write_csv(df2, "../Analysis/Dublin_all_data_MMM_SuitableForAnalysis.csv")







#remove data before 2007
Dublin_2007_2016 = Dublin_daily[Dublin_daily[["Date"]] >= "2007-01-01" , ]


#check nas % of data missing
sum(is.na(Dublin_2007_2016))/prod(dim(Dublin_2007_2016))

#count nas per column
sapply(Dublin_2007_2016, function(x) sum(is.na(x)))

#percentage of nas per column
apply(Dublin_2007_2016, 2, function(col)sum(is.na(col))/length(col))

#save the 2007-2018 data
write_csv(Dublin_2007_2016, "../Analysis/Dublin_2007_2016_all_data.csv")








#HIPE HEART DATA------------------
HIPE = read_excel("HIPE_Data_nopw.xlsx")

#subset the data to alter the date format. move all of the columns underneath the first column
HIPE2 = HIPE[c(3:12)]
HIPE2 = data.frame(Date=unlist(HIPE2, use.names = FALSE))

#remove all rows with 0- this is Feb 29th data for every year, searching for and removing rows with 0 didnt work properly
HIPE2 = HIPE2[-c(60, 792, 1158, 1524, 2256, 2622, 2988), ]
HIPE2 = as.data.frame(HIPE2)

#merge the HIPE 2 data with the CO data
Date = data.frame(seq(from = as.Date("2007-01-01"), to = as.Date("2016-12-31"), by = 'day'))
HIPE2 = cbind(Date, HIPE2)
colnames(HIPE2)[1] = "Date"
colnames(HIPE2)[2] = "Cardiovascular_Cases"

HIPE_Heart = HIPE2






#HIPE Respiratory DATA------------------
HIPE = read_excel("HIPE_Data_nopw.xlsx", sheet = "Respiratory")

#subset the data to alter the date format. move all of the columns underneath the first column
HIPE2 = HIPE[c(3:12)]
HIPE2 = data.frame(Date=unlist(HIPE2, use.names = FALSE))

#remove all rows with 0 as a value this is Feb 29th data for every year, searching for and removing rows with 0 didnt work properly
HIPE2 = HIPE2[-c(60, 792, 1158, 1524, 2256, 2622, 2988), ]
HIPE2 = as.data.frame(HIPE2)

#merge the HIPE 2 data with the Cardio data data
Date = data.frame(seq(from = as.Date("2007-01-01"), to = as.Date("2016-12-31"), by = 'day'))
HIPE2 = cbind(Date, HIPE2)
colnames(HIPE2)[1] = "Date"
colnames(HIPE2)[2] = "Respiratory_Cases"

HIPE_Respiratory = HIPE2

HIPE = merge(HIPE_Heart, HIPE_Respiratory)


#save the newly formatted HIPE data
write_csv(HIPE, "../Analysis/HIPE.csv")







#Preparing the 2007-2016 data files for analysis---------------

df = Dublin_2007_2016

#clean the environment of everything except df
rm(list=setdiff(ls(), "df"))


#make all columns numeric except Date
df[, 2:169] = sapply(df[,  2:169], as.numeric)


#summary(df)
#colnames(df)


# #make the values below 0 NAs. this is faulty data caused my uncalibrated sensors
df[df < 0] = NA

#change the column name of Rathmines_PM25 to Rathmines_PM2.5 for averages
colnames(df)[which( colnames(df)=="Rathmines_PM25" )] = "Rathmines_PM2.5"



#determine the mean of the different type of pollutants in Dublin across the different regions
df$Benzene_mean <- rowMeans(df[,grep("Benzene",names(df))], na.rm = TRUE)
df$CO_mean <- rowMeans(df[,grep("CO_Mean",names(df))], na.rm = TRUE)
df$CO_min <- rowMeans(df[,grep("CO_Min",names(df))], na.rm = TRUE)
df$NO_mean <- rowMeans(df[,grep("NO_Mean",names(df))], na.rm = TRUE)
df$NO_min <- rowMeans(df[,grep("NO_Min",names(df))], na.rm = TRUE)
df$NO2_mean <- rowMeans(df[,grep("NO2_Mean",names(df))], na.rm = TRUE)
df$NO2_min <- rowMeans(df[,grep("NO2_Min",names(df))], na.rm = TRUE)
df$NOx_mean <- rowMeans(df[,grep("NOx_Mean",names(df))], na.rm = TRUE)
df$NOx_min <- rowMeans(df[,grep("NOx_Min",names(df))], na.rm = TRUE)
df$SO2_mean <- rowMeans(df[,grep("SO2_Mean",names(df))], na.rm = TRUE)
df$SO2_min <- rowMeans(df[,grep("SO2_Min",names(df))], na.rm = TRUE)
df$PM10 <- rowMeans(df[,grep("PM10",names(df))], na.rm = TRUE)
df$PM25 <- rowMeans(df[,grep("PM2.5",names(df))], na.rm = TRUE)
df$ozone_mean <- rowMeans(df[,grep("ozone_Mean",names(df))], na.rm = TRUE)
df$ozone_min <- rowMeans(df[,grep("ozone_Min",names(df))], na.rm = TRUE)
df$Toluene <- rowMeans(df[,grep("Toluene",names(df))], na.rm = TRUE)



#determine the Max Values across Dublin for pollutants with this data
Max = df[,grep("CO_Max",names(df))]
df$CO_max = apply(Max, 1, max, na.rm = TRUE)

Max = df[,grep("NO_Max",names(df))]
df$NO_max <- rowMeans(df[,grep("NO_Max",names(df))], na.rm = TRUE)

Max = df[,grep("NO2_Max",names(df))]
df$NO2_max <- rowMeans(df[,grep("NO2_Max",names(df))], na.rm = TRUE)

Max = df[,grep("NOx_Max",names(df))]
df$NOx_max <- rowMeans(df[,grep("NOx_Max",names(df))], na.rm = TRUE)

Max = df[,grep("SO2_Max",names(df))]
df$SO2_max <- rowMeans(df[,grep("SO2_Max",names(df))], na.rm = TRUE)

Max = df[,grep("ozone_Max",names(df))]
df$ozone_max <- rowMeans(df[,grep("ozone_Max",names(df))], na.rm = TRUE)



#save the 2007-2018 data
write_csv(df, "../Analysis/Dublin_2007_2016_all_data_including_MMM.csv")



#remove the different region polluants and keep the date, the average values across dublin
which( colnames(df)=="Benzene_mean" )
which( colnames(df)=="ozone_max" )

df2 = df[, -c(2:169)]

#save the 2007-2018 averages and max data
write_csv(df2, "../Analysis/Dublin_2007_2016_MMM_SuitableForAnalysis.csv")


#clean the enviroment
rm(list=ls())
