#Greg Kelly

#libraries used in this script
library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(rio)
library(dplyr)
library(plyr)


#change the working directory setwd('..') brings you out of that folder, up one directory level
#getwd()
#setwd("DublinAQData/")

#read in all files from a directory
#files <- list.files()

# ASHTOWNGROVE DAILY ----
setwd("AshtownGrove/")

#read in the text files
Dublin_AshtownGrove_PM10_1996 <- read_table2("Dublin_AshtownGrove_PM10_1996.txt")
Dublin_AshtownGrove_PM10_1997 <- read_table2("Dublin_AshtownGrove_PM10_1997.txt")

#bind the 2 datasets
PM10 = rbind(Dublin_AshtownGrove_PM10_1996, Dublin_AshtownGrove_PM10_1997)

#remove potential headers after binding the data
PM10 = PM10[- grep("PM10", PM10$`PM10(ug/m3)` ),]

#change the date column so R recognises it as a date column
library(lubridate)

#check if R sees it as a date
#sapply(PM10$Date, is.Date)

#set date column as date
PM10$Date <- as.Date(PM10$Date, format="%d/%m/%Y")

#quick look at the plot
#plot(PM10)

#save the output in a created Folder hourly data in main project directory
write_csv(PM10, "../Gathered_Data/Dublin_AshtownGrove_PM10_ugm3_daily.csv")


#clean the enviroment
rm(list=ls())








#BALBRIGGAN DAILY--------------------------------------------------------------------------------

#change directory to balbriggan, change into the daily folder
setwd('..')
setwd("Balbriggan/")
setwd("daily/")

#search the directory and join by the type of pollutants
benzene_files = dir(pattern = "*Benzene")
PM10_files = dir(pattern = "*PM10")
toluene_files = dir(pattern = "*Toluene")

#bind the PM10 files chronologically
PM10_list = lapply(PM10_files, read_xlsx)
Balbriggan_PM10 = do.call(rbind, PM10_list)

#remove rows with headings rather than values in them after the row bind
Balbriggan_PM10 = Balbriggan_PM10[- grep("ug/m3", Balbriggan_PM10$PM10),]

#check the plot
#plot(Balbriggan_PM10, type = "o")

#balbriggan benzene
benzene_list = lapply(benzene_files, read_xlsx)
Balbriggan_Benzene = do.call(rbind, benzene_list)

#balbriggan toluene
toluene_list  = lapply(toluene_files , read_xlsx)
Balbriggan_Toluene = do.call(rbind, toluene_list)

#merge the data
Balbriggan_daily = merge(Balbriggan_PM10, Balbriggan_Benzene, by = "Date", all = TRUE)
Balbriggan_daily = merge(Balbriggan_daily, Balbriggan_Toluene, by = "Date", all = TRUE)

#remove rows of data that could be headers
Balbriggan_daily = Balbriggan_daily[- grep("ug/m3", Balbriggan_daily$Benzene ),]

#remove hours from data
Balbriggan_daily$Date = as.Date(Balbriggan_daily$Date,format='%Y-%m-%d %H')

#save file to clean data folder
write_csv(Balbriggan_daily, "../../Gathered_Data/Dublin_Balbriggan_Benzene_Toluene_PM10_daily.csv")

#clean the enviroment
rm(list=ls())






#BALBRIGGAN HOURLY------------------------------------------
setwd("../../Balbriggan/")
setwd("hourly/")

#search the directory and join by the type of pollutants
benzene_files = dir(pattern = "*Benzene")
CO_files = dir(pattern = "*CO")
NOx_files = dir(pattern = "*NOx")
SO2_files = dir(pattern = "*SO2")
toluene_files = dir(pattern = "*Toluene")

#bind benzene files together
benzene_list = lapply(benzene_files, read_xlsx)
Balbriggan_Benzene = do.call(rbind, benzene_list)

#bind the CO files chronologically
CO_list = lapply(CO_files, read_xlsx)
Balbriggan_CO = do.call(rbind, CO_list)

#balbriggan NOx
NOx_list  = lapply(NOx_files , read_xlsx)
Balbriggan_NOx = do.call(rbind, NOx_list)

#balbriggan SO2
SO2_list  = lapply(SO2_files , read_xlsx)
Balbriggan_SO2 = do.call(rbind, SO2_list)

#remove rows with headings rather than values in them after the row bind
Balbriggan_CO = Balbriggan_CO[- grep("mg/m3", Balbriggan_CO$CO),]
Balbriggan_NOx = Balbriggan_NOx[- grep("ug/m3", Balbriggan_NOx$NOx),]
Balbriggan_SO2 = Balbriggan_SO2[- grep("ug/m3", Balbriggan_SO2$SO2),]

#Combine the date and the time for the different pollutants to help with graphing
Balbriggan_CO$Date <- with(Balbriggan_CO, as.POSIXct(paste(Balbriggan_CO$Date, Balbriggan_CO$Time), format="%Y-%m-%d %H"))
Balbriggan_CO$Time = NULL

Balbriggan_NOx$Date <- with(Balbriggan_NOx, as.POSIXct(paste(Balbriggan_NOx$Date, Balbriggan_NOx$Time), format="%Y-%m-%d %H"))
Balbriggan_NOx$Time = NULL

Balbriggan_SO2$Date <- with(Balbriggan_SO2, as.POSIXct(paste(Balbriggan_SO2$Date, Balbriggan_SO2$Time), format="%Y-%m-%d %H"))
Balbriggan_SO2$Time = NULL


#check the plot
#plot(Balbriggan_CO)

#calculate min, max and mean for hourly data
#CO
Balbriggan_CO$CO = as.numeric(Balbriggan_CO$CO)
mean = aggregate(Balbriggan_CO[names(Balbriggan_CO)!='Date'], list(hour=cut(Balbriggan_CO$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "CO_Mean"

min = aggregate(Balbriggan_CO[names(Balbriggan_CO)!='Date'], list(hour=cut(Balbriggan_CO$Date,'day')), min, na.rm=F)
colnames(min)[2] = "CO_Min"

max = aggregate(Balbriggan_CO[names(Balbriggan_CO)!='Date'], list(hour=cut(Balbriggan_CO$Date,'day')), max, na.rm=F)
colnames(max)[2] = "CO_Max"

Balbriggan_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#NOx
Balbriggan_NOx$NOx = as.numeric(Balbriggan_NOx$NOx)
Balbriggan_NOx$NO = as.numeric(Balbriggan_NOx$NO)
Balbriggan_NOx$NO2 = as.numeric(Balbriggan_NOx$NO2)

mean = aggregate(Balbriggan_NOx[names(Balbriggan_NOx)!='Date'], list(hour=cut(Balbriggan_NOx$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "NOx_Mean"
colnames(mean)[3] = "NO_Mean"
colnames(mean)[4] = "NO2_Mean"

min = aggregate(Balbriggan_NOx[names(Balbriggan_NOx)!='Date'], list(hour=cut(Balbriggan_NOx$Date,'day')), min, na.rm=F)
colnames(min)[2] = "NOx_Min"
colnames(min)[3] = "NO_Min"
colnames(min)[4] = "NO2_Min"

max = aggregate(Balbriggan_NOx[names(Balbriggan_NOx)!='Date'], list(hour=cut(Balbriggan_NOx$Date,'day')), max, na.rm=F)
colnames(max)[2] = "NOx_Max"
colnames(max)[3] = "NO_Max"
colnames(max)[4] = "NO2_Max"

#merge the data with the CO data
Balbriggan_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Balbriggan_CO_NOx_SO2_hr_MMM_daily,mean,min,max))


#SO2
Balbriggan_SO2$SO2 = as.numeric(Balbriggan_SO2$SO2)
mean = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "SO2_Mean"

min = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), min, na.rm=F)
colnames(min)[2] = "SO2_Min"

max = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), max, na.rm=F)
colnames(max)[2] = "SO2_Max"


#merge the data with existing data
Balbriggan_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Balbriggan_CO_NOx_SO2_hr_MMM_daily,mean,min,max))

#change column name to date
colnames(Balbriggan_CO_NOx_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
Balbriggan_CO_NOx_SO2_hr_MMM_daily$Date = as.Date(Balbriggan_CO_NOx_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Balbriggan_CO_NOx_SO2_hr_MMM_daily, "../../Gathered_Data/Dublin_Balbriggan_CO_NOx_SO2_hr_MMM_daily.csv")


#merge the hoourly data
Balbriggan_hourly = merge(Balbriggan_CO, Balbriggan_NOx, by = "Date", all = TRUE)
Balbriggan_hourly = merge(Balbriggan_hourly, Balbriggan_SO2, by = "Date", all = TRUE)

#write out as a csv file into the directory
write_csv(Balbriggan_hourly, "../../Gathered_Data/Dublin_Balbriggan_CO_NOx_SO2_hr.csv")

#clean the enviroment
rm(list=ls())







#BALLYFERMOT DAILY--------------------------------------------------------------------------------
setwd("../../Ballyfermot/")
setwd("daily/")

#for the daily files I manually opened them in excel and changed them from xlx or txt files to xlxs files
PM10_files = dir(pattern = "*PM10")

#bind the PM10 files chronologically
PM10_list = lapply(PM10_files, read_xlsx)
PM10 = do.call(rbind, PM10_list)

#remove any rows with ug/m3 in them
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#plot(PM10)


#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')


#save the data
write_csv(PM10, "../../Gathered_Data/Dublin_Ballyfermot_PM10_ugm3_daily.csv")


#clean the enviroment
rm(list=ls())






#BALLYFERMOT HOURLY--------------------------------------------------------------------------------
setwd("Ballyfermot/")
setwd("hourly/")

#using the library rio to convert txt files types to .csv file types
#already converted commenting out code for testing
# library(rio)
# txt <- dir(pattern = "txt")
# created <- mapply(convert, txt, gsub("txt", "csv", txt))
# unlink(txt) # delete txt files
# 
# #change xlsx files to csv
# xlsx <- dir(pattern = "xlsx")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)
# 
# #change xls files to csv files
# xls <- dir(pattern = "xls")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xlsx)
# PM10_files



#search for ppb NOX files using logical OR statement
NOx_ppb_files = dir(pattern = "NOx.*ppb|ppb.*NOx")

#NOx ppb files conversion
NOx_ppb_list = lapply(NOx_ppb_files, read_csv)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
#is.numeric(NOx_ppb$NO2)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#convert NOX from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)


#search for ugm3 NOX files using logical OR statement
NOx_files = dir(pattern = "NOx.*ugm3|ugm3.*NOx")
NOx_list = lapply(NOx_files, read_csv)
NOx = do.call(rbind, NOx_list)

#clear rows with ppb written in them
NOx = NOx[- grep("ug/m3", NOx$NO2),]

#combine both type of files into one
NOx = rbind(NOx,NOx_ppb)



#SO2
#remove columns created from txt conversion to csv. rename the column to be the same as the ugm3 files. save the file replacing existing one
#completed, commented out for testing
# Dublin_Ballyfermot_SO2_2006_ugm3_hr <- read_csv("Dublin_Ballyfermot_SO2_2006_ugm3_hr.csv")
# Dublin_Ballyfermot_SO2_2006_ugm3_hr = Dublin_Ballyfermot_SO2_2006_ugm3_hr[,-c(4:6)]
# names(Dublin_Ballyfermot_SO2_2006_ugm3_hr)[3]<-"SO2"
# 
# #Combine the date and the time columns
# Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date = as.Date(Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date, format = "%d/%m/%Y" )
# Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date <- with(Dublin_Ballyfermot_SO2_2006_ugm3_hr, as.POSIXct(paste(Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date, Dublin_Ballyfermot_SO2_2006_ugm3_hr$Time), format="%Y-%m-%d %H"))
# 
# #Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date = as.Date(Dublin_Ballyfermot_SO2_2006_ugm3_hr$Date,format='%Y-%m-%d %H')
# write_csv(Dublin_Ballyfermot_SO2_2006_ugm3_hr, "Dublin_Ballyfermot_SO2_2006_ugm3_hr.csv")



#search for ugm3 NOX files using logical OR statement
SO2_files = dir(pattern = "SO2.*ugm3|ugm3.*SO2")
SO2_list = lapply(SO2_files, read_csv)

#rename columns for binding
SO2_list <- lapply(SO2_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )

#bind the SO2 ugm3 data
SO2 = do.call(rbind, SO2_list)


#search for ppb SO2 files using logical OR statement
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_csv)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#convert SO2 ppb data to ugm3 data for consistency
SO2_ppb$SO2 = as.numeric(SO2_ppb$SO2)
SO2_ppb$SO2 = SO2_ppb$SO2 * (64/22.41)


#bind SO2 data
SO2 = rbind(SO2, SO2_ppb)

#clean possible headings
#SO2 = SO2[- grep("ug/m3", SO2$SO2),]
#SO2 = SO2[- grep("ppb", SO2$SO2),]

#merge the SO2 and NOx data files for Ballyfermot
Ballyfermot_NOx_SO2_hr = merge(NOx,SO2)

#sort time
Ballyfermot_NOx_SO2_hr$Date <- with(Ballyfermot_NOx_SO2_hr, as.POSIXct(paste(Ballyfermot_NOx_SO2_hr$Date, Ballyfermot_NOx_SO2_hr$Time), format="%Y-%m-%d %H"))

#Combine the date and the time for the different pollutants to help with graphing
Ballyfermot_NOx_SO2_hr$Date <- with(Ballyfermot_NOx_SO2_hr, as.POSIXct(paste(Ballyfermot_NOx_SO2_hr$Date, Ballyfermot_NOx_SO2_hr$Time), format="%Y-%m-%d %H"))
Ballyfermot_NOx_SO2_hr$Time = NULL

#save the new ugm3 files in the same directory and delete the old files
write_csv(Ballyfermot_NOx_SO2_hr, "../../Gathered_Data/Dublin_Ballyfermot_NOx_SO2_hr.csv")



#calculate min, max and mean for hourly data
#NOx
NOx$NOx = as.numeric(NOx$NOx)
NOx$NO = as.numeric(NOx$NO)
NOx$NO2 = as.numeric(NOx$NO2)

#atomic vector error workaround
NOx_ave = NOx

#Combine the date and the time
NOx_ave$Date <- with(NOx_ave, as.POSIXct(paste(NOx_ave$Date, NOx_ave$Time), format="%Y-%m-%d %H"))
NOx_ave$Time = NULL

NOx = NOx_ave

mean = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))

min = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))

max = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))

#merge the data with the CO data
Ballyfermot_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))


#SO2
Balbriggan_SO2$SO2 = as.numeric(Balbriggan_SO2$SO2)
mean = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "SO2_Mean"

min = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), min, na.rm=F)
colnames(min)[2] = "SO2_Min"

max = aggregate(Balbriggan_SO2[names(Balbriggan_SO2)!='Date'], list(hour=cut(Balbriggan_SO2$Date,'day')), max, na.rm=F)
colnames(max)[2] = "SO2_Max"



#merge the SO2 data with existing data
Ballyfermot_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Ballyfermot_NOx_SO2_hr_MMM_daily,mean,min,max))


#change column name to date
colnames(Ballyfermot_NOx_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
Ballyfermot_NOx_SO2_hr_MMM_daily$Date = as.Date(Ballyfermot_NOx_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Ballyfermot_NOx_SO2_hr_MMM_daily, "../../Gathered_Data/Dublin_Ballyfermot_NOx_SO2_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())





#BLANCHARDSTOWN DAILY--------------------------------------------------------------------------------
setwd('..')
setwd('..')
setwd("Blanchardstown/daily")

#convert xls files to csv files and delete the xls files. convert all of the data types to csv to help import files better
#csv was chosen as R seems to like it and could not find a way to convert an xls file to xlxs
# xls <- dir(pattern = "(.*)xls$")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xls)
# 
# #convert xlsx files to csv files and delete the xlsx files
# xlsx <- dir(pattern = "(.*)xlsx$")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)


#search for bind the PM10 files chronologically
PM10_files = dir(pattern = "*PM10")
PM10_list = lapply(PM10_files, read_csv)
PM10 = do.call(rbind, PM10_list)

#plot(PM10)

#clear rows with ug/m3 written in them
PM10 = PM10[- grep("ug/m3", PM10$PM10),]


#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')

#save the daily data for Blanchardstown
write_csv(PM10, "../../Gathered_Data/Dublin_Blanchardstown_PM10_daily.csv")





#BLANCHARDSTOWN HOURLY--------------------------------------------------------------------------------
setwd("../hourly")

#NOx ppb files conversion
ppb_NOx = dir(pattern = "ppb")
NOx_ppb_list = lapply(ppb_NOx, read_csv)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
is.numeric(NOx_ppb$NO2)

#convert NOX from ppb to ugm3. molecular weight is 46 formula is ppb x moleucular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#remove the pbb files
unlink(ppb_NOx)

#bind the NOx files chronologically
NOx_files = dir(pattern = "*NOx")
NOx_list = lapply(NOx_files, read_csv)
NOx = do.call(rbind, NOx_list)

#clean the data of headings
NOx = NOx[- grep("ug/m3", NOx$NOx),]

#atomic vector error so putting it into a new dataframe
Blanchardstown_NOx_hr = NOx

#sort time
Blanchardstown_NOx_hr$Date <- with(Blanchardstown_NOx_hr, as.POSIXct(paste(Blanchardstown_NOx_hr$Date, Blanchardstown_NOx_hr$Time), format="%Y-%m-%d %H"))
Blanchardstown_NOx_hr$Time = NULL

#save the output
write.csv(Blanchardstown_NOx_hr, file = "../../Gathered_Data/Dublin_Blanchardstown_NOx_hr.csv")



#determine mean,max, min for the hourly dataset
NOx = Blanchardstown_NOx_hr

#convert all the NO columns from strings to numerical values for calculations
NOx[, 2:4] <- sapply(NOx[,  2:4], as.numeric)


mean = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))

min = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))

max = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))


#merge the data
Blanchardstown_NOx_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))


#change column name to date
colnames(Blanchardstown_NOx_MMM_daily)[1] = "Date"

#remove hours from data
Blanchardstown_NOx_MMM_daily$Date = as.Date(Blanchardstown_NOx_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Blanchardstown_NOx_MMM_daily, "../../Gathered_Data/Dublin_Blanchardstown_NOx_MMM_daily.csv")


#clean the enviroment
rm(list=ls())




#CITY COUNCIL --------------------------------------------------------------------------------
setwd('..')
setwd('..')
setwd("CityCouncil/")


#Smoke
#bind the Smoke files 
Files = dir(pattern = "Smoke")
List = lapply(Files, read_xls)
smoke = do.call(rbind.fill, List)

#change Location column to Date
colnames(smoke)[1] = "Date"

#need to convert numeric excel date to standard date format
library(janitor)
smoke$Date = excel_numeric_to_date(as.numeric(as.character(smoke$Date)), date_system = "modern")


#merge columns that are duplicated due to header name changes
smoke$BrunswickSt <- ifelse(is.na(smoke$`BRUNSWICK ST`), smoke$`BRUNSWICK ST.`, smoke$`BRUNSWICK ST`)
smoke$`BRUNSWICK ST` = NULL
smoke$`BRUNSWICK ST.`= NULL


smoke$RDS2 <- ifelse(is.na(smoke$R.D.S), smoke$RDS, smoke$R.D.S)
smoke$R.D.S = NULL
smoke$RDS = NULL

smoke$HerbertSt <- ifelse(is.na(smoke$`HERBERT ST`), smoke$`HERBERT ST.` , smoke$`HERBERT ST`)
smoke$`HERBERT ST` = NULL
smoke$`HERBERT ST.` = NULL

smoke$OldCountyRd <- ifelse(is.na(smoke$`OLD COUNTY RD`), smoke$`OLD COUNTRY ROAD` , smoke$`OLD COUNTY RD`)
smoke$`OLD COUNTY RD` = NULL
smoke$`OLD COUNTRY ROAD` = NULL

#count nas per column
#map(smoke, ~sum(is.na(.)))

#clean the data of headings
smoke = smoke[- grep("ugm3", smoke$OldCountyRd),]


#add SO2 to all road names
colnames(smoke) <- paste(colnames(smoke), "smoke", sep = "_")
colnames(smoke)[1] = "Date"

#plot(smoke, typle = "l")




#SO2
#bind the Pb files 
Files = dir(pattern = "SO2Bubbler")
List = lapply(Files, read_xls)
SO2 = do.call(rbind.fill, List)

#change Location column to Date
colnames(SO2)[1] = "Date"

#need to convert numeric excel date to standard date format
library(janitor)
SO2$Date = excel_numeric_to_date(as.numeric(as.character(SO2$Date)), date_system = "modern")


#merge columns that are duplicated due to header name changes
SO2$BrunswickSt <- ifelse(is.na(SO2$`BRUNSWICK ST`), SO2$`BRUNSWICK ST.`, SO2$`BRUNSWICK ST`)
SO2$`BRUNSWICK ST` = NULL
SO2$`BRUNSWICK ST.`= NULL


SO2$RDS2 <- ifelse(is.na(SO2$R.D.S), SO2$RDS, SO2$R.D.S)
SO2$R.D.S = NULL
SO2$RDS = NULL

SO2$HerbertSt <- ifelse(is.na(SO2$`HERBERT ST`), SO2$`HERBERT ST.` , SO2$`HERBERT ST`)
SO2$`HERBERT ST` = NULL
SO2$`HERBERT ST.` = NULL

SO2$OldCountyRd <- ifelse(is.na(SO2$`OLD COUNTY RD`), SO2$`OLD COUNTRY ROAD` , SO2$`OLD COUNTY RD`)
SO2$`OLD COUNTY RD` = NULL
SO2$`OLD COUNTRY ROAD` = NULL

#sort columns alphabetically to make sure there are no duplicate street names
SO2[ , order(names(SO2))]

#add SO2 to all road names
colnames(SO2) <- paste(colnames(SO2), "SO2", sep = "_")
colnames(SO2)[1] = "Date"

#clean the data of headings
SO2 = SO2[- grep("ugm3", SO2$OldCountyRd),]


#merge datasets
Dublin_CityCouncil_Old_Smoke_SO2_daily = merge(smoke, SO2)

#save the data
write.csv(Dublin_CityCouncil_Old_Smoke_SO2_daily, file = "../Gathered_Data/Dublin_CityCouncil_Old_Smoke_SO2_daily.csv")



#CLONSKEAGH HOURLY --------------------------------------------------------------------------------
setwd('..')
setwd("Clonskeagh/")

#find ppb files
O3_ppb_files = dir(pattern = "O3.*ppb|ppb.*O3")
O3_ppb_list = lapply(O3_ppb_files, read_csv)
O3_ppb = do.call(rbind, O3_ppb_list)

#clear rows with ppb written in them
O3_ppb = O3_ppb[- grep("ppb", O3_ppb$ozone),]

#convert all the O3 columns from strings to numerical values for calculations
O3_ppb[, 3] <- sapply(O3_ppb[,3], as.numeric)
is.numeric(O3_ppb$ozone)

#convert O3 from ppb to ugm3. molecular weight is 48. formula is ppb x moleucular weight/22.41
O3_ppb$ozone =O3_ppb$ozone * (48/22.41)


#convert mgm3 to ugm3 for 2008 file
#Dublin_Clonskeagh_O3_2008 <- read_csv("Dublin_Clonskeagh_O3_2008.csv")
#Dublin_Clonskeagh_O3_2008$ozone = as.numeric(Dublin_Clonskeagh_O3_2008$ozone)/1000

#convert xls files to csv files and delete the xls files
#steps already completed
# xls <- dir(pattern = "(.*)xls$")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xls)
# 
# #convert xlsx files to csv files and delete the xlsx files
# xlsx <- dir(pattern = "(.*)xlsx$")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)

#search for bind the PM10 files
O3_files = dir(pattern = "O3.*ugm3|ugm3.*O3")
O3_list = lapply(O3_files, read_csv)
O3 = do.call(rbind, O3_list)

#combine converted ppb files
O3 = rbind(O3,O3_ppb)

#comibine date and hour columnsß
O3$Date <- with(O3, as.POSIXct(paste(O3$Date, O3$Time), format="%Y-%m-%d %H"))
O3$Time = NULL

#remove any rows with ug/m3 in them
O3 = O3[- grep("ug/m3", O3$ozone),]
O3 = O3[- grep("ugm-3", O3$ozone),]
O3 = O3[- grep("mg/m3", O3$ozone),]

#see if it looks ok
#plot(O3)

#save the data
write_csv(O3, "../Gathered_Data/Dublin_Clonskeagh_ozone_hr.csv")



#calculate the mean, max and min of ozone
O3$ozone = as.numeric(O3$ozone)
O3$Date = as.Date(O3$Date, format = "%Y-%m-%d")
mean = aggregate(O3[names(O3)!='Date'], list(hour=cut(O3$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("ozone", "ozone_Mean", names(mean))


min = aggregate(O3[names(O3)!='Date'], list(hour=cut(O3$Date,'day')), min, na.rm=F)
names(min) <- gsub("ozone", "ozone_Min", names(min))


max = aggregate(O3[names(O3)!='Date'], list(hour=cut(O3$Date,'day')), max, na.rm=F)
names(max) <- gsub("ozone", "ozone_Max", names(max))


#remove hours from data
# min$hour = as.Date(min$hour,format='%Y-%m-%d %H')
# max$hour = as.Date(max$hour,format='%Y-%m-%d %H')
# mean$hour = as.Date(mean$hour,format='%Y-%m-%d %H')

#merge the data
Clonskeagh_ozone_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))


#change column name to date
colnames(Clonskeagh_ozone_MMM_daily)[1] = "Date"

#remove hours from data
Clonskeagh_ozone_MMM_daily$Date = as.Date(Clonskeagh_ozone_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Clonskeagh_ozone_MMM_daily, "../Gathered_Data/Dublin_Clonskeagh_ozone_MMM_daily.csv")

#clean the enviroment
rm(list=ls())







#CLONTARF  --------------------------------------------------------------------------------
setwd('..')
setwd("Clontarf/")

#import the data using whitespace to seperate the columns
Dublin_Clontarf_PM10_ugm3_daily <- read_table2("Dublin_Clontarf_PM10_1996.txt")

#not much data present so it is unusable

#clean the enviroment
rm(list=ls())







#COLERAINE STREET HOURLY--------------------------------------------------------------------------------
setwd('..')
setwd("ColeraineStreet/hourly")

#convert xls files to csv files and delete the xls files
# xls <- dir(pattern = "(.*)xls$")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xls)
# 
# #convert xlsx files to csv files and delete the xlsx files
# xlsx <- dir(pattern = "(.*)xlsx$")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)
# 
# #convert txt files to csv files and delete the txt files
# txt <- dir(pattern = "(.*)txt$")
# created <- mapply(convert, txt, gsub("txt", "csv", txt))
# unlink(txt)


#CO
#search for and bind the CO files chronologically. combine the time and date columns into one column
CO_files = dir(pattern = "CO.*mgm3|mgm3.*CO")
CO_list = lapply(CO_files, read_csv)
CO = do.call(rbind, CO_list)

#clear old headers
CO = CO[- grep("mg/m3", CO$CO ),]

#import the ppm files and convert to mg/m3
CO_ppm_files = dir(pattern = "CO.*ppm|ppm.*CO")
CO_ppm_list = lapply(CO_ppm_files, read_csv)
CO_ppm = do.call(rbind, CO_ppm_list)

#clear old headers
CO_ppm = CO_ppm[- grep("ppm", CO_ppm$CO ),]

#convert ppm to mgm3. CO molecular weight is 28. convert from a tring to numerical values
CO_ppm$CO = as.numeric(CO_ppm$CO)
CO_ppm$CO = CO_ppm$CO * (28/22.41)

#bind the 2 CO files
CO = rbind(CO,CO_ppm)



#NOx

#NOx ppb files conversion
ppb_NOx = dir(pattern = "NOx.*ppb|ppb.*NOx")
NOx_ppb_list = lapply(ppb_NOx, read_csv)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
#is.numeric(NOx_ppb$NO2)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#convert NOX from ppb to ugm3
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)

#NOx ugm3 files
NOx_files = dir(pattern = "*NOx.*ugm3|ugm3.*NOx")
NOx_list = lapply(NOx_files, read_csv)
NOx = do.call(rbind, NOx_list)

#clean the data of headings
NOx = NOx[- grep("ug/m3", NOx$NOx),]

#bind converted ppb files and ugm3 files
NOx = rbind(NOx, NOx_ppb)

#plot(NOx)





#SO2
#some file preperation required / needed to run this code only once, commented out for testing purpouses
# Dublin_ColeraineSt_SO2_2006_ugm3_hr <- read_csv("Dublin_ColeraineSt_SO2_2006_ugm3_hr.csv")
# Dublin_ColeraineSt_SO2_2006_ugm3_hr = as.data.frame(subset(Dublin_ColeraineSt_SO2_2006_ugm3_hr, select=-c(V4,V5,V6)))
# colnames(Dublin_ColeraineSt_SO2_2006_ugm3_hr)[colnames(Dublin_ColeraineSt_SO2_2006_ugm3_hr) == 'SO2(ug/m3)'] <- 'SO2'
# write.csv(Dublin_ColeraineSt_SO2_2006_ugm3_hr, file = "Dublin_ColeraineSt_SO2_2006_ugm3_hr.csv", row.names=FALSE)

#search for ugm3 SO2 files using logical OR statement
SO2_files = dir(pattern = "SO2.*ugm3|ugm3.*SO2")
SO2_list = lapply(SO2_files, read_csv)

#rename columns for binding. Changing columns labelled Hour to newer Time format used by the EPA. change SO2 columns to the same heading
SO2_list <- lapply(SO2_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )

#bind the SO2 ugm3 data
SO2 = do.call(rbind, SO2_list)

#search for ppb SO2 files using logical OR statement
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_csv)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#bind SO2 data
SO2 = rbind(SO2, SO2_ppb)

#clean possible headings
SO2 = SO2[- grep("ug/m3", SO2$SO2),]
SO2 = SO2[- grep("ppb", SO2$SO2),]

#merge the SO2 and NOx data files for Ballyfermot
ColeraineSt_CO_NOx_SO2_hr = merge(CO,NOx)
ColeraineSt_CO_NOx_SO2_hr = merge(ColeraineSt_CO_NOx_SO2_hr,SO2)

#combine date and hour and write to directory
ColeraineSt_CO_NOx_SO2_hr$Date <- with(ColeraineSt_CO_NOx_SO2_hr, as.POSIXct(paste(ColeraineSt_CO_NOx_SO2_hr$Date, ColeraineSt_CO_NOx_SO2_hr$Time), format="%Y-%m-%d %H"))
ColeraineSt_CO_NOx_SO2_hr$Time = NULL
write_csv(ColeraineSt_CO_NOx_SO2_hr, "../../Gathered_Data/Dublin_ColeraineSt_CO_NOx_SO2_hr.csv")



#calculate min, max and mean for hourly data
#CO
#combine date and hour columns. atomic vector error workaround
CO_ave = CO
CO_ave$Date <- with(CO_ave, as.POSIXct(paste(CO_ave$Date, CO_ave$Time), format="%Y-%m-%d %H"))
CO_ave$Time = NULL
CO = CO_ave


CO$CO = as.numeric(CO$CO)
mean = aggregate(CO[names(CO)!='Date'], list(hour=cut(CO$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "CO_Mean"

min = aggregate(CO[names(CO)!='Date'], list(hour=cut(CO$Date,'day')), min, na.rm=F)
colnames(min)[2] = "CO_Min"

max = aggregate(CO[names(CO)!='Date'], list(hour=cut(CO$Date,'day')), max, na.rm=F)
colnames(max)[2] = "CO_Max"

ColeraineStreet_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))




#NOx
#combine date and hour columns. atomic vector error workaround
NOx_ave = NOx
NOx_ave$Date <- with(NOx_ave, as.POSIXct(paste(NOx_ave$Date, NOx_ave$Time), format="%Y-%m-%d %H"))
NOx_ave$Time = NULL
NOx = NOx_ave

#change from string to numeric values
NOx$NOx = as.numeric(NOx$NOx)
NOx$NO = as.numeric(NOx$NO)
NOx$NO2 = as.numeric(NOx$NO2)

mean = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))

min = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))

max = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))


#merge the data with the CO data
ColeraineStreet_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(ColeraineStreet_CO_NOx_SO2_hr_MMM_daily,mean,min,max))


#SO2
#combine date and hour columns. atomic vector error workaround
SO2_ave = SO2
SO2_ave$Date <- with(SO2_ave, as.POSIXct(paste(SO2_ave$Date, SO2_ave$Time), format="%Y-%m-%d %H"))
SO2_ave$Time = NULL
SO2 = SO2_ave


SO2$SO2 = as.numeric(SO2$SO2)
mean = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), mean, na.rm=F)
colnames(mean)[2] = "SO2_Mean"

min = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), min, na.rm=F)
colnames(min)[2] = "SO2_Min"

max = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), max, na.rm=F)
colnames(max)[2] = "SO2_Max"


#merge the data with existing data
ColeraineStreet_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(ColeraineStreet_CO_NOx_SO2_hr_MMM_daily,mean,min,max))

#change column name to date
colnames(ColeraineStreet_CO_NOx_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
ColeraineStreet_CO_NOx_SO2_hr_MMM_daily$Date = as.Date(ColeraineStreet_CO_NOx_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(ColeraineStreet_CO_NOx_SO2_hr_MMM_daily, "../../Gathered_Data/Dublin_ColeraineSt_CO_NOx_SO2_hr_MMM_daily.csv")


#plot(ColeraineSt_CO_NOx_SO2_hr)

#clean the enviroment
rm(list=ls())



#COLERAINE STREET DAILY--------------------------------------------------------------------------------
#Lead - Pb
setwd('../daily')

#convert xls files to csv files and delete the xls files
#commeneted out as it has already been completed
# xls <- dir(pattern = "(.*)xls$")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xls)
# 
# #convert xlsx files to csv files and delete the xlsx files
# xlsx <- dir(pattern = "(.*)xlsx$")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)
# 
# #convert txt files to csv files and delete the txt files
# txt <- dir(pattern = "(.*)txt$")
# created <- mapply(convert, txt, gsub("txt", "csv", txt))
# unlink(txt)



#Pb
#bind the Pb files 
Pb_files = dir(pattern = "*Pb")
Pb_list = lapply(Pb_files, read_csv)
Pb = do.call(rbind, Pb_list)

#clean the data of headers
Pb = Pb[- grep("ug/m3", Pb$Pb),]


#bind the PM2.5 files 
PM25_files = dir(pattern = "*PM25")
PM25_list = lapply(PM25_files, read_csv)
PM25 = do.call(rbind, PM25_list)

#clean the data of headers
PM25 = PM25[- grep("ug/m3", PM25$PM2.5),]


#commented out for testing code, only needed to run this code once
#PM10
#some files need tidying up ie reorganinsing and head name changes
# Dublin_ColeraineSt_PM10_2004_ugm3 <- read_csv("Dublin_ColeraineSt_PM10_2004_ugm3.csv")
# Dublin_ColeraineSt_PM10_2004_ugm3$`PM10(ug/m3)` = NULL
# names(Dublin_ColeraineSt_PM10_2004_ugm3)[names(Dublin_ColeraineSt_PM10_2004_ugm3) == "Date"] = "PM10"
# names(Dublin_ColeraineSt_PM10_2004_ugm3)[names(Dublin_ColeraineSt_PM10_2004_ugm3) == "V1"] = "Date"
# 
# #change the date format for binding
# Dublin_ColeraineSt_PM10_2004_ugm3$Date = parse_date_time(Dublin_ColeraineSt_PM10_2004_ugm3$Date, c('dmy', 'ymd'))
# 
# write.csv(Dublin_ColeraineSt_PM10_2004_ugm3, file = "Dublin_ColeraineSt_PM10_2004_ugm3.csv", row.names=FALSE)
# 
# #PM10_2005 file
# Dublin_ColeraineSt_PM10_2005_ugm3 <- read_csv("Dublin_ColeraineSt_PM10_2005_ugm3.csv")
# Dublin_ColeraineSt_PM10_2005_ugm3$`PM10(ug/m3)` = NULL
# names(Dublin_ColeraineSt_PM10_2005_ugm3)[names(Dublin_ColeraineSt_PM10_2005_ugm3) == "Date"] = "PM10"
# names(Dublin_ColeraineSt_PM10_2005_ugm3)[names(Dublin_ColeraineSt_PM10_2005_ugm3) == "V1"] = "Date"
# Dublin_ColeraineSt_PM10_2005_ugm3$Date = parse_date_time(Dublin_ColeraineSt_PM10_2005_ugm3$Date, c('dmy', 'ymd'))
# write.csv(Dublin_ColeraineSt_PM10_2005_ugm3, file = "Dublin_ColeraineSt_PM10_2005_ugm3.csv", row.names=FALSE)
# 
# #PM10_2006 file
# Dublin_ColeraineSt_PM10_2006_ugm3 <- read_csv("Dublin_ColeraineSt_PM10_2006_ugm3.csv")
# Dublin_ColeraineSt_PM10_2006_ugm3 <- Dublin_ColeraineSt_PM10_2006_ugm3[, -c(3:4)]
# colnames(Dublin_ColeraineSt_PM10_2006_ugm3)[2] = "PM10"
# Dublin_ColeraineSt_PM10_2006_ugm3$Date = parse_date_time(Dublin_ColeraineSt_PM10_2006_ugm3$Date, c('dmy', 'ymd'))
# write.csv(Dublin_ColeraineSt_PM10_2006_ugm3, file = "Dublin_ColeraineSt_PM10_2006_ugm3.csv", row.names=FALSE)

#bind the PM10 files 
PM10_files = dir(pattern = "*PM10")
PM10_list = lapply(PM10_files, read_csv)
PM10 = do.call(rbind, PM10_list)

#clean the data of headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]
PM10 = PM10[- grep("ugm3", PM10$PM10),]

#set all date columns to time so r can merge the files
PM10$Date = as.Date(PM10$Date)
PM25$Date = as.Date(PM25$Date)
Pb$Date = as.Date(Pb$Date)

#merge daily data
Dublin_ColeraineSt_Pb_PM10_PM25_daily = merge(PM10, Pb)
Dublin_ColeraineSt_Pb_PM10_PM25_daily = merge(Dublin_ColeraineSt_Pb_PM10_PM25_daily, PM25, by = "Date", all = TRUE)

#save the cleaned dateframe
write.csv(Dublin_ColeraineSt_Pb_PM10_PM25_daily, file = "../../Gathered_Data/Dublin_ColeraineSt_Pb_PM10_PM25_daily.csv", row.names=FALSE)

#clean the enviroment
rm(list=ls())



#COLLEGE GREEN DAILY--------------------------------------------------------------------------------
setwd('..')
setwd('..')
setwd("CollegeGreen/")

#read in PM10 files
PM10_files = dir(pattern = "PM10.*txt")
PM10_list = lapply(PM10_files, read_table2)
PM10 = do.call(rbind, PM10_list)

#change column name for binding
colnames(PM10)[2] = "PM10"

#make sure R recognises Date column from txt files as a date column for proper binding
PM10$Date = as.Date(PM10$Date, format="%d/%m/%Y")

#import xls files
PM10_files = dir(pattern = "PM10.*xls")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#make sure R recognises Date column from txt files as a date column for proper binding
PM10_xls$Date = as.Date(PM10_xls$Date, format="%Y/%m/%d")

#bind the text files and the xls file together. clean the data of headers
PM10 = rbind(PM10, PM10_xls)
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#save the data
write.csv(PM10, file = "../Gathered_Data/Dublin_CollegeGreen_PM10_daily.csv", row.names=FALSE)

#clean the enviroment
rm(list=ls())





#COUNTY COUNCIL --------------------------------------------------------------------------------
setwd('..')
setwd("CountyCouncil/")



#Smoke
#bind the smoke files 
Files = dir(pattern = "Smoke")
List = lapply(Files, read_xls)
smoke = do.call(rbind.fill, List)

#change Location column to Date
colnames(smoke)[1] = "Date"

#need to convert numeric excel date to standard date format
library(janitor)
smoke$Date = excel_numeric_to_date(as.numeric(as.character(smoke$Date)), date_system = "modern")

#sort columns alphabetically to make sure there are no duplicate street names
test = smoke[ , order(names(smoke))]

#merge columns that are duplicated due to header name changes
smoke$Avonbeg <- ifelse(is.na(smoke$ABEG), smoke$AVONBEG, smoke$ABEG)
smoke$ABEG = NULL
smoke$AVONBEG= NULL


smoke$Balbriggan <- ifelse(is.na(smoke$BALBRIGGAN), smoke$BBGAN, smoke$BALBRIGGAN)
smoke$BALBRIGGAN = NULL
smoke$BBGAN = NULL


smoke$Brookfield <- ifelse(is.na(smoke$BROOKFIELD ), smoke$BROOK , smoke$BROOKFIELD)
smoke$BROOKFIELD = NULL
smoke$BROOK = NULL


smoke$MountAnville <- ifelse(is.na(smoke$`MOUNT ANVIL` ), smoke$`MOUNT ANVILLE` , smoke$`MOUNT ANVIL`)
smoke$`MOUNT ANVIL`  = NULL
smoke$`MOUNT ANVILLE` = NULL

smoke$QuarryVale <- ifelse(is.na(smoke$QVALE ), smoke$QUARRYVALE , smoke$QVALE)
smoke$QVALE  = NULL
smoke$QUARRYVALE = NULL

smoke$QuarryVale = ifelse(is.na(smoke$QuarryVale), smoke$QUARYVALE, smoke$QuarryVale)
smoke$QUARYVALE = NULL

#count nas per column
map(smoke, ~sum(is.na(.)))

#clean the data of headings
smoke = smoke[- grep("ugm3", smoke$DUNLAOIRE),]


#add smoke to all road names
colnames(smoke) <- paste(colnames(smoke), "smoke", sep = "_")
colnames(smoke)[1] = "Date"

#plot(smoke, typle = "l")


#SO2
#bind the Pb files 
Files = dir(pattern = "SO2Bubbler")
List = lapply(Files, read_xls)
SO2 = do.call(rbind.fill, List)

#change Location column to Date
colnames(SO2)[1] = "Date"

#need to convert numeric excel date to standard date format
library(janitor)
SO2$Date = excel_numeric_to_date(as.numeric(as.character(SO2$Date)), date_system = "modern")

#sort columns alphabetically to make sure there are no duplicate street names
#test = SO2[ , order(names(SO2))]

#merge columns that are duplicated due to header name changes
SO2$MountAnville <- ifelse(is.na(SO2$`MOUNT ANVIL` ), SO2$`MOUNT ANVILLE` , SO2$`MOUNT ANVIL`)
SO2$`MOUNT ANVIL`  = NULL
SO2$`MOUNT ANVILLE` = NULL


#add SO2 to all road names
colnames(SO2) <- paste(colnames(SO2), "SO2", sep = "_")
colnames(SO2)[1] = "Date"

#clean the data of headings
SO2 = SO2[- grep("ugm3", SO2$DUNLAOIRE_SO2),]


#merge datasets
Dublin_CountyCouncil_Old_Smoke_SO2_daily = merge(smoke, SO2)

#save the data
write.csv(Dublin_CountyCouncil_Old_Smoke_SO2_daily, file = "../Gathered_Data/Dublin_CountyCouncil_Old_Smoke_SO2_daily_daily.csv")



#CRUMLIN HOURLY--------------------------------------------------------------------------------
setwd('..')
setwd("Crumlin/")

#import benzene files
benzene_files = dir(pattern = "*Benzene")
benzene_list = lapply(benzene_files, read_xls)
Benzene = do.call(rbind, benzene_list)

Benzene_hr = Benzene

#combine date and hour columns
Benzene_hr$Date <- with(Benzene_hr, as.POSIXct(paste(Benzene_hr$Date, Benzene_hr$Hour), format="%Y-%m-%d %H"))
Benzene_hr$Hour = NULL



#search for ppb NOX files using logical OR statement
NOx_ppb_files = dir(pattern = "NOx.*ppb|ppb.*NOx")

#NOx ppb files conversion
NOx_ppb_list = lapply(NOx_ppb_files, read_xls)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
#is.numeric(NOx_ppb$NO2)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#convert NOX from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)


#NOx files import
NOx_files = dir(pattern = "NOx.*ugm3")
NOx_list = lapply(NOx_files, read_xls)
NOx = do.call(rbind, NOx_list)

#clear rows with ppb written in them
NOx = NOx[- grep("ug/m3", NOx$NO2),]

#change NO2 to numeric format for binding
NOx$NO2 = as.numeric(NOx$NO2)

#rowbind data with missing columns
library(data.table)
NOx = bind_rows(NOx, NOx_ppb)

#avoid atomic vector errors
NOx_hr = NOx

#combine date and hour columns
NOx_hr$Date <- with(NOx_hr, as.POSIXct(paste(NOx_hr$Date, NOx_hr$Hour), format="%Y-%m-%d %H"))
NOx_hr$Hour = NULL

#merge the datasets
Crumlin_Benzene_NOx_ugm3_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(NOx_hr, Benzene_hr))

#clean the data
Crumlin_Benzene_NOx_ugm3_hr = Crumlin_Benzene_NOx_ugm3_hr[- grep("ug/m3", Crumlin_Benzene_NOx_ugm3_hr$Benzene),]


#save the data
write_csv(Crumlin_Benzene_NOx_ugm3_hr, "../Gathered_Data/Dublin_Crumlin_Benzene_NOx_ugm3_hr.csv")


#NOx
#change from string to numeric values
Crumlin_Benzene_NOx_ugm3_hr$NOx = as.numeric(Crumlin_Benzene_NOx_ugm3_hr$NOx)
Crumlin_Benzene_NOx_ugm3_hr$NO = as.numeric(Crumlin_Benzene_NOx_ugm3_hr$NO)
Crumlin_Benzene_NOx_ugm3_hr$NO2 = as.numeric(Crumlin_Benzene_NOx_ugm3_hr$NO2)
Crumlin_Benzene_NOx_ugm3_hr$Benzene = as.numeric(Crumlin_Benzene_NOx_ugm3_hr$Benzene)


#Mean min and max
mean = aggregate(Crumlin_Benzene_NOx_ugm3_hr[names(Crumlin_Benzene_NOx_ugm3_hr)!='Date'], list(hour=cut(Crumlin_Benzene_NOx_ugm3_hr$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("Benzene", "Benzene_Mean", names(mean))

min = aggregate(Crumlin_Benzene_NOx_ugm3_hr[names(Crumlin_Benzene_NOx_ugm3_hr)!='Date'], list(hour=cut(Crumlin_Benzene_NOx_ugm3_hr$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("Benzene", "Benzene_Min", names(min))

max = aggregate(Crumlin_Benzene_NOx_ugm3_hr[names(Crumlin_Benzene_NOx_ugm3_hr)!='Date'], list(hour=cut(Crumlin_Benzene_NOx_ugm3_hr$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("Benzene", "Benzene_Max", names(max))


#merge the data
Crumlin_Benzene_NOx_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Crumlin_Benzene_NOx_hr_MMM_daily)[1] = "Date"

#remove hours from data
Crumlin_Benzene_NOx_hr_MMM_daily$Date = as.Date(Crumlin_Benzene_NOx_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Crumlin_Benzene_NOx_hr_MMM_daily, "../Gathered_Data/Dublin_Crumlin_Benzene_NOx_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())








#DAVITT ROAD DAILY --------------------------------------------------------------------------------
setwd('..')
setwd("DavittRd/")

#bind the PM10 files 
PM10_files = dir(pattern = "*PM10")
PM10_list = lapply(PM10_files, read_xlsx)
PM10 = do.call(rbind, PM10_list)

#remove headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#swap year and date in the time column
PM10$Date = parse_date_time(PM10$Date, c('dmy', 'ymd'))

#save the data
write_csv(PM10,"../Gathered_Data/Dublin_DavittRd_PM10_ugm3_daily.csv")

#clean the enviroment
rm(list=ls())






#DUNLAOIGHAIRE HOURLY--------------------------------------------------------------------------------
setwd('..')
setwd("DunLaoighaire/")

# #convert xls files to csv files and delete the xls files
#already run so commenting out
# xls <- dir(pattern = "(.*)xls$")
# created <- mapply(convert, xls, gsub("xls", "csv", xls))
# unlink(xls)
# 
# #convert xlsx files to csv files and delete the xlsx files
# xlsx <- dir(pattern = "(.*)xlsx$")
# created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))
# unlink(xlsx)



#NOx hourly data
#search for ppb NOX files using logical OR statement for conversion
NOx_ppb_files = dir(pattern = "NOx.*ppb|ppb.*NOx")
NOx_ppb_list = lapply(NOx_ppb_files, read_csv)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#convert NOX from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)

#search for ugm3 NOX files using logical OR statement
NOx_files = dir(pattern = "NOx.*ugm3|ugm3.*NOx")
NOx_list = lapply(NOx_files, read_csv)
NOx = do.call(rbind, NOx_list)

#clear rows with ppb written in them
NOx = NOx[- grep("ug/m3", NOx$NO2),]

#combine both type of files into one
NOx_final = rbind(NOx,NOx_ppb)

#Combine the date and the time for the different pollutants to help with graphing
NOx_final$Date <- with(NOx, as.POSIXct(paste(NOx_final$Date, NOx_final$Time), format="%Y-%m-%d %H"))
NOx_final$Time = NULL


#save the data
write_csv(NOx_final,"../Gathered_Data/Dublin_DunLaoighaire_NOx_ugm3_hr.csv")



#calculate min/max/mean for NOx
#change from string to numeric values
NOx = NOx_final
NOx$NOx = as.numeric(NOx$NOx)
NOx$NO = as.numeric(NOx$NO)
NOx$NO2 = as.numeric(NOx$NO2)

mean = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))

min = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))

max = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))


#merge the data with the CO data
Dunlaoighaire_NOx_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dunlaoighaire_NOx_hr_MMM_daily)[1] = "Date"

#remove hours from data
Dunlaoighaire_NOx_hr_MMM_daily$Date = as.Date(Dunlaoighaire_NOx_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dunlaoighaire_NOx_hr_MMM_daily, "../Gathered_Data/Dublin_DunLaoighaire_NOx_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())






#DUNLAOIGHAIRE DAILY--------------------------------------------------------------------------------

#PM10 daily data
#bind the PM10 files 
PM10_files = dir(pattern = "*PM10")
PM10_list = lapply(PM10_files, read_csv)
PM10 = do.call(rbind, PM10_list)

#remove headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]


#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')


#save the data
write_csv(PM10, "../Gathered_Data/Dublin_DunLaoighaire_PM10_ugm3_daily.csv")

#clean the enviroment
rm(list=ls())









#FINGLAS DAILY --------------------------------------------------------------------------------
setwd('..')
setwd("Finglas/")

#search for ugm3 PM25 files using logical OR statement
PM25_files = dir(pattern = "PM25")
PM25_list = lapply(PM25_files, read_xlsx)
PM25 = do.call(rbind, PM25_list)

#remove headers
PM25 = PM25[- grep("ug/m3", PM25$PM2.5),]

#save the data
write_csv(PM25, "../Gathered_Data/Dublin_Finglas_PM25_ugm3_daily.csv")

#clean the enviroment
rm(list=ls())






#KILBARRACK --------------------------------------------------------------------------------
setwd('..')
setwd("Kilbarrack/")

#search for ugm3 Pb files using logical OR statement
Pb_files = dir(pattern = "Pb")
Pb_list = lapply(Pb_files, read_xls)
Pb = do.call(rbind, Pb_list)

#remove headers
Pb = Pb[- grep("ug/m3", Pb$Pb),]

#save the data
write_csv(Pb, "../Gathered_Data/Dublin_Kilbarrack_Pb_ugm3_daily.csv")

#clean the enviroment
rm(list=ls())





#KNOCKLYON DAILY--------------------------------------------------------------------------------
setwd('..')
setwd("Knocklyon/")

#daily data
#Arsenic, As data and remove header
As <- read_excel("Dublin_Knocklyon_As_2008_ngm3_day.xls")
As = As[- grep("ng/m3", As$As),]

#Cadmium, Cd import and clean header data
Cd <- read_excel("Dublin_Knocklyon_Cd_2008_ngm3_day.xls")
Cd = Cd[- grep("ng/m3", Cd$Cd),]

#Nickel, Ni data and remove header
Ni <- read_excel("Dublin_Knocklyon_Ni_2008_ngm3_day.xls")
Ni = Ni[- grep("ng/m3", Ni$Ni),]

#import lead data, Pb, and remove the headers
Pb <- read_excel("Dublin_Knocklyon_Pb_2008_ugm3_day.xls")
Pb = Pb[- grep("ug/m3", Pb$Pb),]

#import Pm10 data and remove the headers
PM10 <- read_excel("Dublin_Knocklyon_PM10_2008_ugm3_day.xls")
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#merge daily data
Dublin_Knocklyon_As_Cd_Ni_Pb_PM10_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(As,Cd,Ni,Pb,PM10))

#save the data
write_csv(Dublin_Knocklyon_As_Cd_Ni_Pb_PM10_daily, "../Gathered_Data/Dublin_Knocklyon_As_Cd_Ni_Pb_PM10_daily.csv")


#KNOCKLYON HOURLY--------------------------------------------------------------------------------


#CO
#search for and bind the CO files
CO_files = dir(pattern = "CO.*mgm3|mgm3.*CO")
CO_list = lapply(CO_files, read_xls)
CO = do.call(rbind, CO_list)

#clear old headers
CO = CO[- grep("mg/m3", CO$CO ),]

#import the ppm files and convert to mg/m3
CO_ppm_files = dir(pattern = "CO.*ppm|ppm.*CO")
CO_ppm_list = lapply(CO_ppm_files, read_xls)
CO_ppm = do.call(rbind, CO_ppm_list)

#clear old headers
CO_ppm = CO_ppm[- grep("ppm", CO_ppm$CO ),]

#convert ppm to mgm3. CO molecular weight is 28. convert from a string to numerical values
CO_ppm$CO = as.numeric(CO_ppm$CO)
CO_ppm$CO = CO_ppm$CO * (28/22.41)

#bind the 2 CO files
CO = rbind(CO,CO_ppm)


#NOx data, import and convert from ppb to ugm3
#search for ppb NOX files using logical OR statement for conversion
NOx_ppb_files = dir(pattern = "NOx.*ppb|ppb.*NOx")
NOx_ppb_list = lapply(NOx_ppb_files, read_xls)
NOx_ppb = do.call(rbind, NOx_ppb_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)

#convert NO from ppb to ugm3. molecular weight is 30. formula is ppb x moleucular weight/22.41
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)

#convert NO2 from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#convert NOX from ppb to ugm3. molecular weight is 46. formula is ppb x moleucular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)

NOx = NOx_ppb



#SO2
#search for ugm3 SO2 files using logical OR statement
SO2_files = dir(pattern = "SO2.*ugm3|ugm3.*SO2")
SO2_list = lapply(SO2_files, read_xls)
SO2 = do.call(rbind, SO2_list)

#search for ppb SO2 files using logical OR statement
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_xls)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#convert SO2 ppb data to ugm3 data for consistency
SO2_ppb$SO2 = as.numeric(SO2_ppb$SO2)
SO2_ppb$SO2 = SO2_ppb$SO2 * (64/22.41)

#bind SO2 data
SO2 = rbind(SO2, SO2_ppb)


#combine CO, NOx, SO2
#Dublin_Knocklyon_CO_NOx_SO2_hr = merge(CO, NOx)
#Dublin_Knocklyon_CO_NOx_SO2_hr = merge(Dublin_Knocklyon_CO_NOx_SO2_hr, SO2)

Dublin_Knocklyon_CO_NOx_SO2_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(CO, NOx, SO2))

#bind the time and the date columns
Dublin_Knocklyon_CO_NOx_SO2_hr$Date <- with(Dublin_Knocklyon_CO_NOx_SO2_hr, as.POSIXct(paste(Dublin_Knocklyon_CO_NOx_SO2_hr$Date, Dublin_Knocklyon_CO_NOx_SO2_hr$Time), format="%Y-%m-%d %H"))
Dublin_Knocklyon_CO_NOx_SO2_hr$Time = NULL

#save the data
write_csv(Dublin_Knocklyon_CO_NOx_SO2_hr, "../Gathered_Data/Dublin_Knocklyon_CO_NOx_SO2_hr.csv")


#calculate min/max/mean for the data
#change from string to numeric values
Dublin_Knocklyon_CO_NOx_SO2_hr$NOx = as.numeric(Dublin_Knocklyon_CO_NOx_SO2_hr$NOx)
Dublin_Knocklyon_CO_NOx_SO2_hr$NO = as.numeric(Dublin_Knocklyon_CO_NOx_SO2_hr$NO)
Dublin_Knocklyon_CO_NOx_SO2_hr$NO2 = as.numeric(Dublin_Knocklyon_CO_NOx_SO2_hr$NO2)
Dublin_Knocklyon_CO_NOx_SO2_hr$CO = as.numeric(Dublin_Knocklyon_CO_NOx_SO2_hr$CO)
Dublin_Knocklyon_CO_NOx_SO2_hr$SO2 = as.numeric(Dublin_Knocklyon_CO_NOx_SO2_hr$SO2)


mean = aggregate(Dublin_Knocklyon_CO_NOx_SO2_hr[names(Dublin_Knocklyon_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Knocklyon_CO_NOx_SO2_hr$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("CO", "CO_Mean", names(mean))
names(mean) <- gsub("SO2", "SO2_Mean", names(mean))


min = aggregate(Dublin_Knocklyon_CO_NOx_SO2_hr[names(Dublin_Knocklyon_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Knocklyon_CO_NOx_SO2_hr$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("CO", "CO_Min", names(min))
names(min) <- gsub("SO2", "SO2_Min", names(min))


max = aggregate(Dublin_Knocklyon_CO_NOx_SO2_hr[names(Dublin_Knocklyon_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Knocklyon_CO_NOx_SO2_hr$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("CO", "CO_Max", names(max))
names(max) <- gsub("SO2", "SO2_Max", names(max))


#merge the data with the CO data
Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily$Date = as.Date(Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily, "../Gathered_Data/Dublin_Knocklyon_CO_NOx_SO2_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())






#MARINO HOURLY DATA --------------------------------------------------------------------------------
setwd('..')
setwd("Marino/")

#2001 data
#benzene
Benzene <- read_table2("Dublin_Marino_Benzene_TR_2001.txt", col_types = cols(X4 = col_skip()))

#CO
CO <- read_table2("Dublin_Marino_CO_TR_2001.txt", col_types = cols(X4 = col_skip()))


#NOx
NOx <- read_table2("Dublin_Marino_NOx_TR_2001.txt", col_types = cols(X6 = col_skip()))


#SO2
SO2 <- read_table2("Dublin_Marino_SO2_TR_2001.txt", col_types = cols(X4 = col_skip()))


#merge 2001 data
Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(Benzene, CO, NOx, SO2))

#set date column as a date R will recognise
Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Date <-parse_date_time(Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Date, orders = c("dmy"))

#Combine the date and the time columns
Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Date <- with(Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr, as.POSIXct(paste(Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Date, Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Time), format="%Y-%m-%d %H"))
Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr$Time = NULL


#export the data, no data overlaps so 3 different files for this area
write_csv(Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr, "../Gathered_Data/Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr.csv")

#check for rows with complete data
#Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr[complete.cases(Dublin_Marino_Benzene_CO_NOx_SO2_2001_hr), ]

#too little data, not worth getting min/max/max


#MARINO DAILY DATA --------------------------------------------------------------------------------
#PM2.5
#bind the PM2.5 files
PM25_files = dir(pattern = "*PM25")
PM25_list = lapply(PM25_files, read_xlsx)
PM25 = do.call(rbind, PM25_list)

#clean the data of headers
PM25 = PM25[- grep("ug/m3", PM25$PM2.5),]


#PM10 data
#import txt data first
PM10_files = dir(pattern = "*PM10(.*)txt$")
PM10_list = lapply(PM10_files, read_table2)
PM10 = do.call(rbind, PM10_list)

#change the column name for binding
colnames(PM10)[2] = "PM10"

#sort the dates out for merging with lubridate, the dates have different formats
mdy = mdy(PM10$Date)
dmy = dmy(PM10$Date) 
mdy[is.na(mdy)] = dmy[is.na(mdy)] 
PM10$Date = mdy      


#import xls files
PM10_files = dir(pattern = "*PM10(.*)xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#bind the two data sets
PM10 = rbind(PM10,PM10_xls)

#remove hours from data
PM25$Date = as.Date(PM25$Date,format='%Y-%m-%d %H')


#export the data, no data overlaps so different files for this area
write_csv(PM10, "../Gathered_Data/Dublin_Marino_PM10_daily.csv")
write_csv(PM25, "../Gathered_Data/Dublin_Marino_PM25_daily.csv")


#clean the enviroment
rm(list=ls())




#PHOENIX PARK DAILY --------------------------------------------------------------------------------
setwd('..')
setwd("PhoenixPark/")

#import txt data first
PM10_files = dir(pattern = "*PM10(.*)txt$")
PM10_list = lapply(PM10_files, read_table2)
PM10 = do.call(rbind, PM10_list)

#change the column name for binding
colnames(PM10)[2] = "PM10"

#rearrange the date for binding
mdy = mdy(PM10$Date)
dmy = dmy(PM10$Date) 
mdy[is.na(mdy)] = dmy[is.na(mdy)] 
PM10$Date = mdy      

#import xls data
PM10_files = dir(pattern = "*PM10(.*)xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#import xls data
PM10_files = dir(pattern = "*PM10(.*)xlsx$")
PM10_list = lapply(PM10_files, read_xlsx)
PM10_xlsx = do.call(rbind, PM10_list)

#bind the data
PM10 = rbind(PM10, PM10_xls, PM10_xlsx)

#sort the data by date
PM10 = PM10[order(as.Date(PM10$Date, format="%Y/%m/%d")),]

#clean the data of headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#save the gathered data
write_csv(PM10, "../Gathered_Data/Dublin_PhoenixPark_PM10_daily.csv")

#clean the enviroment
rm(list=ls())






#RATHMINES MONTHLY --------------------------------------------------------------------------------
#MONTHLY DATA
setwd('..')
setwd("Rathmines/")


#Asrsenic, AS
#import xls data
As_files = dir(pattern = "As")
As_list = lapply(As_files, read_xlsx)
As = do.call(rbind, As_list)

#clean the data
As = As[- grep("ng/m3", As$As),]

#create a date column
As$Date = seq(as.Date("2009/1/1"), by = "month", length.out = nrow(As))

#remove old date months and replace with new date column and swap so the date column is first
As$X__1 = NULL
As = As[,c(2,1)]



#BaP
BaP_files = dir(pattern = "BaP")
BaP_list = lapply(BaP_files, read_xlsx)
BaP = do.call(rbind, BaP_list)

#clean the data
BaP = BaP[- grep("ng/m3", BaP$`B(a)P`),]

#create a date column
BaP$Date = seq(as.Date("2010/1/1"), by = "month", length.out = nrow(BaP))

#remove old date months and replace with new date column and swap so the date column is first
BaP$X__1 = NULL
BaP = BaP[,c(2,1)]



#Cadmium, Cd
Cd_files = dir(pattern = "Cd")
Cd_list = lapply(Cd_files, read_xlsx)
Cd = do.call(rbind, Cd_list)

#clean the data
Cd = Cd[- grep("ng/m3", Cd$Cd ),]

#create a date column
Cd$Date = seq(as.Date("2009/1/1"), by = "month", length.out = nrow(Cd))

#remove old date months and replace with new date column and swap so the date column is first
Cd$X__1 = NULL
Cd = Cd[,c(2,1)]



#Nickel, Ni
Ni_files = dir(pattern = "Ni")
Ni_list = lapply(Ni_files, read_xlsx)
Ni = do.call(rbind, Ni_list)

#clean the data
Ni = Ni[- grep("ng/m3", Ni$Ni ),]

#create a date column
Ni$Date = seq(as.Date("2009/1/1"), by = "month", length.out = nrow(Ni))

#remove old date months and replace with new date column and swap so the date column is first
Ni$X__1 = NULL
Ni = Ni[,c(2,1)]


#Lead, Pb
#library(readbulk)
#import xlsx files - monthly data
Pb_files = dir(pattern = "*Pb(.*)xlsx$")
Pb_list = lapply(Pb_files, read_xlsx)
Pb = do.call(rbind, Pb_list)

#clean the data
Pb = Pb[- grep("ng/m3", Pb$Pb ),]

#create a date column
Pb$Date = seq(as.Date("2009/1/1"), by = "month", length.out = nrow(Pb))

#remove old date months and replace with new date column and swap so the date column is first
Pb$X__1 = NULL
Pb = Pb[,c(2,1)]

#merge monthly data
Dublin_Rathmines_As_BaP_Cd_Ni_Pb_monthly = Reduce(function(x, y) merge(x, y, all=TRUE), list(As, BaP, Cd, Ni, Pb))

#change the date format to just year/month
Dublin_Rathmines_As_BaP_Cd_Ni_Pb_monthly$Date <- format(as.Date(Dublin_Rathmines_As_BaP_Cd_Ni_Pb_monthly$Date), "%Y-%m")


#save the gathered data
write_csv(Dublin_Rathmines_As_BaP_Cd_Ni_Pb_monthly, "../Gathered_Data/Dublin_Rathmines_As_BaP_Cd_Ni_Pb_monthly.csv")

#clean the enviroment
rm(list=ls())




#RATHMINES DAILY------------------------------------------------------------------
#PB daily files
#import xls files- daily date
Pb_files = dir(pattern = "*Pb(.*)xls$")
Pb_list = lapply(Pb_files, read_xls)
Pb = do.call(rbind, Pb_list)

#clean the data
Pb = Pb[- grep("ug/m3", Pb$Pb ),]

Pb$Date = as.Date(Pb$Date)





#Benzene hourly
Benzene_files = dir(pattern = "*Benzene.*hr")
Benzene_list = lapply(Benzene_files, read_xls)
Benzene_hr = do.call(rbind, Benzene_list)

#clean the data
Benzene_hr = Benzene_hr[- grep("ug/m3", Benzene_hr$Benzene ),]
Benzene_hr$Time = NULL

#calcuate the daily average for the hourly dataset 
Benzene_hr$Date = as.Date(Benzene_hr$Date)
Benzene_hr$Benzene = as.numeric(Benzene_hr$Benzene)
Benzene_daily = aggregate(cbind(Benzene_hr$Benzene) ~ Date, Benzene_hr, mean)

#change the column name for binding
colnames(Benzene_daily)[2] = "Benzene"

#Benzene daily files xls
Benzene_files = dir(pattern = "*Benzene.*day(.*)xls$")
Benzene_list = lapply(Benzene_files, read_xls)
Benzene = do.call(rbind, Benzene_list)

#benzene files xlsx
Benzene_files = dir(pattern = "*Benzene.*day(.*)xlsx$")
Benzene_list = lapply(Benzene_files, read_xlsx)
Benzene_xlxs = do.call(rbind, Benzene_list)

#bind the coverted hourly data and the xls and xlxs files
Benzene = rbind(Benzene_daily, Benzene, Benzene_xlxs)

#plot(Benzene)
#sort the data by date
Benzene = Benzene[order(as.Date(Benzene$Date, format="%Y/%m/%d")),]

#clean the data
Benzene = Benzene[- grep("ug/m3", Benzene$Benzene ),]









#Ethylbenzene
Ethylbenzene_files = dir(pattern = "*ethylbenzene")
Ethylbenzene_list = lapply(Ethylbenzene_files, read_xlsx)

#rows wouldnt bind due to lowercase 'e' in the header of the 2010 file. Changed this manually in excel
#bind the data
Ethylbenzene = do.call(rbind, Ethylbenzene_list)

#clean the data
Ethylbenzene = Ethylbenzene[- grep("ug/m3", Ethylbenzene$Ethylbenzene ),]





#MP Xylene
#import the data and change the column names fpr binding
Xylene_files = dir(pattern = "*MP_xylene.*ppb(.*)xlsx$")
Xylene_list = lapply(Xylene_files, read_xlsx)
colnames = c("Date", "m,p xylene")
Xylene_list <- lapply(Xylene_list, setNames, colnames)
Xylene_ppb = do.call(rbind, Xylene_list)

#convert from ppb to ugm3
Xylene_ppb$`m,p xylene`= as.numeric(Xylene_ppb$`m,p xylene`)
Xylene_ppb$`m,p xylene` = Xylene_ppb$`m,p xylene` * (106/22.41)

#import ugm3 files
Xylene_files = dir(pattern = "mp_xylene.*ugm3(.*)xlsx$")
Xylene_list = lapply(Xylene_files, read_xlsx)

#rename columns for binding and then bind the files
Xylene_list <- lapply(Xylene_list, setNames, colnames)
mp_Xylene = do.call(rbind, Xylene_list)

#bind the ppb and ugm3 datasets
mp_Xylene = rbind(mp_Xylene, Xylene_ppb)

#clean the data
mp_Xylene = mp_Xylene[- grep("ug/m3", mp_Xylene$`m,p xylene` ),]

##sort the data by date
mp_Xylene = mp_Xylene[order(as.Date(mp_Xylene$Date, format="%Y/%m/%d")),]





#o_Xylene
#ppb file import and conversion to ugm3
oXylene_files = dir(pattern = "*o_xylene.*ppb")
oXylene_list = lapply(oXylene_files, read_xlsx)
oXylene_ppb = do.call(rbind, oXylene_list)

#convert from ppb to ugm3. convert from sting to numerals
oXylene_ppb$`o-xylene` = as.numeric(oXylene_ppb$`o-xylene`) * (106/22.41)

#change column name for binding
colnames(oXylene_ppb)[2] = "oXylene"

#read data in
oXylene_files = dir(pattern = "*o_xylene.*ugm3")
oXylene_list = lapply(oXylene_files, read_xlsx)

#change the header names so they are all the same for binding
colnames = c("Date", "oXylene")
oXylene_list <- lapply(oXylene_list, setNames, colnames)
oXylene = do.call(rbind, oXylene_list)

#bind the ppb and ugm3 datasets
oXylene = rbind(oXylene, oXylene_ppb)

#clean the data
oXylene = oXylene[- grep("ug/m3", oXylene$oXylene ),]

##sort the data by date
oXylene = oXylene[order(as.Date(oXylene$Date, format="%Y/%m/%d")),]




#PM10 files
#txt
PM10_files = dir(pattern = "*PM10.*(.*)txt$")
PM10_list = lapply(PM10_files, read_table2)
PM10_txt = do.call(rbind, PM10_list)
colnames(PM10_txt)[2] = "PM10"

#change the date format for binding
PM10_txt$Date = parse_date_time(PM10_txt$Date, c('dmy', 'ymd'))

#xls
PM10_files = dir(pattern = "*PM10.*(.*)xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#xlsx
PM10_files = dir(pattern = "*PM10.*(.*)xlsx$")
PM10_list = lapply(PM10_files, read_xlsx)
PM10_xlsx = do.call(rbind, PM10_list)

#bind all data together
PM10 = rbind(PM10_txt, PM10_xls, PM10_xlsx)

#clean the data
PM10 = PM10[- grep("ug/m3", PM10$PM10 ),]



#convert 2003 file from hourly to daily data to file in a gap in the data
PM10_2003_hr <- read_excel("Dublin_Rathmines_pm10_2003_hr.xls")
PM10_2003_hr$Hour = NULL

#clean the data
PM10_2003_hr = PM10_2003_hr[- grep("ug/m3", PM10_2003_hr$PM10 ),]
PM10_2003_hr$Date = as.Date(PM10_2003_hr$Date)

#compute daily averages
PM10_2003_hr$PM10 = as.numeric(PM10_2003_hr$PM10)

#find nas in case they are causing issues when trying to calculate the daily average
sum(is.na(PM10_2003_hr$Date))

#remove nas
PM10_2003_hr = PM10_2003_hr[!is.na(PM10_2003_hr$Date), ]

#calcuate the daily average
PM10_2003_daily = aggregate(cbind(PM10_2003_hr$PM10) ~ Date, PM10_2003_hr, mean)


#combine covereted hourly data with the rest of the daily data
colnames(PM10_2003_daily)[2] = "PM10"
PM10 = rbind(PM10, PM10_2003_daily)

#sort the data by date
PM10 = PM10[order(as.Date(PM10$Date, format="%Y/%m/%d")),]

#plot(PM10)


#PM25
#import PM25 files
PM25_files = dir(pattern = "*PM25.*(.*)xlsx$")
PM25_list = lapply(PM25_files, read_xlsx)

#rename columns for binding and then bind the files
colnames = c("Date", "PM25")
PM25_list <- lapply(PM25_list, setNames, colnames)
PM25 = do.call(rbind, PM25_list)




#Toulene
Toluene_files = dir(pattern = "*Toluene")
Toluene_list = lapply(Toluene_files, read_xlsx)
Toluene = do.call(rbind, Toluene_list)

#clean the data
Toluene = Toluene[- grep("ug/m3", Toluene$Toluene ),]


Benzene$Date = as.Date(Benzene$Date, format = "%Y-%m-%d")


#merge the daily data together
Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Ethylbenzene, mp_Xylene, oXylene, Pb, Toluene))
#Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Benzene, Ethylbenzene, mp_Xylene, oXylene, Pb, Toluene))


#TODO
#something weird with benzene- cant work it out come back!  
#Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily, Benzene))


#save the gathered data
write_csv(Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily,"../Gathered_Data/Dublin_Rathmines_Benzene_Ethlybenzene_mpXylene_oXylene_Pb_Toluene_daily.csv")

#merge the daily data together for PM
Dublin_Rathmines_PM10_PM25_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(PM10, PM25))

#clean the data
Dublin_Rathmines_PM10_PM25_daily = Dublin_Rathmines_PM10_PM25_daily[- grep("ug/m3", Dublin_Rathmines_PM10_PM25_daily$PM25 ),]

#remove hours from data
Dublin_Rathmines_PM10_PM25_daily$Date = as.Date(Dublin_Rathmines_PM10_PM25_daily$Date,format='%Y-%m-%d %H')


#save the gathered data
write_csv(Dublin_Rathmines_PM10_PM25_daily, "../Gathered_Data/Dublin_Rathmines_PM10_PM25_daily.csv")


#clean the enviroment
rm(list=ls())




#RATHMINES HOURLY --------------------------------------------------------------------

#NOx

#covert the ppb files to ugm3
#NOx text files
NOx_files = dir(pattern = "*NOx*(.*)txt")
NOx_list = lapply(NOx_files, read_table2)
NOx_txt = do.call(rbind, NOx_list)

#have to change Headers of the txt files so they match for binding with the xls and xlsx files
colnames(NOx_txt)[2] = "Time"
colnames(NOx_txt)[3] = "NOx"
colnames(NOx_txt)[4] = "NO"
colnames(NOx_txt)[5] = "NO2"

#change the date format for binding
NOx_txt$Date = parse_date_time(NOx_txt$Date, c('dmy', 'ymd'))

#Combine the date and the time columns
NOx_txt$Date <- with(NOx_txt, as.POSIXct(paste(NOx_txt$Date, NOx_txt$Time), format="%Y-%m-%d %H"))
NOx_txt$Time = NULL



#NOx xls files
NOx_files = dir(pattern = "*NOx*(.*)xls$")
NOx_list = lapply(NOx_files, read_xls)

#have to change Hour column to Time and have to change headers Nox to NOx for binding
NOx_list <- lapply(NOx_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )
NOx_list <- lapply(NOx_list, function(x) setNames(x, gsub("^Nox$", "NOx", names(x))) )

#bind the files
NOx_xls = do.call(rbind, NOx_list)

#Combine the date and the time columns
NOx_xls$Date <- with(NOx_xls, as.POSIXct(paste(NOx_xls$Date, NOx_xls$Time), format="%Y-%m-%d %H"))
NOx_xls$Time = NULL

#clean the data
NOx_xls = NOx_xls[- grep("ppb", NOx_xls$NO ),]


#import the ppb xlxs file
NOx_xlsx <- read_excel("Dublin_Rathmines_NOx_2010_ppb_hr.xlsx")

#Combine the date and the time columns
NOx_xlsx$Date <- with(NOx_xlsx, as.POSIXct(paste(NOx_xlsx$Date, NOx_xlsx$Time), format="%Y-%m-%d %H"))
NOx_xlsx$Time = NULL


#bind all the ppb data
NOx_ppb = rbind(NOx_xls, NOx_txt, NOx_xlsx)

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 2:4] <- sapply(NOx_ppb[,  2:4], as.numeric)

#convert ppb to ugm3. NB NOx additions dont add up from NO and NO2 like in the newer datsets 
#maybe as a result of the results being rounded to nearest numbers here
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)


#import NOx ugm3 files
NOx_files = dir(pattern = "*NOx.*ugm3")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_xlsx = do.call(rbind, NOx_list)

#clean the data
NOx_xlsx = NOx_xlsx[- grep("ug/m3", NOx_xlsx$NO ),]

#combine date and time columes
NOx_xlsx$Date <- with(NOx_xlsx, as.POSIXct(paste(NOx_xlsx$Date, NOx_xlsx$Time), format="%Y-%m-%d %H"))
NOx_xlsx$Time = NULL

#bind all of the NOx data
NOx = rbind(NOx_xlsx, NOx_ppb)

#plot(NOx)

#sort the data by date
NOx = NOx[order(as.Date(NOx$Date, format="%Y/%m/%d")),]




#OZONE HOURLY DATA
#import Ozone txt files
O3_files = dir(pattern = "*O3(.*)txt$")
O3_list = lapply(O3_files, read_table2)
O3_txt = do.call(rbind, O3_list)

#rename the columns for binding
colnames(O3_txt)[3] = "ozone"

#change the date structure
O3_txt$Date = parse_date_time(O3_txt$Date, c('dmy', 'ymd'))


#import the xls O3 files
O3_files = dir(pattern = "*O3(.*)xls$")
O3_list = lapply(O3_files, read_xls)

#alter headers that have hour instead of time
O3_list <- lapply(O3_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )

#bind the files in the list
O3_xls = do.call(rbind, O3_list)


#import the xlsx O3 ppb files
O3_files = dir(pattern = "*O3.*ppb(.*)xlsx$")
O3_list = lapply(O3_files, read_xlsx)
O3_xlsx_ppb = do.call(rbind, O3_list)

#convert ppb to ugm3
O3_xlsx_ppb$ozone = as.numeric(O3_xlsx_ppb$ozone) * (48/22.41)



#import xlxs ugm3 ozone files
O3_files = dir(pattern = "*O3.*ugm3(.*)xlsx$")
O3_list = lapply(O3_files, read_xlsx)
O3_xlsx = do.call(rbind, O3_list)


#bind all the ozone files
O3 = rbind(O3_xlsx, O3_xlsx_ppb, O3_xls, O3_txt)

#sort the data by date
O3 = O3[order(as.Date(O3$Date, format="%Y/%m/%d")),]

#clean the data
O3 = O3[- grep("ug/m3", O3$ozone ),]
O3 = O3[- grep("mg/m3", O3$ozone ),]

#Combine the date and the time columns
O3$Date <- with(O3, as.POSIXct(paste(O3$Date, O3$Time), format="%Y-%m-%d %H"))
O3$Time = NULL

#Suplur dioxide, SO2
#import the text files
SO2_files = dir(pattern = "*SO2(.*)txt$")
SO2_list = lapply(SO2_files, read_table2)
SO2_txt = do.call(rbind, SO2_list)

#rename the columns for binding
colnames(SO2_txt)[3] = "SO2"

#change the date structure
SO2_txt$Date = parse_date_time(SO2_txt$Date, c('dmy', 'ymd'))


#import the xls SO2 files
SO2_files = dir(pattern = "*SO2(.*)xls$")
SO2_list = lapply(SO2_files, read_xls)
SO2_xls = do.call(rbind, SO2_list)

#change the column name for binding
colnames(SO2_xls)[2] = "Time"

#import the xlsx SO2 ppb files
SO2_files = dir(pattern = "*SO2.*ppb(.*)xlsx$")
SO2_list = lapply(SO2_files, read_xlsx)
SO2_xlsx_ppb = do.call(rbind, SO2_list)

#convert ppb to ugm3
SO2_xlsx_ppb$SO2 = as.numeric(SO2_xlsx_ppb$SO2) * (64/22.41)



#import xlxs ugm3 SO2 files
SO2_files = dir(pattern = "*SO2.*ugm3(.*)xlsx$")
SO2_list = lapply(SO2_files, read_xlsx)
SO2_xlsx = do.call(rbind, SO2_list)


#bind all SO2 files
SO2 = rbind(SO2_txt, SO2_xls, SO2_xlsx_ppb, SO2_xlsx)

#clean the data
SO2 = SO2[- grep("ug/m3", SO2$SO2 ),]

#sort the data by date
SO2 = SO2[order(as.Date(SO2$Date, format="%Y/%m/%d")),]

#atomic vector issue when combinding the date and hour
SO2atomicvector = SO2

#Combine the date and the time columns
SO2atomicvector$Date <- with(SO2atomicvector, as.POSIXct(paste(SO2atomicvector$Date, SO2atomicvector$Time), format="%Y-%m-%d %H"))
SO2atomicvector$Time = NULL

SO2 = SO2atomicvector

#remove nas from Date columns to prevent a thousands of NAs being genereated when merging the datasets due to many permutuations of merging the NA rows
NOx = NOx[!is.na(NOx$Date), ]
SO2 = SO2[!is.na(SO2$Date), ]
O3 = O3[!is.na(O3$Date), ]

#combine the hourly datasets for Rathmines
#merge the daily data together
Dublin_Rathmines_NOx_O3_SO2_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(NOx, O3, SO2))


#save the gathered data
write_csv(Dublin_Rathmines_NOx_O3_SO2_hr, "../Gathered_Data/Dublin_Rathmines_NOx_O3_SO2_hr.csv")


#calculate min/max/mean for the data
#change from string to numeric values
Dublin_Rathmines_NOx_O3_SO2_hr$NOx = as.numeric(Dublin_Rathmines_NOx_O3_SO2_hr$NOx)
Dublin_Rathmines_NOx_O3_SO2_hr$NO = as.numeric(Dublin_Rathmines_NOx_O3_SO2_hr$NO)
Dublin_Rathmines_NOx_O3_SO2_hr$NO2 = as.numeric(Dublin_Rathmines_NOx_O3_SO2_hr$NO2)
Dublin_Rathmines_NOx_O3_SO2_hr$ozone = as.numeric(Dublin_Rathmines_NOx_O3_SO2_hr$ozone)
Dublin_Rathmines_NOx_O3_SO2_hr$SO2 = as.numeric(Dublin_Rathmines_NOx_O3_SO2_hr$SO2)


mean = aggregate(Dublin_Rathmines_NOx_O3_SO2_hr[names(Dublin_Rathmines_NOx_O3_SO2_hr)!='Date'], list(hour=cut(Dublin_Rathmines_NOx_O3_SO2_hr$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("ozone", "ozone_Mean", names(mean))
names(mean) <- gsub("SO2", "SO2_Mean", names(mean))


min = aggregate(Dublin_Rathmines_NOx_O3_SO2_hr[names(Dublin_Rathmines_NOx_O3_SO2_hr)!='Date'], list(hour=cut(Dublin_Rathmines_NOx_O3_SO2_hr$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("ozone", "ozone_Min", names(min))
names(min) <- gsub("SO2", "SO2_Min", names(min))


max = aggregate(Dublin_Rathmines_NOx_O3_SO2_hr[names(Dublin_Rathmines_NOx_O3_SO2_hr)!='Date'], list(hour=cut(Dublin_Rathmines_NOx_O3_SO2_hr$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("ozone", "ozone_Max", names(max))
names(max) <- gsub("SO2", "SO2_Max", names(max))


#merge the data with the CO data
Dublin_Rathmines_NOx_O3_SO2_MMM_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_Rathmines_NOx_O3_SO2_MMM_hr)[1] = "Date"

#remove hours from data
Dublin_Rathmines_NOx_O3_SO2_MMM_hr$Date = as.Date(Dublin_Rathmines_NOx_O3_SO2_MMM_hr$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_Rathmines_NOx_O3_SO2_MMM_hr, "../Gathered_Data/Dublin_Rathmines_NOx_O3_SO2_MMM_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())









#RINGSEND DAILY --------------------------------------------------------------------------------
setwd('..')
setwd("Ringsend/")


#BENZENE
#import Benzene data
benzene_files = dir(pattern = "*Benzene")
benzene_list = lapply(benzene_files, read_xlsx)
Benzene = do.call(rbind, benzene_list)

#clean the data
Benzene = Benzene[- grep("ug/m3", Benzene$Benzene ),]


#PM10
#import xls files
PM10_files = dir(pattern = "PM10.*xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#import xlsx files
PM10_files = dir(pattern = "PM10.*xlsx$")
PM10_list = lapply(PM10_files, read_xlsx)
PM10_xlsx = do.call(rbind, PM10_list)

#bind the files together. clean the data of headers
PM10 = rbind(PM10_xls, PM10_xlsx)
PM10 = PM10[- grep("ug/m3", PM10$PM10),]


#Toulene
#import xlsx files
Toluene_files = dir(pattern = "Toluene.*xlsx$")
Toluene_list = lapply(Toluene_files, read_xlsx)
Toluene = do.call(rbind, Toluene_list)

#clean the data of headers
Toluene = Toluene[- grep("ug/m3", Toluene$Toluene),]


#merge daily data for Ringsend
Dublin_Ringsend_Benzene_PM10_Toluene_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(Benzene, PM10, Toluene))

#remove hours from data
Dublin_Ringsend_Benzene_PM10_Toluene_daily$Date = as.Date(Dublin_Ringsend_Benzene_PM10_Toluene_daily$Date,format='%Y-%m-%d %H')


#save the gathered data
write_csv(Dublin_Ringsend_Benzene_PM10_Toluene_daily, "../Gathered_Data/Dublin_Ringsend_Benzene_PM10_Toluene_daily.csv")


#clean the enviroment
rm(list=ls())




#RINGSEND HOURLY --------------------------------------------------------------------------------

#CARBON MONOXIDE, CO
#import CO ppm data
CO_files = dir(pattern = "CO.*ppm|ppm.*CO")
CO_list = lapply(CO_files, read_xlsx)
CO_ppm = do.call(rbind, CO_list)

#convert ppm to mgm3. CO molecular weight is 28
CO_ppm$CO = as.numeric(CO_ppm$CO)
CO_ppm$CO = CO_ppm$CO * (28/22.41)


#import CO mgm3 data
CO_files = dir(pattern = "CO.*mgm3|ppm.*mgm3")
CO_list = lapply(CO_files, read_xlsx)
CO = do.call(rbind, CO_list)

#bind the 2 CO files
CO_final = rbind(CO,CO_ppm)

#combine the date and time columns
CO_final$Date <- with(CO_final, as.POSIXct(paste(CO_final$Date , CO_final$Time), format="%Y-%m-%d %H"))
CO_final$Time = NULL

#clean the data
CO_final = CO_final[- grep("mg/m3", CO_final$CO),]

#change name for consistency
CO = CO_final




#NOx
#NOx ppb files conversion
ppb_NOx = dir(pattern = "NOx.*ppb|ppb.*NOx")
NOx_list = lapply(ppb_NOx, read_xlsx)
NOx_ppb = do.call(rbind, NOx_list)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
#is.numeric(NOx_ppb$NO2)

#convert from ppb to ugm3. molecular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)

#bind the NOx files chronologically
NOx_files = dir(pattern = "NOx.*ugm3|ugm3.*NOx")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_ugm3 = do.call(rbind, NOx_list)

#clean the data of headings
NOx_ugm3 = NOx_ugm3[- grep("ug/m3", NOx_ugm3$NOx),]

#bind the ppm and ugm3 files
NOx_final = rbind(NOx_ppb, NOx_ugm3)

#combine the date and time columns
NOx_final$Date <- with(NOx_final, as.POSIXct(paste(NOx_final$Date , NOx_final$Time), format="%Y-%m-%d %H"))
NOx_final$Time = NULL

#change name for consistency-aids with atomic errors
NOx = NOx_final



#SO2
#search for ppb SO2 files using logical OR statement
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_xlsx)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#convert ppb to ugm3
SO2_ppb$SO2 = as.numeric(SO2_ppb$SO2) * (64/22.41)

#search for ugm3 NOX files using logical OR statement
SO2_files = dir(pattern = "SO2.*ugm3|ugm3.*SO2")
SO2_list = lapply(SO2_files, read_xlsx)
SO2_ugm3 = do.call(rbind, SO2_list)

#bind SO2 data
SO2 = rbind(SO2_ugm3, SO2_ppb)

#clean possible headings
SO2 = SO2[- grep("ug/m3", SO2$SO2),]

#atomic vector error workaround
SO2_ave = SO2

#combine the date and time columns
SO2_ave$Date <- with(SO2_ave, as.POSIXct(paste(SO2_ave$Date , SO2_ave$Time), format="%Y-%m-%d %H"))
SO2_ave$Time = NULL

SO2 = SO2_ave

#merge daily data for Ringsend
Dublin_Ringsend_CO_NOx_SO2_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(CO, NOx, SO2))
Dublin_Ringsend_CO_NOx_SO2_hr$Time = NULL


#save the gathered data
write_csv(Dublin_Ringsend_CO_NOx_SO2_hr, "../Gathered_Data/Dublin_Ringsend_CO_NOx_SO2_hr.csv")





#calculate min/max/mean for the data
#change from string to numeric values
Dublin_Ringsend_CO_NOx_SO2_hr$NOx = as.numeric(Dublin_Ringsend_CO_NOx_SO2_hr$NOx)
Dublin_Ringsend_CO_NOx_SO2_hr$NO = as.numeric(Dublin_Ringsend_CO_NOx_SO2_hr$NO)
Dublin_Ringsend_CO_NOx_SO2_hr$NO2 = as.numeric(Dublin_Ringsend_CO_NOx_SO2_hr$NO2)
Dublin_Ringsend_CO_NOx_SO2_hr$CO = as.numeric(Dublin_Ringsend_CO_NOx_SO2_hr$CO)
Dublin_Ringsend_CO_NOx_SO2_hr$SO2 = as.numeric(Dublin_Ringsend_CO_NOx_SO2_hr$SO2)


mean = aggregate(Dublin_Ringsend_CO_NOx_SO2_hr[names(Dublin_Ringsend_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Ringsend_CO_NOx_SO2_hr$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("CO", "CO_Mean", names(mean))
names(mean) <- gsub("SO2", "SO2_Mean", names(mean))


min = aggregate(Dublin_Ringsend_CO_NOx_SO2_hr[names(Dublin_Ringsend_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Ringsend_CO_NOx_SO2_hr$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("CO", "CO_Min", names(min))
names(min) <- gsub("SO2", "SO2_Min", names(min))


max = aggregate(Dublin_Ringsend_CO_NOx_SO2_hr[names(Dublin_Ringsend_CO_NOx_SO2_hr)!='Date'], list(hour=cut(Dublin_Ringsend_CO_NOx_SO2_hr$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("CO", "CO_Max", names(max))
names(max) <- gsub("SO2", "SO2_Max", names(max))


#merge the data
Dublin_Ringsend_CO_NOx_SO2_MMM_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_Ringsend_CO_NOx_SO2_MMM_hr)[1] = "Date"

#remove hours from data
Dublin_Ringsend_CO_NOx_SO2_MMM_hr$Date = as.Date(Dublin_Ringsend_CO_NOx_SO2_MMM_hr$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_Ringsend_CO_NOx_SO2_MMM_hr, "../Gathered_Data/Dublin_Ringsend_CO_NOx_SO2_MMM_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())










#ROSEMOUNT MONTHLY--------------------------------------------------------------------------------
setwd('..')
setwd("Rosemount/")


#All metals 2012
Dublin_Rosemount_AllMetalDeposition_2012_ngm3_month <- read_excel("Dublin_Rosemount_AllMetalDeposition_2012_ngm3_month.xlsx")


#As
#Arsenic, As data and remove header
As_files = dir(pattern = "As")
As_list = lapply(As_files, read_xlsx)
As = do.call(rbind, As_list)

As = As[- grep("ug m-3 day-1", As$As),]
As = As[- grep("ug m-2 day-1", As$As),]

#create a date column for merging data
Dates = as.data.frame(seq(as.Date("2010/1/1"), by = "month", length.out = nrow(As) + 12))
colnames(Dates)[1] = "Date"

#there is no 2011 data so remove that from dates
Date1 = as.Date("2011-12-01")  
Date2 = as.Date("2011-01-01")
Dates = filter(Dates, Dates$Date > Date1 | Dates$Date < Date2)

#bind dates with As
As = cbind(As, Dates)

#remove old date months and replace with new date column and swap so the date column is first
As$X__1 = NULL
As = As[,c(2,1)]




#Cadmium, Cd import and clean header data
Cd_files = dir(pattern = "Cd")
Cd_list = lapply(Cd_files, read_xlsx)
Cd = do.call(rbind, Cd_list)

Cd = Cd[- grep("ug m-3 day-1", Cd$Cd),]
Cd = Cd[- grep("ug m-2 day-1", Cd$Cd),]

#same dates as As, add the date column created for that
Cd = cbind(Cd, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Cd$X__1 = NULL
Cd  = Cd[,c(2,1)]




#Hg
Hg_files = dir(pattern = "Hg")
Hg_list = lapply(Hg_files, read_xlsx)
Hg = do.call(rbind, Hg_list)

#clean headers
Hg = Hg[- grep("ug m-2 day-1", Hg$Hg),]

#change the months to dates with years
Hg$Date = seq(as.Date("2012/1/1"), by = "month", length.out = nrow(Hg))

#remove old date months and replace with new date column and swap so the date column is first
Hg$X__1 = NULL
Hg  = Hg[,c(2,1)]





#Ni
#Nickel, import Ni data and remove header
Ni_files = dir(pattern = "Ni")
Ni_list = lapply(Ni_files, read_xlsx)
Ni = do.call(rbind, Ni_list)

#had to remove rows manually, the search function wouldnt work for some reason
Ni = Ni[- grep("ug m-3 day-1", Ni$Ni),]
Ni = Ni[-c(13, 26), ]

#same dates as As, add the date column created for that
Ni = cbind(Ni, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Ni$X__1 = NULL
Ni  = Ni[,c(2,1)]




#Pb
#import lead data, Pb, and remove the headers
Pb_files = dir(pattern = "Pb")
Pb_list = lapply(Pb_files, read_xlsx)
Pb = do.call(rbind, Pb_list)

Pb = Pb[- grep("ug m-3 day-1", Pb$Pb),]
Pb = Pb[- grep("ug m-2 day-1", Pb$Pb),]


#same dates as As, add the date column created for that
Pb = cbind(Pb, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Pb$X__1 = NULL
Pb  = Pb[,c(2,1)]





#merge the Rosemount monthly data together
#merge daily data for Ringsend
Dublin_Rosemount_As_Cd_Hg_Ni_Pb = Reduce(function(x, y) merge(x, y, all=TRUE), list(As,Cd,Hg,Ni,Pb))

#remove the day from the date column
Dublin_Rosemount_As_Cd_Hg_Ni_Pb$Date = format(Dublin_Rosemount_As_Cd_Hg_Ni_Pb$Date, format="%Y-%m")

#save the gathered data
write_csv(Dublin_Rosemount_As_Cd_Hg_Ni_Pb, "../Gathered_Data/Dublin_Rosemount_As_Cd_Hg_Ni_Pb_monthly.csv")

#clean the enviroment
rm(list=ls())







#ST. ANNES PARK HOURLY --------------------------------------------------------------------------------
setwd('..')
setwd("StAnnesPark/")

#HOURLY NOx
#import and bind the NOx files
NOx_files = dir(pattern = "NOx.*ugm3|ugm3.*NOx")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_final = do.call(rbind, NOx_list)

#clean the data of headings
NOx_final = NOx_final[- grep("ug/m3", NOx_final$NOx),]

#combine date and time column
#Combine the date and the time for the different pollutants to help with graphing
NOx_final$Date <- with(NOx_final, as.POSIXct(paste(NOx_final$Date, NOx_final$Time), format="%Y-%m-%d %H"))
NOx_final$Time = NULL

NOx = NOx_final

#save the gathered data
write_csv(NOx, "../Gathered_Data/Dublin_StAnnesPark_NOx_hr.csv")



#Convert to daily values
#calculate min/max/mean for the data
#change from string to numeric values
NOx$NOx = as.numeric(NOx$NOx)
NOx$NO = as.numeric(NOx$NO)
NOx$NO2 = as.numeric(NOx$NO2)

mean = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))

min = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))

max = aggregate(NOx[names(NOx)!='Date'], list(hour=cut(NOx$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))


#merge the data with the CO data
Dublin_StAnnesPark_NOx_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_StAnnesPark_NOx_MMM_daily)[1] = "Date"

#remove hours from data
Dublin_StAnnesPark_NOx_MMM_daily$Date = as.Date(Dublin_StAnnesPark_NOx_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_StAnnesPark_NOx_MMM_daily, "../Gathered_Data/Dublin_StAnnesPark_NOx_MMM_daily.csv")


#clean the enviroment
rm(list=ls())






#ST. ANNES PARK DAILY --------------------------------------------------------------------------------

#PM10
#import PM10 files
PM10_files = dir(pattern = "PM10.*xlsx$")
PM10_list = lapply(PM10_files, read_xlsx)
PM10 = do.call(rbind, PM10_list)

#clean the data of headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')

#save the gathered data
write_csv(PM10, "../Gathered_Data/Dublin_StAnnesPark_PM10_daily.csv")


#clean the enviroment
rm(list=ls())




#SWORDS HOURLY ------------------------------------------------------------------------------------
setwd('..')
setwd("Swords/")

#HOURLY NOx 
#bind the NOx files chronologically
NOx_files = dir(pattern = "*NOx")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_final = do.call(rbind, NOx_list)


#clear rows with ppb written in them
NOx_final = NOx_final[- grep("ug/m3", NOx_final$NO2),]

#Combine the date and the time for the different pollutants to help with graphing
NOx_final$Date <- with(NOx_final, as.POSIXct(paste(NOx_final$Date, NOx_final$Time), format="%Y-%m-%d %H"))
NOx_final$Time = NULL

NOx = NOx_final


#Ozone 
#find ppb files
O3_ppb_files = dir(pattern = "O3.*ppb|ppb.*O3")
O3_ppb_list = lapply(O3_ppb_files, read_xlsx)
O3_ppb = do.call(rbind, O3_ppb_list)

#clear rows with ppb written in them
O3_ppb = O3_ppb[- grep("ppb", O3_ppb$ozone),]

#convert all the O3 rows from strings to numerical values for calculations
O3_ppb[, 3] <- sapply(O3_ppb[,3], as.numeric)

#convert O3 from ppb to ugm3. molecular weight is 48. formula is ppb x moleucular weight/22.41
O3_ppb$ozone =O3_ppb$ozone * (48/22.41)

#import ugm3 files
O3_ugm3_files = dir(pattern = "O3.*ugm3|ugm3*O3")
O3_ugm3_list = lapply(O3_ugm3_files, read_xlsx)
O3_ugm3 = do.call(rbind, O3_ugm3_list)

#clear headers
O3_ugm3 = O3_ugm3[- grep("ug/m3", O3_ugm3$ozone),]


#bind the ppb and ugm3 data
O3_final = rbind(O3_ppb, O3_ugm3)


#Combine the date and the time for the different pollutants to help with graphing
O3_final$Date <- with(O3_final, as.POSIXct(paste(O3_final$Date, O3_final$Time), format="%Y-%m-%d %H"))
O3_final$Time = NULL

#plot(O3_final, type = "l")
O3 = O3_final


#merge the data for Swords
Dublin_Swords_NOx_Ozone_hr = Reduce(function(x, y) merge(x, y, all=TRUE), list(NOx, O3))

#save the gathered data
write_csv(Dublin_Swords_NOx_Ozone_hr, "../Gathered_Data/Dublin_Swords_NOx_Ozone_hr.csv")




#Convert to daily values
#calculate min/max/mean for the data
#change from string to numeric values
Dublin_Swords_NOx_Ozone_hr$NOx = as.numeric(Dublin_Swords_NOx_Ozone_hr$NOx)
Dublin_Swords_NOx_Ozone_hr$NO = as.numeric(Dublin_Swords_NOx_Ozone_hr$NO)
Dublin_Swords_NOx_Ozone_hr$NO2 = as.numeric(Dublin_Swords_NOx_Ozone_hr$NO2)
Dublin_Swords_NOx_Ozone_hr$ozone = as.numeric(Dublin_Swords_NOx_Ozone_hr$ozone)

mean = aggregate(Dublin_Swords_NOx_Ozone_hr[names(Dublin_Swords_NOx_Ozone_hr)!='Date'], list(hour=cut(Dublin_Swords_NOx_Ozone_hr$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("ozone", "ozone_Mean", names(mean))

min = aggregate(Dublin_Swords_NOx_Ozone_hr[names(Dublin_Swords_NOx_Ozone_hr)!='Date'], list(hour=cut(Dublin_Swords_NOx_Ozone_hr$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("ozone", "ozone_Min", names(min))

max = aggregate(Dublin_Swords_NOx_Ozone_hr[names(Dublin_Swords_NOx_Ozone_hr)!='Date'], list(hour=cut(Dublin_Swords_NOx_Ozone_hr$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("ozone", "ozone_Max", names(max))



#merge the data with the CO data
Dublin_Swords_NOx_Ozone_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_Swords_NOx_Ozone_hr_MMM_daily)[1] = "Date"

#remove hours from data
Dublin_Swords_NOx_Ozone_hr_MMM_daily$Date = as.Date(Dublin_Swords_NOx_Ozone_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_Swords_NOx_Ozone_hr_MMM_daily, "../Gathered_Data/Dublin_Swords_NOx_Ozone_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())








#TALLAGHT DAILY ------------------------------------------------------------------------------------
setwd('..')
setwd("Tallaght/")

#DAILY PM10
#import PM10 xls file
PM10_files = dir(pattern = "PM10.*xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#import xls files
PM10_files = dir(pattern = "PM10.*xlsx")
PM10_list = lapply(PM10_files, read_xlsx)
PM10_xlsx = do.call(rbind, PM10_list)

#bind the text files and the xls file together. clean the data of headers
PM10 = rbind(PM10_xls, PM10_xlsx)
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')

#save the gathered data
write_csv(PM10, "../Gathered_Data/Dublin_Tallaght_PM10_daily.csv")




#TALLAGHT HOURLY ------------------------------------------------------------------------------------
#SO2
#search for ppb SO2 files using logical OR statement
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_xlsx)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#clean possible headings
SO2_ppb = SO2_ppb[- grep("ppb", SO2_ppb$SO2),]

#change to numeric values for maths
SO2_ppb$SO2 = as.numeric(SO2_ppb$SO2) * (64/22.41)

#search for ugm3 SO2 files using logical OR statement
SO2_files = dir(pattern = "SO2.*ugm3|ugm3*SO2")
SO2_list = lapply(SO2_files, read_xlsx)
SO2_ugm3 = do.call(rbind, SO2_list)

#bind SO2 data
SO2 = rbind(SO2_ppb, SO2_ugm3)

#clean possible headings
SO2 = SO2[- grep("ug/m3", SO2$SO2),]

#save the gathered data
write_csv(SO2, "../Gathered_Data/Dublin_Tallaght_SO2_hr.csv")


#atomic error workaround
SO2_ave = SO2

#Combine the date and the time for the different pollutants to help with graphing
SO2_ave$Date <- with(SO2_ave, as.POSIXct(paste(SO2_ave$Date, SO2_ave$Time), format="%Y-%m-%d %H"))
SO2_ave$Time = NULL

SO2 = SO2_ave


#Convert to daily values
#calculate min/max/mean for the data
#change from string to numeric values
SO2$SO2 = as.numeric(SO2$SO2)

mean = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("SO2", "SO2_Mean", names(mean))

min = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), min, na.rm=F)
names(min) <- gsub("SO2", "SO2_Min", names(min))

max = aggregate(SO2[names(SO2)!='Date'], list(hour=cut(SO2$Date,'day')), max, na.rm=F)
names(max) <- gsub("SO2", "SO2_Max", names(max))



#merge the data with the CO data
Dublin_Tallaght_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_Tallaght_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
Dublin_Tallaght_SO2_hr_MMM_daily$Date = as.Date(Dublin_Tallaght_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_Tallaght_SO2_hr_MMM_daily, "../Gathered_Data/Dublin_Tallaght_SO2_hr_MMM_daily.csv")


#clean the enviroment
rm(list=ls())



#WINETAVERN STREET MONTHLY ------------------------------------------------------------------------------------
setwd('..')
setwd("WinetavernSt/")


#Monthly
#As
#Arsenic, As data and remove header
As_files = dir(pattern = "As")
As_list = lapply(As_files, read_xlsx)
As = do.call(rbind, As_list)

As = As[- grep("ng/m3", As$As),]

#create a date column for merging data. added 12 for the extra year missing missing in the data
Dates = as.data.frame(seq(as.Date("2009/1/1"), by = "month", length.out = nrow(As) + 12))
colnames(Dates)[1] = "Date"

#there is no 2011 data so remove that from dates
Date1 = as.Date("2010-12-01")  
Date2 = as.Date("2010-01-01")
Dates = filter(Dates, Dates$Date > Date1 | Dates$Date < Date2)

#bind dates with As
As = cbind(As, Dates)

#remove old date months and replace with new date column and swap so the date column is first
As$X__1 = NULL
As = As[,c(2,1)]





#BaP
BaP_files = dir(pattern = "BaP")
BaP_list = lapply(BaP_files, read_xlsx)
BaP = do.call(rbind, BaP_list)

#clean headers
BaP = BaP[- grep("ng/m3", BaP$`B(a)P`),]

#same dates as As, add the date column created for that
BaP = cbind(BaP, Dates)

#remove old date months and replace with new date column and swap so the date column is first
BaP$X__1 = NULL
BaP  = BaP[,c(2,1)]





#Cadmium, Cd import and clean header data
Cd_files = dir(pattern = "Cd")
Cd_list = lapply(Cd_files, read_xlsx)
Cd = do.call(rbind, Cd_list)

#Clean old headers in the data
Cd = Cd[- grep("ng/m3", Cd$Cd),]

#same dates as As, add the date column created for that
Cd = cbind(Cd, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Cd$X__1 = NULL
Cd  = Cd[,c(2,1)]





#Ni
#Nickel, import Ni data and remove header
Ni_files = dir(pattern = "Ni")
Ni_list = lapply(Ni_files, read_xlsx)
Ni = do.call(rbind, Ni_list)

#had to remove rows manually, the search function wouldnt work for some reason
Ni = Ni[- grep("ng/m3", Ni$Ni),]

#same dates as As, add the date column created for that
Ni = cbind(Ni, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Ni$X__1 = NULL
Ni  = Ni[,c(2,1)]




#Pb
#import lead data, Pb, and remove the headers
Pb_files = dir(pattern = "Pb")
Pb_list = lapply(Pb_files, read_xlsx)
Pb = do.call(rbind, Pb_list)

#clean old headers from the data
Pb = Pb[- grep("ng/m3", Pb$Pb),]

#same dates as As, add the date column created for that
Pb = cbind(Pb, Dates)

#remove old date months and replace with new date column and swap so the date column is first
Pb$X__1 = NULL
Pb  = Pb[,c(2,1)]


#merge monthly data for WinetavernSt
Dublin_WinetavernSt_As_BaP_Cd_Ni_Pb = Reduce(function(x, y) merge(x, y, all=TRUE), list(As,BaP,Cd,Ni,Pb))

#remove the day from the date column
Dublin_WinetavernSt_As_BaP_Cd_Ni_Pb$Date = format(Dublin_WinetavernSt_As_BaP_Cd_Ni_Pb$Date, format="%Y-%m")

#save the gathered data
write_csv(Dublin_WinetavernSt_As_BaP_Cd_Ni_Pb, "../Gathered_Data/Dublin_WinetavernSt_As_BaP_Cd_Ni_Pb_monthly.csv")

#clean the enviroment
rm(list=ls())




#WINETAVERN STREET DAILY ------------------------------------------------------------------------------------
#PM10

#xls files
PM10_files = dir(pattern = "*PM10.*(.*)xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#txt
PM10_files = dir(pattern = "*PM10.*(.*)txt$")
PM10_list = lapply(PM10_files, read_table2)
PM10_txt = do.call(rbind, PM10_list)
colnames(PM10_txt)[2] = "PM10"

#change the date format for binding
PM10_txt$Date = parse_date_time(PM10_txt$Date, c('dmy', 'ymd'))


#xlsx
PM10_files = dir(pattern = "*PM10.*(.*)xlsx$")
PM10_list = lapply(PM10_files, read_xlsx)
PM10_xlsx = do.call(rbind, PM10_list)

#bind the PM10 data
PM10 = rbind(PM10_xls, PM10_txt ,PM10_xlsx)

#sort the data by date
PM10 = PM10[order(as.Date(PM10$Date, format="%Y/%m/%d")),]

#clean the data of headers
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#remove hours from data
PM10$Date = as.Date(PM10$Date,format='%Y-%m-%d %H')

#save the gathered data
write_csv(PM10, "../Gathered_Data/Dublin_WinetavernSt_PM10_daily.csv")

#clean the enviroment
rm(list=ls())





#WINETAVERN STREET HOURLY ------------------------------------------------------------------------------------

#CO
#xls files
CO_files = dir(pattern = "*CO.*(.*)xls$")
CO_list = lapply(CO_files, read_xls)

#rename columns for binding. Changing columns labelled Hour to newer Time format used by the EPA. change CO columns to the same heading
CO_list <- lapply(CO_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )
CO_xls = do.call(rbind, CO_list)


#txt
CO_files = dir(pattern = "*CO*(.*)txt$")
CO_list = lapply(CO_files, read_table2)
CO_txt = do.call(rbind, CO_list)
colnames(CO_txt)[3] = "CO"

#change the date format for binding
CO_txt$Date = parse_date_time(CO_txt$Date, c('dmy', 'ymd'))


#xlsx ppm file import and convert to ugm3
CO_files = dir(pattern = "CO.*ppm")
CO_list = lapply(CO_files, read_xlsx)
CO_ppm = do.call(rbind, CO_list)

#clear old headers
CO_ppm = CO_ppm[- grep("ppm", CO_ppm$CO ),]

#convert ppm to mgm3. CO molecular weight is 28. convert from a tring to numerical values
CO_ppm$CO = as.numeric(CO_ppm$CO)
CO_ppm$CO = CO_ppm$CO * (28/22.41)


#xlsx ugm3 file import
CO_files = dir(pattern = "CO.*mgm3")
CO_list = lapply(CO_files, read_xlsx)
CO_mgm3 = do.call(rbind, CO_list)

#clear old headers
CO_mgm3 = CO_mgm3[- grep("mg/m3", CO_mgm3$CO ),]


#bind the CO datasets
CO_final = rbind(CO_xls, CO_txt, CO_ppm, CO_mgm3)

#clear old headers
CO_final = CO_final[- grep("mg/m3", CO_final$CO ),]

#Combine the date and the time for the different pollutants to help with graphing
CO_final$Date <- with(CO_final, as.POSIXct(paste(CO_final$Date, CO_final$Time), format="%Y-%m-%d %H"))
CO_final$Time = NULL
CO = CO_final

#plot(CO, type = "l")



#NOX
#NOX ppb files import
ppb_NOx = dir(pattern = "NOx.*ppb.*(.*)xls$")
NOx_list = lapply(ppb_NOx, read_xls)


#rename columns for binding
NOx_list <- lapply(NOx_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))) )
NOx_list <- lapply(NOx_list, function(x) setNames(x, gsub("^Nox$", "NOx", names(x))) )
NOx_list <- lapply(NOx_list, function(x) setNames(x, gsub("^NOX$", "NOx", names(x))) )

#bind the data
NOx_ppb = do.call(rbind, NOx_list)

#import ppb xlsx file
NOx_files = dir(pattern = "NOx.*ppb.*(.*)xlsx$")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_ppb_xlsx = do.call(rbind, NOx_list)

#bind ppb dateframes
NOx_ppb = rbind(NOx_ppb, NOx_ppb_xlsx)

#clear rows with ppb written in them
NOx_ppb = NOx_ppb[- grep("ppb", NOx_ppb$NO2),]

#convert all the NO columns from strings to numerical values for calculations
NOx_ppb[, 3:5] <- sapply(NOx_ppb[,  3:5], as.numeric)
#is.numeric(NOx_ppb$NO2)

#convert from ppb to ugm3. molecular weight/22.41
NOx_ppb$NOx = NOx_ppb$NOx * (46/22.41)
NOx_ppb$NO = NOx_ppb$NO * (30/22.41)
NOx_ppb$NO2 = NOx_ppb$NO2 * (46/22.41)


#import ugm3 xls file
NOx_files = dir(pattern = "NOx.*ugm3(.*)xls$")
NOx_list = lapply(NOx_files, read_xls)
NOx_ugm3_xls = do.call(rbind, NOx_list)

#change hour column to Time
colnames(NOx_ugm3_xls)[2] = "Time"


#import ugm3 xlsx file
NOx_files = dir(pattern = "NOx.*ugm3(.*)xlsx$")
NOx_list = lapply(NOx_files, read_xlsx)
NOx_ugm3_xlsx = do.call(rbind, NOx_list)

#xls file has only 3 columns so need to use rbind fill in plyr library
library(plyr)

#bind all NOx data
NOx_final = rbind.fill(NOx_ppb, NOx_ugm3_xls, NOx_ugm3_xlsx)

#clear rows with ppb written in them
NOx_final = NOx_final[- grep("ug/m3", NOx_final$NO2),]

#Combine the date and the time for the different pollutants to help with graphing
NOx_final$Date <- with(NOx_final, as.POSIXct(paste(NOx_final$Date, NOx_final$Time), format="%Y-%m-%d %H"))
NOx_final$Time = NULL

#sort dataframe by time
NOx_final = NOx_final[order(as.Date(NOx_final$Date, format="%Y/%m/%d")),]

NOx = NOx_final


#SO2
#search for ppb SO2 files 
SO2_ppb_files = dir(pattern = "SO2.*ppb|ppb.*SO2")
SO2_ppb_list = lapply(SO2_ppb_files, read_xlsx)
SO2_ppb = do.call(rbind, SO2_ppb_list)

#convert SO2 ppb data to ugm3 data for consistency
SO2_ppb$SO2 = as.numeric(SO2_ppb$SO2)
SO2_ppb$SO2 = SO2_ppb$SO2 * (64/22.41)

#search for xls SO2 files 
SO2_files = dir(pattern = "SO2.*(.*)xls$")
SO2_list = lapply(SO2_files, read_xls)

#rename columns for binding
SO2_list <- lapply(SO2_list, function(x) setNames(x, gsub("^Hour$", "Time", names(x))))

#bind the files
SO2_xls = do.call(rbind, SO2_list)

#search for ppb SO2 files 
SO2_files = dir(pattern = "SO2.*(.*)txt$")
SO2_list = lapply(SO2_files, read_table2)
SO2_txt = do.call(rbind, SO2_list)

#rename column for binding
colnames(SO2_txt)[3] = "SO2"

#change the date format for binding
SO2_txt$Date = parse_date_time(SO2_txt$Date, c('dmy', 'ymd'))

SO2_txt$Date = as.Date(SO2_txt$Date)
#is.Date(SO2_txt$Date)


#search for xlsx SO2 files 
SO2_files = dir(pattern = "SO2.*ugm3(.*)xlsx$")
SO2_list = lapply(SO2_files, read_xlsx)
SO2_xlsx = do.call(rbind, SO2_list)


#bind all the SO2 data
SO2_final = rbind(SO2_ppb, SO2_xls, SO2_txt, SO2_xlsx)

#sort dataframe by time
SO2_final = SO2_final[order(as.Date(SO2_final$Date, format="%Y/%m/%d")),]

#Combine the date and the time for the different pollutants to help with graphing
SO2_final$Date <- with(SO2_final, as.POSIXct(paste(SO2_final$Date, SO2_final$Time), format="%Y-%m-%d %H"))
SO2_final$Time = NULL

SO2 = SO2_final

#clear old headers
SO2 = SO2[- grep("ug/m3", SO2$SO2 ),]

#remove Date NA's due to issues when merging
SO2 = SO2 %>% drop_na(Date)
CO = CO %>% drop_na(Date)
NOx = NOx %>% drop_na(Date)

#merge hourly data for WinetavernSt
Dublin_WinetavernSt_CO_NOx_SO2 = Reduce(function(x, y) merge(x, y, all=TRUE), list(CO, NOx, SO2))



#save the gathered data
write_csv(Dublin_WinetavernSt_CO_NOx_SO2, "../Gathered_Data/Dublin_WinetavernSt_CO_NOx_SO2_hr.csv")




#Convert to daily values
#calculate min/max/mean for the data
#change from string to numeric values
Dublin_WinetavernSt_CO_NOx_SO2$NOx = as.numeric(Dublin_WinetavernSt_CO_NOx_SO2$NOx)
Dublin_WinetavernSt_CO_NOx_SO2$NO = as.numeric(Dublin_WinetavernSt_CO_NOx_SO2$NO)
Dublin_WinetavernSt_CO_NOx_SO2$NO2 = as.numeric(Dublin_WinetavernSt_CO_NOx_SO2$NO2)
Dublin_WinetavernSt_CO_NOx_SO2$CO = as.numeric(Dublin_WinetavernSt_CO_NOx_SO2$CO)
Dublin_WinetavernSt_CO_NOx_SO2$SO2 = as.numeric(Dublin_WinetavernSt_CO_NOx_SO2$SO2)


mean = aggregate(Dublin_WinetavernSt_CO_NOx_SO2[names(Dublin_WinetavernSt_CO_NOx_SO2)!='Date'], list(hour=cut(Dublin_WinetavernSt_CO_NOx_SO2$Date,'day')), mean, na.rm=F)
names(mean) <- gsub("NOx", "NOx_Mean", names(mean))
names(mean) <- gsub("NO$", "NO_Mean", names(mean))
names(mean) <- gsub("NO2", "NO2_Mean", names(mean))
names(mean) <- gsub("CO", "CO_Mean", names(mean))
names(mean) <- gsub("SO2", "SO2_Mean", names(mean))


min = aggregate(Dublin_WinetavernSt_CO_NOx_SO2[names(Dublin_WinetavernSt_CO_NOx_SO2)!='Date'], list(hour=cut(Dublin_WinetavernSt_CO_NOx_SO2$Date,'day')), min, na.rm=F)
names(min) <- gsub("NOx", "NOx_Min", names(min))
names(min) <- gsub("NO$", "NO_Min", names(min))
names(min) <- gsub("NO2", "NO2_Min", names(min))
names(min) <- gsub("CO", "CO_Min", names(min))
names(min) <- gsub("SO2", "SO2_Min", names(min))


max = aggregate(Dublin_WinetavernSt_CO_NOx_SO2[names(Dublin_WinetavernSt_CO_NOx_SO2)!='Date'], list(hour=cut(Dublin_WinetavernSt_CO_NOx_SO2$Date,'day')), max, na.rm=F)
names(max) <- gsub("NOx", "NOx_Max", names(max))
names(max) <- gsub("NO$", "NO_Max", names(max))
names(max) <- gsub("NO2", "NO2_Max", names(max))
names(max) <- gsub("CO", "CO_Max", names(max))
names(max) <- gsub("SO2", "SO2_Max", names(max))




#merge the data with the CO data
Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily = Reduce(function(x, y) merge(x, y, all=TRUE), list(mean,min,max))

#change column name to date
colnames(Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily)[1] = "Date"

#remove hours from data
Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily$Date = as.Date(Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily$Date,format='%Y-%m-%d %H')

#save the converted hourly data
write_csv(Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily, "../Gathered_Data/Dublin_WinetavernSt_CO_NOx_SO2_hr_MMM_daily.csv")



#clean the enviroment
rm(list=ls())








#WOODQUAY DAILY --------------------------------------------------------------------------------
setwd('..')
setwd("Woodquay/")

#import PM10 xls file
PM10_files = dir(pattern = "PM10.*xls$")
PM10_list = lapply(PM10_files, read_xls)
PM10_xls = do.call(rbind, PM10_list)

#import PM10 xls file
PM10_files = dir(pattern = "PM10.*txt$")
PM10_list = lapply(PM10_files, read_table2)
PM10_txt = do.call(rbind, PM10_list)

#make sure R recognises Date column from txt files as a date column for proper binding
PM10_txt$Date = as.Date(PM10_txt$Date, format="%d/%m/%Y")


#change column name for binding
colnames(PM10_txt)[2] = "PM10"

#bind all PM10 data
PM10 = rbind(PM10_xls, PM10_txt)

#clean headers from the data
PM10 = PM10[- grep("ug/m3", PM10$PM10),]

#save the gathered data
write_csv(PM10, "../Gathered_Data/Dublin_Woodquay_PM10_daily.csv")




#WOODQUAY HOURLY --------------------------------------------------------------------------------
#import the Benzene file for this region
Dublin_Woodquay_Benzene_2001 <- read_excel("Dublin_Woodquay_Benzene_2001.xls")

#clean headers from the data
Dublin_Woodquay_Benzene_2001 = Dublin_Woodquay_Benzene_2001[- grep("ug/m3", Dublin_Woodquay_Benzene_2001$Benzene),]

#not enough data to save and use

#clean the enviroment
rm(list=ls())


