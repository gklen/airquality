library(downloader)
library(data.table)
library(readr)
library(lubridate)

#Hourly data ----------
#MET - web scraping of the data
#pull data using downloader library to obtain if from the website
#data.table library then unzips the file and searches and takes the csv file needed
CarlowOakPark =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly375.zip | tar -xf- --to-stdout *hly375.csv")
CavanBallyhaise = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly675.zip | tar -xf- --to-stdout *hly675.csv")
ClareShannonairport = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly518.zip | tar -xf- --to-stdout *hly518.csv")
CorkAirport =     fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly3904.zip | tar -xf- --to-stdout *hly3904.csv")
CorkMoorePark =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly575.zip | tar -xf- --to-stdout *hly575.csv")
CorkRochesPoint = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1075.zip | tar -xf- --to-stdout *hly1075.csv")
CorkSherkinIsland = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly775.zip | tar -xf- --to-stdout *hly775.csv")
DonegalFinner =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly2075.zip | tar -xf- --to-stdout *hly2075.csv")
DonegalMalinHead = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1575.zip | tar -xf- --to-stdout *hly1575.csv")
DublinCasement =  fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly3723.zip | tar -xf- --to-stdout *hly3723.csv")
DublinAirport =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly532.zip | tar -xf- --to-stdout *hly532.csv")
DublinPhoenixPark = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly175.zip | tar -xf- --to-stdout *hly175.csv")
GalwayAthenry =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1875.zip | tar -xf- --to-stdout *hly1875.csv")
GalwayMaceHead =  fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly275.zip | tar -xf- --to-stdout *hly275.csv")
KerryValentia =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly2275.zip | tar -xf- --to-stdout *hly2275.csv")
MayoBelmullet =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly2375.zip | tar -xf- --to-stdout *hly2375.csv")
MayoClaireMorris = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly2175.zip | tar -xf- --to-stdout *hly2175.csv")
MayoKnockAirport = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly4935.zip | tar -xf- --to-stdout *hly4935.csv")
MayoNewport =     fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1175.zip | tar -xf- --to-stdout *hly1175.csv")
MeathDunsany = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1375.zip | tar -xf- --to-stdout *hly1375.csv")
RoscommonMtDillon = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1975.zip | tar -xf- --to-stdout *hly1975.csv")
SligoMarkree = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1275.zip | tar -xf- --to-stdout *hly1275.csv")
TipperaryGurteen = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1475.zip | tar -xf- --to-stdout *hly1475.csv")
WestmeathMullingar = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly875.zip | tar -xf- --to-stdout *hly875.csv")
WexfordJohnstownhill = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly1775.zip | tar -xf- --to-stdout *hly1775.csv")

#buoy data if we need it
BuoyM2 = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly62091.zip | tar -xf- --to-stdout *hly62091.csv")
BuoyM3 = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly62092.zip | tar -xf- --to-stdout *hly62092.csv")
BuoyM4 = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly62093.zip | tar -xf- --to-stdout *hly62093.csv")
BuoyM5 = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly62094.zip | tar -xf- --to-stdout *hly62094.csv")
BuoyM6 = fread("curl https://cli.fusio.net/cli/climate_data/webdata/hly62095.zip | tar -xf- --to-stdout *hly62095.csv")

#adding names of the stations to the dataset in case of future merging
colnames(CarlowOakPark) <- paste("CarlowOakPark", colnames(CarlowOakPark), sep = "_")
colnames(CarlowOakPark)[1] = "Date"

colnames(CavanBallyhaise) <- paste("CavanBallyhaise", colnames(CavanBallyhaise), sep = "_")
colnames(CavanBallyhaise)[1] = "Date"

colnames(ClareShannonairport) <- paste("ClareShannonairport", colnames(ClareShannonairport), sep = "_")
colnames(ClareShannonairport)[1] = "Date"

colnames(CorkAirport) <- paste("CorkAirport", colnames(CorkAirport), sep = "_")
colnames(CorkAirport)[1] = "Date"

colnames(CorkMoorePark) <- paste("CorkMoorePark", colnames(CorkMoorePark), sep = "_")
colnames(CorkMoorePark)[1] = "Date"

colnames(CorkRochesPoint) <- paste("CorkRochesPoint", colnames(CorkRochesPoint), sep = "_")
colnames(CorkRochesPoint)[1] = "Date"

colnames(CorkSherkinIsland) <- paste("CorkSherkinIsland", colnames(CorkSherkinIsland), sep = "_")
colnames(CorkSherkinIsland)[1] = "Date"

colnames(DonegalFinner) <- paste("DonegalFinner", colnames(DonegalFinner), sep = "_")
colnames(DonegalFinner)[1] = "Date"

colnames(DonegalMalinHead) <- paste("DonegalMalinHead", colnames(DonegalMalinHead), sep = "_")
colnames(DonegalMalinHead)[1] = "Date"

colnames(DublinCasement) <- paste("DublinCasement", colnames(DublinCasement), sep = "_")
colnames(DublinCasement)[1] = "Date"

colnames(DublinAirport) <- paste("DublinAirport", colnames(DublinAirport), sep = "_")
colnames(DublinAirport)[1] = "Date"

colnames(DublinPhoenixPark) <- paste("DublinPhoenixPark", colnames(DublinPhoenixPark), sep = "_")
colnames(DublinPhoenixPark)[1] = "Date"

colnames(GalwayAthenry) <- paste("GalwayAthenry", colnames(GalwayAthenry), sep = "_")
colnames(GalwayAthenry)[1] = "Date"

colnames(GalwayMaceHead) <- paste("GalwayMaceHead", colnames(GalwayMaceHead), sep = "_")
colnames(GalwayMaceHead)[1] = "Date"

colnames(KerryValentia) <- paste("KerryValentia", colnames(KerryValentia), sep = "_")
colnames(KerryValentia)[1] = "Date"

colnames(MayoBelmullet) <- paste("MayoBelmullet", colnames(MayoBelmullet), sep = "_")
colnames(MayoBelmullet)[1] = "Date"

colnames(MayoClaireMorris) <- paste("MayoClaireMorris", colnames(MayoClaireMorris), sep = "_")
colnames(MayoClaireMorris)[1] = "Date"

colnames(MayoKnockAirport) <- paste("MayoKnockAirport", colnames(MayoKnockAirport), sep = "_")
colnames(MayoKnockAirport)[1] = "Date"

colnames(MayoNewport) <- paste("MayoNewport", colnames(MayoNewport), sep = "_")
colnames(MayoNewport)[1] = "Date"

colnames(MeathDunsany) <- paste("MeathDunsany", colnames(MeathDunsany), sep = "_")
colnames(MeathDunsany)[1] = "Date"

colnames(RoscommonMtDillon) <- paste("RoscommonMtDillon", colnames(RoscommonMtDillon), sep = "_")
colnames(RoscommonMtDillon)[1] = "Date"

colnames(SligoMarkree) <- paste("SligoMarkree", colnames(SligoMarkree), sep = "_")
colnames(SligoMarkree)[1] = "Date"

colnames(TipperaryGurteen) <- paste("TipperaryGurteen", colnames(TipperaryGurteen), sep = "_")
colnames(TipperaryGurteen)[1] = "Date"

colnames(WestmeathMullingar) <- paste("WestmeathMullingar", colnames(WestmeathMullingar), sep = "_")
colnames(WestmeathMullingar)[1] = "Date"

colnames(WexfordJohnstownhill) <- paste("WexfordJohnstownhill", colnames(WexfordJohnstownhill), sep = "_")
colnames(WexfordJohnstownhill)[1] = "Date"

colnames(BuoyM2) <- paste("BuoyM2", colnames(BuoyM2), sep = "_")
colnames(BuoyM2)[1] = "Date"

colnames(BuoyM3) <- paste("BuoyM3", colnames(BuoyM3), sep = "_")
colnames(BuoyM3)[1] = "Date"

colnames(BuoyM4) <- paste("BuoyM4", colnames(BuoyM4), sep = "_")
colnames(BuoyM4)[1] = "Date"

colnames(BuoyM5) <- paste("BuoyM5", colnames(BuoyM5), sep = "_")
colnames(BuoyM5)[1] = "Date"

colnames(BuoyM6) <- paste("BuoyM6", colnames(BuoyM6), sep = "_")
colnames(BuoyM6)[1] = "Date"



#multiple ind columns prevent the data from merging, removing columns from the datasets
deleteCols = grep("ind", colnames(CarlowOakPark))
CarlowOakPark2 = CarlowOakPark[, (deleteCols) := NULL]
CarlowOakPark = CarlowOakPark2

deleteCols = grep("ind", colnames(CavanBallyhaise))
CavanBallyhaise2 = CavanBallyhaise[, (deleteCols) := NULL]
CavanBallyhaise = CavanBallyhaise2

deleteCols = grep("ind", colnames(ClareShannonairport))
ClareShannonairport2 = ClareShannonairport[, (deleteCols) := NULL]
ClareShannonairport = ClareShannonairport2

deleteCols = grep("ind", colnames(CorkAirport))
CorkAirport2 = CorkAirport[, (deleteCols) := NULL]
CorkAirport = CorkAirport2

deleteCols = grep("ind", colnames(CorkMoorePark))
CorkMoorePark2 = CorkMoorePark[, (deleteCols) := NULL]
CorkMoorePark = CorkMoorePark

deleteCols = grep("ind", colnames(CorkRochesPoint))
CorkRochesPoint2 = CorkRochesPoint[, (deleteCols) := NULL]
CorkRochesPoint = CorkRochesPoint2

deleteCols = grep("ind", colnames(CorkSherkinIsland))
CorkSherkinIsland2 = CorkSherkinIsland[, (deleteCols) := NULL]
CorkSherkinIsland = CorkSherkinIsland2

deleteCols = grep("ind", colnames(DonegalFinner))
DonegalFinner2 = DonegalFinner[, (deleteCols) := NULL]
DonegalFinner = DonegalFinner2

deleteCols = grep("ind", colnames(DonegalMalinHead))
DonegalMalinHead2 = DonegalMalinHead[, (deleteCols) := NULL]
DonegalMalinHead = DonegalMalinHead2

deleteCols = grep("ind", colnames(DublinAirport))
DublinAirport2 = DublinAirport[, (deleteCols) := NULL]
DublinAirport = DublinAirport2

deleteCols = grep("ind", colnames(DublinCasement))
DublinCasement2 = DublinCasement[, (deleteCols) := NULL]
DublinCasement = DublinCasement2

deleteCols = grep("ind", colnames(DublinPhoenixPark))
DublinPhoenixPark2 = DublinPhoenixPark[, (deleteCols) := NULL]
DublinPhoenixPark = DublinPhoenixPark2

deleteCols = grep("ind", colnames(GalwayAthenry))
GalwayAthenry2 = GalwayAthenry[, (deleteCols) := NULL]
GalwayAthenry = GalwayAthenry2

deleteCols = grep("ind", colnames(GalwayMaceHead))
GalwayMaceHead2 = GalwayMaceHead[, (deleteCols) := NULL]
GalwayMaceHead = GalwayMaceHead2

deleteCols = grep("ind", colnames(KerryValentia))
KerryValentia2 = KerryValentia[, (deleteCols) := NULL]
KerryValentia = KerryValentia2

deleteCols = grep("ind", colnames(MayoBelmullet))
MayoBelmullet2 = MayoBelmullet[, (deleteCols) := NULL]
MayoBelmullet = MayoBelmullet2

deleteCols = grep("ind", colnames(MayoClaireMorris))
MayoClaireMorris2 = MayoClaireMorris[, (deleteCols) := NULL]
MayoClaireMorris = MayoClaireMorris2

deleteCols = grep("ind", colnames(MayoKnockAirport))
MayoKnockAirport2 = MayoKnockAirport[, (deleteCols) := NULL]
MayoKnockAirport = MayoKnockAirport2

deleteCols = grep("ind", colnames(MayoNewport))
MayoNewport2 = MayoNewport[, (deleteCols) := NULL]
MayoNewport = MayoNewport2

deleteCols = grep("ind", colnames(MeathDunsany))
MeathDunsany2 = MeathDunsany[, (deleteCols) := NULL]
MeathDunsany = MeathDunsany2

deleteCols = grep("ind", colnames(RoscommonMtDillon))
RoscommonMtDillon2 = RoscommonMtDillon[, (deleteCols) := NULL]
RoscommonMtDillon = RoscommonMtDillon2

deleteCols = grep("ind", colnames(SligoMarkree))
SligoMarkree2 = SligoMarkree[, (deleteCols) := NULL]
SligoMarkree = SligoMarkree2

deleteCols = grep("ind", colnames(TipperaryGurteen))
TipperaryGurteen2 = TipperaryGurteen[, (deleteCols) := NULL]
TipperaryGurteen = TipperaryGurteen2

deleteCols = grep("ind", colnames(WestmeathMullingar))
WestmeathMullingar2 = WestmeathMullingar[, (deleteCols) := NULL]
WestmeathMullingar = WestmeathMullingar2

deleteCols = grep("ind", colnames(WexfordJohnstownhill))
WexfordJohnstownhill2 = WexfordJohnstownhill[, (deleteCols) := NULL]
WexfordJohnstownhill = WexfordJohnstownhill2


#bind all the weather data
#merge daily data for Ringsend
buoys = Reduce(function(x, y) merge(x, y, all=TRUE), list(BuoyM2, BuoyM3, BuoyM4, BuoyM5, BuoyM6))

write_csv(buoys, "Analysis/Buoy_weather_hourly_data.csv")


#dublin weather
Dublin_weather = Reduce(function(x, y) merge(x, y, all=TRUE), list(DublinAirport, DublinCasement, DublinPhoenixPark))


#ireland weather data
weather = Reduce(function(x, y) merge(x, y, all=TRUE), list(CarlowOakPark,
                                                            CavanBallyhaise, ClareShannonairport, CorkAirport, CorkMoorePark,
                                                            CorkRochesPoint, CorkSherkinIsland, DonegalFinner, DonegalMalinHead,
                                                            DublinAirport, DublinCasement, DublinPhoenixPark, GalwayAthenry,
                                                            GalwayMaceHead, KerryValentia, MayoBelmullet, MayoClaireMorris, MayoKnockAirport,
                                                            MayoNewport, MeathDunsany, RoscommonMtDillon, SligoMarkree, TipperaryGurteen,
                                                            WestmeathMullingar, WexfordJohnstownhill))



#sort the Dublin data by date
Dublin_weather$Date = as.Date(Dublin_weather$Date, format = "%d-%b-%Y")
Dublin_weather = Dublin_weather[order(as.Date(Dublin_weather$Date, format="%d/%m/%Y")),]

#sort the Ireland data by date
weather$Date = as.Date(weather$Date, format = "%d-%b-%Y")
weather = weather[order(as.Date(weather$Date, format="%d/%m/%Y")),]

#save the hourly weather data for Dublin
write_csv(Dublin_weather, "Analysis/Dublin_weather_hourly_data.csv")

#save the hourly weather data for Ireland
write_csv(weather, "Analysis/Ireland_weather_hourly_data.csv")







#DAILY DATA-----------
#TODO examine ind, maybe the model created should be weighted by this?
#hourly data MET - web scraping of the data
#pull data using downloader library to obtain if from the website
#data.table library then unzips the file and searches and takes the csv file needed
CarlowOakPark =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly375.zip | tar -xf- --to-stdout *dly375.csv")
CavanBallyhaise = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly675.zip | tar -xf- --to-stdout *dly675.csv")
ClareShannonairport = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly518.zip | tar -xf- --to-stdout *dly518.csv")
CorkAirport =     fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly3904.zip | tar -xf- --to-stdout *dly3904.csv")
CorkMoorePark =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly575.zip | tar -xf- --to-stdout *dly575.csv")
CorkRochesPoint = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1075.zip | tar -xf- --to-stdout *dly1075.csv")
CorkSherkinIsland = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly775.zip | tar -xf- --to-stdout *dly775.csv")
DonegalFinner =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly2075.zip | tar -xf- --to-stdout *dly2075.csv")
DonegalMalinHead = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1575.zip | tar -xf- --to-stdout *dly1575.csv")
DublinCasement =  fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly3723.zip | tar -xf- --to-stdout *dly3723.csv")
DublinAirport =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly532.zip | tar -xf- --to-stdout *dly532.csv")
DublinPhoenixPark = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly175.zip | tar -xf- --to-stdout *dly175.csv")
GalwayAthenry =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1875.zip | tar -xf- --to-stdout *dly1875.csv")
GalwayMaceHead =  fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly275.zip | tar -xf- --to-stdout *dly275.csv")
KerryValentia =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly2275.zip | tar -xf- --to-stdout *dly2275.csv")
MayoBelmullet =   fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly2375.zip | tar -xf- --to-stdout *dly2375.csv")
MayoClaireMorris = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly2175.zip | tar -xf- --to-stdout *dly2175.csv")
MayoKnockAirport = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly4935.zip | tar -xf- --to-stdout *dly4935.csv")
MayoNewport =     fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1175.zip | tar -xf- --to-stdout *dly1175.csv")
MeathDunsany = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1375.zip | tar -xf- --to-stdout *dly1375.csv")
RoscommonMtDillon = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1975.zip | tar -xf- --to-stdout *dly1975.csv")
SligoMarkree = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1275.zip | tar -xf- --to-stdout *dly1275.csv")
TipperaryGurteen = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1475.zip | tar -xf- --to-stdout *dly1475.csv")
WestmeathMullingar = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly875.zip | tar -xf- --to-stdout *dly875.csv")
WexfordJohnstownhill = fread("curl https://cli.fusio.net/cli/climate_data/webdata/dly1775.zip | tar -xf- --to-stdout *dly1775.csv")


#adding names of the stations to the dataset in case of future merging
colnames(CarlowOakPark) <- paste("CarlowOakPark", colnames(CarlowOakPark), sep = "_")
colnames(CarlowOakPark)[1] = "Date"

colnames(CavanBallyhaise) <- paste("CavanBallyhaise", colnames(CavanBallyhaise), sep = "_")
colnames(CavanBallyhaise)[1] = "Date"

colnames(ClareShannonairport) <- paste("ClareShannonairport", colnames(ClareShannonairport), sep = "_")
colnames(ClareShannonairport)[1] = "Date"

colnames(CorkAirport) <- paste("CorkAirport", colnames(CorkAirport), sep = "_")
colnames(CorkAirport)[1] = "Date"

colnames(CorkMoorePark) <- paste("CorkMoorePark", colnames(CorkMoorePark), sep = "_")
colnames(CorkMoorePark)[1] = "Date"

colnames(CorkRochesPoint) <- paste("CorkRochesPoint", colnames(CorkRochesPoint), sep = "_")
colnames(CorkRochesPoint)[1] = "Date"

colnames(CorkSherkinIsland) <- paste("CorkSherkinIsland", colnames(CorkSherkinIsland), sep = "_")
colnames(CorkSherkinIsland)[1] = "Date"

colnames(DonegalFinner) <- paste("DonegalFinner", colnames(DonegalFinner), sep = "_")
colnames(DonegalFinner)[1] = "Date"

colnames(DonegalMalinHead) <- paste("DonegalMalinHead", colnames(DonegalMalinHead), sep = "_")
colnames(DonegalMalinHead)[1] = "Date"

colnames(DublinCasement) <- paste("DublinCasement", colnames(DublinCasement), sep = "_")
colnames(DublinCasement)[1] = "Date"

colnames(DublinAirport) <- paste("DublinAirport", colnames(DublinAirport), sep = "_")
colnames(DublinAirport)[1] = "Date"

colnames(DublinPhoenixPark) <- paste("DublinPhoenixPark", colnames(DublinPhoenixPark), sep = "_")
colnames(DublinPhoenixPark)[1] = "Date"

colnames(GalwayAthenry) <- paste("GalwayAthenry", colnames(GalwayAthenry), sep = "_")
colnames(GalwayAthenry)[1] = "Date"

colnames(GalwayMaceHead) <- paste("GalwayMaceHead", colnames(GalwayMaceHead), sep = "_")
colnames(GalwayMaceHead)[1] = "Date"

colnames(KerryValentia) <- paste("KerryValentia", colnames(KerryValentia), sep = "_")
colnames(KerryValentia)[1] = "Date"

colnames(MayoBelmullet) <- paste("MayoBelmullet", colnames(MayoBelmullet), sep = "_")
colnames(MayoBelmullet)[1] = "Date"

colnames(MayoClaireMorris) <- paste("MayoClaireMorris", colnames(MayoClaireMorris), sep = "_")
colnames(MayoClaireMorris)[1] = "Date"

colnames(MayoKnockAirport) <- paste("MayoKnockAirport", colnames(MayoKnockAirport), sep = "_")
colnames(MayoKnockAirport)[1] = "Date"

colnames(MayoNewport) <- paste("MayoNewport", colnames(MayoNewport), sep = "_")
colnames(MayoNewport)[1] = "Date"

colnames(MeathDunsany) <- paste("MeathDunsany", colnames(MeathDunsany), sep = "_")
colnames(MeathDunsany)[1] = "Date"

colnames(RoscommonMtDillon) <- paste("RoscommonMtDillon", colnames(RoscommonMtDillon), sep = "_")
colnames(RoscommonMtDillon)[1] = "Date"

colnames(SligoMarkree) <- paste("SligoMarkree", colnames(SligoMarkree), sep = "_")
colnames(SligoMarkree)[1] = "Date"

colnames(TipperaryGurteen) <- paste("TipperaryGurteen", colnames(TipperaryGurteen), sep = "_")
colnames(TipperaryGurteen)[1] = "Date"

colnames(WestmeathMullingar) <- paste("WestmeathMullingar", colnames(WestmeathMullingar), sep = "_")
colnames(WestmeathMullingar)[1] = "Date"

colnames(WexfordJohnstownhill) <- paste("WexfordJohnstownhill", colnames(WexfordJohnstownhill), sep = "_")
colnames(WexfordJohnstownhill)[1] = "Date"



#multiple ind columns prevent the data from merging, removing columns from the datasets
deleteCols = grep("ind", colnames(CarlowOakPark))
CarlowOakPark2 = CarlowOakPark[, (deleteCols) := NULL]
CarlowOakPark = CarlowOakPark2

deleteCols = grep("ind", colnames(CavanBallyhaise))
CavanBallyhaise2 = CavanBallyhaise[, (deleteCols) := NULL]
CavanBallyhaise = CavanBallyhaise2

deleteCols = grep("ind", colnames(ClareShannonairport))
ClareShannonairport2 = ClareShannonairport[, (deleteCols) := NULL]
ClareShannonairport = ClareShannonairport2

deleteCols = grep("ind", colnames(CorkAirport))
CorkAirport2 = CorkAirport[, (deleteCols) := NULL]
CorkAirport = CorkAirport2

deleteCols = grep("ind", colnames(CorkMoorePark))
CorkMoorePark2 = CorkMoorePark[, (deleteCols) := NULL]
CorkMoorePark = CorkMoorePark

deleteCols = grep("ind", colnames(CorkRochesPoint))
CorkRochesPoint2 = CorkRochesPoint[, (deleteCols) := NULL]
CorkRochesPoint = CorkRochesPoint2

deleteCols = grep("ind", colnames(CorkSherkinIsland))
CorkSherkinIsland2 = CorkSherkinIsland[, (deleteCols) := NULL]
CorkSherkinIsland = CorkSherkinIsland2

deleteCols = grep("ind", colnames(DonegalFinner))
DonegalFinner2 = DonegalFinner[, (deleteCols) := NULL]
DonegalFinner = DonegalFinner2

deleteCols = grep("ind", colnames(DonegalMalinHead))
DonegalMalinHead2 = DonegalMalinHead[, (deleteCols) := NULL]
DonegalMalinHead = DonegalMalinHead2

deleteCols = grep("ind", colnames(DublinAirport))
DublinAirport2 = DublinAirport[, (deleteCols) := NULL]
DublinAirport = DublinAirport2

deleteCols = grep("ind", colnames(DublinCasement))
DublinCasement2 = DublinCasement[, (deleteCols) := NULL]
DublinCasement = DublinCasement2

deleteCols = grep("ind", colnames(DublinPhoenixPark))
DublinPhoenixPark2 = DublinPhoenixPark[, (deleteCols) := NULL]
DublinPhoenixPark = DublinPhoenixPark2

deleteCols = grep("ind", colnames(GalwayAthenry))
GalwayAthenry2 = GalwayAthenry[, (deleteCols) := NULL]
GalwayAthenry = GalwayAthenry2

deleteCols = grep("ind", colnames(GalwayMaceHead))
GalwayMaceHead2 = GalwayMaceHead[, (deleteCols) := NULL]
GalwayMaceHead = GalwayMaceHead2

deleteCols = grep("ind", colnames(KerryValentia))
KerryValentia2 = KerryValentia[, (deleteCols) := NULL]
KerryValentia = KerryValentia2

deleteCols = grep("ind", colnames(MayoBelmullet))
MayoBelmullet2 = MayoBelmullet[, (deleteCols) := NULL]
MayoBelmullet = MayoBelmullet2

deleteCols = grep("ind", colnames(MayoClaireMorris))
MayoClaireMorris2 = MayoClaireMorris[, (deleteCols) := NULL]
MayoClaireMorris = MayoClaireMorris2

deleteCols = grep("ind", colnames(MayoKnockAirport))
MayoKnockAirport2 = MayoKnockAirport[, (deleteCols) := NULL]
MayoKnockAirport = MayoKnockAirport2

deleteCols = grep("ind", colnames(MayoNewport))
MayoNewport2 = MayoNewport[, (deleteCols) := NULL]
MayoNewport = MayoNewport2

deleteCols = grep("ind", colnames(MeathDunsany))
MeathDunsany2 = MeathDunsany[, (deleteCols) := NULL]
MeathDunsany = MeathDunsany2

deleteCols = grep("ind", colnames(RoscommonMtDillon))
RoscommonMtDillon2 = RoscommonMtDillon[, (deleteCols) := NULL]
RoscommonMtDillon = RoscommonMtDillon2

deleteCols = grep("ind", colnames(SligoMarkree))
SligoMarkree2 = SligoMarkree[, (deleteCols) := NULL]
SligoMarkree = SligoMarkree2

deleteCols = grep("ind", colnames(TipperaryGurteen))
TipperaryGurteen2 = TipperaryGurteen[, (deleteCols) := NULL]
TipperaryGurteen = TipperaryGurteen2

deleteCols = grep("ind", colnames(WestmeathMullingar))
WestmeathMullingar2 = WestmeathMullingar[, (deleteCols) := NULL]
WestmeathMullingar = WestmeathMullingar2

deleteCols = grep("ind", colnames(WexfordJohnstownhill))
WexfordJohnstownhill2 = WexfordJohnstownhill[, (deleteCols) := NULL]
WexfordJohnstownhill = WexfordJohnstownhill2

#dublin weather
Dublin_weather = Reduce(function(x, y) merge(x, y, all=TRUE), list(DublinAirport, DublinCasement, DublinPhoenixPark))

#come back to this for the PhD!!
weather = Reduce(function(x, y) merge(x, y, all=TRUE), list(CarlowOakPark,
                                                            CavanBallyhaise, ClareShannonairport, CorkAirport, CorkMoorePark,
                                                            CorkRochesPoint, CorkSherkinIsland, DonegalFinner, DonegalMalinHead,
                                                            DublinAirport, DublinCasement, DublinPhoenixPark, GalwayAthenry,
                                                            GalwayMaceHead, KerryValentia, MayoBelmullet, MayoClaireMorris, MayoKnockAirport,
                                                            MayoNewport, MeathDunsany, RoscommonMtDillon, SligoMarkree, TipperaryGurteen,
                                                            WestmeathMullingar, WexfordJohnstownhill))

#sort the Dublin data by date
Dublin_weather$Date = as.Date(Dublin_weather$Date, format = "%d-%b-%Y")
Dublin_weather = Dublin_weather[order(as.Date(Dublin_weather$Date, format="%d/%m/%Y")),]



#sort the Ireland data by date
weather$Date = as.Date(weather$Date, format = "%d-%b-%Y")
weather = weather[order(as.Date(weather$Date, format="%d/%m/%Y")),]




#Average the weather data across dublin. first cahnge the dates to the same format as other data
#desciption of the data https://www.met.ie/cms/assets/uploads/2018/05/KeyDaily.txt
df = Dublin_weather
df$Date = as.Date(df$Date, format = "%d-%b-%Y")
df = subset(df, Date >= "2007-01-01" & Date < "2017-01-01")
df = df[order(as.Date(df$Date, format="%Y-%m-%d")),]

rain = df
rain = subset(df, select=c(Date, DublinAirport_rain, DublinPhoenixPark_rain, DublinCasement_rain))
maxtp = subset(df, select=c(Date, DublinAirport_maxtp, DublinCasement_maxtp, DublinPhoenixPark_maxtp))
mintp = subset(df, select=c(Date, DublinAirport_mintp, DublinCasement_mintp, DublinPhoenixPark_mintp))
hum = subset(df, select=c(Date, DublinAirport_hm, DublinCasement_hm))
windspeed = subset(df, select=c(Date, DublinAirport_wdsp, DublinCasement_wdsp))
winddirection = subset(df, select=c(Date, DublinAirport_ddhm, DublinCasement_ddhm))

rain$rain = rowMeans(rain[,2:4], na.rm = TRUE)
maxtp$maxtp = rowMeans(maxtp[,2:4], na.rm = TRUE)
mintp$mintp = rowMeans(mintp[,2:4], na.rm = TRUE)
hum$hum = rowMeans(hum[,2:3], na.rm = TRUE)
windspeed$windspeed = rowMeans(windspeed[,2:3], na.rm = TRUE)
winddirection$winddirection = rowMeans(winddirection[,2:3], na.rm = TRUE)

avgweather = Reduce(function(x, y) merge(x, y, all=TRUE), list(rain, maxtp, mintp, hum, windspeed, winddirection))
Dublin_avg_weather = subset(avgweather, select=c( "Date", "rain", "maxtp", "mintp", "hum", "windspeed", "winddirection"))



#save the Dublin average weather data
write_csv(Dublin_avg_weather, "Analysis/Dublin_daily_avg_weather.csv")


#save the Dublin weather data
write_csv(Dublin_weather, "Analysis/Dublin_weather_daily_data.csv")


#save the Ireland weather data
write_csv(weather, "Analysis/Ireland_weather_daily_data.csv")






