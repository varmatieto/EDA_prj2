#read remote file and unzip it

if (!file.exists("data/summarySCC_PM25.rds")) 
{
  url<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(url, "exdata_data_NEI_data.zip")
  unzip("exdata_data_NEI_data.zip", exdir = "data")
  file.remove("exdata_data_NEI_data.zip")
}


dir("data")

## load data file 
NEI <- readRDS("data/summarySCC_PM25.rds")

myyears<-levels(factor(NEI$year))
myyears<-as.numeric(myyears)

# select data for Baltimora only
baNEI<-NEI[NEI$fips == "24510",]
str(baNEI)

# summarize emissions per year in Baltimora

totemBA<-sapply(myyears, function(x) sum(baNEI$Emissions[baNEI$year==x]))
names(totemBA)<-myyears
totemBAM<-totemBA/1000

png("submitted_plot/plot2.png", bg = "white")

barplot(totemBAM, ylab="in million tons", 
        main=" PM2.5 emissions in Baltimore per year", col=3:6)
dev.off()
#

