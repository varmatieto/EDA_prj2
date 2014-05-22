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

# summarize emissions per year

totemiss<-sapply(myyears, function(x) sum(NEI$Emissions[NEI$year==x]))
names(totemiss)<-myyears
totemM<-totemiss/1000000


png("submitted_plot/plot1.png", bg = "white")

barplot(totemM, ylab="in million tons", col=rainbow(4),
        main=" Total PM2.5 emissions in United States per year")

dev.off()
