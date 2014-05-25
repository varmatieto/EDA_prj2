
#QUESTION 4
#  Across the United States,how have emissions from coal 
#  combustion-related sources changed from 1999-2008?

library (ggplot2)

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
SCC <- readRDS("data/Source_Classification_Code.rds")

# there are 3 EI.Sector related to coal
levels(SCC$EI.Sector) [grepl("Coal",levels(SCC$EI.Sector))]

# there are 99 SCC related to coal
coalSCC<-SCC$SCC[grepl("Coal",SCC[,"EI.Sector"])]
length(coalSCC)


coalNEI<-NEI[NEI$SCC%in%coalSCC,]


# 28480 out of 6497651 observation in NEI relates with coal

str(coalNEI)

# levels in dataset are in the three EI.Sector related to coal
levels(factor(coalNEI$SCC))%in%coalSCC

# clean out all emissions==0
coalNEIt<-coalNEI[(!coalNEI$Emissions==0),]
coalNEIt$year<-factor(coalNEIt$year)

str(coalNEIt)

png("submitted_plot/plot4.png")

ggplot(coalNEIt, aes(x=log(Emissions), color=year, 
                     fill=year)) +
    geom_density(alpha=.2,size = 1.5, ) + 
    labs(title = "PM2.5 emissions in U.S related to coal per year\n",
         x = "log(PM2.5 in ton)", y = "density")

dev.off()



