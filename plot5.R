#QUESTION 5
#  How have emissions from motor vehicle sources 
#  changed from 1999-2008 in Baltimore City 

library (ggplot2)

#read remote file and unzip it

url<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "exdata_data_NEI_data.zip")
unzip("exdata_data_NEI_data.zip", exdir = "data")
file.remove("exdata_data_NEI_data.zip")

dir()

## load data file 
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")


# there are 4 EI.Sector related to motor vehicle sources 
levels(SCC$EI.Sector) [grepl("Mobile - On-Road",levels(SCC$EI.Sector))]

# there are 1138 SCC related to Mobile - On-Road
motorSCC<-SCC$SCC[grepl("Mobile - On-Road",SCC[,"EI.Sector"])]
length(motorSCC)

# select data for Baltimora only
baNEI<-NEI[NEI$fips == "24510",]
str(baNEI)


# in baNEI 1119 out of 2096 are about motor vehicles

motor_baNEI<-baNEI[baNEI$SCC%in%motorSCC,]
str(motor_baNEI)


# levels in dataset is only ON-ROAD
levels(factor(motor_baNEI$type))
motor_baNEI$year<-as.factor(motor_baNEI$year)
motor_baNEI$type<-as.factor(motor_baNEI$type)
levels(motor_baNEI$type)

png("submitted_plot/plot5.png")

ggplot(motor_baNEI, aes( x=year, y=log(Emissions),fill=year) ) +
  geom_boxplot( show_guide = FALSE) + coord_flip() +
  labs(title = "Emissions in Baltimora related to motor vehicle per year",
       y = "log(PM2.5 in ton)", x = "year")

dev.off()
