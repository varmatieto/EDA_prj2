library (lattice)
library (ggplot2)


if (!file.exists("data/summarySCC_PM25.rds")) 
{
  url<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(url, "exdata_data_NEI_data.zip")
  unzip("exdata_data_NEI_data.zip", exdir = "data")
  file.remove("exdata_data_NEI_data.zip")
}


dir("data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

str(NEI)

table(NEI$year)

str(SCC)

head(SCC)
colnames(SCC)

table(SCC$Data.Category)

head (SCC$Short.Name[SCC$Data.Category=="Biogenic"])
head (SCC$Short.Name[SCC$Data.Category=="Event"])
head (SCC$Short.Name[SCC$Data.Category=="Nonpoint"])
head (SCC$Short.Name[SCC$Data.Category=="Nonroad"])
head (SCC$Short.Name[SCC$Data.Category=="Onroad"])
head (SCC$Short.Name[SCC$Data.Category=="Point"])

table(NEI$Pollutant)
table(NEI$type)

# NON-ROAD NONPOINT  ON-ROAD    POINT 
# 2324262   473759  3183599   516031 



myyears<-levels(factor(NEI$year))
myyears<-as.numeric(myyears)

mytype<-levels(factor(NEI$type))


sapply(myyears, function(x) x)

########################plot1 #################################

totemiss<-sapply(myyears, function(x) sum(NEI$Emissions[NEI$year==x]))
names(totemiss)<-myyears
totemM<-totemiss/1000000
       
barplot(totemM, ylab="total in million tons",
        main=" PM2.5 emissions in United States per year")

#########lattice

histogram (  ~ year | type , data = NEI)

barchart(totemM, horizontal=T)

qplot(log(Emission), data =NEI)




#########################plot2 #####################


baNEI<-NEI[NEI$fips == "24510",]
str(baNEI)

summary(baNEI$Emissions[baNEI$year==1999])
boxplot(baNEI$Emissions[baNEI$year==1999 & baNEI$Emissions>2 ], breaks=100)

totemBA<-sapply(myyears, function(x) sum(baNEI$Emissions[baNEI$year==x]))
names(totemBA)<-myyears
totemBAM<-totemBA/1000

barplot(totemBAM, ylab=", color= type",
        main=" PM2.5 emissions in Baltimore per year", col=1:4)

barchart(totemBAM, horizontal=T)

#
########################plot3 #####################

baNEI<-NEI[NEI$fips == "24510",]
str(baNEI)


baNEI$year<-factor(baNEI$year)
baNEI$type<-factor(baNEI$type)
levels(baNEI$type)


############# ggplot2

qplot(log(Emissions), data=baNEI,  fill =  type, facets= .~ year, binwidth=.8 )+ coord_flip()

qplot(log(Emissions), data=baNEI,  fill =  type, facets= .~ year,
      geom="bar", position="stack")
qplot(log(Emissions), data=baNEI,  fill =  type, #facets= .~ year,
      geom="bar", position="dodge", binwidth=.8 )
qplot(log(Emissions), data=baNEI,  fill =  type, facets= .~ year,
      geom="bar", position="fill")
qplot(log(Emissions), data=baNEI,  fill =  type, facets= .~ year,
      geom="bar", position="identity")


qplot(log(Emissions), data=baNEI,  geom = "density", col= type, facets= .~ year)

qplot(log(Emissions), data=baNEI,  geom = "density", shape= type, facets= .~ year)

png("eda2_31.png")

qplot(factor(year), log(Emissions), data=baNEI, 
      color= type, facets= type~., legend = FALSE,
      geom=c("point"), xlab="", ylab="log(Emissions)",
      main=" PM2.5 emissions in Baltimore per year")

dev.off()

png("eda2_30.png")

qplot(year, log(Emissions), data = baNEI, 
      facets= type~.  , color= type, show_guide = FALSE,
      geom=c("boxplot"), xlab="", ylab="log(Emissions)",
      main=" PM2.5 emissions in Baltimore per year
      ")

dev.off()




png("eda2_33.png")

ggplot(baNEI, aes(type,Emissions)) +
geom_point(aes(color = type),legend = FALSE,
               size = 4, alpha = 1/2) + facet_grid(. ~ year) +
    labs(title = "Emissions in Baltimore per year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()


g<-ggplot(baNEI, aes(type,log(Emissions)))

g + geom_point() + facet_grid(. ~ year) 

g + geom_point(aes(color = type),legend = FALSE,
               size = 4, alpha = 1/2) + facet_grid(. ~ year) 

png("eda2_34.png")

ggplot(baNEI, aes(type,log(Emissions))) + 
            geom_point(aes(color = type),show_guide = FALSE,size = 4, alpha = 1/2) + 
            facet_grid(. ~ year) +
            labs(title = "Emissions in Baltimore per year") + 
            labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()


png("eda2_35.png")

ggplot(baNEI, aes(year,log(Emissions))) + 
    geom_boxplot(aes(color = type),show_guide = FALSE,size = 1.2, alpha = 1/2) + 
    facet_grid(type ~ .) +
    labs(title = "Emissions in Baltimore per year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()

png("eda2_36.png", width = 600, height = 480)

baNEI$type<-factor(baNEI$type, labels=c("N_R","N_P","O_R","P"))
ggplot(baNEI, aes(type,log(Emissions),show_guide = FALSE)) + 
  geom_boxplot(aes(color = type),show_guide = FALSE,size = 1.1) + 
  facet_grid(. ~ year) +
  labs(title = "Emissions in Baltimore per year") + 
  labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()
########lattice

barchart( type ~ totEm| factor(year), data = badf, 
          auto.key = list(columns = 4),
          main = "PM2.5 emissions in Baltimore per year", 
          xlab = "total Emissions in tons")

barchart( ~ totEm| factor(year), data = badf, 
          groups=type ,auto.key = list(columns = 4),
          main = "PM2.5 emissions in Baltimore per year", 
          xlab = "total Emissions in tons")

########################################################################

bamtx<- matrix(0,4,4)
colnames(bamtx)<-myyears
rownames(bamtx)<-mytype

for (i in 1:4) {
    
    tt<-mytype[i]
    bamtx[i,]<-  sapply(myyears, function(x) sum(baNEI$Emissions[baNEI$year==x & baNEI$type==tt]))
    
}

bamtx

barplot(bamtx, col=rainbow(4))


badfsy<-ddply(baNEI, .(type,year), summarise,
        sum= round(sum(Emissions),2),
        mean = round(mean(Emissions), 2),
        sd = round(sd(Emissions), 2))
str(badfsy)
head(badfsy)

qplot(factor(year), sum, data=badfsy, geom="bar", 
      stat="identity", fill=type)



######

bamtx2<- matrix(0,16,3)

for (i in 1:4) {
    
    tt<-mytype[i]
    
    for(j in 1:4) {
        r<-(i-1)*4+j    
        x<-myyears[j]  
        
        bamtx2[r,1]<-x  
        bamtx2[r,2]<- tt  
        bamtx2[r,3]<-round(sum(baNEI$Emissions[baNEI$year==x & baNEI$type==tt]),3)
    }
    
    
}

bamtx2


badf<-as.data.frame(bamtx2, stringsAsFactors =F)
colnames(badf)<-c("year","type","totEm")
badf$totEm<-as.numeric(badf$totEm)
str(badf)
head(badf)


############## ggplot2


qplot(year , totEm, data=badf, fill=type)
qplot(totEm ,year, data=badf, color=type, size = 40,geom = "point")

##
sg<-ggplot(badf, aes(type,totEm, fill = type) )

sg + geom_bar( stat="identity" )

sg  + geom_bar(width=.8, stat="identity")  + facet_grid(. ~ year) 

sg + geom_bar(width=.8, stat="identity", 
               show_guide = FALSE) + facet_grid(.~ year)

##################################################################
#QUESTION 4
#  Across the United States,how have emissions from coal 
#  combustion-related sources changed from 1999-2008?


for (i in colnames(SCC)){
    message(paste('Number of hits in column ',i,': ',sum(grepl("Coal",SCC[,i]))))
}

str(SCC)
colnames(SCC)
levels(SCC$EI.Sector) 
levels(SCC$EI.Sector) [grepl("Coal",levels(SCC$EI.Sector))]
SCC$Short.Name[grepl("Coal",SCC[,"EI.Sector"])]
SCC$SCC[grepl("Coal",SCC[,"EI.Sector"])]

# there are 99 SCC related to coal

coalSCC<-SCC$SCC[grepl("Coal",SCC[,"EI.Sector"])]

str(NEI)


coalNEI<-NEI[NEI$SCC%in%coalSCC,]


# 28480 out of 6497651 observation in NEI relates with coal

str(coalNEI)

levels(factor(coalNEI$SCC))%in%coalSCC



png("eda2_40.png")

qplot(factor(year), log(Emissions), data = coalNEI, 
      facets= type~.  , color= type, legend = FALSE,
      geom=c("boxplot"), xlab="", ylab="log(Emissions)",
      main=" PM2.5 emissions in U.S related to coal per year  ")

dev.off()

png("eda2_41.png")

qplot(factor(year), log(Emissions), data = coalNEI, 
      fill= type, legend = FALSE,
      geom=c("boxplot"), xlab="", ylab="log(Emissions)",
      main=" PM2.5 emissions in U.S related to coal per year  ")

dev.off()


png("eda2_43.png")

g<-ggplot(coalNEI, aes(type,Emissions))
g + geom_point(aes(color = type),legend = FALSE,
               size = 4, alpha = 1/2) + facet_grid(. ~ year) +
    labs(title = "PM2.5 emissions in U.S related to coal per year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()


g<-ggplot(coalNEI, aes(type,log(Emissions)))

g + geom_point() + facet_grid(. ~ year) 

g + geom_point(aes(color = type),legend = FALSE,
               size = 4, alpha = 1/2) + facet_grid(. ~ year) 

png("eda2_44.png")

g + geom_point(aes(color = type),
               size = 4, alpha = 1/2) + facet_grid(. ~ year) +
    labs(title = "PM2.5 emissions in U.S related to coal per year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()

###### former plot in project as plot4

png("eda2_46.png")
g<-ggplot(coalNEI, aes(type,log(Emissions)))

g + geom_boxplot(aes(color = type), show_guide = FALSE,
                 size = 1, alpha = 1/2) + facet_grid(. ~ year) +
    labs(title = "PM2.5 emissions in U.S related to coal per year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()

coalNEI$year<-factor(coalNEI$year)

png("eda2_45.png")

ggplot(coalNEI, aes(x=log(Emissions), color=year, 
                     fill=year)) +
    geom_density(alpha=.2,size = 1.5, ) + 
    labs(title = "PM2.5 emissions in U.S related to coal per year\n",
         x = "log(PM2.5 in ton)", y = "density")

dev.off()



#QUESTION 5
#  How have emissions from motor vehicle sources 
#  changed from 1999-2008 in Baltimore City 

for (i in colnames(SCC)){
    message(paste('Number of hits in column ',i,': ',sum(grepl("Mobile - On-Road",SCC[,i]))))
}

str(SCC)
colnames(SCC)
SCC$Short.Name[grepl("Mobile - On-Road",SCC[,"EI.Sector"])]
SCC$SCC[grepl("Mobile - On-Road",SCC[,"EI.Sector"])]
levels(SCC$SCC.Level.One) 
levels(SCC$EI.Sector) 
levels(SCC$EI.Sector) [grepl("Mobile - On-Road",levels(SCC$EI.Sector))]

SCC$Short.Name[SCC$EI.Sector=="Mobile - Non-Road Equipment - Diesel"]

# there are 1138 SCC related to Mobile - On-Road

motorSCC<-SCC$SCC[grepl("Mobile - On-Road",SCC[,"EI.Sector"])]

str(baNEI)


# in baNEI 1119 out of 2096 are about motor vehicles

motor_baNEI<-baNEI[baNEI$SCC%in%motorSCC,]
head(motor_baNEI)

motor_baNEI$year<-as.factor(motor_baNEI$year)
motor_baNEI$type<-as.factor(motor_baNEI$type)
str(motor_baNEI)


qplot(log(Emissions), facets= year~.  ,
      data = motor_baNEI, binwidth = .4, fill = year)

png("eda2_50.png")

qplot(year, log(Emissions), data = motor_baNEI, 
      facets= type~.  , color= type, legend = FALSE,
      geom=c("boxplot"), xlab="", ylab="log(Emissions)",
      main="PM2.5 emissions in Baltimora related to motor vehicle per year  ")

dev.off()

png("eda2_51.png")

qplot(year, log (Emissions), data = motor_baNEI, 
      color= type, legend = FALSE,
      geom=c("boxplot"), xlab="", ylab="log(Emissions)",
      main="PM2.5 emissions in Baltimora related to motor vehicle per year")

dev.off()

png("eda2_52.png")

qplot(log(Emissions), facets= year~.  ,
      data = motor_baNEI, binwidth = .4, fill = year,
      ylab="frequency", xlab="log(Emissions)",
      main="PM2.5 emissions in Baltimora related to motor vehicle per year")

dev.off()

png("eda2_53.png")

g<-ggplot(motor_baNEI, aes(year,Emissions))
g + geom_point(aes(color = year),size = 4, alpha = 1/2) + 
    labs(title = "Emissions in Baltimora related to motor vehicle per year") + 
    labs(x = "type", y = "PM2.5 in ton")

dev.off()


png("eda2_54.png")

    ggplot(motor_baNEI, aes(year,log(Emissions))) +
    geom_point(aes(color = type), size = 4, alpha = 1/2)  +
    labs(title = "Emissions in Baltimora related to motor vehicle year") + 
    labs(x = "type", y = "log(PM2.5 in ton)")

dev.off()

png("eda2_55.png", width = 640, height = 480)

ggplot(motor_baNEI, aes(x=log(Emissions), colour=year)) +
    geom_density(size = 2) + 
    labs(title = "Emissions in Baltimora related to motor vehicle per year",
    x = "log(PM2.5 in ton)", y = "density")

dev.off()

png("eda2_56.png")

ggplot(motor_baNEI, aes(x=year, y=log(Emissions),  fill=year)) +
    geom_boxplot() +  guides( fill=FALSE) + 
    labs(title = "Emissions in Baltimora related to motor vehicle per year",
    y = "log(PM2.5 in ton)", x = "year")
dev.off()

#QUESTION 6
#.Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources
# in Los Angeles County, California


balsNEI<-NEI[NEI$fips == "24510" | NEI$fips == "06037" ,]
str(balsNEI)

str(balsNEI)

balsNEI$fips<-factor(balsNEI$fips)
balsNEI$year<-factor(balsNEI$year)
balsNEI$type<-factor(balsNEI$type)

levels(balsNEI$fips)<-c("LA","BA")
levels(balsNEI$type)
levels(balsNEI$fips)
levels(balsNEI$year)

###

motorSCC<-SCC$SCC[grepl("Mobile - On-Road",SCC[,"EI.Sector"])]

str(motorSCC)


# in baNEI 1119 out of 2096 are about motor vehicles

motor_balsNEI<-balsNEI[balsNEI$SCC%in%motorSCC,]
str(motor_balsNEI)

motor_balsNEI$fips<-factor(motor_balsNEI$fips)
motor_balsNEI$year<-factor(motor_balsNEI$year)
motor_balsNEI$type<-factor(motor_balsNEI$type)
levels(motor_balsNEI$type)
levels(motor_balsNEI$fips)
levels(motor_balsNEI$year)

levels(motor_balsNEI$fips)<-c("LA","BA")

str(motor_balsNEI)

table(motor_balsNEI$type)


mySCC<-motor_balsNEI$SCC[motor_balsNEI$type=="POINT"]

str(SCC)

SCC$EI.Sector[SCC$SCC%in%mySCC]
SCC [SCC$SCC%in%mySCC, 1:3]

ggplot(motor_balsNEI, aes( x=fips, y=log(Emissions),fill=fips) ) +
  geom_boxplot( show_guide = FALSE) +
  facet_grid(. ~ year) +
  labs(title = "Emissions in Baltimora and Los Angeles related to motor vehicle",
       y = "log(PM2.5 in ton)", x = "cities in years")


ggplot(motor_balsNEI, aes( log(Emissions),fill=fips) ) +
  geom_density(alpha=.3) +
  facet_wrap( ~ year, ncol=2) +

  labs(title = "Emissions in Baltimora and Los Angeles related to motor vehicle",
       y = "log(PM2.5 in ton)", x = "cities in years")

library (plyr)

fiye<-ddply(motor_balsNEI, .(fips,year), summarise,
             totemis= sum(Emissions)/1000,
             meanemis=mean(Emissions)/1000,
            ltotemis= sum(log(Emissions)),
            lmeanemis=mean(log(Emissions))
            
            )

fiye

ggplot(fiye, aes(year, ltotemis)) + 
  geom_point(aes(color = fips), show_guide = FALSE,
             size = 10, alpha = 1/2) + 
  ggtitle("Emissions in Baltimora and Los Angeles related to motor vehicle") +
  ylab("tot log(PM2.5 in ton)") +
  geom_text(aes(label=fips), size=5) 

ggplot(fiye, aes(x=year, y=ltotemis, fill = fips) ) + 
  geom_bar(stat="identity", position=position_dodge(), show_guide = FALSE) + 
  facet_grid(fips ~ .)
  ggtitle("Emissions in Baltimora and Los Angeles related to motor vehicle") +
  ylab("tot log(PM2.5 in ton)") +
  geom_text(aes(label=fips), size=5) 

