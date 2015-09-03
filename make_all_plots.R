library(readr)
library(dplyr)
library(ggplot2)
library(zoo)


#### Utility functions ####

savePlotAsSvg <- function(plotToSave, baseFileName, widthInPx=300, heightInPx=250,
                          pngFallback=TRUE, fallbackWidth=1200, fallbackHeight=1000){
  plotDpi <- 72
  # Subtract a fudge factor to make sure we're just _inside_ the bounding box
  plotWidthInches <- widthInPx / plotDpi * 0.99
  plotHeightInches <- heightInPx / plotDpi * 0.99
  
  fallbackDpi <- 300
  plotWidthFallbackInches <- fallbackWidth / fallbackDpi
  plotHeightFallbackInches <- fallbackHeight / fallbackDpi
  
  svgDevice <- function(file, width, height) {
    library(RSvgDevice)
    devSVG(file = file, width = width, height = height, bg = "white", fg = "black",
           onefile = TRUE, xmlHeader = TRUE)
  }
  
  ggsave(plot=plotToSave, filename=paste0(baseFileName, ".svg"), device=svgDevice,
         width=plotWidthInches, height=plotHeightInches, units="in")
  if(pngFallback){
    ggsave(plot=plotToSave, filename=paste0(baseFileName, ".png"),
           width=plotWidthFallbackInches, height=plotHeightFallbackInches, units="in")
  }
}

#### Data ingest and tidying: Sydney ####

tidyHousingData <- function(df, type){
  dataSliced <- df[c(1,seq(2,ncol(df),2))]
  dateSequence <- as.Date(paste0("01-", dataSliced[1,2:ncol(dataSliced)]), format="%d-%b-%y")
  tidyData <- data.frame(date = dateSequence,
                         price = as.numeric(dataSliced[2,2:ncol(dataSliced)]),
                         location = dataSliced$X1[2],
                         type = type)
  for(r in 3:nrow(dataSliced)){
    areaName <- dataSliced$X1[r]
    areaPrice <- as.numeric(gsub(",","",dataSliced[r,2:ncol(dataSliced)]))
    tidyData <- rbind(tidyData, data.frame(date = dateSequence, price=areaPrice,
                                           location=areaName, type=type))
  }
  return(tidyData)
}

extrapolateNextQuarter <- function(df){
  previousQuarter <- sort(unique(df$date), decreasing=TRUE)[2]
  lastMonth <- as.numeric(format(max(df$date), "%m"))
  lastYear <- as.numeric(format(max(df$date), "%Y"))
  nextMonth <- ifelse(lastMonth == 12, 3, lastMonth+3)
  nextYear <- ifelse(lastMonth == 12, lastYear + 1, lastYear)
  nextDate <- as.Date(paste(nextYear, nextMonth, "01", sep="-"))
  
  ## For I(1) naive forecast:
#   previousQuarterData <- df %>% filter(date == previousQuarter) %>%
#     mutate(oldPrice = price) %>% select(location, oldPrice)
#   nextQuarterData <- df %>% filter(date == max(date)) %>%
#     inner_join(previousQuarterData, by='location') %>%
#     mutate(price = price * (price/oldPrice)) %>%
#     select(-oldPrice) %>%
#     mutate(date = nextDate)
  # Using RP Data estimate for all Sydney
  nextQuarterData <- df %>% filter(date==max(date)) %>%
    mutate(price = price*1.031,
           date = nextDate)
  return(rbind(df, nextQuarterData))
}

aptRentRaw <- read_csv("Syd_Rent_AllBr_Flats.csv", skip=1, col_names=FALSE)
aptRentData <- tidyHousingData(aptRentRaw, "Rent")

houseRentRaw <- read_csv("Syd_Rent_AllBr_Houses.csv", skip=1, col_names=FALSE)
houseRentData <- tidyHousingData(houseRentRaw, "Rent")

aptSalesRaw <- read_csv("Syd_Sales_Strata.csv", skip=3, col_names=FALSE)
aptSalesData <- tidyHousingData(aptSalesRaw, "Price") %>% extrapolateNextQuarter()

houseSalesRaw <- read_csv("Syd_Sales_Nonstrata.csv", skip=3, col_names=FALSE)
houseSalesData <- tidyHousingData(houseSalesRaw, "Price") %>% extrapolateNextQuarter()

makeYieldData <- function(rentDf, salesDf) {
  return (rentDf %>% rename(rent=price) %>% mutate(location=toupper(location)) %>%
            inner_join(salesDf %>% mutate(location=toupper(location)),
                       by=c("date","location")) %>%
            mutate(Yield=(52*rent)/(price*1000)*100) %>%
            select(date, Yield, location))
}

#### Data ingest and tidying: Melbourne ####

melbHouseRentsRaw <- read_csv("Melb_Rent_2Br_Houses.csv", col_names = FALSE)
melbRentDateSequence <- as.Date(paste0("01 ",
                                       melbHouseRentsRaw[1,seq(2,ncol(melbHouseRentsRaw),2)]),
                                format="%d %b %Y")
rowsToAverage <- which(melbHouseRentsRaw$X1 %in% c("Melbourne", "Yarra", "Port Phillip"))
rentAvg <- apply(melbHouseRentsRaw[rowsToAverage,seq(3,ncol(melbHouseRentsRaw),2)],
                 MARGIN=2,
                 FUN=function(x){
                   return(mean(as.numeric(gsub("$","",x,fixed=TRUE))))
                 })
melbHouseRents <- data.frame(date = melbRentDateSequence,
                             rent = rentAvg,
                             location = "Melbourne")


#### Exploratory plots ####

# Based on http://jfly.iam.u-tokyo.ac.jp/color/
colourPalette <- c("#E69F00", "#56B4E9", "#009E73")

## Sydney rental yields: apartments, by location

aptYieldLabels <- data.frame(date = c(as.Date("1995-01-01"), as.Date("1999-01-01"), as.Date("2008-01-01")),
                          location=c("INNER RING", "MIDDLE RING", "OUTER RING"),
                          Yield = c(4.2, 5.8, 5.5),
                          label = c("Inner Ring", "Middle Ring", "Outer Ring") )
aptYieldPlot <- ggplot(makeYieldData(aptRentData, aptSalesData) %>%
                      filter(location %in% c("INNER RING", "MIDDLE RING", "OUTER RING"))) +
  aes(x=date, y=Yield, colour=location) + geom_line(size=1.5) +
  xlab("") + ylab("Yield (%)") + ggtitle("Sydney Apartment Rental Yields") +
  scale_colour_manual(values=colourPalette) +
  guides(colour="none") +
  geom_text(data=aptYieldLabels, aes(label=label))


## Sydney rental yields: houses, by location

houseYieldPlot <- ggplot(makeYieldData(houseRentData, houseSalesData) %>%
                           filter(location %in% c("INNER RING", "MIDDLE RING", "OUTER RING"))) +
  aes(x=date, y=Yield, colour=location) + geom_line(size=1.5) +
  xlab("") + ylab("Yield (%)") + ggtitle("Sydney House Rental Yields") +
  scale_colour_manual(values=colourPalette) +
  guides(colour="none") +
  geom_text(data=aptYieldLabels, aes(label=label))


yieldLabels <- data.frame(date = c(as.Date("1995-01-01"), as.Date("2008-01-01")),
                             type = c("Houses", "Flats"),
                             Yield = c(4.2, 5.8),
                             label = c("Houses", "Flats") )

## Sydney rental yields: greater sydney, houses and apartments

combinedYieldPlot <- ggplot(makeYieldData(houseRentData, houseSalesData) %>%
                              filter(location == "GREATER SYDNEY") %>%
                              mutate(type="Houses") %>%
                              rbind(makeYieldData(aptRentData, aptSalesData) %>%
                                    filter(location == "GREATER SYDNEY") %>%
                                    mutate(type="Flats"))) +
  aes(x=date, y=Yield, colour=type) + geom_line() +
  xlab("") + ylab("Annualised %") + ggtitle("Sydney Rental Yields") +
  scale_colour_manual(values=colourPalette) +
  guides(colour="none") +
  geom_text(data=yieldLabels, aes(label=label))
savePlotAsSvg(combinedYieldPlot, "yields")

## Sydney rents: houses, by location

rentLabels <- data.frame(date = c(as.Date("2006-01-01"), as.Date("1995-01-01"), as.Date("2011-01-01")),
                             location=c("Inner Ring", "Middle Ring", "Outer Ring") ,
                             price = c(710,400,250),
                             label = c("Inner Ring", "Middle Ring", "Outer Ring") )
sydneyRentPlot <- ggplot(houseRentData %>%
                           filter(location %in% c("Inner Ring", "Middle Ring", "Outer Ring"))) +
  aes(x=date, y=price, colour=location) + geom_line(size=1.5) +
  xlab("") + ylab("Median Rent ($/wk)") + ggtitle("Sydney House Rents") +
  scale_colour_manual(values=colourPalette) +
  guides(colour="none") +
  geom_text(data=rentLabels, aes(label=label))


## Sydney and Melbourne rents

twoCitiesRentLabels <- data.frame(date=as.Date(c("1997-02-01", "2011-03-01")),
                                  rent=c(500,300),
                                  location=c("Sydney","Melbourne"),
                                  label=c("Sydney","Melbourne"))
twoCitiesRentPlot <- ggplot(houseRentData %>% filter(location == "Inner Ring") %>%
         mutate(rent=price, location="Sydney") %>%
         select(date, rent, location) %>% 
         rbind(melbHouseRents)) +
  guides(colour="none") +
  aes(x=date, y=rent, colour=location) + geom_line() +
  geom_text(data=twoCitiesRentLabels, aes(label=label)) +
  xlab('') + ylab("$/wk") +
  ggtitle("Median Rents: Inner-Ring Houses")

savePlotAsSvg(twoCitiesRentPlot, "medianRent")

## Rental affordability measures: data ingest and preparation

wages <- read_csv("Average_Wages.csv", skip=6)

wagesNSW <- zoo(wages$NSWwage, wages$date)
wagesVIC <- zoo(wages$VICwage, wages$date)
sydRent <- zoo(houseRentData %>%
                      filter(location == "GREATER SYDNEY", date >= wages$date[1]) %>%
                      .[["price"]],
                    houseRentData %>%
                      filter(location == "GREATER SYDNEY", date >= wages$date[1]) %>%
                      .[["date"]])
sydRentAfford <- merge(wagesNSW, sydRent)
sydRentAfford$wage <- na.approx(sydRentAfford$wagesNSW, rule=1:2)

melbMetroRent <- apply(melbHouseRentsRaw[which(melbHouseRentsRaw$X1=="Metro"),seq(3,ncol(melbHouseRentsRaw),2)],
                       MARGIN=2, FUN=function(x){ return(as.numeric(gsub("$","",x,fixed=TRUE))) })
melbRent <- zoo(melbMetroRent, melbRentDateSequence)
melbRentAfford <- merge(wagesVIC, melbRent)
melbRentAfford$wage <- na.approx(melbRentAfford$wagesVIC, rule=1:2)

rentShareData <- as.data.frame(sydRentAfford) %>%
  cbind(data.frame(date=index(sydRentAfford))) %>%
  mutate(wageShare = sydRent/wage * 100) %>%
  filter(!is.na(wageShare)) %>%
  select(date, wageShare) %>%
  mutate(location="Sydney") %>%
  rbind(
    as.data.frame(melbRentAfford) %>%
      cbind(data.frame(date=index(melbRentAfford))) %>%
      mutate(wageShare = melbRent/wage * 100) %>%
      filter(!is.na(wageShare)) %>%
      select(date, wageShare) %>%
      mutate(location="Melbourne")
  )

## Rental affordability plot

rentShareLabels <- data.frame(date=c(as.Date("1997-04-01"), as.Date("2012-07-01")),
                              wageShare = c(32, 25.5),
                              location = c("Sydney","Melbourne"),
                              label = c("Sydney", "Melbourne"))
rentSharePlot <- ggplot(rentShareData) + aes(x=date, y=wageShare, colour=location) +
  geom_line() + guides(colour="none") +
  xlab('') + ylab("(%)") + scale_y_continuous(breaks=c(25,30,35)) +
  ggtitle("Rents: Share of Average Wage") +
  geom_text(data=rentShareLabels, aes(label=label))

savePlotAsSvg(rentSharePlot, "rentShare")


## Alternative version: apartments and houses in Sydney
sydRentFlats <- zoo(aptRentData %>%
                 filter(location == "GREATER SYDNEY", date >= wages$date[1]) %>%
                 .[["price"]],
                 aptRentData %>%
                 filter(location == "GREATER SYDNEY", date >= wages$date[1]) %>%
                 .[["date"]])
sydRentAffordFlats <- merge(wagesNSW, sydRentFlats)
sydRentAffordFlats$wage <- na.approx(sydRentAffordFlats$wagesNSW, rule=1:2)

rentShareFlatHouseData <- as.data.frame(sydRentAfford) %>%
  cbind(data.frame(date=index(sydRentAfford))) %>%
  mutate(wageShare = sydRent/wage * 100) %>%
  filter(!is.na(wageShare)) %>%
  select(date, wageShare) %>%
  mutate(type="Houses") %>%
  rbind(
    as.data.frame(sydRentAffordFlats) %>%
      cbind(data.frame(date=index(sydRentAffordFlats))) %>%
      mutate(wageShare = sydRentFlats/wage * 100) %>%
      filter(!is.na(wageShare)) %>%
      select(date, wageShare) %>%
      mutate(type="Flats")
  )

rentShareFlatHousesPlot <- ggplot(rentShareFlatHouseData) +
  aes(x=date, y=wageShare, colour=type) +
  geom_line() + guides(colour="none") +
  xlab('') + ylab("(%)") + scale_y_continuous(breaks=c(25,30,35)) +
  ggtitle("Rents: Share of Average Wage")


