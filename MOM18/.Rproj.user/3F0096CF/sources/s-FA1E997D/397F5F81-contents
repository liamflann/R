###############################################################################
##############################CLEANING#########################################
#This section removes constituent codes that we don't want to contact as well
#as removing duplicate lines, and cleaninng up the zip codes etc. This will be
#followed by assigning anchor levels and coding schemes to donors for analysis.
##############################################################################

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#Load csv
m <- read_csv("test.csv")

#Remove bad constituent codes.
m <- m[!m$`Constituency Code`=="Business",]
m <- m[!m$`Constituency Code`=="Client of The Gathering Place",]
m <- m[!m$`Constituency Code`=="Estate",]
m <- m[!m$`Constituency Code`=="Religious Organization",]
m <- m[!m$`Constituency Code`=="Vendor",]
m <- m[!m$`Constituency Code`=="Church",]
m <- m[!m$`Constituency Code`=="Club",]
m <- m[!m$`Constituency Code`=="Foundations",]
m <- m[!m$`Constituency Code`=="Government",]
m <- m[!m$`Constituency Code`=="Organization",]
m <- m[!m$`Constituency Code`=="Schools",]
m <- m[!m$`Key Indicator`=="Organization",]
m <- m[!m$`Preferred Address Lines` == "",]

#Remove duplicate rows.
dim(m)
m <- m %>% distinct(`Constituent Import ID`, `Name`, `Key Indicator`,
                    `Last Gift Amount`, `Last Gift Date`,
                    `Largest Gift Amount`, `Largest Gift Date`, 
                    `Gift Average Amount_1`, `Gift Total Number of Gifts_1`,
                    `Preferred Address Lines`, `Preferred City_ State`,
                    `Preferred ZIP`, `Primary Addressee`, `Primary Salutation`,
                    .keep_all = TRUE)
dim(m)


m <- m %>% distinct(`Constituent Import ID`,
                    .keep_all = TRUE)
dim(m)

m <- m %>% separate(`Preferred City_ State`, into=c("City", "State"), sep=",")
ms <- m %>% select(Name, `Preferred Address Lines`,
                   City, State,`Preferred ZIP`)


dim(ms)
write_csv(ms,"Surpression.csv")

#Remove $ from numbers so we can convert to numeric
m$`Last Gift Amount` <- parse_number(m$`Last Gift Amount`)
m$`Largest Gift Amount` <- parse_number(m$`Largest Gift Amount`)
m$`Gift Average Amount_1` <- parse_number(m$`Gift Average Amount_1`)
m$`Gift Average Amount_2` <- parse_number(m$`Gift Average Amount_2`)

#Remove low level donors
m <- m[!m$`Largest Gift Amount`<20,]

m$`Last Gift Date` <- mdy(m$`Last Gift Date`)

m <- m[!(m$`Last Gift Date` < mdy(12312007) & 
           m$`Gift Total Number of Gifts_1` < 5),]

dim(m)
###############################################################################
####################ASSIGNING PACKAGE CODES####################################
###############################################################################

###Creating the year
m <- m %>% separate(`Last Gift Date`,
                    into =c("Year", "Month", "Day"))
m$Year <- (2018-as.numeric(m$Year))
colnames(m)
#Remove Monthly donors
m <- m[!(m$`Year` == 0 & m$`Last Gift Appeal ID`=="Safe Place"),]
m <- m[!(m$`Year` == 0 & m$`Last Gift Appeal ID`=="Monthly Giver"),]

m$Month <- NULL
m$Day <- NULL

m$Level <- NA
m$Level <- ((m$`Last Gift Amount`+m$`Largest Gift Amount`)/2)
m$levelcode <- NA

m$diff <- NA
m$diff <- abs(m$`Gift Average Amount_2` - m$Level)

###If last gift largest gift average is more than 1.5 times the average gift amount
## we use the average gift amount (non zero gifts) to determine the prompts
m$greaterthan <- NA
m$greaterthan <-ifelse(m$diff > 1.5*m$`Gift Average Amount_2`, TRUE, FALSE)

#Separate into two separate categores, ones where we need to use average, and
#one we need to use LGLG. mu=average
UseMu <- subset(m, m$greaterthan==TRUE)
#Give every donor the level "C"
UseMu$levelcode[UseMu$Year < 28] <- "C"
#All donors get level B if their gift is under $250.
UseMu$levelcode[UseMu$Year < 28 & UseMu$`Gift Average Amount_2` < 250] <- "B"
#All donors get level A if their gift is under 100.
UseMu$levelcode[UseMu$Year < 28 & UseMu$`Gift Average Amount_2` < 100] <- "A"
#If their gift is less than 18 years old, and gift under 2500, get D
UseMu$levelcode[UseMu$Year < 18] <- "D"
#If their gift is less than 18 years old and gift under 750, get C
UseMu$levelcode[UseMu$Year < 18 & UseMu$`Gift Average Amount_2` < 750] <- "C"
UseMu$levelcode[UseMu$Year < 18 & UseMu$`Gift Average Amount_2` < 250] <- "B"
UseMu$levelcode[UseMu$Year < 18 & UseMu$`Gift Average Amount_2` < 100] <- "A"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` < 5000] <- "E"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` < 2500] <- "D"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` < 750] <- "C"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` < 250] <- "B"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` < 100] <- "A"
UseMu$levelcode[UseMu$Year < 7 & UseMu$`Gift Average Amount_2` > 5000] <- "MAJOR"
summary(factor(UseMu$levelcode))

UseDiff <- subset(m, m$greaterthan==FALSE)

UseDiff$levelcode[UseDiff$Year < 28] <- "C"
UseDiff$levelcode[UseDiff$Year < 28 & UseDiff$Level < 250] <- "B"
UseDiff$levelcode[UseDiff$Year < 28 & UseDiff$Level < 100] <- "A"
UseDiff$levelcode[UseDiff$Year < 18] <- "D"
UseDiff$levelcode[UseDiff$Year < 18 & UseDiff$Level < 750] <- "C"
UseDiff$levelcode[UseDiff$Year < 18 & UseDiff$Level < 250] <- "B"
UseDiff$levelcode[UseDiff$Year < 18 & UseDiff$Level < 100] <- "A"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level < 5000] <- "E"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level < 2500] <- "D"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level < 750] <- "C"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level < 250] <- "B"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level < 100] <- "A"
UseDiff$levelcode[UseDiff$Year < 7 & UseDiff$Level > 5000] <- "MAJOR"
summary(factor(UseDiff$levelcode))

mom <- rbind(UseDiff,UseMu)
summary(factor(mom$levelcode))
mom$Code <- NA
mom$Code <- paste("M",mom$Year,mom$levelcode,"18", sep="")

mom$AnchorLow <- NA
mom$AnchorMid <- NA
mom$AnchorHigh <- NA

#A level
mom$AnchorLow[mom$levelcode == "A"] <- "$35 provides a nutritious meal to 11 members"
mom$AnchorMid[mom$levelcode == "A"] <- "$75 provides clean clothing for 11 members"
mom$AnchorHigh[mom$levelcode == "A"] <- "$125 supplies a month of infant nutrition to 5 mothers"

#B level
mom$AnchorLow[mom$levelcode == "B"] <- "$150 pays for 2 weeks of jobs and education assistance"
mom$AnchorMid[mom$levelcode == "B"] <- "$200 provides a nutritious meal to 80 members"
mom$AnchorHigh[mom$levelcode == "B"] <- "$250 supplies a month of infant nutrition to 10 mothers"
#C Level
mom$AnchorLow[mom$levelcode == "C"] <- "$300 pays for 1 month of jobs and education assistance"
mom$AnchorMid[mom$levelcode == "C"] <- "$500 provides a nutritious meal to over 200 members"
mom$AnchorHigh[mom$levelcode == "C"] <- "$700 pays for 6 months of high school equivalency exam fees"
#D level
mom$AnchorLow[mom$levelcode == "D"] <- "$700 pays for 6 months of high school equivalency exam fees"
mom$AnchorMid[mom$levelcode == "D"] <- "$1000 supplies a month of infant nutrition to 45 mothers"
mom$AnchorHigh[mom$levelcode == "D"] <- "$1500 provides 3 days of nutritious meals to over 200 daily visitors"
#E level
mom$AnchorLow[mom$levelcode == "E"] <- "$1500 provides 3 days of nutritious meals to over 200 daily visitors"
mom$AnchorMid[mom$levelcode == "E"] <- "$2000 supplies 2 months of infant nutrition to 45 mothers"
mom$AnchorHigh[mom$levelcode == "E"] <- "$3750 supplies the family area for 3 months"

summary(factor(mom$levelcode))
colnames(mom)

momfinal <- mom %>% select(`Constituent Import ID`, `Primary Addressee`,
                           `Primary Salutation`, `Preferred Address Lines`,
                           City, State,`Preferred ZIP`, Code,
                           AnchorLow, AnchorMid, AnchorHigh, Year, `Gift Average Amount_2`,
                           `Largest Gift Amount`, levelcode)
dim(momfinal)


dim(ms)
write_csv(momfinal, "FinalList.csv")



