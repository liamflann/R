library(readr)
library(dplyr)
library(tidyr)
library(sqldf)
library(stringr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(rmarkdown)
library(ggthemes)
library(aod)

###HT to https://www.tidytextmining.com/ and http://r4ds.had.co.nz/

# Document cleans the staff communications and incident reports
# so that we are able to analyze the date distributions, and
# commonalities in the language used for staff communictions
# prior to an incident report.


##########################CLEANING################################
##################################################################
##################################################################
theme <-  theme(axis.text.x = element_text(angle=90, hjust=1))+
  theme_calc() #for plots
####Open the file, delete blank row, concatenate Name and DOB,
####delete unecessary rows, and reorder the columns.
sc <- read_csv("SCHistory.csv") #staff comm
ic <- read_csv("IRHistory.csv") #incident reports

###Staff Communication general cleaning
sc <- sc[-(1),]
sc <- sc %>% separate(`DATE OF BIRTH`, into=c("Month","Day","Year"))

sc$uniqueID <-NA
sc$uniqueID <- paste(sc$`PARTICIPANT LAST NAME`,sc$`PARTICIPANT FIRST NAME`,
                   sc$Month,sc$Day,sc$Year,sep=".") #create a unique ID

sc<- sc[,-(0:7)]
sc <- sc %>% select(uniqueID, `DATE OF CONTACT`, `TIME SPENT ON CONTACT`,
                     NOTES)

###Incident Report general cleaning

ic <- ic %>% select(`Respondent`, `Date of Birth of Member`,
                    `Staff completing this form`, `Date of Incident`, 
                    `Day of the Week`, `Time of Incident`, `Type of Incident`,
                    `Description of the Incident`,`Which slip was member given`)


ic <- ic %>% separate(Respondent, into=c("Last Name", "First Name"), sep = ",")
ic <- ic %>% separate(`First Name`, into=c("Space", "First Name"), sep = " ")
ic <- ic %>% separate(`Date of Birth of Member`, into=c("Month", "Day", "Year"))
ic$uniqueID <- paste(ic$`Last Name`,ic$`First Name`,ic$Month, ic$Day, ic$Year,
                     sep= ".")
ic <- ic %>% select(uniqueID, `Staff completing this form`, `Date of Incident`, 
                    `Day of the Week`, `Time of Incident`, `Type of Incident`,
                    `Description of the Incident`,`Which slip was member given`)


ic$`Which slip was member given`[ic$`Which slip was member given` !=
                                   "None"] <- "Yellow"

slips <- ic %>% select(uniqueID, `Which slip was member given`, `Date of Incident`)


##########################END CLEANING############################
##################################################################
##################################################################


##########################TOP STAFF AND MEMBERS###################
##################################################################
##################################################################
#Parse the NOTES column to determine most frequently occuring writer.
#Find staff with most staff communications.
sc$Notes_Kept <- sc$NOTES

#Separate based on how people write their name and title. Examples:
#Johh Smith, Manager OR John Smith: Manager, or John Smith; Manager
sc <- sc %>% separate(NOTES, into=c("Staff"), sep= ",")
sc <- sc %>% separate(Staff, into=c("Staff"), sep= ":")
sc <- sc %>% separate(Staff, into=c("Staff"), sep= ";")


#Staff and members with most communications
(top <- sort(table(sc$Staff), decreasing = TRUE)[1:5])
(top2 <-sort(table(sc$uniqueID), decreasing = TRUE)[1:5])

#Members with most incident reports.
(ir2 <- sort(table(ic$uniqueID), decreasing= TRUE)[1:5])

####Create a table with the number of staff communications per member
scMem <- sc %>% select(uniqueID)
scMem$Count <- 1 
scMem <- scMem %>% group_by(uniqueID) %>% summarise(Count = sum(as.numeric(Count)))
MembersToReview <- sqldf("select * from scMem where Count > 5")
MembersToReviewNotes <- left_join(MembersToReview, sc, by="uniqueID")


######################END TOP STAFF AND MEMBERS###################
##################################################################
##################################################################

###################DATE DISTRIBUTION OF STAFF COMM################
##################################################################
##################################################################

##Create plots of when the staff communications happen, smoothing
##by 5 day and 30.42 days.
timelineSC <- sc %>% select(uniqueID, `DATE OF CONTACT`, `TIME SPENT ON CONTACT`)
colnames(timelineSC)[colnames(timelineSC)=="DATE OF CONTACT"] ="staffcommDate"
colnames(timelineSC)[colnames(timelineSC)=="TIME SPENT ON CONTACT"]= "staffcommTime"

timelineSC$staffcommCount <- 1

##Create staffComm table and summarize count by date.
cal <- timelineSC %>% select(staffcommDate, staffcommCount) %>%
  group_by(staffcommDate) %>% summarise(count = sum(staffcommCount))

#Delete all NA values, and set to date format
cal <- cal[!is.na(cal$staffcommDate),]
cal$staffcommDate <- mdy(cal$staffcommDate)

#Create years for 2016-17 and 2017-18 (July to July)
cal1617 <- cal %>% select(staffcommDate, count) %>% 
  filter(staffcommDate <= "2017-07-19")
cal1617 %>% ggplot(aes(x = staffcommDate, y = count))+
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 7)+
  ggtitle("Staff Communications 07/2016 to 07/2017")+
  xlab("Staff Communication Date")+
  scale_x_date(date_breaks = "1 months") +
  theme

cal1718 <- cal %>% select(staffcommDate, count) %>% 
  filter(staffcommDate > "2017-07-19")
cal1718 %>% ggplot(aes(x = staffcommDate, y = count)) + 
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 7)+
  ggtitle("Staff Communications 07/2017 to 07/2018")+
  xlab("Staff Communication Date")+
  scale_x_date(date_breaks = "1 months") +
  theme
#Generate plots: week-ish and average month
cal %>% ggplot(aes(staffcommDate, count)) +
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 5)+
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Staff Communications 7/2016 to 7/2018") +
  xlab("Staff Communication Date")+
  scale_x_date(date_breaks = "1 months") +
  theme

cal %>% ggplot(aes(staffcommDate, count)) +
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 30.42)+
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Staff Communications 7/2016 to 7/2018") +
  xlab("Staff Communication Date")+
  scale_x_date(date_breaks = "1 months") +
  theme

###############END DATE DISTRIBUTION OF STAFF COMM################
##################################################################
##################################################################

#############DATE DISTRIBUTION OF INCIDENT REPORTS################
##################################################################
##################################################################

#Perform the same calculations for Incident Reports.
timelineIR <- ic %>% select(uniqueID, `Date of Incident`)
colnames(timelineIR)[colnames(timelineIR)=="Date of Incident"] = "incidentDate"

timelineIR$incidentCount <- 1

cal <- timelineIR %>% select(incidentDate, incidentCount) %>%
  group_by(incidentDate) %>% summarise(count = sum(incidentCount))

cal$incidentDate <- mdy(cal$incidentDate)
cal <- cal[!(cal$incidentDate <= "2016-07-19" ),]

cal1617 <- cal %>% select(incidentDate, count) %>% 
  filter(incidentDate <= "2017-07-19")
cal1617 %>% ggplot(aes(x = incidentDate, y = count))+
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 7)+
  ggtitle("Incident Reports 07/2016 to 07/2017")+
  xlab("Incident Date")+
  scale_x_date(date_breaks = "1 months") +
  theme

cal1718 <- cal %>% select(incidentDate, count) %>% 
  filter(incidentDate > "2017-07-19")
cal1718 %>% ggplot(aes(x = incidentDate, y = count)) + 
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 7)+
  ggtitle("Incident Reports 07/2017 to 07/2018")+
  xlab("Incident Date")+
  scale_x_date(date_breaks = "1 months") +
  theme

cal %>% ggplot(aes(incidentDate, count)) +
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 5)+
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Incident Reports 7/2016 to 7/2018") +
  xlab("Incident Report Date")+
  theme

cal %>% ggplot(aes(incidentDate, count)) +
  stat_summary_bin(fun.y = mean, geom = "line", binwidth = 30.42)+
  scale_x_date(date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Incident Reports 7/2016 to 7/2018") +
  xlab("Incident Report Date")+
  theme


#########END DATE DISTRIBUTION OF INCIDENT REPORTS################
##################################################################
##################################################################

############COUNT STAFF COMMS PRIOR TO AN INCIDENT################
##################################################################
##################################################################

#Some individuals have more than one incident report. This segement
#counts the number of staff communications before EACH report. For
#example, Sally has two incident reports: one on 10/1 and one on
#11/15. Before 10/1, she had six staff communications, and before
#11/15, but after 10/1 she had two. We want to know the average 
#number before each incident report. In this case, Sally has a total
#of eight staff communications, but an average of four before each
#communication. We recognize the limitations of the average, but
#will explore other options later.

##Create IR IDs
timelineIR$incidentDatecopy <- timelineIR$incidentDate
timelineIR <- timelineIR %>% separate(`incidentDatecopy`, into=c("Month","Day","Year"))
timelineIR$IRuniqueID <- paste(timelineIR$uniqueID, timelineIR$Month,
                               timelineIR$Day, timelineIR$Year, sep=".")

timelineIR <- timelineIR[!(is.na(timelineIR$incidentDate)),]


###join the Incident Report and Staff Communication reports,
##pulling only those whoe have an IR. Joine by uniqueID
tlf <- left_join(timelineIR, timelineSC)
tlf <- tlf[!(is.na(tlf$staffcommDate)),]

######Create the loop
#Here we loop through by unique ID to calculate the number of
#staff comms prior to an incident report. In order to rbind
#the table, I created a new table of just the first person
#then it loops over each person, binding them to the table.

names <- tlf %>% select(uniqueID) %>% distinct(uniqueID)
nameLength <- length(names$uniqueID)
n = 1

test <- sqldf(paste0("SELECT * FROM tlf WHERE uniqueID =", "'", names$uniqueID[n], "'"))
test$incidentDate <- mdy(test$incidentDate)
test$staffcommDate <- mdy(test$staffcommDate)

incidents <- test %>% select (IRuniqueID, incidentDate) %>% 
  distinct(incidentDate,.keep_all = TRUE)

t <- test %>% group_by(IRuniqueID) %>%
  summarise(ir1 = sum(staffcommCount[staffcommDate <= incidents$incidentDate[1]]),
            ir2 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[2]]))-
              ir1,
            ir3 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[3]]))-
              (ir1+ir2),
            ir4 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[4]]))-
              (ir1+ir2 + ir3),
            ir5 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[5]]))-
              (ir1+ir2 + ir3 + ir4))

#Increment number, create the rbind table. This loop
#creates a new table for each unique incident ID, and then
#binds all the tables together. We later remove the duplicate
#names.

n = 2
new <- t

while (n < nameLength){
  test <- sqldf(paste0("SELECT * FROM tlf WHERE uniqueID =", "'", names$uniqueID[n], "'"))
  test$incidentDate <- mdy(test$incidentDate)
  test$staffcommDate <- mdy(test$staffcommDate)
  
  incidents <- test %>% select (IRuniqueID, incidentDate) %>% 
    distinct(incidentDate,.keep_all = TRUE)
  
  t <- test %>% group_by(IRuniqueID) %>%
    summarise(ir1 = sum(staffcommCount[staffcommDate <= incidents$incidentDate[1]]),
              ir2 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[2]]))-
                ir1,
              ir3 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[3]]))-
                (ir1+ir2),
              ir4 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[4]]))-
                (ir1+ir2 + ir3),
              ir5 = (sum(staffcommCount[staffcommDate <= incidents$incidentDate[5]]))-
                (ir1+ir2 + ir3 + ir4))
  
  new <- rbind(new, t)
  
  n=n+1
}

#Remove duplicate names for each IRuniqueID, find the original uniqueID
#join these tables together
new2 <- new %>% distinct(IRuniqueID, .keep_all=TRUE) 

tableCount <- tlf %>% select(IRuniqueID, uniqueID) %>% distinct(IRuniqueID, .keep_all = TRUE)
finalTable <- left_join(tableCount, new2)

finalTable <- finalTable %>% distinct(uniqueID, .keep_all = TRUE)
finalTable <- finalTable[,-(1)]
(sum(!is.na(finalTable$ir2))/length(finalTable$uniqueID))*100
#This just calculates the straight summary of all staff comms before
#incident reports
ft <- finalTable %>% select(ir1, ir2, ir3, ir4, ir5)
ft <- stack(ft)
summary(ft)

#Write a table of the staff commms with the member name.
write_csv(finalTable, "SC_Before_IR_Counts.csv")
##########END COUNT STAFF COMMS PRIOR TO AN INCIDENT##############
##################################################################
##################################################################


##########START REGRESSION ON INCIDENT REPORTS####################
##################################################################
##################################################################

#Fit a line to calculate whether number of staffcomms is s.s.
IRcount <- tlf %>% select(IRuniqueID, uniqueID) %>% 
  distinct(IRuniqueID, uniqueID)
View(tlf)
IRcount$Count <- 1
IRcount <- IRcount %>% group_by(uniqueID) %>% 
  summarise(IRtotals = sum(Count))

SCcount <- sc
SCcount$count <- 1
SCcount <- SCcount %>% group_by(uniqueID) %>%
  summarise(SCtotals = sum(count))

joined <- left_join(IRcount, SCcount)
dim(joined)
joined2 <- full_join(IRcount, SCcount)
joined2$IRtotals[is.na(joined2$IRtotals)] <- 0
lmod1 <- lm(joined$IRtotals ~ joined$SCtotals)


summary(lmod1)
##The effect of number of staff comms is statistically significant


####Create a logisitic regression on a Y/N for Incident Reports
joined2$IRy_n <- NA
joined2$IRy_n <- if_else(joined2$IRtotals==0, 0, 1)
summary(factor(joined2$IRy_n))
lmod2 <- glm(joined2$IRy_n ~ joined2$SCtotals, family = "binomial")
exp(coef(lmod2))
#For every one more SC, the odds of having an incident report increase
#by a factor of 1.164, expect to see a 16% increase in the odds of having
#an Incident Report
summary(lmod2)
############END REGRESSION ON INCIDENT REPORTS####################
##################################################################
##################################################################

############DAYS BETWEEN INCIDENT REPORTS#########################
##################################################################
##################################################################

###Calculate the number of days between each incident report.

daysIR <- sqldf("SELECT * FROM IRcount WHERE IRtotals > 1")
daysBetween <- left_join(daysIR, tlf)
names2 <- daysBetween %>% select(uniqueID) %>% distinct(uniqueID)
nameLength2 <- length(names2$uniqueID)

n=1
test2 <- sqldf(paste0("SELECT * FROM daysBetween WHERE uniqueID =", "'", names2$uniqueID[n], "'"))

incidents2 <- test2 %>% select (IRuniqueID, incidentDate) %>% 
  distinct(incidentDate,.keep_all = TRUE)

t2 <- test2 %>% group_by(IRuniqueID) %>%
  summarise(between1_2 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[2])-
                                             mdy(incidents2$incidentDate[1]))))/60)/60)/24,
            between2_3 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[3])-
                                             mdy(incidents2$incidentDate[2]))))/60)/60)/24,
            between3_4 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[4])-
                                             mdy(incidents2$incidentDate[3]))))/60)/60)/24,
            between4_5 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[5])-
                                             mdy(incidents2$incidentDate[4]))))/60)/60)/24)

#Increment number, create the rbind table. This loop
#creates a new table for each unique incident ID, and then
#binds all the tables together. We later remove the duplicate
#names.

n = 2
new3 <- t2

while (n < nameLength2){
  test2 <- sqldf(paste0("SELECT * FROM tlf WHERE uniqueID =", "'", names2$uniqueID[n], "'"))
  
  incidents2 <- test2 %>% select (IRuniqueID, incidentDate) %>% 
    distinct(incidentDate,.keep_all = TRUE)
  
  t2 <- test2 %>% group_by(IRuniqueID) %>%
    summarise(between1_2 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[2])-
                                               mdy(incidents2$incidentDate[1]))))/60)/60)/24,
              between2_3 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[3])-
                                               mdy(incidents2$incidentDate[2]))))/60)/60)/24,
              between3_4 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[4])-
                                               mdy(incidents2$incidentDate[3]))))/60)/60)/24,
              between4_5 =(((as.numeric(as.duration(mdy(incidents2$incidentDate[5])-
                                               mdy(incidents2$incidentDate[4]))))/60)/60)/24)
  
  new3 <- rbind(new3, t2)
  
  n=n+1
}


new4 <- new3 %>% distinct(IRuniqueID, .keep_all=TRUE) 

tableCount2 <- tlf %>% select(IRuniqueID, uniqueID) %>% distinct(IRuniqueID, .keep_all = TRUE)
finalTable2 <- left_join(new4, tableCount2)

finalTable2 <- finalTable2 %>% distinct(uniqueID, .keep_all = TRUE)

finalTable2 <- sqldf("SELECT * FROM finalTable2 WHERE between1_2 >=0")

finalTable2$bin <- NA
finalTable2$bin[finalTable2$between1_2 >= 0 &
                 finalTable2$between1_2 <= 30] <- "a.1 month"

finalTable2$bin[finalTable2$between1_2 > 30 &
                 finalTable2$between1_2 <= 60] <- "b.2 months"

finalTable2$bin[finalTable2$between1_2 > 60 &
                 finalTable2$between1_2 <= 90] <- "c.3 months"

finalTable2$bin[finalTable2$between1_2 > 90 &
                 finalTable2$between1_2 <= 120] <- "d.4 months"

finalTable2$bin[finalTable2$between1_2 > 120 &
                 finalTable2$between1_2 <= 150] <- "e.5 months"

finalTable2$bin[finalTable2$between1_2 > 150 &
                 finalTable2$between1_2 <= 180] <- "f.6 months"

finalTable2$bin[finalTable2$between1_2 > 180 &
                 finalTable2$between1_2 <= 210] <- "g.7 months"

finalTable2$bin[finalTable2$between1_2 > 210 &
                 finalTable2$between1_2 <= 240] <- "h.8 months"

finalTable2$bin[finalTable2$between1_2 > 240 &
                 finalTable2$between1_2 <= 270] <- "i.9 months"

finalTable2$bin[finalTable2$between1_2 > 270 &
                 finalTable2$between1_2 <= 300] <- "j.10 months"

finalTable2$bin[finalTable2$between1_2 > 300 &
                 finalTable2$between1_2 <= 330] <- "k.11 months"
finalTable2$bin[finalTable2$between1_2 > 330 &
                 finalTable2$between1_2 <= 365] <- "l.12 months"

finalTable2$bin[finalTable2$between1_2 > 365] <- "more than one year"
summary(factor(finalTable2$bin))


finalTable2 %>% ggplot(aes(x=bin)) +
  geom_bar()+
  ggtitle("Time between 1st and 2nd Incident Report")+
  theme

repeats<- sqldf("SELECT * FROM finalTable2 WHERE between1_2 BETWEEN 0 and 90")

(sum(!is.na(repeats$between2_3))/length(repeats$uniqueID))*100


############END DAYS BETWEEN INCIDENT REPORTS#####################
##################################################################
##################################################################

############START GROUPING IR BY TYPE OF INCIDENT#################
##################################################################
##################################################################
View(ic)
colnames(ic)
reasonIR <- ic %>% select(uniqueID, `Date of Incident`, `Type of Incident`)
colnames(reasonIR)[colnames(reasonIR)=="Type of Incident"] <- "incidentType"
colnames(reasonIR)[colnames(reasonIR)=="Date of Incident"] = "incidentDate"

#reasonIR <- reasonIR[!(reasonIR$incidentDate <= "2013-01-01" ),]
#reasonIR <- reasonIR %>% separate("incidentDate", into =c("year", "month", "day"))
reasonIR <- reasonIR %>% separate(incidentType, into = c("reason1", 
                                                           "reason2",
                                                           "reason3",
                                                           "reason4",
                                                           "reason5",
                                                           "reason6"),
                                    sep = ",")

reasonIR <- reasonIR %>% gather(`reason1`,`reason2`,`reason3`,
                                 `reason4`,`reason5`, `reason6`,
                                 key = "cats",
                                 value = "Reasons")


reasonIR <- reasonIR[!is.na(reasonIR$Reasons),]

reasonIR$Reasons <- str_trim(reasonIR$Reasons, side = "left")

View(sc)

crisis <- sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Crisis Intervention'")
crisis <- left_join(crisis, sc)

fightingPhysical  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Fighting Physical'"),
                               sc)
medicalNotOnsite  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Medical (other than on site injury)'"),
                               sc)

followingRules  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Not Following Rules'"),
                             sc)

other <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Other - explain below'"),sc)

theft  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Theft'"),sc)

dv <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Domestic Abuse'"),sc)

fightingVerbal<- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Fighting Verbal'"),sc)

mental  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Mental Health Related'"),sc)

onsiteInjury  <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Onsite Injury'"),sc)

substanceAbuse<- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Subtance Abuse'"),sc)

threat <- left_join(sqldf("SELECT * FROM reasonIR WHERE Reasons = 'Threatening'"),sc)


levels(factor(reasonIR$Reasons))


View(substanceAbuse)
View(reasonIR)
##############END GROUPING IR BY TYPE OF INCIDENT#################
##################################################################
##################################################################

############START TEXT ANALYSIS ON STAFF COMMUNICATIONS###########
##################################################################
##################################################################


###This is tidytext of staff communications, then we take all those
#who have incident reports, and put their staff communiciations in
#one document. Then we figure out the top words. The beginning is
#ONLY people with staff communications. We will later investigate
#the commonalities of those staff communictions for people who have
#incident reports as well.

MemberComm <- sc %>% group_by(uniqueID) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(Notes_Kept, regex("^chapter [\\divxlc]",
                                                       ignore_case = TRUE)))) %>%
  ungroup()

tidyComm <- MemberComm %>% unnest_tokens(word, Notes_Kept)

#Need to create an anti-join table with more titles and common words.

commonWords <- c("program manager", "ra", "resource advocate", "manager", "volunteer",
                 "advocate", "bilingual", "volunteer program", "programs and services",
                 "coordinator", "internal resources", "specialist", "arts program")
df = data.frame(commonWords)
colnames(df)[colnames(df)=="commonWords"] <- "word"
data(stop_words)
tidyComm <- tidyComm %>% anti_join(stop_words)
tidyComm <- tidyComm %>% anti_join(df)
topWords <- tidyComm %>% count(word, sort = TRUE)
topWords

###Locate the bad sentiment words.
sent1 <- tidyComm %>%
  inner_join(get_sentiments("bing")) %>%
  count(uniqueID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


wordCounts1 <- tidyComm %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
wordCounts1

#####Sentiment analysis

tidyCommIR <- left_join(joined, tidyComm)

nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
(negative <- tidyCommIR %>% inner_join(nrc_negative) %>%
  count(word, sort = TRUE))

nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
(positive <- tidyCommIR %>% inner_join(nrc_positive) %>%
  count(word, sort = TRUE))


nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
(anger <- tidyCommIR %>% inner_join(nrc_anger) %>%
  count(word, sort = TRUE))

nrc_disgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
(disgust <- tidyCommIR %>% inner_join(nrc_disgust) %>%
  count(word, sort = TRUE))

nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
(fear <- tidyCommIR %>% inner_join(nrc_fear) %>%
  count(word, sort = TRUE))

###This looks at how each person's overall sentiment is.
sent <- tidyCommIR %>%
  inner_join(get_sentiments("bing")) %>%
  count(uniqueID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

wordCounts <- tidyCommIR %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
wordCounts

wordCounts %>% group_by(sentiment) %>% top_n(10) %>%
  ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to Sentiment",
       x = NULL) +
  coord_flip()


#############END  TEXT ANALYSIS ON STAFF COMMUNICATIONS###########
##################################################################
##################################################################
