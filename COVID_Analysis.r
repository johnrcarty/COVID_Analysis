library(RCurl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)

setwd('desktop/covid_analysis')


#change me to the state you want to see
stateSelect = 'California' 
abb <- state.abb[state.name == stateSelect]


#Get us covid cases by month posted by NY Times
c <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
dailyCases <- read.csv(text = c, stringsAsFactors = FALSE)
dailyCases_redCol <- dailyCases[,5:ncol(dailyCases)]

#Our World in Data's vaccination records by month
v <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")
dailyVaccines <- read.csv(text = v, stringsAsFactors = FALSE)
names(dailyVaccines)[2] <- 'state'

#US Census data downloaded from 
#https://data.census.gov/cedsci/map?q=Total%20Population%20in%20the%20United%20States&g=0100000US.04000.001&tid=PEPPOP2019.PEPANNRES&vintage=2019&layer=VT_2019_040_00_PP_D1&cid=POP&mode=thematic
census <- read.csv("PEPPOP2019.PEPANNRES_data_with_overlays_2021-03-25T125809.csv", stringsAsFactors = FALSE)
unique(census$DATE_CODE) #"7/1/2019 population estimate"
census <- subset(census, DATE_CODE=="7/1/2019 population estimate")
census.redCol <- census[,c(2,4)]


#merge the cases data set to the vaccines dataset
casesVaccines<- merge(dailyCases, dailyVaccines, by=c('date', 'state'))
casesVaccines$date <- as.Date(casesVaccines$date)

#merge the census data in
casesVaccines.final <- merge(casesVaccines, census.redCol, by.x="state", by.y="NAME")

casesVaccines.byDay.ST <- subset(casesVaccines.final, state==stateSelect)
names(casesVaccines.byDay.ST)

#  [1] "date"                                "state"                               "fips"                               
#  [4] "cases"                               "deaths"                              "total_vaccinations"                 
#  [7] "total_distributed"                   "people_vaccinated"                   "people_fully_vaccinated_per_hundred"
# [10] "total_vaccinations_per_hundred"      "people_fully_vaccinated"             "people_vaccinated_per_hundred"      
# [13] "distributed_per_hundred"             "daily_vaccinations_raw"              "daily_vaccinations"                 
# [16] "daily_vaccinations_per_million"      "share_doses_used"                    "POP"     

x <- "cases"
y <- "people_fully_vaccinated"
x.pos <- which(names(casesVaccines.byDay.ST) == x)
y.pos <- which(names(casesVaccines.byDay.ST) == y)

small <- casesVaccines.byDay.ST[,c(2,x.pos, y.pos,18)]
head(small)
dayOffset <- data.frame(date=small$date, 
                        nextDay=small$date+1, 
                        cases.priorday=small$cases, 
                        vaccinated.priorday=small$people_fully_vaccinated
                        )
small <- merge(small, dayOffset, by.x="date", by.y="nextDay")
headers <- c("date"
             , "cases"
             , "vaccinated"
             , "POP"
             , "date.rm"
             , "cases.priorDay"
             , "vaccinated.priorday")

names(small) <- headers

min(small$date) #2021-01-13
max(small$date) #2021-03-30

small$newcases <- small$cases - small$cases.priorDay
small$newvaccine <- small$vaccinated - small$vaccinated.priorday
names(small)
final <- small[,c(1:4,6:9)]
final$cases.per1000 <- final$cases/(as.numeric(final$POP)/1000)
final$vaccinated.per1000 <- final$vaccinated/(as.numeric(final$POP)/1000)
names(final)
# [1] "date"                "cases"               "vaccinated"         
# [4] "cases.priorDay"      "vaccinated.priorday" "newcases"           
# [7] "newvaccine"  

lm_new = lm(newcases~newvaccine, data = final)



p1 <- ggplot(final, aes(x=date, y=cases.per1000)) +
  geom_line(color="red", size=2) +
  ggtitle(paste(abb, "COVID 19 Cases")) +
  xlab("Date") + ylab("Total Cases Per 1000 People")+ 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12)
  )
  
p2 <- ggplot(final, aes(x=date, y=vaccinated.per1000)) +
  geom_line(color="#69b3a2",size=2)  +
  ggtitle(paste(abb, "COVID 19 Vaccinations")) +
  xlab("Date") + ylab("Total Vaccinations Per 1000 People") + 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12)
  )

# Display both charts side by side
p3 <- p1 + p2

p3


ggsave(
  "casesAndVaccinations.png",
  plot = p3,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)

p4 <- ggplot(final, aes(x = newcases, y = newvaccine)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  ggtitle(paste(abb, "COVID 19 Vaccination by New Cases Regression")) +
  xlab("Daily New Cases") + ylab("Daily New Vaccinated") + 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12)
  )


p4


ggsave(
  "newCasesNewVaccine_regression.png",
  plot = p4,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)
