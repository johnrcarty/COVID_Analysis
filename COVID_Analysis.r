library(RCurl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(tidyverse)
library(viridis)

setwd('desktop/covid_analysis')


#change me to the state you want to see
stateSelect <- 'California'
vaccineRollOut <- 3  # 1 = Post; 2 = Pre; 3 = All

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


if(vaccineRollOut == 1) {
  #merge the cases data set to the vaccines dataset and exclude days with no vaccine
  casesVaccines<- merge(dailyCases, dailyVaccines, by=c('date', 'state'))
  casesVaccines$date <- as.Date(casesVaccines$date)
  
} else if(vaccineRollOut == 2) {
  #merge the cases data set to the vaccines dataset include days before vaccine roll out
  casesVaccines<- merge(dailyCases, dailyVaccines, by=c('date', 'state'), all.x = TRUE)
  casesVaccines$date <- as.Date(casesVaccines$date)
  casesVaccines <- casesVaccines[rowSums(is.na(casesVaccines))>0 ,]
  max(casesVaccines$date)
  min(casesVaccines$date)
  
} else {
  casesVaccines<- merge(dailyCases, dailyVaccines, by=c('date', 'state'), all.x = TRUE)
  casesVaccines$date <- as.Date(casesVaccines$date)
}


#merge the census data in
casesVaccines.final <- merge(casesVaccines, census.redCol, by.x="state", by.y="NAME")

names(casesVaccines.final)

#  [1] "state"                               "date"                                "fips"                               
#  [4] "cases"                               "deaths"                              "total_vaccinations"                 
#  [7] "total_distributed"                   "people_vaccinated"                   "people_fully_vaccinated_per_hundred"
# [10] "total_vaccinations_per_hundred"      "people_fully_vaccinated"             "people_vaccinated_per_hundred"      
# [13] "distributed_per_hundred"             "daily_vaccinations_raw"              "daily_vaccinations"                 
# [16] "daily_vaccinations_per_million"      "share_doses_used"                    "POP"     

x <- "cases"
y <- "people_fully_vaccinated"
x.pos <- which(names(casesVaccines.final) == x)
y.pos <- which(names(casesVaccines.final) == y)

small <- casesVaccines.final[,c(1,2,x.pos, y.pos,18,5)]
head(small)
dayOffset <- data.frame(state=small$state,
                        date=small$date, 
                        nextDay=small$date+1, 
                        cases.priorday=small$cases, 
                        vaccinated.priorday=small$people_fully_vaccinated,
                        deaths.priorday=small$deaths
                        )
small <- merge(small, dayOffset, by.x=c("state", "date"), by.y=c("state", "nextDay"))
headers <- c("state"
             , "date"
             , "cases"
             , "vaccinated"
             , "POP"
             , "deaths"
             , "date.rm"
             , "cases.priorDay"
             , "vaccinated.priorday"
             , "deaths.priorday")

names(small) <- headers

min(small$date) #2021-01-13
max(small$date) #2021-03-30

small$newcases <- small$cases - small$cases.priorDay
small$newvaccine <- small$vaccinated - small$vaccinated.priorday
names(small)
final <- small[,c(1:6,8:12)]
final$cases.per100000 <- (final$cases/(as.numeric(final$POP))*100000)
final$newcases.per100000 <- (final$newcases/(as.numeric(final$POP))*100000)
final$vaccinated.per100000 <- (final$vaccinated/(as.numeric(final$POP))*100000)
final$deaths.per100000 <- (final$deaths/(as.numeric(final$POP))*100000)
names(final)
# [1] "date"                "cases"               "vaccinated"         
# [4] "cases.priorDay"      "vaccinated.priorday" "newcases"           
# [7] "newvaccine"  

final2 <- final
final <- subset(final, state==stateSelect)


p1 <- ggplot(final, aes(x=date, y=newcases.per100000)) +
  geom_line(color="red") +
  ggtitle(paste(abb, "COVID 19 Cases")) +
  xlab("Date") + ylab("Cases Per 100,000 People")+ 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12)
  )
  
p2 <- ggplot(final, aes(x=date, y=vaccinated.per100000)) +
  geom_line(color="#69b3a2")  +
  ggtitle(paste(abb, "COVID 19 Vaccinations")) +
  xlab("Date") + ylab("Vaccinations Per 100,000 People") + 
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

coeff <- 100

p5 <- ggplot(data = final, aes(x = date)) +
  geom_col(aes(y = newcases.per100000*coeff, colour = "New Cases"))  +
  geom_line(aes(y = vaccinated.per100000, colour = "Vaccination")) +
  geom_line(aes(y = cases.per100000, colour = "Cases")) +
  scale_colour_manual("", 
                      breaks = c("Vaccination", "Cases", "New Cases"),
                      values = c("blue", "red", "lightgrey")) +
  xlab("Date") +
  scale_y_continuous("Total Cases / Vacinated To Date\n(per 100,000 people)", limits = c(0,NA), 
                     sec.axis = sec_axis(~./coeff, name="Daily New Cases\n(Per 100,000 people)")) + 
  labs(title=paste(abb, "COVID 19 Rates To Date"), fill="", linetype="") + 
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12)
  ) +
  guides(fill = guide_legend(override.aes=list(fill=c("white"))))

p5

ggsave(
  "totalCasesAndVaccinations.png",
  plot = p5,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)


states <- final2 %>% 
  filter(state %in% c("California", "Texas", "Ohio", "Florida",   "New York", "Alabama", "North Dakota", "Louisiana", "New Jersey"))
  #filter(date > '2020-04-01')

p6 <- states %>%
  ggplot( aes(x=date, y=newcases.per100000, group=state, fill=state)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_date(breaks = as.Date(c("2020-03-01", "2020-9-01", "2021-03-01")),
               labels = c("Mar 2020", "Sep 2020", "Mar 2021")) +
  theme(legend.position="none") +
  ggtitle("Daily New Covid 19 Cases per 100,000 by State") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~state, scale="free_y")


p6

ggsave(
  "9_StateDailyNewCases.png",
  plot = p6,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE
)
