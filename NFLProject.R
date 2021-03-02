library(readxl)


###Data Import from Excel ####
# Set working directory to my folder for Final Project 
# Had to remove a column in R as it was only used to directly impact 
# the Time of Possession Data needed, test


setwd("/Users/ryan/Desktop/Desktop/MAT_252/")
NFL <- read_excel('Mini-projects/Final Project/Final Project Data.xlsx', 
                  col_types = c('text', 'text', 'numeric', 'numeric', 'numeric', 
                                'numeric', 'date', 'numeric', 'numeric', 'date'))

NFL
NFL$`Time of Possession...7` <- NULL
names(NFL)[9] <- "Time of Possession"
NFL$`Time of Possession` <- as.numeric(NFL$`Time of Possession`)
NFL$Year <- as.factor(NFL$Year)

####Exploratory Data Analysis####
#Distribution of Variables
#Test Normal Distributions

hist(NFL$`Offensive DVOA`, breaks=8, col='green', main='Distribution of DVOA Metrics', xlab='DVOA Rankings')
qqnorm(NFL$`Offensive DVOA`, main='Quantiles of DVOA');qqline(NFL$`Offensive DVOA`)

hist(NFL$Turnovers, col='yellow',main='Distribution of NFL Turnovers', xlab='Number of Turnovers for an NFL Team in a Season')
qqnorm(NFL$Turnovers);qqline(NFL$Turnovers)

hist(NFL$`Time of Possession`, breaks=5, col='orange', main='NFL Time of Possession Distribution', xlab='Number of Seconds for a Team Time of Possession, Per Season')
qqnorm(NFL$`Time of Possession`)

summary(lm(NFL$Wins~NFL$`Offensive DVOA`))
summary(lm(NFL$Wins~NFL$`Defensive DVOA`))
summary(lm(NFL$Wins~NFL$`Special Teams DVOA`))
summary(lm(NFL$Wins~NFL$`Strength of Schedule`))
summary(lm(NFL$Wins~NFL$Turnovers))
summary(lm(NFL$Wins~as.numeric(NFL$`Time of Possession`)))

RegModel1 <- summary(lm(Wins~factor(Year)+`Offensive DVOA`*`Defensive DVOA`*`Special Teams DVOA`*`Strength of Schedule`*
                          Turnovers*`Time of Possession`,data=NFL))

RegModel2 <- summary(lm(Wins~NFL$`Offensive DVOA`*NFL$`Defensive DVOA`*NFL$Turnovers, data=NFL))

RegModel3 <- summary(lm(Wins~ `Defensive DVOA`*`Offensive DVOA`*Turnovers*`Special Teams DVOA`, data=NFL))

RegModel4 <- summary(lm(Wins~ `Defensive DVOA`*`Offensive DVOA`*Turnovers*`Strength of Schedule`, data=NFL))

RegModel5 <- summary(lm(Wins~`Defensive DVOA`*`Offensive DVOA`*Turnovers*factor(Year), data=NFL))

RegModel6 <- summary(lm(Wins~ `Defensive DVOA`*`Offensive DVOA`*Turnovers*factor(Year), data=NFL))

plot(lm(Wins~NFL$`Offensive DVOA`*NFL$`Defensive DVOA`*NFL$Turnovers, data=NFL))

cor.test(x=NFL$`Offensive DVOA`, y=NFL$Turnovers, method='spearman')
cor.test(x=NFL$`Offensive DVOA`, y=NFL$`Defensive DVOA`, method='spearman')
cor.test(x=NFL$Turnovers, y=NFL$`Defensive DVOA`, method='spearman')

cor.test(NFL$`Offensive DVOA`, NFL$Turnovers)
cor.test(NFL$`Offensive DVOA`, NFL$`Defensive DVOA`)
cor.test(NFL$Turnovers, NFL$`Defensive DVOA`)

13.9286-(.0607*17)+(.0311*1)-(.045*12)-(.011*17*1)-(.003*17*12)-(.006*1*12)+(.0003*17*1*12)
13.9286-(.0607*26)+(.0311*17)-(.045*17)-(.011*17*26)-(.003*17*26)-(.006*17*17)+(.0003*17*17*26)