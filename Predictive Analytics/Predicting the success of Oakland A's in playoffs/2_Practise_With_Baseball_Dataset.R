#Practise with moneyball dataset

#Getting and setting working directory
getwd()
setwd("E:\\Other Courses\\Analytics Edge\\Chapter - 3 - Linear Regression\\Datasets")
getwd()

#Reading the baseball.csv and storing it to baseball data frame and viewing its structure
baseball=read.csv("baseball.csv")
str(baseball)

#Getting a subset of baseball.csv data with years prior to 2002 and storing it in moneyball dataframe to analyze Dave PoDesta's moneyball claims
moneyball=subset(baseball, Year<"2002")
str(moneyball)

#Adding a new column run difference the differnce between runs scored and runs allowed to moneyball data frame
moneyball$RD=moneyball$RS-moneyball$RA
str(moneyball)

#Viewing the relationship between Run Difference and Wins
plot(jitter(moneyball$RD), jitter(moneyball$W), xlab="Run Difference", ylab="Wins", main="Run Difference vs Wins")

#To print the plot to pdf
dev.copy(pdf, "1_Scatter Plot of Run Difference vs Wins.pdf")
dev.off()

#Regression Model to predict Wins as a function of Run Difference, its summary
WinsReg=lm(W~RD, data=moneyball)
summary(WinsReg)
WinsRegSSE=sum(WinsReg$residuals^2)
WinsRegSSE

#To see the number of runs to be scored more than runs allowed for 95 Wins based on regression equation
#The regression equation would be W=80.881375+0.105766RD+Error
#For 95 wins, RD would be RD=(95-80.881375)/0.105766
RDWinsReg=(95-80.881375)/0.105766
RDWinsReg

#The number of games expected to be won by a team scoring 713 runs and allowing 614 runs
Win_Given_RD=80.881375+(0.105766*(713-614))
Win_Given_RD

#Predicting Runs Scored with On Base Percentage, Slugging Percentage and Batting Average, the hitting statistics
RS_Reg1=lm(RS~OBP+SLG+BA, data=moneyball)
summary(RS_Reg1)

#Checking the relation between On Base Percentage and Batting Average since the results seem counter intuitive
cor(moneyball$OBP, moneyball$BA)

#Removing OBP and retaining BA to check the change in R-squared values
RS_Reg_Check=lm(RS~BA+SLG, data=moneyball)
summary(RS_Reg_Check)

#Removing the batting average and retaining on base percentage
RS_Reg_final=lm(RS~OBP+SLG, data=moneyball)
summary(RS_Reg_final)

#Creating a linear model to predict runs allowed or opponent runs with pitching statistics viz., OOBP and OSLG
RA_Reg=lm(RA~OOBP+OSLG, data=moneyball)
summary(RA_Reg)

#Runs expected to be scored by a team with OBP 0.311 and SLG 0.405
summary(RS_Reg_final)
RS_Given_OBP_SLG=-804.63+(2737.77*0.311)+(1584.91*0.405)
RS_Given_OBP_SLG

#Runs expected to be allowed to an opponent team with OOBP 0.297 and OSLG 0.370
summary(RA_Reg)
RA_Given_OOBP_OSLG=-837.38+(2913.60*0.297)+(1514.29*0.370)
RA_Given_OOBP_OSLG

#Evaluating Runs Scored for Players
RS_Eric_Chavez=-804.63+(2737.77*0.338)+(1584.91*0.540)
RS_Jeremy_Giambi=-804.63+(2737.77*0.391)+(1584.91*0.450)
RS_Frank_Menechino=-804.63+(2737.77*0.369)+(1584.91*0.374)
RS_Greg_Myers=-804.63+(2737.77*0.313)+(1584.91*0.447)
RS_Carlos_Pena=-804.63+(2737.77*0.361)+(1584.91*0.500)
RS_Eric_Chavez
RS_Jeremy_Giambi
RS_Frank_Menechino
RS_Greg_Myers
RS_Carlos_Pena

#Evaluating the correlations between TeamRanks and Wins
TeamRank=c(1,2,3,3,4,4,4,4,5,5)
Wins2012=c(94,88,95,88,93,94,98,97,93,94)
Wins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(TeamRank,Wins2012)
cor(TeamRank,Wins2013)



