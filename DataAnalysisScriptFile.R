getwd()

#set working directory
setwd("F://Data Challenges//Wharton")

library(readxl)
#Load three data sets in excel 
data2016 <- read_excel("DataSet_final.xlsx")
data2017 <- read_excel("DataSet_final.xlsx",sheet = 2)
universityDetails <- read_excel("DataSet_final.xlsx",sheet = 3)
#convert the data sets into dataframes
data2016 <- data.frame(data2016)
data2017 <- data.frame(data2017)
universityDetails <- data.frame(universityDetails)



#Convert chr to factor type variable function
ConvertToFactor <- function(dataFrame) {

  #Get the columns with lesser unique values
    colsToFactor <-
    sapply(
      dataFrame,
      FUN = function(col)
        length(unique(col)) < log10(length(col))
    )
  #Convert the features selected from the above line to factor
  dataFrame[colsToFactor] <- lapply(dataFrame[colsToFactor] , factor)
  return(dataFrame)
}


#Apply convert to factor 
data2016 <- ConvertToFactor(data2016)
data2017 <- ConvertToFactor(data2017)
universityDetails <- ConvertToFactor(universityDetails)

#Check the structures of the data frames
str(data2016)
str(data2017)
str(universityDetails)

#Check the levels of the selectivity column
table(data2016$US.World.News...Report.Selectivity)
#correct the values of the More selective to More Selective
data2016$US.World.News...Report.Selectivity <- ifelse(data2016$US.World.News...Report.Selectivity=='More selective','More Selective',data2016$US.World.News...Report.Selectivity)
#Check the levels of the selectivity column
table(data2016$US.World.News...Report.Selectivity)

#If Not Met then N is updated in 2016 dataset
data2017$Met. <- as.character(data2017$Met.)
data2017[is.na(data2017$Met.),'Met.'] <- 'N'
data2017$Met. <- as.factor(data2017$Met.)
#If Not Met then N is updated in 2017 dataset
data2016$Met. <- as.character(data2016$Met.)
data2016[is.na(data2016$Met.),'Met.'] <- 'N'
data2016$Met. <- as.factor(data2016$Met.)



#COlumn names to be changed
names(data2016)[8] <- 'Tier2016'
names(data2017)[8] <- 'Tier2017'

x1 <- table(data2016$Tier2016,data2016$Disposition..Step.)
x2 <-table(data2017$Tier2017,data2017$Disposition..Step.)

#library(ggplot2)
x1percent <- (prop.table(x1,1)*100)
x2percent  <- prop.table(x2*100,1)*100

#Plot the tier and application status
opar=par()
par(mfrow=c(1,2))
plot(x1percent)
plot(x2percent)
par(opar)


data2017[is.na(data2017$US.World.News...Report.Selectivity),]

#Explore the data of the data 2016 and 2017
table(data2017$Disposition..Step.,data2017$Tier2017)
table(data2017$Disposition..Step.,data2017$US.World.News...Report.Selectivity)
table(data2016$Disposition..Step.,data2016$Tier2016)
table(data2016$Disposition..Step.,data2016$US.World.News...Report.Selectivity)
?table


#H0: The The two variables are independent
#H1: The two variables are related
chisq.test(data2017$Disposition..Step.,data2017$Tier2017)
chisq.test(data2017$Disposition..Step.,data2017$US.World.News...Report.Selectivity)
chisq.test(data2017$Disposition..Step.,data2017$Met.)

chisq.test(data2016$Disposition..Step.,data2016$Tier2016)
chisq.test(data2016$Disposition..Step.,data2016$US.World.News...Report.Selectivity)
chisq.test(data2016$Disposition..Step.,data2016$Met.)

#application deadline doesnt have any effect in selection process

str(data2016)
#Feature Engineerin
#Add new columns to know whether candidate has Minor subject or not
data2016$IsMinor <- as.factor(ifelse( is.na(data2016$Minor),'No','Yes'))
data2017$IsMinor <- as.factor(ifelse( is.na(data2017$Minor),'No','Yes'))

#Add new columns to know whether candidate has Major2 subject or not
data2016$IsMajor2 <- as.factor(ifelse( is.na(data2016$Major.2),'No','Yes'))
data2017$IsMajor2 <- as.factor(ifelse( is.na(data2017$Major.2),'No','Yes'))


table(data2016$IsMinor,data2016$Disposition..Step.)
table(data2016$IsMajor2,data2016$Disposition..Step.)

table(data2017$IsMinor,data2017$Disposition..Step.)
table(data2017$IsMajor2,data2017$Disposition..Step.)

chisq.test(table(data2016$IsMinor,data2016$IsMajor2))


#nrow(data2016[is.na(data2016$Undergrad.University.ID),])
library(sqldf)
#Join the datasets of undergrads selected and university details
data2016final <-  sqldf('select * from data2016 d6 left join universityDetails u on d6.[Undergrad.University.ID]=u.[University.ID];')
data2017final <-sqldf('select * from data2017 d6 left join universityDetails u on d6.[Undergrad.University.ID]=u.[University.ID];')

#write.csv(x = data2016final,file = "data2016final.csv")
#write.csv(x = data2017final,file = "data2017final.csv")

SummaryofDf(data2016final)
SummaryofDf(data2017final)


#Update the Source as No Source if NA
data2016final[is.na(data2016final$Sourced.by.RT.vs.Sign.up.Form),'Sourced.by.RT.vs.Sign.up.Form'] <- 'No Source'
data2017final[is.na(data2017final$Sourced.by.RT.vs.Sign.up.Form),'Sourced.by.RT.vs.Sign.up.Form'] <- 'No Source'

#Merge Corp Members and Current TFA members as the effect is same
data2016final[,'XofCurrent CorpMembers and TFAStaff'] <- 
  data2016final$X..of.Current.CMs.who.attended.school.in.undergrad + data2016final$X..of.Current.TFA.Staff.who.attended.school.in.undergrad
data2017final[,'XofCurrent CorpMembers and TFAStaff'] <- data2017final$X..of.Current.CMs.who.attended.school.in.undergrad + data2017final$X..of.Current.TFA.Staff.who.attended.school.in.undergrad

#Remove the corp members and tfa members
data2016final$X..of.Current.CMs.who.attended.school.in.undergrad <- NULL
data2016final$X..of.Current.TFA.Staff.who.attended.school.in.undergrad <- NULL
data2017final$X..of.Current.CMs.who.attended.school.in.undergrad <- NULL
data2017final$X..of.Current.TFA.Staff.who.attended.school.in.undergrad <- NULL

#Divide the Id with year_ID to ID only
x<- stringr::str_split(data2016$ID,pattern = "\\_")
data2016final$ID <- sapply(x, "[[", 2) #stringr::str_split(data2016$ID,pattern = "\\_")[2]
y<- stringr::str_split(data2017$ID,pattern = "\\_")
data2017final$ID <- sapply(y, "[[", 2) #stringr::str_split(data2016$ID,pattern = "\\_")[2]

#Delete the Major2, Minor, AppYear, ApplicationDeadline and Id in two datasets 
data2016final$Major.2 <- NULL
data2016final$Minor <- NULL
data2017final$Major.2 <- NULL
data2017final$Minor <- NULL
data2016final$ID <- NULL
data2017final$ID <- NULL
data2016final$AppYear <- NULL
data2017final$AppYear <- NULL
data2016final$Application.Deadline <- NULL
data2017final$Application.Deadline <- NULL

#Check the values
unique(data2016final$Calculated.Prospect.Type)
unique(data2016final$Awareness.Level)
table(factor(data2016final$Awareness.Level),data2016final$US.World.News.Selectivity)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

remSpaceDot <- function(data){
  colnames(data)  <- gsub("\\.","",colnames(data))
  colnames(data)  <- gsub(" ","",colnames(data) )
  return(data)
}
#Remove the dot and space in the column names of the data's
data2016final<-  remSpaceDot(data2016final)
data2017final<-  remSpaceDot(data2017final)


library(MASS)

colnames(data2017final)


#Perfrom Multinomial logistic regression on both the data sets after removing the University Id's
data <- data2016final[,-c(3,12)]
colName <- 'DispositionStep'
refLevel <- 'WITHDRAWN'

data[,colName] <- relevel(data[,colName],ref = refLevel)
x<-  colnames(data)==colName
predictors <- colnames(data[,-which(x[TRUE])])

Xvar <- vector()
for(i in seq(1,length(predictors))){
  if(i==1){
    sep=""
  }else{sep="+"}
  Xvar <- paste(Xvar,predictors[i],sep=sep)
}
data<- na.omit(data)

test <- NULL
data$AppYear<-NULL
test <- multinom(DispositionStep ~ . ,data=data,model=T)
newModel <-  stepAIC(test,trace=F)
results2016<- newModel



data2017final$X2016RecruitmentTier <- as.character(data2017final$X2016RecruitmentTier)
 data <- data2017final[,-c(4,13)]
colName <- 'DispositionStep'
refLevel <- 'WITHDRAWN'

data[,colName] <- relevel(data[,colName],ref = refLevel)
x<-  colnames(data)==colName
predictors <- colnames(data[,-which(x[TRUE])])

Xvar <- vector()
for(i in seq(1,length(predictors))){
  if(i==1){
    sep=""
  }else{sep="+"}
  Xvar <- paste(Xvar,predictors[i],sep=sep)
}
data<- na.omit(data)

test <- NULL
data$AppYear<-NULL
test <- multinom(DispositionStep ~ . ,data=data,model=T)
newModel <-  stepAIC(test,trace=F)
results2017 <- newModel


sum2016 <-  summary(results2016)
sum2017 <- summary(results2017)
sum2016$coefficients
sum2016$deviance
sum2016$anova
View(sum2016$model)
View(data2016final)

sum2017$coefficients
sum2017$deviance
View(sum2017$model)
edit(sum2017$coefficients)
