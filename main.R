##################
###Loading Data###
##################
rawData <- read.csv("data/rawData.csv")
#Columns to keep: 1,6,9,21-24,31-33,35,37,38,39 (needs cleaning/normalization!), 40-47,49 (with cleaning), 51-56, 79, 90(cohort), 91(starting CS), 92(ending CS), 93(website score), 94(project), 100(teachers tech/nontech),101(students tech/nontech)
keeps<-c(1,6,9,21:24,31:33,35,37,38,39, 40:47,49, 51:56, 79, 90, 91, 92, 93, 94, 100,101)
myColNames<-c('name','applicationTotalScore','cohort','address1','address2','zipcode','DoB','Paddress1','Paddress2','Pzipcode','year','school','Pub/Priv','GPA','GPAScale','reducedLunch','financialAide','race','CSExp','CSatSchool','takenCSAPCourse','otherCourses','skills','Q1','Q2','Q3','Q4','Q5','Qscore','ReadScore','cohortCheck','startCS','endCS','websiteScore','projectName','teacherTech','studentTech')
keepData <- rawData[,keeps]
colnames(keepData)<-myColNames

###################
###CLEANING DATA###
###################
#Exploring uniqueness
uniqueVals<-apply(keepData,2, function(x) unique(x))

#Correcting student data
#Change ibrahim #33 in descending order to "Junior" under $year
keepData$year[33]<-"Junior"

#address and Paddress corrections

#Zipcodes need corrections
#Cyril suggest Attendance sheets for each cohort

#Standardize DoB
convertDate<-function(input){
  #Converts the string date from various formats into 1 standard format
  #M/D/Y
  if(grepl("/..$", input)){
    return (as.character(as.Date(input,format='%m/%d/%y')))
  }else{
    return (as.character(as.Date(input,format='%m/%d/%Y')))
  }
  
}

keepData$DoB<-sapply(keepData$DoB, 1,convertDate)

#Standardize GPAs (pretty much impossible)

#Standardizing Race
convertRace<-function(input){
  input<-gsub(" and ", " ", input)
  input<-gsub("\\|", " ", input)
  input<-gsub("/", " ", input)
  input<-gsub(",", " ", input)
  return(input)
}

keepData$race<-sapply(keepData$race, convertRace)

#Correct Schools
con <- file("ProperSchools.txt", "r")
keepData$school<-readLines(con)
close(con)
