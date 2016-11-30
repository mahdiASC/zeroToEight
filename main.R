##################
###Loading Data###
##################
rawData <- read.csv("data/rawData.csv")
#Columns to keep: 1,6,9,21-24,31-33,35,37,38,39 (needs cleaning/normalization!), 40-47,49 (with cleaning), 51-56, 79, 90(cohort), 91(starting CS), 92(ending CS), 93(website score), 94(project), 100(teachers tech/nontech),101(students tech/nontech)
keeps<-c(1,6,9,21:24,31:33,35,37,38,39, 40:47,49, 51:56, 79, 90, 91, 92, 93, 94, 100,101)
myColNames<-c('name','applicationTotalScore','cohort','address1','address2','zipcode','DoB','Paddress1','Paddress2','Pzipcode','year','school','Pub/Priv','GPA','GPAScale','reducedLunch','financialAide','race','CSExp','CSatSchool','takenCSAPCourse','otherCourses','skills','Q1','Q2','Q3','Q4','Q5','Qscore','ReadScore','cohortCheck','startCS','endCS','websiteScore','projectName','teacherTech','studentTech')
keepData <- rawData[,keeps]
colnames(keepData)<-myColNames

rm(rawData)
gc(verbose = FALSE)

###################
###CLEANING DATA###
###################
#Exploring uniqueness
uniqueVals<-apply(keepData,2, function(x) unique(x))

#Correcting student data
#Change ibrahim #33 in descending order to "Junior" under $year
keepData$year[33]<-"Junior"

#address zipcode corrections
correctZip<-function(input){
  input<-as.character(input)
  if(nchar(input)!=5){
    return (paste("0",input, sep=""))
  }else{
    return (input)
  }
}
keepData$zipcode<-sapply(keepData$zipcode,correctZip)
keepData$Pzipcode<-sapply(keepData$Pzipcode,correctZip)

correctAddress<-function(input){
  output<-tolower(as.character(input))
  #zicode is for Queens
  if (output=="11693"){
    output <- "Queens"   
  }else if(grepl("new york",output)){
    output <- "New York City"
  }else if(grepl("brook",output)){
    output <- "Brooklyn"
  }else if(grepl("bronx",output)){
    output <- "The Bronx"
  }
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
          sep = "", collapse = " ")
  }
  
  return (simpleCap(output))
}
keepData$address1<-sapply(keepData$address1, correctAddress)
keepData$Paddress1<-sapply(keepData$Paddress1, correctAddress)

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

keepData$DoB<-sapply(keepData$DoB,convertDate)

#Standardize GPAs (pretty much impossible)

cleanNums<-function(input){
  
}

x<-sapply(keepData$GPA,cleanNums)
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
