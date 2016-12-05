##################
###Loading Data###
##################
rawData <- read.csv("data/rawData.csv",stringsAsFactors = FALSE)
#Columns to keep: 1,6,9,21-24,31-33,35,37,38,39 (needs cleaning/normalization!), 40-47,49 (with cleaning), 51-56, 79, 90(cohort), 91(starting CS), 92(ending CS), 93(website score), 94(project), 100(teachers tech/nontech),101(students tech/nontech)
keeps<-c(1,6,9,21:24,31:33,35,37,38,39, 40:47,49, 51:56, 79, 87, 90, 91, 92, 93, 94, 100,101)
myColNames<-c('name','applicationTotalScore','cohort','address1','address2','zipcode','DoB','Paddress1','Paddress2','Pzipcode','year','school','Pub_Priv','GPA','GPAScale','reducedLunch','financialAide','race','CSExp','CSatSchool','takenCSAPCourse','otherCourses','skills','Q1','Q2','Q3','Q4','Q5','Qscore','ReadScore','zerotoeight','cohortCheck','startCS','endCS','websiteScore','projectName','teacherTech','studentTech')
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
testDF<-keepData[,14:15]
#Taric Lyazghi missing GPA!!!
testDF[74,1]<-NA
testDF[,1]<-as.character(testDF[,1])

#Wrong Scale
testDF[c(34,50),2]<-"0 - 100%"
testDF[c(29),2]<-"0.0 - 4.0"

cleanNums<-function(input){
  #Input is a dataset of two columns, the first being grades and 2nd being scales
  uniqScales<-unique(input[,2])
  growingOutput<-list()
  for (i in 1:length(input[,1])){
    input[i,1]<-sub("~","",sub("%", "", input[i,1]))
    if (input[i,2]==uniqScales[1]){
      #100 scale
      growingOutput<-c(growingOutput, round(as.numeric(input[i,1])/100,2))
    }else if(input[i,2]==uniqScales[2]){
      #4 scale
      growingOutput<-c(growingOutput, round(as.numeric(input[i,1])/4,2))
    }else if(input[i,2]==uniqScales[3]){
      #5 scale
      growingOutput<-c(growingOutput, round(as.numeric(input[i,1])/5,2))
    }
  }
  return (growingOutput)
}

keepData$scaledGPA<-unlist(cleanNums(testDF))

#x<-sapply(keepData$GPA,cleanNums)
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

#Introducing proper NAs

correctNAs<-function(input){
  if(input == "" || input == "N/A" || is.na(input)){
    return (NA)
  }else{
    #Also correcting tech and non-tech
    if(input=='Tech'){
      return (input)
    }else if(input == 'Non-Tech' || input == 'Neutral'){
      return ("Non-Tech")
    }else{
      return (as.numeric(input))  
    }
    
  }
} 

keepData$startCS<-sapply(keepData$startCS, correctNAs)
keepData$endCS<-sapply(keepData$endCS, correctNAs)
keepData$websiteScore<-sapply(keepData$websiteScore, correctNAs)
keepData$teacherTech<-sapply(keepData$teacherTech, correctNAs)
keepData$studentTech<-sapply(keepData$studentTech, correctNAs)

#Correcting zerotoeight flag
correctFlag<-function(input){
  if (input==""){
    return ("unchecked")
  }else{
    return (input)
  }
}
keepData$zerotoeight<-sapply(keepData$zerotoeight, correctFlag)

cleanData<-keepData[,-c(14,15,23)]

#Splitting into Categorical and Numeric datasets
catData<-cleanData[,c(3:6,8:15,17:25,28)]
numData<-cleanData[,c(2,26:28,30:32,36)]

rm(cleanData,keepData, testDF)
gc()
#################
##DATA ANALYSIS##
#################

#HOW TO HANDLE DIFFERENT DATA
#Dependent Variable   Categorical   Continuous
#Independent Variable
#Categorical          Chi Square     t-test, ANOVA
#Continuous           LDA, QDA       Regression

#Independent variables = scaledGPA, Q1:Q5, Qscore, ReadScore, cohort, school, Pub_Priv, applicationTotalScore, startCS
#Dependent Variables = zerotoeight

#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
#https://www.r-bloggers.com/computing-and-visualizing-lda-in-r/

#chisq.test(table(dependent, independent))

#http://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html

#lda(group ~ numericIndependents + nextNumericIndependent)