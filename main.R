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
  growingOutput<-list()
  for (i in 1:length(input$GPA)){
    input$GPA[i]<-sub("~","",sub("%", "", input$GPA[i]))
    if (input$GPAScale[i]=="0 - 100%"){
      #100 scale
      growingOutput<-c(growingOutput, round(as.numeric(input$GPA[i])/100,2))
    }else if(input$GPAScale[i]=="0.0 - 4.0"){
      #4 scale
      growingOutput<-c(growingOutput, round(as.numeric(input$GPA[i])/4,2))
    }else if(input$GPAScale[i]=="0.0 - 5.0"){
      #5 scale
      growingOutput<-c(growingOutput, round(as.numeric(input$GPA[i])/5,2))
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

#rm(cleanData,keepData, testDF)
#gc()

########################
##EXPLORATORY ANALYSIS##
########################

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

################
##NUMERIC DATA##
################

myNAs<-apply(numData,2,function(x){
  return (sum(is.na(x))/length(x))
})

data.frame(sort(myNAs,decreasing=TRUE))
#will trying omitting variables with too many NAs as they interfere with the PCA
#will also try imputing the data (using average)
#If too much of a difference will revisit

#websiteScore has too much missing data
numData<-numData[,-c(5:7)]

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(numData[,-4],10)
#applicationTotalScore and Readscore or highly correlated, therefore shouldn't use covariates and will only try keeping 1

#data w/out applicationTotalScore
NUMnoAppScoreData<-numData[,-c(1,4)]

#data w/out ReadScore
NUMnoReadScore<-numData[,-c(3,4)]

#Standardizing the data. Can also scale in pca function
#standNumData <- as.data.frame(scale(numData[,-4]))
#sapply(standNumData, sd, na.rm=TRUE) #SD the same for all variable, therefore, comparable for PCA


#PCA without NAs
pcaNoApp.NAs<-prcomp(na.omit(NUMnoAppScoreData), retx=TRUE, center=TRUE, scale=TRUE)
pcaNoRead.NAs<-prcomp(na.omit(NUMnoReadScore), retx=TRUE, center=TRUE, scale=TRUE) #Smaller stn dev.

#PCA with/imputed NAs
library(mice)
#Responds much better to additive effects on Variance in the data (First two components account for over 80% of the variance in the data)
pcaNoApp.imputed<-prcomp(complete(mice(NUMnoAppScoreData, printFlag = FALSE)),retx=TRUE, center=TRUE, scale=TRUE)#More std. dev. than without NAs
pcaNoRead.imputed<-prcomp(complete(mice(NUMnoReadScore, printFlag = FALSE)),retx=TRUE, center=TRUE, scale=TRUE)#More std. dev. than without NAs 
#http://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction

#Best accounting for variation in data w/Cumulative Proportion
summary(pcaNoApp.imputed)

#all suggest all 3 components are significant (>10% of variation each)
#I'll use all 3 and imputed without App (best first component)
#Checking for important variables for first components
pcaNoApp.imputed$rotation[,1]
#Qscore and GPA are important in 1st component
pcaNoApp.imputed$rotation[,2]
#GPA and Qscore are most important in 2nd component
pcaNoApp.imputed$rotation[,3]
#GPA and readscore are most important in 3rd component

###########################
##MAKING PREDICTIVE MODEL##
###########################
almostNumData<-complete(mice(NUMnoAppScoreData, printFlag = FALSE))
almostNumData$zerotoeight<-numData$zerotoeight

library(caret)
library(MASS)
finalNumData<-preProcess(almostNumData, c("BoxCox", "center", "scale"))
finalNumData<-data.frame(predict(finalNumData, almostNumData), stringsAsFactors = FALSE)

myLDA<-lda(zerotoeight~ReadScore*scaledGPA, data = finalNumData)
myPrediction<-predict(myLDA)

#Best prediction
#mean(predict(lda(zerotoeight~ReadScore*scaledGPA, data = finalNumData))$class==finalNumData$zerotoeight)

table(finalNumData$zerotoeight, myPrediction$class)
#Low sensitivity
#high selectivity (just labels almost everyone as unchecked!)
#            checked  unchecked
#checked         3        16
#unchecked       1        60

mean(myPrediction$class==finalNumData$zerotoeight)
#Acc
#63/80 = 78.75%

#High QScore
by(finalNumData$Qscore, finalNumData$zerotoeight, FUN=mean)

#Not high ReadScore
by(finalNumData$ReadScore, finalNumData$zerotoeight, FUN=mean)

#not high GPA
by(finalNumData$scaledGPA, finalNumData$zerotoeight, FUN=mean)

#           Qscore  ReadScore  scaledGPA
#checked   4.421053  4.368421 0.8615789
#unchecked 4.229508  4.934426 0.8768852

############
##Plotting##
############
library(ggplot2)
prop.pca = pcaNoApp.imputed$sdev^2/sum(pcaNoApp.imputed$sdev^2)
prop.lda = myLDA$svd^2/sum(myLDA$svd^2)

dataset = data.frame(cats = finalNumData$zerotoeight, pca = pcaNoApp.imputed$x, lda = myPrediction$x)
p1 <- ggplot(dataset) + geom_point(aes(LD1, c(0), size=LD1, colour = cats, shape="circle")) + facet_grid(cats ~ .)
p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, size=pca.PC1, colour = cats, shape = "circle"))

print(p2) #PCA - Good at reducing variables to most necessary
print(p1) #LDA - Good at finding seperatability between groups

####################
##CATEGORICAL DATA##
####################

#chisq.test(table(dependent, independent))

#further filtering cat.data
alphaCatData<-catData[,-c(1,3,4,6,7)]

#Q1
correctQ1<-function(input){
  if(input=="1" || input == "Figure A"){
    return ("1")
  }else{
    return ("0")
  }
}

alphaCatData$Q1<-sapply(alphaCatData$Q1, correctQ1)

#Q2
correctQ2<-function(input){
  if(input=="1" || input == "forward(1), turnLeft(90),  forward(1),  turnLeft(90),  forward(1),  turnLeft(90),  forward(1)"){
    return ("1")
  }else{
    return ("0")
  }
}

alphaCatData$Q2<-sapply(alphaCatData$Q2, correctQ2)

#Q3
correctQ3<-function(input){
  if(input=="1" || input == "\"Splinter\" is to \"Wood\""){
    return ("1")
  }else{
    return ("0")
  }
}

alphaCatData$Q3<-sapply(alphaCatData$Q3, correctQ3)

#Q4
correctQ4<-function(input){
  if(input==1 || input == 89754){
    return ("1")
  }else{
    return ("0")
  }
}

alphaCatData$Q4<-sapply(alphaCatData$Q4, correctQ4)

#Q5
correctQ5<-function(input){
  if(input=="1" || input == "Shape B"){
    return ("1")
  }else{
    return ("0")
  }
}

alphaCatData$Q5<-sapply(alphaCatData$Q5, correctQ5)

#Exploring Data
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwiQvs3w0O_QAhXqjlQKHYkJDEgQFggaMAA&url=http%3A%2F%2Fwww.ats.ucla.edu%2Fstat%2Fr%2Fdae%2Flogit.htm&usg=AFQjCNGT6-Qp31SYHb0DDSSHysaZHbRfpA&sig2=111mVOQ5TR7eT1jFfVg39g&bvm=bv.141320020,d.eWE


#FIRST: Find independence/correlation with Chi Square test
#chisq.test(table(alphaCatData$zerotoeight, alphaCatData$takenCSAPCourse))
getChis<- function(sig = .1){
  outcome <- list()
  for (i in colnames(alphaCatData)[-17]){
    x<-chisq.test(table(alphaCatData$zerotoeight, alphaCatData[[i]]))$p.value
    if (x<sig){
      outcome<-c(outcome, i)
    }
  }
  return (outcome)
}

#At 10% significance, students having some CS experience from other courses outside of school or belonging to schools offering CS have a high liklihood of being a 0->8 student
getChis()

alphaCatData<-as.data.frame(apply(alphaCatData, 2, factor))
#Can use glm to create predictive model for student (data must be dataframe with factor data)
myGLM<-glm(zerotoeight~otherCourses+CSatSchool, data=alphaCatData, family = "binomial")

myPred<-predict(myGLM, type = "response") #give predictions of what

convertCat <- function(x, myMax=.75){
  if (x>=myMax){
    return ("checked")
  }else{
    return ("unchecked")
  }
}

getAcc <- function(input){
  output<-(input[1]+input[4])/80
  if(is.na(output)){
    return (0)
  }else{
    return (output) 
  }
}

growingList<-list()
optimize<-function(){
  output<-0
  bestAcc<-0
  sto<-0
  while(sto!=100){
    newPred<-predict(myGLM, type = "response")
    newPred<-sapply(newPred,function(x){
      convertNewCat(x, newMax= sto*.01)
    })
    Acc<-getAcc(table(alphaCatData$zerotoeight, newPred))
    growingList<<-c(growingList,Acc)
    if (Acc>bestAcc){
      bestAcc<-Acc
      output<-sto*.01
    }
    sto<-sto+1
  }
  return (output)
}

plot(unlist(growingList))
#@best acc(57.5%) uses cut-off of .89 as optimal prob.
table(alphaCatData$zerotoeight, sapply(predict(myGLM, type = "response"), function(x){
  convertNewCat(x, .89)
}))

#Acc
#46/80 = 57.5%

#Making Confidence intervals
#http://www.stat.columbia.edu/~martin/W2024/R11.pdf
exp(confint.default(myGLM))

##################################################
##Anny's attempt at Combining Cat. and Num. data##
##################################################

#READ THROUGH THIS FIRST
#https://www.r-bloggers.com/logistic-regression-and-categorical-covariates/

#Clean up data (cleanData)

cleanData$Q1<-sapply(cleanData$Q1, correctQ1)
cleanData$Q2<-sapply(cleanData$Q2, correctQ2)
cleanData$Q3<-sapply(cleanData$Q3, correctQ3)
cleanData$Q4<-sapply(cleanData$Q4, correctQ4)
cleanData$Q5<-sapply(cleanData$Q5, correctQ5)

#Imputting only numbers
cleanImput<-complete(mice(cleanData, printFlag = FALSE))
cleanImput<-cleanImput[,-c(1,3:10,16,29,30,32:35)]

#Only works with factors
for (i in 1:dim(cleanImput)[2]){
  if(!(i%in%c(1,16,17,20))){
    cleanImput[,i]<-factor(cleanImput[,i])  
  }
}

for (i in c(1,16,17,20)){
  cleanImput[,i]<-as.numeric(cleanImput[,i])
}

#Add -1 to see all intercepts directly (meh)
#http://stats.stackexchange.com/questions/60817/significance-of-categorical-predictor-in-logistic-regression

#Best info
newGLM<-glm(zerotoeight~ReadScore+CSatSchool, data= cleanImput, family = binomial(link = "logit"))
#ReadScore and SCatSchool v. important
summary(newGLM)

newPred<-predict(newGLM, type = "response")

convertNewCat <- function(x, newMax=.75){
  if (x>=newMax){
    return ("checked")
  }else{
    return ("unchecked")
  }
}

#newPred<-sapply(newPred, convertNewCat)

# table(alphaCatData$zerotoeight, newPred)

getAcc <- function(input){
  output<-(input[1]+input[4])/80
  if(is.na(output)){
    return (0)
  }else{
    return (output) 
  }
}

growingList<-list()
optimize<-function(){
  output<-0
  bestAcc<-0
  sto<-0
  while(sto!=100){
    newPred<-predict(newGLM, type = "response")
    newPred<-sapply(newPred,function(x){
      convertNewCat(x, newMax= sto*.01)
    })
    Acc<-getAcc(table(alphaCatData$zerotoeight, newPred))
    growingList<<-c(growingList,Acc)
    if (Acc>bestAcc){
      bestAcc<-Acc
      output<-sto*.01
    }
    sto<-sto+1
  }
  return (output)
}

plot(unlist(growingList))
#@best acc(57.5%) uses cut-off of .88 as optimal prob.
table(alphaCatData$zerotoeight, sapply(predict(newGLM, type = "response"), function(x){
  convertNewCat(x, .88)
}))

############
##Boosting##
############
#Last ditch effort
library(mboost)

boostGLM<-glmboost(zerotoeight~ReadScore+CSatSchool+Qscore, data= cleanImput, family = Binomial(link = c("logit")))

######
growingList<-list()
optimize<-function(){
  output<-0
  bestAcc<-0
  sto<-0
  while(sto!=100){
    newPred<-predict(boostGLM, type = "response")
    newPred<-sapply(newPred,function(x){
      convertNewCat(x, newMax= sto*.01)
    })
    Acc<-getAcc(table(alphaCatData$zerotoeight, newPred))
    growingList<<-c(growingList,Acc)
    if (Acc>bestAcc){
      bestAcc<-Acc
      output<-sto*.01
    }
    sto<-sto+1
  }
  return (output)
}
val<-optimize()
table(alphaCatData$zerotoeight, sapply(predict(boostGLM, type = "response"), function(x){
  convertNewCat(x, val)
}))
plot(unlist(growingList))

#61/80=76.25%
#######

###################
##Testing Dataset##
###################

testingRaw<-read.csv('sec.csv', stringsAsFactors = FALSE)

#Cleaning
testingRaw<-testingRaw[,5:10]
colnames(testingRaw)<-c("ReadScore","name","GPA","GPAScale","CSatSchool","Qscore")

#Correcting GPA
testingRaw$GPA[21]=3.5
testingRaw$GPA[64]=4.0
#Taric Lyazghi and Malaak Hoppie not having GPA
testingRaw<-testingRaw[-c(122,166),]
testingRaw$GPA<-as.numeric(gsub("%","",testingRaw$GPA))

testingRaw$GPAScale[c(36,41,53,72,74,96,105,111,133,174)]="0 - 100%"
testingRaw$GPAScale[c(1,64,68)]="0.0 - 4.0"


testingRaw$scaledGPA<-unlist(cleanNums(testingRaw))
testingRaw$CSatSchool<-factor(testingRaw$CSatSchool)

#Scaling all data
for (i in c(1,6,7)){
  testingRaw[,i]<-as.numeric(testingRaw[,i])
}

#Only works with factors
for (i in 1:dim(testingRaw)[2]){
  if(!(i%in%c(1,6,7))){
    testingRaw[,i]<-factor(testingRaw[,i])  
  }
}

#Boosted GLM
x<-sapply(predict(boostGLM, newdata = testingRaw,type = "response"), function(x){
  convertNewCat(x, val)
})

#Get these students! (that qualify)
#4 "missed", but likely more ()
BGLM.missed<-as.vector(testingRaw$name[which(x=="checked")])

#LDA
testing<-predict(preProcess(almostNumData, c("BoxCox", "center", "scale")), testingRaw)
y<-predict(myLDA, newdata = testing)
sum(y$class=="checked")

#Get these students! (that qualify)
#38 "missed"
LDA.missed<-testing$name[y$class=="checked"]

missed<-c(BGLM.missed, LDA.missed)

which(cleanData$name%in%missed)
