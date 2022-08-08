library(dplyr)
library(stringr)
library(caret)

makeTest <- function(pclass,title,sex,age,fare,cabinletters,cabinnumbers,embarked,sibsp,parch){
  embarked <- ifelse(embarked=="Cherbourg","C",ifelse(
    embarked=="Queenstown","Q","S"))
  test <- data.frame(Pclass=pclass,title=title,Sex=sex,Age=age,
                     Fare=fare,cabinLetters=cabinletters,
                     cabinNumbers=cabinnumbers,
                     SibSp=sibsp,Parch=parch)
  test
}
makeTest(1,"Mr","female",40,50,"A",80,)

test <- data.frame(Survived=c(1,0),Pclass=c(2,3),Name=c("Braund, Mr. Owen Harris","Futrelle, Mrs. Jacques Heath (Lily May Peel)"),
                    Sex=c("male","female"),Age=c(30,45),SibSp=c(0,4),
                    Parch=c(0,3),Fare=c(5,30), Cabin=c("C80",""), Embarked=c("S","Q"))

getTitle <- function(test){
  test <- test %>%
    mutate(title=ifelse(grepl("MR",toupper(Name))==TRUE & grepl("MRS",toupper(Name))==FALSE,"Mr",
                        ifelse(grepl("MRS",toupper(Name))==TRUE,"Mrs",
                               ifelse(grepl("MISS",toupper(Name)),"Miss",
                                      ifelse(grepl("MASTER",toupper(Name))==TRUE,"Master","other")))))
  test %>% select(-Name)
  
}
test <- getTitle(test)

firstLetter <- function(test){
    #pulling first letter
    test <- test %>% 
    mutate(cabinLetters=substr(Cabin,1,1))
  #setting rare labels to mode
    test[test$cabinLetters %in% c("G","T"),"cabinLetters"] <- ""
  #replacing blank space factor with "none"
    test[test$cabinLetters=="","cabinLetters"] <- "none"
    test
}
test <- firstLetter(test)

cabinNumbers <- function(test){
  test <- test %>% 
    mutate(cabinNumbers=substr(word(Cabin,1),2,nchar(word(Cabin,1))))
  
  test[test$cabinNumbers=="","cabinNumbers"] <- 0
  
  test$cabinNumbers <- as.integer(test$cabinNumbers)
  
  test[test$cabinNumbers!=0 & test$cabinNumbers<=75,"cabinNumbers"] <- 2
  
  test[test$cabinNumbers>75,"cabinNumbers"] <- 1
  test <- test %>% select(-Cabin)
  test
}
test <- cabinNumbers(test)

hasSibling <- function(test){
  test <- test %>% 
    mutate(hasSibSp=ifelse(SibSp==0,0,1))
  test
}
test <- hasSibling(test)

parent <- function(test){
  test <- test %>%
    mutate(parent=ifelse(Age>18 & Parch>0,1,0))
  test
}
test <- parent(test)


sexToOrdered <- function(test){
  test[test$Sex=="male","Sex"] <- 1
  test[test$Sex=="female","Sex"] <- 0
  test$Sex <- as.integer(test$Sex)
  test
}
test <- sexToOrdered(test)

preProcessFunction <- function(test){
  centerScale <- readRDS("centerScale.rds")
  test <- predict(centerScale,test)
  test
}
test <- preProcessFunction(test)

Pclass <- function(test){
  test <- test %>% mutate(Pclass=Pclass-1)
  test
}
test <- Pclass(test)

factorVariables <- function(test){
  test$Embarked <- factor(test$Embarked,levels=c("C","Q","S"))
  test$cabinLetters <- factor(test$cabinLetters,levels=c("A","B","C","D","E","F","none"))
  test$title <- factor(test$title,levels=c("Master","Miss","Mr","Mrs","other"))
  test
}
test <- factorVariables(test)

makeDummy <- function(test){
  dummied <- readRDS("dummied.rds")
  test <- predict(dummied,test)
  test <- as.data.frame(test)
  test
}
test <- makeDummy(test)


gbmModProbs <- function(test){
  gbmMod <- readRDS("gbmMod.rds")
  mypredictions <- predict(gbmMod,test,type="prob")
  mypredictions$`1`
}
prediction <- gbmModProbs(test)
prediction
