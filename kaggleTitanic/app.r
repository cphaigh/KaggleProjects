library(shiny)
library(dplyr)
library(caret)
library(rsconnect)

ui <- fluidPage(
  titlePanel("Titanic Survivorship"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("pclass","Ticket Class",min = 1,max=3,step = 1,value=2),
      selectInput("title","Title",choices = c("Mrs","Miss","Mr","Master","other")),
      selectInput("sex","Gender",choices=c("female","male")),
      sliderInput("age","Age",min = 1,max=80,step=1,value=40),
      sliderInput("fare","Price of Ticket",min=0,max=500,step=5,value=100),
      selectInput("cabinletters","Cabin Letter",choices = c("A","B","C","D","E","F","none")),
      sliderInput("cabinnumbers","Cabin Number",min=0,max=148,step=1,value=74),
      selectInput("embarked","City where boarded the ship",choices = c("Cherbourg","Queenstown","Southampton")),
      sliderInput("sibsp","Siblings and Spouses on Ship",min=0,max=8,step=1,value=0),
      sliderInput("parch","Parents and Children on Ship",min=0,max=8,step=1,value=0)),
    mainPanel(
      h2("Survival Percentage"),
      textOutput("myoutput"),
      tags$head(tags$style("#myoutput{color: green;
                                 font-size: 30px;
            font-style: bold;
            }")
      ))))

server <- function(input, output, session) {
  output$myoutput <- renderText({
    # creating data frame
    test <- data.frame(Pclass=input$pclass,title=input$title,
                       Sex=input$sex,Age=input$age,
                       Fare=input$fare,cabinLetters=input$cabinletters,
                       cabinNumbers=input$cabinnumbers,
                       Embarked=input$embarked,
                       SibSp=input$sibsp,Parch=input$parch)
    
    # embarked names to letters
    test[test$Embarked=="Queenstown","Embarked"] <- "Q"
    test[test$Embarked=="Cherbourg","Embarked"] <- "C"
    test[test$Embarked=="Southampton","Embarked"] <- "S"
    
    #grouping cabin numbers
    test[test$cabinNumbers==0,"cabinNumbers"] <- 0
    test[test$cabinNumbers!=0 & test$cabinNumbers<=75,"cabinNumbers"] <- 2
    test[test$cabinNumbers>75,"cabinNumbers"] <- 1
    
    #has sibling
    test <- test %>% 
      mutate(hasSibSp=ifelse(SibSp==0,0,1))
    
    #isParent
    test <- test %>%
      mutate(parent=ifelse(Age>18 & Parch>0,1,0))
    
    #sex to ordered
    test[test$Sex=="male","Sex"] <- 1
    test[test$Sex=="female","Sex"] <- 0
    test$Sex <- as.integer(test$Sex)
    
    #center and scale
    centerScale <- readRDS("centerScale.rds")
    test <- predict(centerScale,test)
    
    #pclass - 1
    test[test$Pclass==1,"Pclass"] <- 0
    test[test$Pclass==2,"Pclass"] <- 1
    test[test$Pclass==3,"Pclass"] <- 2
    
    #to factors
    test$Embarked <- factor(test$Embarked,levels=c("C","Q","S"))
    test$cabinLetters <- factor(test$cabinLetters,levels=c("A","B","C","D","E","F","none"))
    test$title <- factor(test$title,levels=c("Master","Miss","Mr","Mrs","other"))
    
    #to dummies
    test$Survived <- 1
    dummied <- readRDS("dummied.rds")
    test <- predict(dummied,test)
    test <- as.data.frame(test)
    #needed to add for dummy variables
    test <- test %>% select(-Survived)
    
    gbmMod <- readRDS("gbmMod.rds")
    mypredictions <- predict(gbmMod,test,type="prob")
    mypredictions$`1`
    
  })
}
shinyApp(ui, server)

rsconnect::setAccountInfo(name='cphaigh', token='168131008E1C174A06A4308ECE55808C', secret='DfnLEgzuFbMBn3RP8SNoktx0R9Ta8WBDxpfzice0')
rsconnect::deployApp('C:/Users/cphai/OneDrive/Desktop/all_programs/R/724titanic/app.r')
