#####################################################################
rm(list = ls())

#####################################################################
# Loading R packages
library(shiny)
library(ggplot2)
library(datasets)
library(survival)
library(dplyr)
library(shinyjs)
library(KMsurv)
library(DT)
library(lubridate)  # ymd()
library(doBy)       # recodeVar()
library(caTools)
#####################################################################
# UI
ui = fluidPage(
  shinyjs::useShinyjs(),
  
  titlePanel("Step I: Data Manipulation"),
  h4("Created by: Nancy"),
  
  sidebarLayout(
    sidebarPanel(
      
      # uploading files:
      fileInput(inputId = "file1", 
                label = "Choose the first EXCEL(.csv) file (Ex. Data)",
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      tags$hr(),
      
      fileInput(inputId = "file2", 
                label = "Choose the second EXCEL(.csv) file (Ex. Education)",
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      tags$hr(),
      
      fileInput(inputId = "file3", 
                label = "Choose the third EXCEL(.csv) file (Ex. Hiring)",
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      tags$hr(),
      
      fileInput(inputId = "file4", 
                label = "Choose the forth EXCEL(.csv) file (Ex. Movement)",
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      tags$hr(),
      
      h3("Please Read it before 'Download':"),

      h4(""),
      
      downloadButton(outputId = 'downloadData', label = 'Download')
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Data Overview",
                           dataTableOutput('contents')
                           )
                  )
      )
  )
)

#####################################################################
# Server
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  getData1 <- reactive({
    inFile1 <- input$file1
    req(inFile1)
    data <- read.csv(inFile1$datapath, header=TRUE, sep=',')
    # Lv: last employee level (Sales ONLY)
    data <- filter(data,Lv =="S1"|Lv =="S2"|Lv =="S3"|
                      Lv =="S6"|Lv =="S7")
    # rename a single column
    colnames(data)[colnames(data)=="Lv"] <- "lastEELevel"
    data
  })
  
  getData2 <- reactive({

    inFile2 <- input$file2
    req(inFile2)
    edu <- read.csv(inFile2$datapath, header=TRUE, sep=',')
    
    Highestdegree <- rep(NA,nrow(edu))
    Highestdegree[grep("Diploma", edu$Certificate)]<-"Diploma"
    Highestdegree[grep("Associate", edu$Certificate)]<-"Associate"
    Highestdegree[grep("Bachelor", edu$Certificate)]<-"Bachelor"
    Highestdegree[grep("Master", edu$Certificate)]<-"Master"
    Highestdegree[grep("MBA", edu$Certificate)]<-"Master"
    Highestdegree[grep("Doctor", edu$Certificate)]<-"Doctor"
    
    education <- data.frame(edu$EE.ID,Highestdegree)
    colnames(education)<-c('EE.ID','Highestdegree')
    ID <- unique(education$EE.ID)
    edu <- c()
    for (i in 1:length(ID)){
      temp <- education[which(education$EE.ID==ID[i]),]
      edu <-rbind(edu, temp[1,])
    }
    edu
  })

  getData3 <- reactive({

    inFile3 <- input$file3
    req(inFile3)
    hire <- read.csv(inFile3$datapath, header=TRUE, sep=',')
    
    hiring <- data.frame(hire$EE.ID, hire$Hiring.Source)
    colnames(hiring) <- c("EE.ID","hiresource")
    hiresource_com <- rep("Social Recruitment", nrow(hiring))
    hiresource_com[which(hiring$hiresource==
                           "Campus Recruitment")]<-"Campus Recruitment"
    hire <- cbind(hiring, hiresource_com)
    hire

  })
  
  getData4 <- reactive({
    
    inFile4 <- input$file4
    req(inFile4)
    movement <- read.csv(inFile4$datapath, header=TRUE, sep=',')
    
    move <- data.frame(movement$EE.ID, movement$Lv)
    move <- filter(move,movement.Lv =="S1"|
                           movement.Lv =="S2"|movement.Lv =="S3"|
                           movement.Lv =="S6"|movement.Lv =="S7")
    names(move) <- c("EE.ID","Lv")
    
    ID = unique(move$EE.ID)
    
    movement <- c()
    for (i in 1:length(ID) ) {
      temp <- subset(move, EE.ID == ID[i])
      movement <- rbind(movement, temp[1,])
    }
    # Lv: first employee level (Sales ONLY)
    names(movement)<-c("EE.ID","firstEELevel")
    movement
  })
  
  mergeData <- reactive({
    data <- getData1()
    edu <- getData2()
    hire <- getData3()
    movement <- getData4()
    
    temp1 <- merge(data, edu, by = "EE.ID", all.x = TRUE)
    temp2 <- merge(temp1, hire, by = "EE.ID", all.x = TRUE)
    temp3 <- merge(temp2, movement, by = "EE.ID", all.x = TRUE)
    
    for(i in 1:length(temp3$EE.ID)){
      if(temp3$Termination.Date[i]==""){
        temp3$ifTerminate[i]=0
        }
      else{temp3$ifTerminate[i]=1}
    }
    
    Tday<-Sys.Date() # Data of Today
    temp3$Birthday<-ymd(temp3$Birthday) # "yyyy-mm-dd"
    temp3$Position.Entry.Date<-ymd(temp3$Position.Entry.Date)
    temp3$Original.Hire.Date<-ymd(temp3$Original.Hire.Date)
    temp3$Social.Service.Date<-ymd(temp3$Social.Service.Date)
    temp3$Termination.Date<-ymd(temp3$Termination.Date)
    timenode<-rep(Tday,length(temp3$EE.ID))
    timenode<-ymd(timenode)
    
    for(i in 1:length(temp3$EE.ID)){
      # Social Service Year at Hire
      temp3$SSYatHire[i]<-(temp3$Original.Hire.Date[i]-
                             temp3$Social.Service.Date[i])/365.25
      # Age at Hire
      temp3$AgeAtHire[i]<-floor((temp3$Original.Hire.Date[i]-
                                   temp3$Birthday[i])/365.25)
      
      if(temp3$ifTerminate[i]==1){
        # Service Year At Lilly
        temp3$ServiceYear[i]<-(temp3$Termination.Date[i]-
                                 temp3$Original.Hire.Date[i])/365.25
        # Last Position Year
        temp3$lastpositionyear[i]<-
          (temp3$Termination.Date[i]-temp3$Position.Entry.Date[i])/365.25
      }
      
      if(temp3$ifTerminate[i]==0) {
        # Service Year At Lilly
        temp3$ServiceYear[i]<-
          (timenode[i]-temp3$Original.Hire.Date[i])/365.25
        # Last Position Year
        temp3$lastpositionyear[i]<-
          (timenode[i]-temp3$Position.Entry.Date[i])/365.25
      }
        
    }
    
    # source values
    src <- list("BioMedicines BU-Cialis & Retail",
                "BioMedicines BU-CNS", "BioMedicines BU-OP",
               c("Diabetes","Lilly Diabetes HR","Sales Region"),
               "Oncology")
    # target values
    tgt <- list("Cialis","CNS","OP","Diabetes","Oncology")
    # Buiness Unit: recode values of a vector
    temp3$BU3<-recodeVar(x = temp3$Bus.Node.3.Title, 
                           src=src, tgt=tgt)
    
    # treat all null value as NA 
    temp3<-within(temp3, {AgeAtHire[AgeAtHire==""]<-NA})
    temp3<-within(temp3, {Gender.key[Gender.key==""]<-NA})  
    temp3<-within(temp3, {SSYatHire[SSYatHire<0]<-NA})  
    temp3<-within(temp3, {ServiceYear[ServiceYear<=0]<-NA}) 
    temp3<-within(temp3, {BU3[BU3==""]<-NA})
    temp3<-within(temp3, {Highestdegree[Highestdegree=="NA"]<-NA})
    temp3<-within(temp3, {lastEELevel[lastEELevel==""]<-NA})
    temp3<-within(temp3, {firstEELevel[firstEELevel==""]<-NA})
    temp3<-within(temp3, {hiresource_com[hiresource_com==""]<-NA})
    temp3<-within(temp3, {lastpositionyear[lastpositionyear==""]<-NA})
    temp3<-within(temp3, {Ar.[Ar.==""]<-NA})
    
    final <- temp3[ ,c("EE.ID", "ifTerminate", "AgeAtHire","Gender.key", 
                     "Ar.", "firstEELevel", "lastEELevel", 
                     "Highestdegree", "hiresource_com", "BU3", 
                     "SSYatHire", "lastpositionyear", "ServiceYear")]
    colnames(final)[colnames(final)=="Ar."] <-"City.Tier"
    colnames(final)[colnames(final)=="Gender.key"] <-"Gender"
    # Remove all observations with missing data (6414 to 2815)
    final <- na.omit(final)
    
    # Fit a PH Reg Model:
    coxph.final<-coxph(Surv(ServiceYear, ifTerminate)~Gender +
                         factor(City.Tier) + AgeAtHire +
                         Highestdegree + firstEELevel +
                         lastEELevel + BU3 + lastpositionyear,
                       data=final, method='breslow')
    #summary()

    # Calculate risk score by Cox
    not_terminate <- subset(final, ifTerminate==0)
    terminate <- subset(final, ifTerminate==1)
    risk <- predict(coxph.final, newdata=not_terminate, type="risk")
    not_terminate$risk.score <- risk
    terminate$risk.score <- rep(NA, nrow(terminate))
    final <- rbind(terminate, not_terminate)
    
    # create a cutoff line for risk score
    temp <- final[complete.cases(final), ]
    temp <- arrange(temp, desc(risk.score)) # descending
    cut <- temp[ceiling(nrow(temp)*0.05), "risk.score"]
    final$cut <- rep(cut, nrow(final))
    final <-  final[order(final$EE.ID),]
    
    # rounding selected variables
    final$cut <- round(final$cut, 2)
    final$SSYatHire <- round(final$SSYatHire, 4)
    final$ServiceYear <- round(final$ServiceYear, 4)
    final$lastpositionyear <- round(final$lastpositionyear, 4)
    final$risk.score <- round(final$risk.score, 4)

    final
  })
  

  output$contents <- renderDataTable({

    final <- mergeData()
    
    final
    
  }, options = list(pageLength = 10))
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("final", '.csv', sep='') 
    },
    
    content = function(file) {
      write.csv(mergeData(), file)
    }
  )
  
  
}

#####################################################################  
# Return a Shiny app object
shinyApp(ui = ui, server = server)    

      
      
      