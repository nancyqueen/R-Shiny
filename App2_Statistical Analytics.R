#####################################################################
rm(list = ls())

library(shiny)
library(ggplot2)
library(datasets)
library(survival)
library(dplyr)
library(DT) 
library(shinyjs) 
#####################################################################

# Define the UI
# UI (user interface) layout: bootstrap
ui = fluidPage(
  # set up shinyjs
  shinyjs::useShinyjs(),
  # App title
  titlePanel("Step II: Business Analytics"),
  h4("Created by: Nancy"),
  
  sidebarLayout(
    # UI Input:
    sidebarPanel(
      
      # uploading file:
      fileInput(inputId = "file", label = "Choose a CSV file",
                accept = c('text/csv', 
                        'text/comma-separated-values,text/plain', 
                        '.csv')),
      tags$hr(), # make a horizontal line as a thematic break

      
      # Create a select list input control
      selectInput(inputId = "variable", 
                  label = "Choose a Key Variable:",
                  choices = c("Please Select",
                              "Age at Hire" = "AgeAtHire",
                              "Business Unit"="BU3", 
                              "City Tier"="City.Tier",
                              "First Employee Level"="firstEELevel", 
                              "Gender"="Gender", 
                              "Highest Degree"="Highestdegree", 
                              "Hire Source"="hiresource_com", 
                              "Last Employee Level"="lastEELevel", 
                              "Service Year"= "ServiceYear")
                  ),
      
      
      # Filter inputs
      h4("Filter"),
      sliderInput(inputId = "age_at_hire", label = "Age at Hire",
                  min = 19, max = 46, value = c(19, 46)),
      # no choices before uploading
      selectInput(inputId = "city_tier", label = "City Tier",
                  choices = NULL
      ),
      selectInput(inputId = "gender", label = "Gender",
                  choices = NULL
      ),
      selectInput(inputId = "highest_degree", 
                  label = "Highest Degree",
                  choices = NULL
      ),
      selectInput(inputId = "bu3", label = "Business Unit",
                  choices = NULL
      ),
      selectInput(inputId = "hire_source", label = "Hire Source",
                  choices = NULL
      ),
      sliderInput(inputId = "service_year", 
                  label = "Service Year",
                  min = 0, max = 20, value = c(0, 20)),

      # Create Checkbox Group Input Control
      checkboxGroupInput(inputId ="status", label = "Terminate",
                         choices = NULL,
                         selected = NULL
                         ),
   
      
      # User-friendly Notes:
      tags$small(paste0(
        "Note: This is the Shiny App #2",
        "",
        ""
      ))
    ),
    
    # UI Output:
    mainPanel(
      # inserts caption, plot, summary
      
      tabsetPanel(type = "tabs",
        tabPanel("Data Overview",
                 h3(textOutput("caption1", container = span)),
                 dataTableOutput('contents')),
        tabPanel("Model Interpretation",
                 h3(textOutput("caption4", container = span)),
                 verbatimTextOutput("model"),
                 htmlOutput("explain")),
        tabPanel("Descriptive Statistics",
                 h3(textOutput("caption2", container = span)),
                 plotOutput(outputId = 'plot', width = "60%", 
                            height = "500px"),
                 verbatimTextOutput("summary"),
                 plotOutput(outputId = 'KM', width = "60%", 
                            height = "450px"),
                 verbatimTextOutput("median")),
        tabPanel("Prediction",
                 plotOutput(outputId = 'box', width = "60%", 
                            height = "480px"),
                 h3(textOutput("caption3", container = span)),
                 dataTableOutput("list"))
      )

    )
  )
)

#####################################################################
# Define the server code
server <- function(input, output, session) {
  
  observe({
    shinyjs::toggleState("gender",
                         input$variable != "Gender")
    shinyjs::toggleState("city_tier",
                         input$variable != "City.Tier")
    shinyjs::toggleState("highest_degree",
                         input$variable != "Highestdegree")
    shinyjs::toggleState("bu3",
                         input$variable != "BU3")
    shinyjs::toggleState("hire_source",
                         input$variable != "hiresource_com")

  })

  
  # Filter the baseinf, returning a data frame
  baseinf_df <- reactive({

    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header=TRUE, sep=',')
    
    data$ifTerminate <- as.factor(data$ifTerminate)
    data$ifTerminate <- ifelse(data$ifTerminate==1, "Yes", "No")

    # Update select inputs right after uploading file
    updateSelectInput(session, "city_tier", 
                      choices = c("All", 
                                  unique(as.character(data$City.Tier))))

    updateSelectInput(session, "gender",
                      choices = c("All",
                                  unique(as.character(data$Gender)))
    )
    updateSelectInput(session, "highest_degree",
                      choices = c("All",
                                  unique(as.character(data$Highestdegree)))
    )
    updateSelectInput(session, "bu3",
                      choices = c("All",
                                  unique(as.character(data$BU3)))
    )
    updateSelectInput(session, "hire_source",
                      choices = c("All",
                                  unique(as.character(data$hiresource_com)))
    )

    updateCheckboxGroupInput(session, "status",
                             choices = c(unique(data$ifTerminate)),
                             selected = c(unique(data$ifTerminate))
                       )
    
    data
    
  })
  
  baseinf_df2 <- reactive({
    
    inFile <- input$file
      
    if (is.null(inFile))
        return(NULL)
      
    data <- baseinf_df()
      
      minAgeAtHire <- input$age_at_hire[1]
      maxAgeAtHire<- input$age_at_hire[2]
      
      minServiceYear <- input$service_year[1]
      maxServiceYear <- input$service_year[2]
      
      # Apply filters, begin with cts variable
      data <- filter(data,
                    AgeAtHire >= minAgeAtHire,
                    AgeAtHire <= maxAgeAtHire)
      
      data <- filter(data,
                     ServiceYear >= minServiceYear,
                     ServiceYear <= maxServiceYear)
       
      # Filter by City Tire
      if (input$city_tier!= "All") {
        data <- filter(data, City.Tier==input$city_tier)
        }
    
      # Filter by Gender
      if (input$gender!= "All") {
        data <- filter(data, Gender==input$gender)
      }

      # Filter by Highest Degree
      if (input$highest_degree!= "All") {
        data <- filter(data, Highestdegree==input$highest_degree)
      }

      # Filter by Business Unit
      if (input$bu3!= "All") {
        data <- filter(data, BU3==input$bu3)
      }
      
      if (input$hire_source!= "All") {
        data <- filter(data, hiresource_com==input$hire_source)
      }
  

      # Filter by Terminate
      data <- filter(data, ifTerminate%in%input$status)
      data

  })
  
  runSur<- reactive({

    inFile <- input$file
    if (is.null(inFile))
      return(NULL)

    data <- baseinf_df2()
    
    data$Terminate <- ifelse(data$ifTerminate=="Yes", 1, 0)
    
    # K-M estimate
    survfit(as.formula(paste("Surv(ServiceYear,Terminate) ~ ", 
                             paste(input$variable))),
            conf.type="log-log", data= data)
    
 })
  
  output$caption1 <- renderText({
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    paste("Take A Quick Look At Uploaded Data Set: ")
  })
  
  output$contents <- renderDataTable({
    req(baseinf_df2())
    t <- baseinf_df2()
    
    t <- t[, c("EE.ID", "ifTerminate", "AgeAtHire",
             "Gender","City.Tier", "Highestdegree",
             "firstEELevel", "lastEELevel", "hiresource_com",  
             "BU3", "SSYatHire", "lastpositionyear",
             "ServiceYear","risk.score","cut")]
    t 
  }, options = list(pageLength = 10))
  
  output$caption2 <- renderText({
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)
    
    else if(input$variable == "Please Select")
      return(NULL)
    
    paste("Descriptive Statistics")
  })
  
  # Rendering function: creat a plot - renderPlot()
  output$plot <- renderPlot({

    test <- baseinf_df2()
    
    if(input$variable == "Please Select"){
      return(NULL)
    }
    
    else if(input$variable == "AgeAtHire"){
      
      ggplot(test, aes(x = AgeAtHire,
                       group=ifTerminate, fill=ifTerminate)) +
        geom_histogram(binwidth=1, colour="black") +
        ylab("Number of Employees") +
        labs(fill='Leaving') +
        theme(plot.title = element_text(size=20, face="bold"), 
              axis.text=element_text(size=12),
              axis.title=element_text(size=15)) + 
        xlim(15, 50) + ggtitle("Histogram of Continuous Variable")
    }
    
    
    else if(input$variable != "AgeAtHire" &
            input$variable !="ServiceYear"){
      
      # Attention: pass input$variable using aes_string()
      ggplot(test, aes_string(x = input$variable)) +
        aes(group = ifTerminate, fill = ifTerminate) +
        # y axis: count (default)
        geom_bar(position="stack") +
        labs(fill="Terminate?") +
        ggtitle("Bar Chart of Categorical Variable") +
        ylab("Number of Employees") + 
        theme(plot.title = element_text(size=20, face="bold"), 
              axis.text=element_text(size=12),
              axis.title=element_text(size=15))
    }
    
  })
    

  output$summary <- renderPrint({
    #test <-baseinf_df2()[[1]]
    test <- baseinf_df2()
    
    # A function, which displays freq and % in a table
    multi.fun <- function(x) {
      cbind(Freq = table(x),
            Percentage = prop.table(table(x))*100)
    }

    if(input$variable == "AgeAtHire"){
      summary(test$AgeAtHire)
    }
    
    else if(input$variable =="ServiceYear") {
      summary(test$ServiceYear)
    }
    
    else if(input$variable != "AgeAtHire" & 
            input$variable != "Please Select"){
      # using [[]] instead of $ sign
      multi.fun(test[[input$variable]])
    }
  })

  output$KM <- renderPlot({
    
    test <- baseinf_df2() # for legend

    #baseinf_df2()[[2]]
    if(input$variable == "AgeAtHire"){
      return(NULL)
    }
    
    else if(input$variable == "ServiceYear"){
      return(NULL)
    }
  
    # KM plotting
    else if(input$variable != "Please Select" & 
            input$variable != "AgeAtHire" &
            input$variable != "ServiceYear"){
      
      plot(runSur(), lty=c(1,1,1,1,1), 
           lwd = 2, cex.lab=1.2, cex.main = 1.5,
           col=c("red","sky blue","green","purple","orange"), 
           xlab="Follow-up Time (yrs)", ylab="Survival Probability, S(t)",  
           main="Survival Estimation over Time")
      legend("right", lty=c(1,1,1,1,1), title = paste(input$variable),
             legend = sort(unique(as.character(test[[input$variable]]))),
             col = c("red","sky blue","green","purple","orange"),
             cex = 1.2)
      abline(h=0.5, col = "gray", lty = 2, cex=1.5)
  
    }

})
  
  output$median <- renderPrint({
    
    if(input$variable != "Please Select" & 
            input$variable != "AgeAtHire" &
            input$variable != "ServiceYear"){
    # result is a matrix  
    result <- quantile(runSur(), probs = 0.5)$quantile
    colnames(result) <- "Median in years"
    round(result, 3)
    }
    
  })
  
  output$box <- renderPlot({
    
    req(baseinf_df2())
    test <- baseinf_df2()
    temp <- test[complete.cases(test), ]
    
    temp$City.Tier <- as.factor(temp$City.Tier)
    temp <- temp[order(-temp$risk.score), ] # descending
    
    if(input$variable == "Please Select"){
      ggplot(temp, aes(x = risk.score)) +
        geom_histogram(binwidth=0.5, colour="black") +
        ggtitle("Histogram of Risk Score") + 
        xlim(0,20) + ylim(0,150) +
        theme(plot.title = element_text(size=20, face="bold"),
              axis.text=element_text(size=12),
              axis.title=element_text(size=15))
    }
    
    else if(input$variable != "AgeAtHire" &
            input$variable !="ServiceYear"){
      
      # Attention: pass input$variable using aes_string()
      ggplot(temp, aes_string(x = input$variable)) +
        aes(y = risk.score) +
        geom_boxplot(fill = "pink", colour = "purple") +
        ggtitle("Boxplot of Risk Score by Categorical Variable") +
        geom_hline(yintercept = temp[1,"cut"], colour = "red",
                   linetype = "longdash") + 
        scale_y_continuous(name = "Risk Score", 
                           breaks = seq(0,16,1), limits = c(0,16)) +
        theme(plot.title = element_text(size=20, face="bold"),
              axis.text=element_text(size=12),
              axis.title=element_text(size=15,face="bold"))
      }
    
  })
  
  output$caption3 <- renderText({
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    else if(input$variable == "Please Select" | 
            input$variable == "AgeAtHire")
      return(NULL)
    
    paste("List of Sales Employees in High Risk")
  })
  
  output$list<- renderDataTable({
    req(baseinf_df2())
    
    test = baseinf_df2()
    test <- na.omit(test)

    test <- test[order(-test$risk.score), ] # descending
    test$risk.score <- round(test$risk.score, 3)

    t <- test[1:ceiling(nrow(test)*0.05), 
              c("EE.ID", "ifTerminate", "AgeAtHire",
               "Gender","City.Tier", "Highestdegree",
               "firstEELevel", "lastEELevel", "hiresource_com",
               "BU3", "risk.score","cut")]
    
    if(input$variable != "Please Select" &
       input$variable != "AgeAtHire") {
      t
      }
    }, options = list(pageLength = 10))
  
  output$caption <- renderText({
    
    paste("Modelling")
  })
  
  output$model <- renderPrint({
    
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)
    
    test <- baseinf_df2()
    
    test$ifTerminate <- ifelse(test$ifTerminate=="Yes", 1, 0)

    coxph.final<-coxph(Surv(ServiceYear, ifTerminate)~Gender +
                         factor(City.Tier) + AgeAtHire +
                         Highestdegree + firstEELevel +
                         lastEELevel + BU3 + lastpositionyear,
                       data=test, method='breslow')

    l <- list(coxph.final$call, round(coxph.final$coefficients,4),
         round(exp(coxph.final$coefficients),4))
    names(l) <- c("model", "coefficient", "exp(coefficient)")
    l
    
  })
  
  output$explain <- renderUI({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    #paste("")
    str1 <- paste("How to interpret the coefficients above?")
    str2 <- paste("First of all, ...")
    str3 <- paste("For example, ...")
    
    HTML(paste(str1,str2,str3,sep = "<br/><br/>"))
  })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
