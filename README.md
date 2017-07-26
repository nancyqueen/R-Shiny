# R Shiny App

#### R Shiny: a web application framework for R
#### Turn analyses into interactive web applications

https://shiny.rstudio.com/

### Loading required R packages: 
```
library(shiny)
# Perform JavaScript operations in Shiny apps using plain R code
library(shinyjs)
```
```
library(ggplot2)
library(survival)
library(dplyr)
library(DT)
```

### Loading optioanl R packages: 
```
library(devtools)
library(datasets)
library(KMsurv)
library(lubridate)  
library(doBy)      
library(caTools)
```

## Basic Features
#### 1. Upload file(s)
#### 2. Download file
#### 3. Checkbox, Selectbox, Sliders


### R shiny Showcase:
https://www.rstudio.com/products/shiny/shiny-user-showcase/
### R shiny Gallery (examples):
https://shiny.rstudio.com/gallery/


## Part I: Define UI (user interface)
```
ui = fluidPage()
```
- UI Layout:
```
shinyjs::useShinyjs() 
titlePanel(), sidebarLayout(sidebarPanel(),mainPanel())
```
- UI input: 
```
fileInput(inputId="", label="", accept=), 
sliderInput(inputId="", label="", min=, max=, value=), 
selectInput(inputId="variable", label="", choices=), 
checkboxGroupInput(inputId="", label="", choices=, selected=),
checkboxInput(), ...
```
- UI output：
```
dataTableOutput(outpuId="")
plotOutput(outputId = 'plot', width = "60%", height = "500px")
verbatimTextOutput(),
htmlOutput(""),
h4(),h3(),...
```

## Part II: Define Server
```
server <- function(input, output, session) {}
```
```
observe({})
shinyjs::toggleState("inputId", input$variable!="")
```
- reacitve({}): 
```
updateSelectInput(session, "inputId", choices = c()), 
updateCheckboxGroupInput(session, "inputId", choices=, selected=),
filter(data, X==input$variable)
survfit(as.formula(paste("Surv(Time,Status) ~ ", paste(input$variable))),
        conf.type="log-log", data= data) # Kaplan-Meier Estimate
```
- Render Functions:
```
renderPlot({ggplot()})
renderText({paste()})
renderPrint({summary()})
renderDataTable({}, options = list(pageLength = 10))
```

### Function Reference:
https://shiny.rstudio.com/reference/shiny/latest/


## Part III: Return a Shiny app object
```
shinyApp(ui = ui, server = server)
```



