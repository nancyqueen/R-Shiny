# R Shiny App

### Loading required R packages: 
```
library(shiny)
library(ggplot2)
library(survival)
library(shinyjs)
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
### 1. Upload file(s)
### 2. Download file
### 3. Checkbox
### 4. Selectbox
### 5. Sliders


## Part I: Define UI (user interface)
- title, layout, ...
- UI input: 
```
fileInput, sliderInput(), checkboxInput(), selectInput(), checkboxGroupInput(),...
```
- UI output：
```
tableOutput(), plotOutput(), verbatimTextOutput(), h4(),...
```

## Part II: Define Server
```
shinyjs::toggleState
reacitve({}): updateSelectInput(), updateCheckboxGroupInput(),...
reacitve({}): filter
reacitve({}): survfit() # Kaplan-Meier Estimate
output: renderPlot(ggplot, ggsurvplot), renderText(), renderPrint(), renderTable(),...
```

## Part III: Return a Shiny app object
```
shinyApp(ui = ui, server = server)
```



