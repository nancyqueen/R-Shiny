# R Shiny App

#### R Shiny: a web application framework for R
#### Turn analyses into interactive web applications

https://shiny.rstudio.com/

### Loading required R packages: 
```
library(shiny)
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

### Function Reference:
https://shiny.rstudio.com/reference/shiny/latest/


## Part III: Return a Shiny app object
```
shinyApp(ui = ui, server = server)
```



