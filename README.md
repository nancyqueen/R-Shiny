# R-Shiny-Stats

### Loading required R packages: 
```
library(shiny)
library(ggplot2)
library(survival)
library(shinyjs)
library(devtools)
```

## part I: Define UI (user interface)
- title, layout, ...
- UI input: 
```
sliderInput, checkboxInput, selectInput, checkboxGroupInput,...
```
- UI output：

```
tableOutput, plotOutput, verbatimTextOutput, h4,...
```
## part II: Define Server
```
shinyjs::toggleState
reacitve({}): updateSelectInput, updateCheckboxGroupInput,...
reacitve({}): filter
reacitve({}): survfit # Kaplan-Meier Estimate
output: renderPlot (ggplot, ggsurvplot), renderText, renderPrint, renderTable,...
```

## part III: Return a Shiny app object



