# RShiny

### Loading required R packages: 
```
library(shiny)
library(ggplot2)
library(survival)
library(shinyjs)
library(devtools)
```

## Part I: Define UI (user interface)
- title, layout, ...
- UI input: 
```
sliderInput, checkboxInput, selectInput, checkboxGroupInput,...
```
- UI output：

```
tableOutput, plotOutput, verbatimTextOutput, h4,...
```
## Part II: Define Server
```
shinyjs::toggleState
reacitve({}): updateSelectInput, updateCheckboxGroupInput,...
reacitve({}): filter
reacitve({}): survfit() # Kaplan-Meier Estimate
output: renderPlot (ggplot, ggsurvplot), renderText, renderPrint, renderTable,...
```

## Part III: Return a Shiny app object
```
shinyApp(ui = ui, server = server)
```



