---
title: "Our Shiny App"
output: 
  html_document: 
    keep_md: yes
---

This Shiny App shows how amounts of COVID-19 cases have changed from the beginning of the pandemic to more recently.

----



This is the code we used:

```r
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
```

```
## The following object is masked from 'package:graphics':
## 
##     box
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
options(scipen = 999)

covid <- read.csv("Data/WHO-COVID-19-global-data.csv") %>% clean_names()

covid$date_reported <- lubridate::mdy(covid$date_reported)

covid <- covid %>% mutate_if(is.integer,as.numeric)

covid <- covid %>% 
  select(date_reported, country, cumulative_cases, cumulative_deaths)

ui <- dashboardPage(skin="blue",
                    dashboardHeader(title = "COVID-19 Information"),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                      fluidRow(
                        box(title = "Plot Options", width = 3,
                            selectInput("fill", "Select Country", choices =unique(covid$country)),
                            radioButtons("x", "Select X Variable", choices = c("cumulative_cases", "cumulative_deaths"), selected = "cumulative_cases"),
                            box(title = "Disease Abundance", width = 9,
                                plotOutput("plot", width = "800px", height = "500px", click="plot_click"),
                                verbatimTextOutput("info")
                            ) 
                        ) 
                      )
                    ) 
)

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    covid %>% 
      filter(country==input$fill) %>% 
      ggplot(aes_string(x = "date_reported", y = input$x, fill=input$x)) + 
      geom_col()+
      labs(x="Date reported")
  })
}

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}

