##### Load library ########
library(readr)
library(DT)
library(magrittr) 
library(ggplot2)
library(pdp)
library(shiny)
library(XML)
library(jsonlite)
library(tibble)
library(tidytext)

library(data.table)
library(dplyr) # For `filter_all` and `mutate_all`.
library(Hmisc)
library(quanteda)
library(stringr)

library(hcandersenr)
library(tidyverse)
library(tidytext)

library(SnowballC) # For wordStem

library(dynprog) # Dynamic Programming
library(Biostrings) # Dynamic Programming (pairwiseAlignment)

##### UI ########
ui = tagList(
  # https://stackoverflow.com/questions/57037758/r-shiny-how-to-color-margin-of-title-panel
  titlePanel(h1("IR Project 2",
                style='background-color:#ece4db;  
                     color:#474973;
                     font-weight: 500;
                     font-family: Arial Black;
                     line-height: 1.2;
                     padding-left: 15px')), 
  fluidPage(
  sidebarLayout(
    position = "right",
    sidebarPanel(
      fileInput("file1", "Choose XML Files", accept = ".xml", multiple = T),
      fileInput("file2", "Choose JSON File", accept = ".json", multiple = T),
      actionButton("SearchKW", "Go"),
      textInput("word_select", label = "Word to search")
      
    ),
    
    mainPanel(
      tableOutput("SumTable"),
      plotOutput("HisFig"))
  ),
  fluidRow(
    dataTableOutput("table")
  )
)
)