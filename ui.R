library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(shinyjs)


shinyUI(dashboardPage(skin = "green",
                      dashboardHeader(title = "RadialPheno", dropdownMenuOutput("messageMenu"), 
                                      dropdownMenuOutput("taskMenu"),
                                      tags$li(class = "dropdown",
                                              tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
                                              tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
                      ),
                      dashboardSidebar(disable=T,sidebarMenuOutput("menu")),
                      dashboardBody(
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "appRadial.css")),
                        
                        fluidPage(
                          box(width=2, 
                              # App title ----
                              titlePanel("Upload File options"),
                              
                              
                              # Input: Checkbox if file has header ----
                              checkboxInput("header", "Header", TRUE),
                              
                              # Input: Select separator ----
                              radioButtons("sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              
                              # Input: Select quotes ----
                              radioButtons("quote", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = '"'),
                              
                              
                              # Input: Select number of rows to display ----
                              radioButtons("disp", "Display",
                                           choices = c(Head = "head",
                                                       All = "all"),
                                           selected = "head"),
                              
                              # Input: Select a file ----
                              fileInput("file1", "Choose CSV File",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              # Dynamic filters
                              uiOutput("controllers")
                              
                          ),
                          useShinyjs(),     
                          box(width=10, 
                              hidden(
                                #controllers plot
                                actionButton("previousDrawID", "<<"),
                                actionButton("nextDrawID", ">>"),
                                actionButton("savePdfID", "SAVE PDF"),
                                actionButton("saveImgID", "SAVE IMG"),
                                actionButton("convertYearsID", "Change Years")
                              ),
                              plotOutput("contentsPlot", height = 800, click = "plot_click")),
                          
                          hidden(
                            div(id="boxDataTable", 
                                box(width = 6, 
                                    titlePanel("Data selected"), 
                                    dataTableOutput("tableData")
                                )
                            )
                          )
                        )
                        
                      )
))