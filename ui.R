library(shiny)
library(shinythemes)
library(knitr)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(RCurl)
library(tidyverse)

library(base)
library(datasets)
library(dplyr)
library(forcats)
library(graphics)
library(grDevices)
library(methods)
library(purrr)
library(readr)
library(stats)
library(stringr)
library(tibble)
library(tidyr)
library(utils)

fluidPage(theme = shinytheme("flatly"),
                  tagList(tags$head(
                            HTML(
                              "
                              <script>
                              var socket_timeout_interval
                              var n = 0
                              $(document).on('shiny:connected', function(event) {
                              socket_timeout_interval = setInterval(function(){
                              Shiny.onInputChange('count', n++)
                              }, 20000)
                              });
                              $(document).on('shiny:disconnected', function(event) {
                              clearInterval(socket_timeout_interval)
                              });
                              </script>
                              "
                            )
                            ),
                    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(1) {
                           float: right;
                           }
                           .navbar-nav > li:nth-child(2) {
                           float: right;
                           }
                           .navbar-nav > li:nth-child(3) {
                           float: right;
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           }
                           ")))
                    , 
                    
                    navbarPage("Personality Prediction System", id = "inTabset", 
                               
                               tabPanel("About", 
                                        div(includeMarkdown("about_page.Rmd"), align = "justify"), 
                                        HTML('<hr style="color: black;">'), 
                                        textOutput("keepAlive"), 
                                        tags$h6("This application was designed and developed by Kulwant Singh Dhama, in 2020. For any query or suggestion, contact at dhamaks@gmail.com", 
                                                align = "center")
                               ),
                               
                               tabPanel("Result", 
                                        mainPanel(
                                            tags$h1("Result", align = "center"),
                                            verbatimTextOutput('contents'),
                                            
                                            plotOutput(outputId = "distPlot"), 
                                            
                                            tags$label(h3("Openness")), 
                                            textOutput("intro_openness"),
                                            textOutput("openness"), 
                                            
                                            tags$label(h3("Conscientiousness")), 
                                            textOutput("intro_conscientiousness"),
                                            textOutput("conscientiousness"),
                                            
                                            tags$label(h3("Extraversion")), 
                                            textOutput("intro_extraversion"),
                                            textOutput("extraversion"), 
                                            
                                            tags$label(h3("Agreeableness")), 
                                            textOutput("intro_agreeableness"),
                                            textOutput("agreeableness"), 
                                            
                                            tags$label(h3("Neuroticism")), 
                                            textOutput("intro_neuroticism"),
                                            textOutput("neuroticism")
                                        )
                               ),
                               
                               tabPanel("Test", 
                                        
                                        radioButtons("NEU1", label = h4(strong("Question 1 -     I get stressed out easily.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR1", label = h4(strong("Question 2 -     I feel little concern for others.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT1", label = h4(strong("Question 3 -     I am the most lively person at a party.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN1", label = h4(strong("Question 4 -     I am always prepared.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN1", label = h4(strong("Question 5 -     I have a good vocabulary.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU2", label = h4(strong("Question 6 -     I am relaxed most of the time.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR2", label = h4(strong("Question 7 -     I am interested in people.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT2", label = h4(strong("Question 8 -     I don't talk a lot.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN2", label = h4(strong("Question 9 -     I leave my belongings around.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN2", label = h4(strong("Question 10 -     I have difficulty understanding abstract ideas.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU3", label = h4(strong("Question 11 -     I worry about things.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR3", label = h4(strong("Question 12 -     I insult people.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT3", label = h4(strong("Question 13 -     I feel comfortable around people.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN3", label = h4(strong("Question 14 -     I pay attention to details.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN3", label = h4(strong("Question 15 -     I have a vivid imagination.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU4", label = h4(strong("Question 16 -     I seldom feel sad.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR4", label = h4(strong("Question 17 -     I sympathize with others' feelings.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT4", label = h4(strong("Question 18 -     I am mostly unnoticed.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN4", label = h4(strong("Question 19 -     I make a mess of things.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN4", label = h4(strong("Question 20 -     I am not interested in abstract ideas.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU5", label = h4(strong("Question 21 -     I am easily disturbed.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR5", label = h4(strong("Question 22 -     I am not interested in other people's problems.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT5", label = h4(strong("Question 23 -     I start conversations.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN5", label = h4(strong("Question 24 -     I get chores done right away.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN5", label = h4(strong("Question 25 -     I have excellent ideas.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU6", label = h4(strong("Question 26 -     I get upset easily.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR6", label = h4(strong("Question 27 -     I am a soft hearted person.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT6", label = h4(strong("Question 28 -     I have little to say.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN6", label = h4(strong("Question 29 -     I often forget to put things back in their proper place.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN6", label = h4(strong("Question 30 -     I do not have a good imagination.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU7", label = h4(strong("Question 31 -     I change my mood a lot.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR7", label = h4(strong("Question 32 -     I am not really interested in others.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT7", label = h4(strong("Question 33 -     I talk to a lot of different people at parties.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN7", label = h4(strong("Question 34 -     I like order.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN7", label = h4(strong("Question 35 -     I am quick to understand things.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU8", label = h4(strong("Question 36 -     I have frequent mood swings.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR8", label = h4(strong("Question 37 -     I take time out for others.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT8", label = h4(strong("Question 38 -     I don't like to draw attention to myself.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN8", label = h4(strong("Question 39 -     I avoid my duties.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN8", label = h4(strong("Question 40 -     I use difficult words.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU9", label = h4(strong("Question 41 -     I get irritated easily.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR9", label = h4(strong("Question 42 -     I feel others' emotions.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT9", label = h4(strong("Question 43 -     I don't mind being the center of attention.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN9", label = h4(strong("Question 44 -     I follow a schedule.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN9", label = h4(strong("Question 45 -     I spend time reflecting on things.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("NEU10", label = h4(strong("Question 46 -     I often feel sad.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("AGR10", label = h4(strong("Question 47 -     I make people feel at ease.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("EXT10", label = h4(strong("Question 48 -     I am quiet around strangers.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("CSN10", label = h4(strong("Question 49 -     I am very strict and specific about my work.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        radioButtons("OPN10", label = h4(strong("Question 50 -     I am full of ideas.")), 
                                                     c("Strongly Disagree" = "SD", 
                                                       "Disagree" = "D", 
                                                       "Neither Disagree nor Agree" = "NDNA", 
                                                       "Agree" = "A", 
                                                       "Strongly Agree" = "SA", 
                                                       "Prefer not to answer" = "NS"), 
                                                     selected = "NS" , 
                                                     inline = TRUE
                                        ),
                                        
                                        actionButton("submit_test", 
                                                     label = "Submit", 
                                                     class = "btn btn-primary")
                               ),
                               
                               tabPanel("Home",
                                        div(includeMarkdown("home_page.Rmd"), align = "justify"),
                                        
                                        actionButton("start_test", 
                                                     label = "Start", 
                                                     class = "btn btn-primary")
                               )
                    )
            )
)
