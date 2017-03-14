
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

## TODO:
# Number of tweest before and after becoming president
# Programmatically include original tweets using
# Timeline with external events
# Use bsplus carousel
# Turn into dashboard?
# Chunk the Twitter query and update DT
# merge with github repo

shinyUI(fluidPage(

  # Application title
  titlePanel("Trump Tweet Sentiment Analysis"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of tweets to analyze (max 3200)", 100, min = 1, max = 3200)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        # tabPanel("Tweets", formattableOutput("tweet_table"), icon = icon("twitter")) ,
        tabPanel("Tweets", DT::dataTableOutput("tweet_table"), icon = icon("twitter")) ,
        tabPanel("Originator", plotly::plotlyOutput("orig", width="100%"), icon = icon("bar-chart")) ,
        tabPanel("Sentiment", radarchart::chartJSRadarOutput("sentiment_radar", width = "450", height = "300"), icon = icon("thermometer-full"))
      )

    )
  )
))
