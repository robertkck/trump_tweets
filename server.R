
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(purrr)
library(twitteR)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(lubridate)
# library(formattable)
library(DT)
library(shiny)
library(htmlwidgets)
library(tidyr)
library(tidytext)
library(radarchart)
library(stringr)
theme_set(theme_bw())

setup_twitter_oauth("c3hqu6uxDtmTd9egeFUpnNvOJ","GCBuVgj3TfLYZLP4e1QQ9dyBpUNBl55ld5Q3xHn4DHjQElPibP",
                    "3929637676-5hkaaSKJpMl48TjboJrz542y7fJfjHQGtkFegAN",
                    "sbUXeS18yX2LiacH90GTzbytjDYvVUZRRgswEUAAElBdL")
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

shinyServer(function(input, output) {

  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
  badness <- c("anger", "disgust", "fear", "negative", "sadness", "surprise" , "trust", "anticipation", "positive", "joy")

  tweets <- reactive({
    n <- input$n

    trump_tweets <- userTimeline("realDonaldTrump", n)
    trump_tweets_df <- tbl_df(map_df(trump_tweets, as.data.frame))

    trump_tweets_df %>%
      select(id, statusSource, text, created, favoriteCount, retweetCount) %>%
      tidyr::extract(statusSource, "source", "Twitter for (.*?)<") %>%
      filter(source %in% c("iPhone", "Android")) %>%
      mutate(originator = ifelse(source == "iPhone", "Staff", "Trump"))

  })

  observeEvent(input$n, {
    if (input$n >= 1000) {
      showNotification(showNotification(paste("Time to grab a coffee. This might take a bit."), type = "warning", duration = 2))
    }
  })

  # output$tweet_table <- renderFormattable({
  #   tweets_ftable <- tweets() %>%
  #     mutate(originator = ifelse(source == "iPhone", "Staff", "Trump")) %>%
  #     select(text, originator)
  #
  #   formattable(tweets_ftable)
  #   })
  observe({
    tweets_table <- tweets() %>%
      select(text, originator, favoriteCount, retweetCount)

    output$tweet_table <- DT::renderDataTable(
      datatable({
            tweets_table
        },
        # rownames = FALSE ,
        colnames = c("Tweet", "Originator", "Likes", "Retweets"),
        options = list(paging = FALSE)
      )
    )
  })

  output$orig <- renderPlotly({
    p <- tweets() %>%
      count(originator, day = as_date(created)) %>% # yday(with_tz(created, "EST"))
      ggplot(aes(day, n, fill = originator)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = '', y= "number of tweets", fill = "") +
      ggtitle("How many tweets per day")
    ggplotly(p)
  })


  output$sentiment_radar <- renderChartJSRadar({
    tweet_words <- tweets() %>%
      filter(!str_detect(text, '^"')) %>%
      mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
      unnest_tokens(word, text, token = "regex", pattern = reg) %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "[a-z]"))

    sources <- tweet_words %>%
      group_by(originator) %>%
      mutate(total_words = n()) %>%
      ungroup() %>%
      distinct(id, originator, total_words)

    radar <- tweet_words %>%
      inner_join(nrc, by = "word") %>%
      count(sentiment, id) %>%
      ungroup() %>%
      complete(sentiment, id, fill = list(n = 0)) %>%
      inner_join(sources) %>%
      group_by(originator, sentiment, total_words) %>%
      summarize(words = sum(n)) %>%
      ungroup() %>%
      mutate(prop = words / total_words)%>%
      # mutate(originator = ifelse(source == "iPhone", "Staff", "Trump")) %>%
      select(sentiment, originator, prop) %>%
      spread( originator, prop) %>%
      slice(match(badness, sentiment))

    radarchart::chartJSRadar(radar)
  })

})
