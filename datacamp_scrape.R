library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)


url <- 'https://www.datacamp.com/community/tutorials'


get_page_count <- function(html){
  page_numbers <- html %>% html_nodes('.Paginator') %>%
    html_nodes('ul') %>%
    html_nodes('li') %>%
    html_text()
  
  last_page_number <- tail(page_numbers, n =1)
}


get_tutorial_name <- function(html){
  html %>%
    html_nodes('.Tutorials') %>%
    html_nodes('h2') %>%
    html_nodes('a') %>%
    html_text()
}

get_tutorial_description <- function(html){
  html %>%
    html_nodes('.description') %>%
    html_text()
}

get_tutorial_author_name <- function(html){
  html %>%
    html_nodes('.name') %>%
    html_text()
}

get_tutorial_published_date <- function(html){
  html %>%
    html_nodes('.date') %>%
    html_text()
}

get_tutorial_topic <- function(html){
  html %>%
    html_nodes('.Tag:not(.mustRead)') %>%
    html_text()
}

get_upvote_count <- function(html){
  html %>%
    html_nodes('.count') %>%
    html_text() %>%
    str_trim() %>%
    unlist()
}


get_data_table <- function(html){
  tutorials <- get_tutorial_name(html)
  description <- get_tutorial_description(html)
  author_name <- get_tutorial_author_name(html)
  published_date <- get_tutorial_published_date(html)
  topic <- get_tutorial_topic(html)
  upvote_count <- get_upvote_count(html)
  combined_data  <- data.frame(tutorial_names = tutorials,
                           tutorial_description = description,
                           tutorial_author_name = author_name,
                           tutorial_published_date = published_date,
                           tutorial_topic = topic,
                           tutorial_upvotes = upvote_count
                           )
}

get_data_from_url <- function(url){
  html <- read_html(url)
  get_data_table(html)
}

scrape_write_table <- function(url){
  page <- read_html(url)
  
  page_count <- get_page_count(page)
  
  list_of_pages <- str_c(url, '?page=', 1:last_page_number)
  
  list_of_pages %>%
    map(get_data_from_url) %>%
    bind_rows()
}


datacamp_scrape <- scrape_write_table(url)

datacamp_scrape$tutorial_upvotes <- as.numeric(datacamp_scrape$tutorial_upvotes)
datacamp_scrape$tutorial_topic <- as.factor(datacamp_scrape$tutorial_topic)
datacamp_scrape$tutorial_published_date <- mdy(datacamp_scrape$tutorial_published_date)