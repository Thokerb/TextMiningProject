require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(ggplot2)
require(tidyverse)
library("lubridate")

comments_2018 <- readRDS("comments2018.rds")
comments_2019 <- readRDS("comments2019.rds")
comments_2020 <- readRDS("comments2020.rds")
comments_2021 <- readRDS("comments2021.rds")
topPosts_2018 <- readRDS("topPostsEvery2Weeks2018.rds")
topPosts_2019 <- readRDS("topPostsEvery2Weeks2019.rds")
topPosts_2020 <- readRDS("topPostsEvery2Weeks2020.rds")
topPosts_2021 <- readRDS("topPostsEvery2Weeks2021.rds")
#Convert created_utc to datetime
if(typeof(comments_2018$created_utc) == 'double'){comments_2018$created_utc <- as_datetime(comments_2018$created_utc)}
if(typeof(comments_2019$created_utc) == 'double'){comments_2019$created_utc <- as_datetime(comments_2019$created_utc)}
if(typeof(comments_2020$created_utc) == 'double'){comments_2020$created_utc <- as_datetime(comments_2020$created_utc)}
if(typeof(comments_2021$created_utc) == 'double'){comments_2021$created_utc <- as_datetime(comments_2021$created_utc)}
if(typeof(topPosts_2018$created_utc) == 'character'){topPosts_2018$created_utc <- as_datetime(strtoi(topPosts_2018$created_utc))}
if(typeof(topPosts_2019$created_utc) == 'character'){topPosts_2019$created_utc <- as_datetime(strtoi(topPosts_2019$created_utc))}
if(typeof(topPosts_2020$created_utc) == 'character'){topPosts_2020$created_utc <- as_datetime(strtoi(topPosts_2020$created_utc))}
if(typeof(topPosts_2021$created_utc) == 'character'){topPosts_2021$created_utc <- as_datetime(strtoi(topPosts_2021$created_utc))}


corp_comments_2018 <- corpus(comments_2018, text_field='body')
corp_comments_2019 <- corpus(comments_2019, text_field='body')
corp_comments_2020 <- corpus(comments_2020, text_field='body')
corp_comments_2021 <- corpus(comments_2021, text_field='body')
corp_topPosts_2018 <- corpus(topPosts_2018, text_field ='title')
corp_topPosts_2019 <- corpus(topPosts_2019, text_field ='title')
corp_topPosts_2020 <- corpus(topPosts_2020, text_field ='title')
corp_topPosts_2021 <- corpus(topPosts_2021, text_field ='title')

#Create DFM 
dfm_comments_2018 <- corp_comments_2018 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 100)

dfm_comments_2019 <- corp_comments_2019 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 100)

dfm_comments_2020 <- corp_comments_2020 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 100)

dfm_comments_2021 <- corp_comments_2021 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 100)

dfm_topPosts_2018 <- corp_topPosts_2018 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 5)

dfm_topPosts_2019 <- corp_topPosts_2019 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 5)

dfm_topPosts_2020 <- corp_topPosts_2020 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 5)

dfm_topPosts_2021 <- corp_topPosts_2021 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 5)

#Frequency of words in comments
freq_comments_2018 <- textstat_frequency(dfm_comments_2018)
freq_comments_2019 <- textstat_frequency(dfm_comments_2019)
freq_comments_2020 <- textstat_frequency(dfm_comments_2020)
freq_comments_2021 <- textstat_frequency(dfm_comments_2021)
freq_topPosts_2018 <- textstat_frequency(dfm_topPosts_2018)
freq_topPosts_2019 <- textstat_frequency(dfm_topPosts_2019)
freq_topPosts_2020 <- textstat_frequency(dfm_topPosts_2020)
freq_topPosts_2021 <- textstat_frequency(dfm_topPosts_2021)

comparison_freq <- data.frame(
  comments_2018=head(freq_comments_2018$feature, 50), 
  comments_2019=head(freq_comments_2019$feature, 50),
  comments_2020=head(freq_comments_2020$feature, 50),
  comments_2021=head(freq_comments_2021$feature, 50),
  topPosts_2018=head(freq_topPosts_2018$feature, 50),
  topPosts_2019=head(freq_topPosts_2019$feature, 50),
  topPosts_2020=head(freq_topPosts_2020$feature, 50),
  topPosts_2021=head(freq_topPosts_2021$feature, 50))

write.csv(comparison_freq, "Output/most_frequent_words.csv")

#wordcloud
textplot_wordcloud(dfm_comments_2018)
textplot_wordcloud(dfm_comments_2019)
textplot_wordcloud(dfm_comments_2020)
textplot_wordcloud(dfm_comments_2021)

textplot_wordcloud(dfm_topPosts_2018)
textplot_wordcloud(dfm_topPosts_2019)
textplot_wordcloud(dfm_topPosts_2020)
textplot_wordcloud(dfm_topPosts_2021)

#------------------
#Sentiment Analysis
#------------------

ids_01_2019 <- filter(topPosts, created_utc >= '2019-01-01' & created_utc <= '2019-03-31')$id
ids_02_2019 <- filter(topPosts, created_utc >= '2019-04-01' & created_utc <= '2019-06-30')$id
ids_03_2019 <- filter(topPosts, created_utc >= '2019-07-01' & created_utc <= '2019-09-30')$id
ids_04_2019 <- filter(topPosts, created_utc >= '2019-10-01' & created_utc <= '2019-12-31')$id
ids_01_2020 <- filter(topPosts, created_utc >= '2020-01-01' & created_utc <= '2020-03-31')$id
ids_02_2020 <- filter(topPosts, created_utc >= '2020-04-01' & created_utc <= '2020-06-30')$id
ids_03_2020 <- filter(topPosts, created_utc >= '2020-07-01' & created_utc <= '2020-09-30')$id
ids_04_2020 <- filter(topPosts, created_utc >= '2020-10-01' & created_utc <= '2020-12-31')$id

comments_01_2019 <- filter(comments_2019, id %in% ids_01_2019)


