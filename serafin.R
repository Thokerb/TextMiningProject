require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
require(ggplot2)

comments_2019 <- readRDS("comments2019.rds")
topPosts <- readRDS("topPostsEvery2Weeks2019.rds")

corp_comments_2019 <- corpus(comments_2019, text_field='body')

#Create DFM 
dfm_comments_2019 <- corp_comments_2019 %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim

#Frequency of words in comments
freq_comments <- textstat_frequency(dfm_comments_2019)
head(freq_comments, 20)

#wordcloud
textplot_wordcloud(dfm_comments_2019)

