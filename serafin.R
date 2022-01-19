require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(ggplot2)
require(tidyverse)
library("lubridate")
require(gridExtra)

get_data <- function(year) {
  
  #Import Comments and Top Posts
  comments <- readRDS(paste("data/comments", year, ".rds", sep=""))
  topPosts <- readRDS(paste("data/topPostsEvery2Weeks", year, ".rds", sep = ""))
  
  #Convert date to datetime format
  if(typeof(comments$created_utc) == 'double'){comments$created_utc <- as_datetime(comments$created_utc)}
  if(typeof(topPosts$created_utc) == 'character'){topPosts$created_utc <- as_datetime(strtoi(topPosts$created_utc))}
  
  #generate corpus
  corp_comments <- corpus(comments, text_field='body')
  corp_topPosts <- corpus(topPosts, text_field ='title')
  
  #create dfm
  dfm_comments <- corp_comments %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
    dfm() %>% 
    dfm_remove(pattern = stopwords("en")) %>%
    dfm_trim(min_termfreq = 100)
  
  dfm_topPosts <- corp_topPosts %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>% 
    dfm() %>% 
    dfm_remove(pattern = stopwords("en")) %>%
    dfm_trim(min_termfreq = 5)
  
  return(list("comments" = dfm_comments, "topPosts" = dfm_topPosts))
}



news2013 <- get_data(2013)
news2014 <- get_data(2014)
news2015 <- get_data(2015)
news2016 <- get_data(2016)
news2017 <- get_data(2017)
news2018 <- get_data(2018)
news2019 <- get_data(2019)
news2020 <- get_data(2020)
news2021 <- get_data(2021)

#Frequency of words in comments
freq_comments_2013 <- textstat_frequency(news2013$comments)
freq_comments_2014 <- textstat_frequency(news2014$comments)
freq_comments_2015 <- textstat_frequency(news2015$comments)
freq_comments_2016 <- textstat_frequency(news2016$comments)
freq_comments_2017 <- textstat_frequency(news2017$comments)
freq_comments_2018 <- textstat_frequency(news2018$comments)
freq_comments_2019 <- textstat_frequency(news2019$comments)
freq_comments_2020 <- textstat_frequency(news2020$comments)
freq_comments_2021 <- textstat_frequency(news2021$comments)

freq_topPosts_2013 <- textstat_frequency(news2013$topPosts)
freq_topPosts_2014 <- textstat_frequency(news2014$topPosts)
freq_topPosts_2015 <- textstat_frequency(news2015$topPosts)
freq_topPosts_2016 <- textstat_frequency(news2016$topPosts)
freq_topPosts_2017 <- textstat_frequency(news2017$topPosts)
freq_topPosts_2018 <- textstat_frequency(news2018$topPosts)
freq_topPosts_2019 <- textstat_frequency(news2019$topPosts)
freq_topPosts_2020 <- textstat_frequency(news2020$topPosts)
freq_topPosts_2021 <- textstat_frequency(news2021$topPosts)


comparison_freq <- data.frame(
  comments_2013=head(freq_comments_2013$feature, 50),
  comments_2014=head(freq_comments_2014$feature, 50),
  comments_2015=head(freq_comments_2015$feature, 50),
  comments_2016=head(freq_comments_2016$feature, 50),
  comments_2017=head(freq_comments_2017$feature, 50),
  comments_2018=head(freq_comments_2018$feature, 50), 
  comments_2019=head(freq_comments_2019$feature, 50),
  comments_2020=head(freq_comments_2020$feature, 50),
  comments_2021=head(freq_comments_2021$feature, 50),
  topPosts_2013=head(freq_topPosts_2013$feature, 50),
  topPosts_2014=head(freq_topPosts_2014$feature, 50),
  topPosts_2015=head(freq_topPosts_2015$feature, 50),
  topPosts_2016=head(freq_topPosts_2016$feature, 50),
  topPosts_2017=head(freq_topPosts_2017$feature, 50),
  topPosts_2018=head(freq_topPosts_2018$feature, 50),
  topPosts_2019=head(freq_topPosts_2019$feature, 50),
  topPosts_2020=head(freq_topPosts_2020$feature, 50),
  topPosts_2021=head(freq_topPosts_2021$feature, 50))

write.csv(comparison_freq, "Output/most_frequent_words.csv")

#wordcloud
textplot_wordcloud(news2013$comments)
textplot_wordcloud(news2014$comments)
textplot_wordcloud(news2015$comments)
textplot_wordcloud(news2016$comments)
textplot_wordcloud(news2017$comments)
textplot_wordcloud(news2018$comments)
textplot_wordcloud(news2019$comments)
textplot_wordcloud(news2020$comments)
textplot_wordcloud(news2021$comments)


textplot_wordcloud(news2013$topPosts)
textplot_wordcloud(news2014$topPosts)
textplot_wordcloud(news2015$topPosts)
textplot_wordcloud(news2016$topPosts)
textplot_wordcloud(news2017$topPosts)
textplot_wordcloud(news2018$topPosts)
textplot_wordcloud(news2019$topPosts)
textplot_wordcloud(news2020$topPosts)
textplot_wordcloud(news2021$topPosts)


##Upvotes-Sentiment

plot_score <- function(year) {
  comments <- readRDS(paste("data/comments", year, ".rds", sep = ""))
  topPosts <- readRDS(paste("data/topPostsEvery2Weeks", year, ".rds", sep = ""))
  corp_comments <- corpus(comments, text_field = "body")
  corp_topPosts <- corpus(topPosts, text_field = "title")
  tok_comments <- tokens(corp_comments, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>%
    tokens_remove(pattern = stopwords("en"))
  tok_topPosts <- tokens(corp_topPosts, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>%
    tokens_remove(pattern = stopwords("en"))
  
  dfm_lsd_comments <- dfm(tokens_lookup(tok_comments, dictionary = data_dictionary_LSD2015))
  dfm_lsd_topPosts <- dfm(tokens_lookup(tok_topPosts, dictionary = data_dictionary_LSD2015))
  output_comments <- convert(dfm_lsd_comments, to="data.frame")
  output_comments$sent_score <- log((output_comments$positive + output_comments$neg_negative + 0.5) / (output_comments$negative + output_comments$neg_positive + 0.5))
  output_topPosts <- convert(dfm_lsd_topPosts, to="data.frame")
  output_topPosts$sent_score <- log((output_topPosts$positive + output_topPosts$neg_negative + 0.5) / (output_topPosts$negative + output_topPosts$neg_positive + 0.5))
  
  comparison_comments <- data.frame("upvotes"=comments$ups, "sent_score" = output_comments$sent_score)
  plot_comments <- ggplot(comparison_comments, aes(upvotes, sent_score)) + geom_line(color="darkblue") + ggtitle(paste("Comments", year))
  comparison_topPosts <- data.frame("post_score"=strtoi(topPosts$score), "sent_score" = output_topPosts$sent_score)
  plot_topPosts <- ggplot(comparison_topPosts, aes(post_score, sent_score)) + geom_line(color="darkblue") + ggtitle(paste("Top Post", year))
  
  grid.arrange(plot_comments, plot_topPosts, nrow=2)
}


plot_score(2015)
plot_score(2020)
