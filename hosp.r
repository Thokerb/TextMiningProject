require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
require(ggplot2)
library(dplyr)



topPosts_file <- "data/topPostsEvery2Weeks2021.RDS"
comments_file <- "data/comments2021.RDS"

#words to be matched
words <- "vaccine|Vaccine"

#load data
topPosts_rds <- readRDS(topPosts_file)
comments_rds <- readRDS(comments_file)

#comments that include a specific word
matched_entries <- comments_rds %>%
  filter(grepl(words, body))

#tokenize data; remove stopwords
toks_topPosts <- tokens(corpus(topPosts_rds, text_field= "title"), remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords("en"))

toks_comments <- tokens(corpus(comments_rds, text_field = "body"), remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords("en"))

toks_matchedwords_comments <- tokens(corpus(matched_entries, text_field = "body"), remove_punct = TRUE) %>%
  tokens_remove(pattern = stopwords("en"))

#Sentiment analyses
dfmat_lsd_topPosts <- dfm(tokens_lookup(toks_topPosts, dictionary = data_dictionary_LSD2015))
topPosts_output <- convert(dfmat_lsd_topPosts, to = "data.frame")
topPosts_output$sent_score <- log((topPosts_output$positive + topPosts_output$neg_negative + 0.5) / (topPosts_output$negative + topPosts_output$neg_positive + 0.5))

dfmat_lsd_comments <- dfm(tokens_lookup(toks_comments, dictionary = data_dictionary_LSD2015))
comments_output <- convert(dfmat_lsd_comments, to = "data.frame")
comments_output$sent_score <- log((comments_output$positive + comments_output$neg_negative + 0.5) / (comments_output$negative + comments_output$neg_positive + 0.5))

dfmat_lsd_comments_matched <- dfm(tokens_lookup(toks_matchedwords_comments, dictionary = data_dictionary_LSD2015))
comments_matched_output <- convert(dfmat_lsd_comments_matched, to = "data.frame")
comments_matched_output$sent_score <- log((comments_matched_output$positive + comments_matched_output$neg_negative + 0.5) / (comments_matched_output$negative + comments_matched_output$neg_positive + 0.5))

#Comparing the means
mean(topPosts_output$sent_score)
mean(comments_output$sent_score)
mean(comments_matched_output$sent_score)
