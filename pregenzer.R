require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(quanteda.corpora)
require(ggplot2)
library(dplyr)

setwd("/Users/thomas/Documents/Thomas/Uni/5_Semester/Text-Mining/TextMiningProject")

topPosts_file <- "data/topPostsEvery2Weeks2021.RDS"
topPosts_rds <- readRDS(topPosts_file)

test <- topPosts_rds %>%
  group_by(domain) %>%
  summarise(full_text = paste(title, collapse = " ; ")) %>%
  filter(nchar(full_text) >= 250)

test2 <- data.frame(test)

corp <- corpus(test2[, 2])
toks <- tokens(corp, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))

dfm_test <- dfm(tokens_lookup(toks, dictionary = data_dictionary_LSD2015))
output <- convert(dfm_test, to = "data.frame")

output_data <- output %>%
  group_by(sen_score = log((positive + neg_negative + 0.5) / (negative + neg_positive + 0.5)), domain = test$domain)

ggplot(output_data, aes(x=sen_score, y=domain)) +
  geom_bar(stat = "identity")
