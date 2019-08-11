#Load the necessary packages

suppressMessages(library(stringi))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(ggplot2))
suppressMessages(library(tm))
suppressMessages(library(quanteda))

#Loan the raw data

blogs_data<-readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news_data<-readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
twitter_data<-readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

#Due to the size of the raw data, we will sample 10% of each file.

set.seed(3878)
blogs_data<-sample(blogs_data, size = length(blogs_data) * 0.1)
news_data<-sample(news_data, size = length(news_data) * 0.1)
twitter_data<-sample(twitter_data, size = length(twitter_data) * 0.1)

#Combine the data and create a corpus.

total_data<-c(blogs_data, news_data, twitter_data)
corp_data<-corpus(total_data)

#Tokenize, clean, and stem the data.

unigramTokens<-tokens(corp_data,
                      what = "word",
                      remove_url = TRUE,
                      remove_punct = TRUE,
                      remove_numbers = TRUE,
                      remove_twitter = TRUE)
unigramTokens<-tokens_tolower(unigramTokens)

unigramStems<-tokens_wordstem(unigramTokens, language = "en")

dataTokens<-tokens(corp_data,
                   what = "word",
                   remove_url = TRUE,
                   remove_punct = TRUE,
                   remove_numbers = TRUE,
                   remove_twitter = TRUE)
dataTokens<-tokens_remove(dataTokens, pattern = stopwords("en"))
dataTokens<-tokens_tolower(dataTokens)

stemWords<-tokens_wordstem(dataTokens, language = "en")

#Create ngrams from the data. Unigrams, bigrams, trigrams, quadgrams, and quintgrams will be
#used to create the predictive model. Create a document feature matrix from the ngrams.

bi_gram<-tokens_ngrams(stemWords, n = 2)
tri_gram<-tokens_ngrams(stemWords, n = 3)
quad_gram<-tokens_ngrams(stemWords, n = 4)
quint_gram<-tokens_ngrams(stemWords, n = 5)

unigramMat<-dfm(unigramStems)
bigramMat<-dfm(bi_gram)
trigramMat<-dfm(tri_gram)
quadgramMat<-dfm(quad_gram)
quintgramMat<-dfm(quint_gram)

unigramMat<-dfm_trim(unigramMat, 3)
bigramMat<-dfm_trim(bigramMat, 3)
trigramMat<-dfm_trim(trigramMat, 3)
quadgramMat<-dfm_trim(quadgramMat, 3)
quintgramMat<-dfm_trim(quintgramMat, 3)

#Create the various ngram dataframes. Save the seperate dataframes as seperate files to load
#into the model.

unigramTop<-topfeatures(unigramMat, n = 40000)
unigramDF<-data.frame(w1 = names(unigramTop), freq = unigramTop)
unigramDF %>% mutate_if(is.factor, as.character) -> unigramDF
saveRDS(unigramDF, file = "shiny/find_next_word/data/unigram.rds")

bigramTop<-topfeatures(bigramMat, n = 40000)
bigramDF<-data.frame(words = names(bigramTop), freq = bigramTop)
bigramDF %>% mutate_if(is.factor, as.character) -> bigramDF
bigramDF$w1<-word(bigramDF$words, 1, sep = fixed("_"))
bigramDF$w2<-word(bigramDF$words, 2, sep = fixed("_"))
bigramDF<-subset(bigramDF, select = -c(words))
bigramDF<-bigramDF[c("w1", "w2", "freq")]
saveRDS(bigramDF, file = "shiny/find_next_word/data/bigram.rds")

trigramTop<-topfeatures(trigramMat, n = 40000)
trigramDF<-data.frame(words = names(trigramTop), freq = trigramTop)
trigramDF %>% mutate_if(is.factor, as.character) -> trigramDF
trigramDF$w1<-word(trigramDF$words, 1, sep = fixed("_"))
trigramDF$w2<-word(trigramDF$words, 2, sep = fixed("_"))
trigramDF$w3<-word(trigramDF$words, 3, sep = fixed("_"))
trigramDF<-subset(trigramDF, select = -c(words))
trigramDF<-trigramDF[c("w1", "w2", "w3", "freq")]
saveRDS(trigramDF, file = "shiny/find_next_word/data/trigram.rds")

quadgramTop<-topfeatures(quadgramMat, n = 40000)
quadgramDF<-data.frame(words = names(quadgramTop), freq = quadgramTop)
quadgramDF %>% mutate_if(is.factor, as.character) -> quadgramDF
quadgramDF$w1<-word(quadgramDF$words, 1, sep = fixed("_"))
quadgramDF$w2<-word(quadgramDF$words, 2, sep = fixed("_"))
quadgramDF$w3<-word(quadgramDF$words, 3, sep = fixed("_"))
quadgramDF$w4<-word(quadgramDF$words, 4, sep = fixed("_"))
quadgramDF<-subset(quadgramDF, select = -c(words))
quadgramDF<-quadgramDF[c("w1", "w2", "w3", "w4", "freq")]
saveRDS(quadgramDF, file = "shiny/find_next_word/data/quadgram.rds")

quintgramTop<-topfeatures(quintgramMat, n = 40000)
quintgramDF<-data.frame(words = names(quintgramTop), freq = quintgramTop)
quintgramDF %>% mutate_if(is.factor, as.character) -> quintgramDF
quintgramDF$w1<-word(quintgramDF$words, 1, sep = fixed("_"))
quintgramDF$w2<-word(quintgramDF$words, 2, sep = fixed("_"))
quintgramDF$w3<-word(quintgramDF$words, 3, sep = fixed("_"))
quintgramDF$w4<-word(quintgramDF$words, 4, sep = fixed("_"))
quintgramDF$w5<-word(quintgramDF$words, 5, sep = fixed("_"))
quintgramDF<-subset(quintgramDF, select = -c(words))
quintgramDF<-quintgramDF[c("w1", "w2", "w3", "w4", "w5", "freq")]
saveRDS(quintgramDF, file = "shiny/find_next_word/data/quintgram.rds")
