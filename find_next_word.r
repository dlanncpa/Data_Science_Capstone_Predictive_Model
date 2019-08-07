suppressMessages(library(stringi))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))

unigram<-readRDS(file = "/data/unigram.rds")
bigram<-readRDS(file = "/data/bigram.rds")
trigram<-readRDS(file = "/data/trigram.rds")
quadgram<-readRDS(file = "/data/quadgram.rds")
quintgram<-readRDS(file = "/data/quintgram.rds")

find_next_word<-function(textInput){
    inputAdj<-removePunctuation(tolower(textInput))
    inputAdj<-removeWords(inputAdj, stopwords("english"))
    inputAdj<-strsplit(inputAdj, " ")[[1]]
    
    if(length(inputAdj)>=4){
        inputAdj<-tail(inputAdj, 4)
        a1<-head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 3], 1)
        a2<-tail(head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 3], 2), n = 1)
        a3<-tail(head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 3], 3), n = 1)
        
        else if(length(inputAdj)==3){
            inputAdj<-tail(inputAdj, 3)
            a1<-head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 3], 1)
            a2<-tail(head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 3], 2), n = 1)
            a3<-tail(head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 3], 3), n = 1)
            
            else if(length(inputAdj)==2){
                inputAdj<-tail(inputAdj, 2)
                a1<-head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 1)
                a2<-tail(head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 2), n = 1)
                a3<-tail(head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 3), n = 1)
                
                else if(length(inputAdj)==1){
                    inputAdj<-tail(inputAdj, 2)
                    a1<-head(bigram[bigram$w1 == inputAdj[1], 3], 1)
                    a2<-tail(head(bigram[bigram$w1 == inputAdj[1], 3], 2), n = 1)
                    a3<-tail(head(bigram[bigram$w1 == inputAdj[1], 3], 3), n = 1)
                    
                    else{
                        a1<-head(unigram[, 3], 1)
                        a2<-tail(head(unigram[, 3], 2), n = 1)
                        a3<-tail(head(unigram[, 3], 3), n = 1)
                    }
                }
            }
        }
    }
}