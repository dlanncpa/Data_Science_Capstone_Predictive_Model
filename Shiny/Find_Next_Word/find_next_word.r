suppressMessages(library(stringi))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(tm))

unigram<-readRDS(file = "data/unigram.rds")
bigram<-readRDS(file = "data/bigram.rds")
trigram<-readRDS(file = "data/trigram.rds")
quadgram<-readRDS(file = "data/quadgram.rds")
quintgram<-readRDS(file = "data/quintgram.rds")

find_next_word<-function(x){
    inputAdj<-removePunctuation(tolower(as.character(x)))
    inputAdj<-removeWords(inputAdj, stopwords("english"))
    inputAdj<-strsplit(inputAdj, " ")[[1]]
    inputAdj<-inputAdj[inputAdj != ""]
    
    if(length(inputAdj)>=4){
        inputAdj<-tail(inputAdj, 4)
        a1<-head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 5], 1)
        a2<-tail(head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 5], 2), n = 1)
        a3<-tail(head(quintgram[quintgram$w1 == inputAdj[1] & quintgram$w2 == inputAdj[2]
                & quintgram$w3 == inputAdj[3] & quintgram$w4 == inputAdj[4], 5], 3), n = 1)
        return(c(a1, a2, a3))
    }
        else if(length(inputAdj)==3){
            inputAdj<-tail(inputAdj, 3)
            a1<-head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 4], 1)
            a2<-tail(head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 4], 2), n = 1)
            a3<-tail(head(quadgram[quadgram$w1 == inputAdj[1] & quadgram$w2 == inputAdj[2]
                    & quadgram$w3 == inputAdj[3], 4], 3), n = 1)
            return(c(a1, a2, a3))
        }
            else if(length(inputAdj)==2){
                inputAdj<-tail(inputAdj, 2)
                a1<-head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 1)
                a2<-tail(head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 2), n = 1)
                a3<-tail(head(trigram[trigram$w1 == inputAdj[1] & trigram$w2 == inputAdj[2], 3], 3), n = 1)
                return(c(a1, a2, a3))
            }
                else if(length(inputAdj)==1){
                    inputAdj<-tail(inputAdj, 2)
                    a1<-head(bigram[bigram$w1 == inputAdj[1], 2], 1)
                    a2<-tail(head(bigram[bigram$w1 == inputAdj[1], 2], 2), n = 1)
                    a3<-tail(head(bigram[bigram$w1 == inputAdj[1], 2], 3), n = 1)
                    return(c(a1, a2, a3))
                }
                    else if(length(find_next_word)==0){
                        a1<-sample(unigram[1:50, 1])
                        a2<-sample(unigram[1:50, 1])
                        a3<-sample(unigram[1:50, 1])
                        return(c(a1, a2, a3))
                    }
    else{
        a1<-sample(unigram[1:50, 1])
        a2<-sample(unigram[1:50, 1])
        a3<-sample(unigram[1:50, 1])
        return(c(a1, a2, a3))
    }
}