# Som code here adapted from https://github.com/CabbagesAndKings/xkcd-Topics/blob/master/scripts/getTranscripts.sh
if (!exists("docz")) load("data/docz.rda")
library(tm)

#### Text Cleaning ####
#Remove alt-text (optional)
#docz$text <- gsub("\\{\\{.*\\}\\}", "", docz$text)

#Remove scene-description.
#This might initially seem like a bad idea, but scene descriptions contain stuff like [[Man standing in a room]] ,etc. 
#I'll revisit this, to see if there's a better solution
docz$text <- gsub("\\[\\[.*?\\]\\]", "", docz$text)

#remove speaker id
#for each pipe-surrounded string, check if there is a : there. If there is, discard the part before the first :
#this is only moderately accurate, but it's the best way I could think of, without manual intervention
RemoveSpeakers <- function(trans){
  trans <- paste0("|", trans, "|")
  frames <- strsplit(trans, "\\|")[[1]]
  processed.frames <- sapply(frames[grep("\\:",frames)], 
                             function(f) {
                               dialogue <- strsplit(f,":")[[1]]
                               do.call(paste, as.list(c(dialogue[2:length(dialogue)], sep="\\:")))
                             })
  frames[grep("\\:",frames)] <- processed.frames
  do.call(paste, as.list(c(frames, sep="|")))
}

docz$text <- sapply(docz$text, RemoveSpeakers)

# randomly sample 10% of docs and hold them out (for our test set)
set.seed(392) # for reproducibility
n <- nrow(docz)
idx <- sample(n, size = floor(n * 0.1))
train <- docz[-idx, ]
test <- docz[idx, ]

source('code/extendedStopwords.R')
dtm.control <- list(
  tolower   		      = T,
  removePunctuation 	= T,
  removeNumbers 		  = T,
  stopwords 			    = c(stopwords("english"), extendedstopwords),
  stemming 			      = T,
  wordLengths 		    = c(3, Inf),
  weighting 		    	= weightTf
)

# convert a character vector (of documents) into a document-term matrix
process_dtm <- function(txt, sparse = 0.999) {
  corp <- Corpus(VectorSource(txt))
  dtm1 <- DocumentTermMatrix(corp, control = dtm.control)
  dtm2 <- removeSparseTerms(dtm1, sparse)
  message("Removed ", dim(dtm1)[2] - dim(dtm2)[1], " sparse terms")
  dtm3 <- dtm2[rowSums(as.matrix(dtm2)) > 0, ]
  message("Removed ", dim(dtm2)[1] - dim(dtm3)[1], 
          " empty documents (after removing sparse terms).")
  dtm3
}

dtm_train <- process_dtm(train$text)
dtm_test <- process_dtm(test$text)
save(dtm_train, file = "data/dtm_train.rda")
save(dtm_test, file = "data/dtm_test.rda")
