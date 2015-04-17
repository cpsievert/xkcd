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

corp <- Corpus(VectorSource(docz$text))
dtm <- DocumentTermMatrix(corp, control = dtm.control)
mat <- as.matrix(dtm1)
# exclude terms that occur less than 5 times
idx <- colSums(mat) > 5
dtm <- dtm[, idx]
# throw out any empty documents
idx <- rowSums(mat) != 0
dtm <- dtm[idx, ]

# randomly sample 10% of docs and hold them out (for our test set)
set.seed(392) # for reproducibility
n <- nrow(docz)
idx <- sample(n, size = floor(n * 0.1))
dtm_train <- dtm[-idx, ]
dtm_test <- dtm[idx, ]

save(dtm_train, file = "data/dtm_train.rda")
save(dtm_test, file = "data/dtm_test.rda")
