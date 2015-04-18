# Some code here adapted from https://github.com/CabbagesAndKings/xkcd-Topics/blob/master/scripts/getTranscripts.sh
library(tm)
if (!exists("docz")) load("data/docz.rda")

# Remove scene-description. 
docz$text <- gsub("\\[\\[.*?\\]\\]", "", docz$text)

# Remove speaker id
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
# exclude terms that occur less than 5 times
idx <- colSums(as.matrix(dtm)) > 5
dtm <- dtm[, idx]
# throw out any empty documents
idx <- rowSums(as.matrix(dtm)) > 0
dtm <- dtm[idx, ]

save(dtm, file = "data/dtm.rda")
