library(topicmodels)
if (!exists("dtm")) load("data/dtm.rda")

# -------------------------------------------------------------------------
# General idea: 
#  (1) Split documents into training/test sets.
#  (2) Fit models (varying by the number of topics) to the training set.
#  (3) Compute the perplexity (log-likelihood of the held-out test set) for
#   each fitted model.
# -----------------------------------------------------------------------

# If we have a term with zero frequency in the training data, that will
# pose problems for LDAvis (can't divide by zero!).
# This function will keep searching for a train/test split that 
# yields a non-zero frequency for every term in the training data.
split_dtm <- function(dtm, perc = 0.1) {
  n <- dtm$nrow
  idx <- sample(n, size = floor(n * perc))
  dtm_train <- dtm[-idx, ]
  dtm_test <- dtm[idx, ]
  termFreqs <- colSums(as.matrix(dtm_train))
  if (any(termFreqs == 0)) {
    split_dtm(dtm)
  } else {
    return(list(dtm_train = dtm_train, dtm_test = dtm_test))
  }
}

set.seed(43523) # for reproducibility
dtms <- split_dtm(dtm)
dtm_train <- dtms$dtm_train
dtm_test <- dtms$dtm_test

# save some summary statistics that we'll need for LDAvis
termFreqs <- colSums(as.matrix(dtm_train))
stopifnot(!any(termFreqs == 0))
save(termFreqs, file = "data/termFreqs.rda")
docLens <- rowSums(as.matrix(dtm_train))
stopifnot(!any(docLens == 0))
save(docLens, file = "data/docLens.rda")

# fit a bunch of models -- varying the number of topics
# section 2.4 of http://www.jstatsoft.org/v40/i13/paper
# has a nice, concise overview of model selection for LDA
ks <- seq(5, 150, by = 5)
models <- lapply(ks, function(k) LDA(dtm_train, k, method = "Gibbs", 
                                     control = list(alpha = 1/k, delta = 0.1, 
                                                    burnin = 1000, iter = 1000, 
                                                    keep = 50)))
save(models, file = "data/models.rda")
perps <- sapply(models, perplexity, dtm_test)
png(filename = "perplexity.png")
plot(ks, perps, xlab = "Number of topics", ylab = "Perplexity")
dev.off()
