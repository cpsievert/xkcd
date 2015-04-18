# read in traning/test data if it doesn't already exist
if (!exists("dtm")) load("data/dtm.rda")

# randomly sample 10% of docs and hold them out (for our test set)
set.seed(43523) # for reproducibility
n <- dtm$nrow
idx <- sample(n, size = floor(n * 0.1))
dtm_train <- dtm[-idx, ]
dtm_test <- dtm[idx, ]

# If we have a term with zero frequency in the training data, that will
# pose problems for LDAvis (can't divide by zero!)
termFreqs <- colSums(as.matrix(dtm_train))
stopifnot(!any(termFreqs == 0))
save(termFreqs, file = "termFreqs.rda")
docLens <- rowSums(as.matrix(dtm_train))
stopifnot(!any(docLens == 0))
save(docLens, file = "docLens.rda")

# fit a bunch of models -- varying the number of topics
# section 2.4 of http://www.jstatsoft.org/v40/i13/paper
# has a nice, concise overview of model selection for LDA
library(topicmodels)
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
