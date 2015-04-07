library(methods)
if (!exists("dtm_train")) load("data/dtm_train.rda")
if (!exists("models")) load("data/models.rda")

# keep the "best" model 
best <- which(sapply(models, function(x) x@k) == 30)
best_model <- models[[best]]

# compute summary statistics from training data
library(topicmodels)
mat <- as.matrix(dtm_train)
docLens <- rowSums(mat)
termFreq <- colSums(mat)

# some sanity checks
stopifnot(all(names(docLens) == best_model@documents))
stopifnot(all(names(termFreq) == best_model@terms))

library(LDAvis)
json <- createJSON(phi = exp(best_model@beta), theta = best_model@gamma,
                   doc.length = docLens, vocab = best_model@terms,
                   term.frequency = termFreq)
serVis(json, "vis", open.browser = FALSE)
