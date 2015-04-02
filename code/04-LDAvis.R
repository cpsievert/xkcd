if (!exists("dtm_train")) load("data/dtm_train.rda")
if (!exists("best_model")) load("data/best_model.rda")

# compute summary statistics from training data
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
serVis(json, "vis")
