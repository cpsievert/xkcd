library(methods)
if (!exists("termFreqs")) load("data/termFreqs.rda")
if (!exists("docLens")) load("data/docLens.rda")
if (!exists("models")) load("data/models.rda")

# keep the "best" model 
best <- which(sapply(models, function(x) x@k) == 20)
best_model <- models[[best]]

# some sanity checks
stopifnot(all(names(docLens) == best_model@documents))
stopifnot(all(names(termFreqs) == best_model@terms))

library(LDAvis)
json <- createJSON(phi = exp(best_model@beta), theta = best_model@gamma,
                   doc.length = docLens, vocab = best_model@terms,
                   term.frequency = termFreqs)
serVis(json, "vis", open.browser = FALSE)
