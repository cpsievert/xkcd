# read in traning/test data if it doesn't already exist
if (!exists("dtm_train")) load("data/dtm_train.rda")
if (!exists("dtm_test")) load("data/dtm_test.rda")

# fit a bunch of models -- varying the number of topics
# section 2.4 of http://www.jstatsoft.org/v40/i13/paper
# has a nice, concise overview of model selection for LDA
library(topicmodels)
#burnin <- 1000
#iter <- 1000
#keep <- 50
ks <- seq(5, 150, by = 5)
models <- lapply(ks, function(k) LDA(dtm_train, k))
# keep the model that minimizes perplexity
perps <- sapply(models, perplexity, dtm_test)
best_model <- models[[which.min(perps)]]
png(filename = "perplexity.png")
plot(ks, perps, xlab = "Number of topics", ylab = "Perplexity")
dev.off()

save(best_model, file = "data/best_model.rda")
