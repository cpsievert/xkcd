all: vis

vis: code/04-LDAvis.R data/models.rda data/termFreqs.rda data/docLens.rda
	Rscript $<
	
data/models.rda: code/03-fitLDA.R data/dtm.rda
	Rscript $<
	
data/dtm.rda: code/02-preprocess.R data/docz.rda
	Rscript $<

data/docz.rda: code/01-scrape.R
	Rscript $<
