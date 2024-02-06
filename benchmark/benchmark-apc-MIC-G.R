library(tibble)
library(tidyr)
library(dplyr)
library(caret)
library(minerva)
library(stringr)
source("benchmark-util.R")
# source("benchmark-util-parallel.R")


validate.mic = function(data.file) {
  
  load(data.file)
  print(gen.data$G %>% abs %>% sum)
  
  abundance.data = gen.data$synthetic.sequencing.data %>% t
  
  mic.raw = mine(abundance.data)$MIC
  diag(mic.raw) = 0
  
  mic.01 = ifelse(mic.raw >=0.2, 1, 0)
  
  data.frame(
    predicted = mic.01 %>% as.vector %>% as.factor,
    reference = gen.data$G %>% sign %>% abs %>% as.vector %>% as.factor
  )
  
}


benchmark.mic.for.dataset = function(dataset.dir) {
  benchmark.for.dataset(
    dataset.dir, 
    validation.function = validate.mic,
    validation.function.name='mic',
    taxa.filter.name = ''
  )
}



all.num.of.species = c(12, 25, 50, 100, 200)
all.num.of.edges.similar = c(22, 48, 98, 198, 398)

all.num.of.edges.sparse = round(all.num.of.species * 0.8)
all.num.of.edges.dense = round(all.num.of.species^2 * 0.8)



for (i in 1:5) {
  
  num.species = all.num.of.species[i]
  num.edges.similar = all.num.of.edges.similar[i]
  num.edges.dense = all.num.of.edges.dense[i]
  num.edges.sparse = all.num.of.edges.sparse[i]  
  
  benchmark.mic.for.dataset(
    str_glue("../gen-data/data-apc-pa-{num.species}-species-30k-depth")
  )
  
  benchmark.mic.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.similar}-{num.species}-species-30k-depth")
  )
  
  benchmark.mic.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.dense}-{num.species}-species-30k-depth")
  )
  
  benchmark.mic.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.sparse}-{num.species}-species-30k-depth")
  )  
  
}


