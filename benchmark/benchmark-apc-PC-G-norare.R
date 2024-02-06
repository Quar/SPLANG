library(tibble)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
source("benchmark-util.R")
# source("benchmark-util-parallel.R")


validate.pc = function(data.file) {
  
  load(data.file)
  
  abs.G = gen.data$G %>% abs
  print(abs.G %>% sum)
  
  taxa.by.col = apply(abs.G > 0, 2, sum) > 0
  taxa.by.row = apply(abs.G > 0, 1, sum) > 0
  taxa.norare = taxa.by.row & taxa.by.col
  print(paste0('no rare taxa number = ', sum(taxa.norare)))
  
  abundance.data = gen.data$synthetic.sequencing.data %>% t
  
  pc.raw = abundance.data %>% cor
  diag(pc.raw) = 0
  pc.raw[is.na(pc.raw)] = 0
  
  pc.sign = ifelse(pc.raw < -0.2, -1, ifelse(pc.raw > 0.2, 1, 0))
  
  data.frame(
    predicted = pc.sign[taxa.norare, taxa.norare] %>% as.vector %>% as.factor,
    reference = gen.data$G[taxa.norare, taxa.norare] %>% sign %>% as.vector %>% as.factor
  )
  
}

benchmark.pc.for.dataset = function(dataset.dir) {
  benchmark.for.dataset(
    dataset.dir, 
    validation.function = validate.pc,
    validation.function.name='pc',
    taxa.filter.name = 'norareG'
  )
}


all.num.of.species = c(12, 25, 50, 100, 200)
all.num.of.edges.similar = c(22, 48, 98, 198, 398)

all.num.of.edges.sparse = round(all.num.of.species * 0.8)
all.num.of.edges.dense = round(all.num.of.species^2 * 0.8)


# for (i in 1:5) {
for (i in 1:5) {
  
  num.species = all.num.of.species[i]
  num.edges.similar = all.num.of.edges.similar[i]
  num.edges.dense = all.num.of.edges.dense[i]
  num.edges.sparse = all.num.of.edges.sparse[i]  
  
  benchmark.pc.for.dataset(
    str_glue("../gen-data/data-apc-pa-{num.species}-species-30k-depth")
  )
  
  benchmark.pc.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.similar}-{num.species}-species-30k-depth")
  )
  
  benchmark.pc.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.dense}-{num.species}-species-30k-depth")
  )
  
  benchmark.pc.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.sparse}-{num.species}-species-30k-depth")
  )  
  
}


