library(tibble)
library(caret)
library(dplyr)
library(stringr)
library(tidyr)
library(PLNmodels)
source("benchmark-util.R")
# source("benchmark-util-parallel.R")


validate.chiquet = function(data.file) {
  
  load(data.file)
  print(data.file)
  
  print(gen.data$G %>% abs %>% sum)
  
  abundance.data = gen.data$synthetic.sequencing.data %>% t
  
  model.my.data = PLN(abundance.data ~ 0)
  
  omega = model.my.data$model_par$Omega
  
  omega[abs(omega) < 1] = 0
  diag(omega) = 0
  
  data.frame(
    predicted = omega %>% sign %>% as.vector %>% as.factor,
    reference = gen.data$G %>% sign %>% as.vector %>% as.factor
  )
  
}


benchmark.pln.for.dataset = function(dataset.dir) {
  benchmark.for.dataset(
    dataset.dir, 
    validation.function = validate.chiquet,
    validation.function.name='pln',
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
  
  benchmark.pln.for.dataset(
    str_glue("../gen-data/data-apc-pa-{num.species}-species-30k-depth")
  )
  
  benchmark.pln.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.similar}-{num.species}-species-30k-depth")
  )

  benchmark.pln.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.dense}-{num.species}-species-30k-depth")
  )

  benchmark.pln.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.sparse}-{num.species}-species-30k-depth")
  )  

}