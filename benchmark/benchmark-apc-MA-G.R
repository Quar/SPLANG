library(tibble)
library(tidyr)
library(dplyr)
library(caret)
library(metaMint)
library(stringr)
source("benchmark-util-parallel.R")


validate.ma = function(data.file) {

  load(data.file)
  print(gen.data$G %>% abs %>% sum)

  # abundance.data.mclr = apply(gen.data$synthetic.sequencing.data, 2, compositions::clr)

  abundance.data.mclr = gen.data$synthetic.sequencing.data %>% mclr
  ma.raw = cggm.pcorr(abundance.data.mclr %>% t, c(0.2), method='glasso')$icov[[1]]

  diag(ma.raw) = 0
  ma.raw[is.na(ma.raw)] = 0
  ma.sign = ifelse(ma.raw < -0.2, -1, ifelse(ma.raw > 0.2, 1, 0))
  # ma.sign = ma.raw


  data.frame(
    predicted = ma.sign %>% sign %>% as.vector %>% as.factor,
    reference = gen.data$G %>% sign %>% as.vector %>% as.factor
  )

}


benchmark.ma.for.dataset = function(dataset.dir) {
  benchmark.for.dataset(
    dataset.dir,
    validation.function = validate.ma,
    validation.function.name='ma',
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

  benchmark.ma.for.dataset(
    str_glue("../gen-data/data-apc-pa-{num.species}-species-30k-depth")
  )

  benchmark.ma.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.similar}-{num.species}-species-30k-depth")
  )

  benchmark.ma.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.dense}-{num.species}-species-30k-depth")
  )

  benchmark.ma.for.dataset(
    str_glue("../gen-data/data-apc-nm{num.edges.sparse}-{num.species}-species-30k-depth")
  )

}

