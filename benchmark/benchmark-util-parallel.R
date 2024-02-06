library(tibble)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)
library(parallel)

benchmark.dir = function(dir.path, validation.function) {
  mclapply(
    list.files(dir.path),
    function(fname) {
      fpath = file.path(dir.path, fname)
      validation.function(fpath)
    }
  )
}

benchmark.for.dataset = function(dataset.dir, validation.function,
                                 validation.function.name,
                                 taxa.filter.name,
                                 save.to.file=TRUE, dataset.root='../gen_data') {

  dataset.path = file.path(dataset.root, dataset.dir)

  df.res.raw = benchmark.dir(dataset.path, validation.function) %>%
    tibble::enframe(name='abundance.file') %>%
    unnest(cols=c('value'))

  rdata.path = file.path(
    dataset.root,
    str_glue(dataset.dir, "-{validation.function.name}-result-G-{taxa.filter.name}.Rdata")
  )

  save(df.res.raw, file=rdata.path)

  cm = confusionMatrix(df.res.raw$predicted, df.res.raw$reference)

  txt.path = file.path(
    dataset.root,
    str_glue(dataset.dir, "-{validation.function.name}-result-G-{taxa.filter.name}.txt")
  )
  if (save.to.file) {
    sink(file=txt.path)
    print(cm)
    sink()
  }

  cm
}
