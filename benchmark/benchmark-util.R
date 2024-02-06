library(tibble)
library(tidyr)
library(dplyr)
library(caret)
library(stringr)

benchmark.dir = function(dir.path, validation.function) {
  sapply(
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
                                 save.to.file=TRUE, dataset.root='./gen_data') {
  
  dataset.path = file.path(dataset.root, dataset.dir)
  
  df.res.raw = benchmark.dir(dataset.path, validation.function) %>% 
    t %>% 
    as_tibble %>% 
    rownames_to_column('abundance.file') %>%
    unnest_longer(everything())
  
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