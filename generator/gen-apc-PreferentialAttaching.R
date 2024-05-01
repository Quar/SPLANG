library(stringr)
library(SPLANG)


generate.synthetic.data = function(
    n.species,
    n.sequencing,
    sequencing.depth,
    n.sample.files,
    dir.path,
    fn.opath = function(n.species, n.files, i) {
      file.path(
        dir.path,
        str_glue("apc-{n.species}-species-{i}-of-{n.sample.files}.Rdata")
      )
    },
    ...
) {
  if (!dir.exists(dir.path)) {
    dir.create(dir.path, recursive = T)
  }
  for (i in 1:n.sequencing) {    
    gen.data = sample.sequencing(n.species, n.sequencing, sequencing.depth, ...)
    save(gen.data, file=fn.opath(n.species, n.sample.files, i))
  }
}


all.num.of.species = c(12, 25, 50, 100, 200)

for (num.species in all.num.of.species) {

  generate.synthetic.data(
    n.species = num.species,
    n.sequencing = 100,
    sequencing.depth = 30000,
    n.sample.files = 100,
    dir.path = str_glue("../gen-data/data-apc-pa-{num.species}-species-30k-depth")
  )
    
}
