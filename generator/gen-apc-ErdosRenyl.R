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

all.num.of.edges.similar = c(22, 48, 98, 198, 398)
all.num.of.edges.sparse = round(all.num.of.species * 0.8)
all.num.of.edges.dense = round(all.num.of.species^2 * 0.8)

n.sequencing = 100
sequencing.depth = 30000
n.sample.files = 100


for (i in 1:5) {
  
  num.species = all.num.of.species[i]
  num.edges.similar = all.num.of.edges.similar[i]
  num.edges.dense = all.num.of.edges.dense[i]
  num.edges.sparse = all.num.of.edges.sparse[i]
  
  generate.synthetic.data(
    n.species = num.species,
    n.sequencing = n.sequencing,
    sequencing.depth = sequencing.depth,
    n.sample.files = n.sample.files,
    m.edges = num.edges.sparse,
    dir.path = str_glue("../gen-data/data-apc-nm{num.edges.sparse}-{num.species}-species-30k-depth")
  )

  generate.synthetic.data(
    n.species = num.species,
    n.sequencing = n.sequencing,
    sequencing.depth = sequencing.depth,
    n.sample.files = n.sample.files,
    m.edges = num.edges.similar,
    dir.path = str_glue("../gen-data/data-apc-nm{num.edges.similar}-{num.species}-species-30k-depth")
  )
  
  generate.synthetic.data(
    n.species = num.species,
    n.sequencing = n.sequencing,
    sequencing.depth = sequencing.depth,
    n.sample.files = n.sample.files,
    m.edges = num.edges.dense,
    dir.path = str_glue("../gen-data/data-apc-nm{num.edges.dense}-{num.species}-species-30k-depth")
  )
  
}
