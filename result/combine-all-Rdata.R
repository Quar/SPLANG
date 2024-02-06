library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(tibble)
library(tidyr)

decode_network_names = function(df) {
  df %>% case_match(
    'nm10' ~ "sparse",
    'nm22' ~ "similar",
    'nm115' ~ "dense",
    'nm20' ~ "sparse",
    'nm48' ~ "similar",
    'nm500' ~ 'dense',
    'nm40' ~ 'sparse',
    'nm98' ~ 'similar',
    'nm2000' ~ 'dense',
    'nm80' ~ 'sparse',
    'nm198' ~ 'similar',
    'nm8000' ~ 'dense',
    'nm160' ~ 'sparse',
    'nm398' ~ 'similar',
    'nm32000' ~ 'dense',
    'pa' ~ 'pa',
    .ptype = factor(levels = c('pa', 'sparse', 'similar', 'dense'), ordered=T)
  )
}

fpath_ptn = ".*data-apc-(?<network>[^-]+)-(?<ntaxa>[0-9]+)-species-30k-depth-(?<method>[^-]+)-result-G-(?<filter>[a-zA-Z]*).Rdata"


load.res.raw = function(fpath, base.dir) {
  tmp.env = new.env()
  full.path = file.path(base.dir, fpath)
  load(full.path, envir = tmp.env)
  print(full.path)
  tmp.env$df.res.raw %>% 
    mutate(mat.cell.id=row_number()) %>%
    mutate_at(vars(abundance.file), as.numeric)
}


scenarios_nspecies = c(12, 25, 50, 100, 200)

for (nspecies in scenarios_nspecies)  {
  
  data.files = tibble(fpath=list.files('output_data', pattern=str_glue(".*{nspecies}-species.*-.Rdata")))

  df.all = data.files %>% 
    mutate(meta.data = str_match(.$fpath, fpath_ptn)[,-1] %>% as.data.frame) %>% 
    rowwise %>% mutate(result.data = list(load.res.raw(fpath, base.dir="output_data"))) %>% ungroup %>%
    unnest(c(meta.data, result.data))

  write.csv(df.all, str_glue('output_aggregated/df_data_{nspecies}_species.csv'))
  
}


for (nspecies in scenarios_nspecies)  {
  
  data.files = tibble(fpath=list.files('output_data', pattern=str_glue(".*{nspecies}-species.*-norareG.Rdata")))
  
  df.all = data.files %>% 
    mutate(meta.data = str_match(.$fpath, fpath_ptn)[,-1] %>% as.data.frame) %>% 
    rowwise %>% mutate(result.data = list(load.res.raw(fpath, base.dir="output_data"))) %>% ungroup %>%
    unnest(c(meta.data, result.data))
  
  write.csv(df.all, str_glue('output_aggregated/df_data_{nspecies}_species-norareG.csv'))
  
}
