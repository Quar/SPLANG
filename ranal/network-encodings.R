library(dplyr)

scenarios = tribble(
  ~ntaxa, ~netcode, ~network,
  12, 'nm10', "sparse",
  12, 'nm22', "similar",
  12, 'nm115', "dense",
  25, 'nm20', "sparse",
  25, 'nm48', "similar",
  25, 'nm500', 'dense',
  50, 'nm40', 'sparse',
  50, 'nm98', 'similar',
  50, 'nm2000', 'dense',
  100, 'nm80', 'sparse',
  100, 'nm198', 'similar',
  100, 'nm8000', 'dense',
  200, 'nm160', 'sparse',
  200, 'nm398', 'similar',
  200, 'nm32000', 'dense',
  12, 'pa', 'pa',
  25, 'pa', 'pa',
  50, 'pa', 'pa',
  100, 'pa', 'pa',
  200, 'pa', 'pa',
)

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
