library(tidyverse)
library(ggpubr)
library(ggh4x)


df_raw = read.csv('../result/aggregated-result-latest.csv')

df = df_raw %>%
  mutate(method = method %>% factor) %>%
  mutate(taxa_filter = filter %>% factor) %>%
  mutate(network = 
      network %>%
        case_match(
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
  ) %>%
  select(-filter)

stopifnot(any(df$network %>% is.na) == F)
df

df.pa = df %>%
  filter(network=='pa') %>%
  pivot_longer(c('polarized_score', 'full_score'), names_to = 'score_name', values_to = 'score', names_transform=factor)
df.pa

fig.pa = df.pa %>% ggplot(aes(x=nspecies, y=score, shape=method, color=method)) +
  geom_point() + geom_line() + 
  facet_grid(taxa_filter ~ score_name)
fig.pa  


df.all = df %>%
  rename('Non-Commensalistic Score'=polarized_score, 'Full Score'=full_score) %>%
  pivot_longer(
    c('Non-Commensalistic Score', 'Full Score'), 
    names_to = 'score_name', values_to = 'score', 
    names_transform=~factor(.x, c('Full Score', 'Non-Commensalistic Score'))
    ) %>%
  mutate(
    taxa_filter=recode(
      taxa_filter,
      norareG="Non-Isolated",
      all="All Taxa"
    )
  )
  


fig.all = df.all %>% ggplot(aes(x=nspecies, y=score, shape=method, color=method)) +
  geom_point() + geom_line() + 
  facet_nested(network + taxa_filter ~ score_name) +
  labs(
    x='Number of Species', 
    y='Accuracy', 
    # title='Combined Prediction Accuracy', 
    color="Inference Algorithm",
    shape="Inference Algorithm"
  )
fig.all


ggsave('fig/plot-score-varying.pdf', fig.all, width = 10, height = 12)
  