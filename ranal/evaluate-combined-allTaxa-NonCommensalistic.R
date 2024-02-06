library(tidyverse)
library(scales)
library(ggh4x)
source('network-encodings.R')

calc_any_correct = function(path.in) {
  n.species = str_match(path.in, '.*_(?<nspecies>\\d+)_specie.*') %>% as.data.frame() %>% pull(nspecies) %>% as.numeric
  df = read_csv(path.in)
  df %>% 
    mutate(reference=reference_method_pln) %>%
    filter(reference!=0) %>%
    select(-ends_with('_mic'), -starts_with('reference_method_')) %>%
    mutate(across(starts_with('predicted_method') | starts_with('reference_method_'), ~sign(.x))) %>%
    mutate(predicted.any= reference == predicted_method_pln | reference == predicted_method_ma | reference == predicted_method_pc) %>%
    mutate(predicted.mode= ( (reference == predicted_method_pln) + (reference == predicted_method_ma) + (reference == predicted_method_pc) >= 2)) %>%
    group_by(network, ntaxa) %>%
    summarize(
      n.species=mean(ntaxa),
      n.correct.any = sum(predicted.any),
      n.incorrect.any = sum(!predicted.any),
      n.correct.mode = sum(predicted.mode),
      n.incorrect.mode = sum(!predicted.mode),
      n.predictions = n()
    ) %>%
    ungroup()
}


files = list.files('output_aggregated_pivoted', full.names=TRUE, pattern='*specie_pivoted.csv')
print(files)

df.res = files %>% map(calc_any_correct) %>% list_rbind 
df.res

df.res.draw = df.res %>%
  mutate(
    OR.correct = n.correct.any / n.predictions,
    OR.incorrect = n.incorrect.any / n.predictions,
    MAJ.correct = n.correct.mode / n.predictions,
    MAJ.incorrect = n.incorrect.mode / n.predictions
  )  %>%
  select(-starts_with('n.'), n.species) %>%
  pivot_longer(ends_with('correct') | ends_with('incorrect'), names_to="predict.result", values_to='predict.counts') %>%
  separate_wider_delim(predict.result, '.', names=c('combine.algorithm', 'predict.result')) %>%
  mutate(predict.result=factor(predict.result, levels=c('incorrect', 'correct'))) %>%
  mutate(combine.algorithm=factor(combine.algorithm, levels=c('OR', 'MAJ'))) %>%
  mutate(network=network %>% decode_network_names)
df.res.draw


### Stacked Bar Plot
df.res.draw %>%
  mutate(n.species=factor(n.species)) %>%
  ggplot(aes(x=n.species, y=predict.counts, fill=predict.result)) +
  geom_bar(stat="identity") +
  # facet_grid(combine.algorithm + network~.) +
  facet_nested_wrap(~ combine.algorithm + network, dir='v', ncol=1, strip.position='left') + 
  geom_text(aes(label=round(predict.counts, 3)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=hue_pal()(2)) + 
  labs(
    x='Number of Species', 
    y='Ratio of Prediction Results',     
    fill="Predict Results"
    )  


ggsave("fig/combined-prediction-accuracy-by-network-types-allTaxa-NonCommensalistic.png", height=8, width=8)
ggsave("fig/combined-prediction-accuracy-by-network-types-allTaxa-NonCommensalistic.pdf", height=8, width=8)


### Line Plot
df.res %>%
  mutate(n.species=factor(n.species)) %>%
  group_by(n.species, ntaxa) %>%
  summarize(
    OR.correct = sum(n.correct.any) / sum(n.predictions),
    OR.incorrect = sum(n.incorrect.any) / sum(n.predictions),
    MAJ.correct = sum(n.correct.mode) / sum(n.predictions),
    MAJ.incorrect = sum(n.incorrect.mode) / sum(n.predictions),
    .groups="drop"
  )  %>%
  pivot_longer(ends_with('correct') | ends_with('incorrect'), names_to="predict.result", values_to='predict.counts') %>%
  separate_wider_delim(predict.result, '.', names=c('combine.algorithm', 'predict.result')) %>%
  mutate(predict.result=factor(predict.result, levels=c('incorrect', 'correct'))) %>%
  mutate(combine.algorithm=factor(combine.algorithm, levels=c('OR', 'MAJ'))) %>%  
  # mutate(predict.result=factor(predict.result, levels=c('wrong', 'correct'))) %>%
  ggplot(aes(x=n.species, y=predict.counts, fill=predict.result)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(predict.counts, 3)), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=hue_pal()(2)) + 
  facet_nested_wrap(~ combine.algorithm, dir='v', ncol=1, strip.position='left') + 
  labs(
    x='Number of Species', 
    y='Ratio of Prediction Results',     
    fill="Predict Results"
  )

ggsave("fig/combined-prediction-accuracy-allTaxa-NonCommensalistic.png", height=8, width=8)
ggsave("fig/combined-prediction-accuracy-allTaxa-NonCommensalistic.pdf", height=8, width=8)



## Generate LaTeX table
output.table = df.res %>%
  mutate(network=network %>% decode_network_names) %>%
  select(-ntaxa) %>%
  arrange(n.species, network) %>%
  rename(
    n.correct.OR=n.correct.any,
    n.incorrect.OR=n.incorrect.any,
    n.correct.MAJ=n.correct.mode,
    n.incorrect.MAJ=n.incorrect.mode,
    n.total.predictions=n.predictions
  )
output.table

library(xtable)
print(
  xtable(output.table, type = "latex"), 
  file = "fig/combined-results-summarize-table-allTaxa-NonCommensalistic.tex",
  include.rownames=FALSE,
  latex.environments=c("TabularTextWidth")
  )
