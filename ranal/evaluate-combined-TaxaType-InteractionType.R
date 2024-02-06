library(tidyverse)
library(scales)
library(ggh4x)
source('network-encodings.R')

calc_any_correct = function(path.in, non.commensalistic.only=FALSE) {
  n.species = str_match(path.in, '.*_(?<nspecies>\\d+)_specie.*') %>% as.data.frame() %>% pull(nspecies) %>% as.numeric
  df = read_csv(path.in)
  df %>% 
    mutate(reference=reference_method_pln) %>%
    filter(non.commensalistic.only == FALSE | reference!=0) %>%
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


process_aggregated_pivoted = function(non.rare.taxa.only=FALSE, non.commensalistic.only=FALSE) {
  
  pivoted_file_pattern = ifelse(non.rare.taxa.only, '*species-norareG_pivoted.csv', '*specie_pivoted.csv')
  cat('use pivoted_file_patern as: ', pivoted_file_pattern)
  
  files = list.files('../result/output_aggregated_pivoted', full.names=TRUE, pattern=pivoted_file_pattern)
  print(files)
  
  df.res = files %>% map(~calc_any_correct(.x, non.commensalistic.only)) %>% list_rbind 
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
    mutate(network=network %>% decode_network_names) %>%
    mutate(taxa.type=ifelse(non.rare.taxa.only, 'Non-Isolated', 'All Taxa')) %>%
    mutate(interaction.type=ifelse(non.commensalistic.only, 'Non-Commensalistic Only', 'All Interactions'))
  
  return(df.res.draw)
}


df.all.taxa.interaction = expand_grid(c(F,T), c(F,T)) %>% 
  tibble(.name_repair=~c('non.rare.taxa.only', 'non.commensalistic.only')) %>% 
  pmap(process_aggregated_pivoted) %>% { do.call(rbind, .) }

### Stacked Bar Plot
df.all.taxa.interaction %>%
  filter(predict.result=='correct') %>%
  ggplot(aes(x=n.species, y=predict.counts, color=combine.algorithm)) +
  geom_point() + geom_line() +
  facet_nested(network + taxa.type ~ interaction.type) +
  labs(
    x='Number of Species', 
    y='Accuracy',     
    color="Fusion Algorithm"
    )  


ggsave("fig/combined-prediction-accuracy-by-network-types-TaxaType-InteractionType.png", height=12, width=10)
ggsave("fig/combined-prediction-accuracy-by-network-types-TaxaType-InteractionType.pdf", height=12, width=10)


# ## Generate LaTeX table
# output.table = df.res %>%
#   mutate(network=network %>% decode_network_names) %>%
#   select(-ntaxa) %>%
#   arrange(n.species, network) %>%
#   rename(
#     n.correct.OR=n.correct.any,
#     n.incorrect.OR=n.incorrect.any,
#     n.correct.MAJ=n.correct.mode,
#     n.incorrect.MAJ=n.incorrect.mode,
#     n.total.predictions=n.predictions
#   )
# output.table
# 
# library(xtable)
# print(
#   xtable(output.table, type = "latex"), 
#   file = "fig/combined-results-summarize-table-TaxaType-InteractionType.tex",
#   include.rownames=FALSE,
#   latex.environments=c("TabularTextWidth")
#   )
