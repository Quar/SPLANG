# data generation based on Chiquet et al. (2019)
library(Matrix)
library(dplyr)
library(stringr)
library(readr)
library(mvtnorm)
library(dplyr)
library(igraph)
library(matlib)


generate.asymmetric.G = function(graph) {
  
  adj.matrix = graph %>% as_adjacency_matrix %>% as.matrix
  
  n.species = adj.matrix %>% nrow
  n.edges = sum(adj.matrix)
  
  possible.interactions = c(-1,1)
  
  sampled.interaction = sample(possible.interactions, n.edges, replace=T)
  
  adj.matrix[adj.matrix>0] = sampled.interaction
  
  return(adj.matrix)
}

generate.pa.G = function(n, power=1) {
  sample_pa(n, power, directed = F) %>% generate.asymmetric.G
}

get.Omega.chiquet = function(G, u, v) {
  tilde.Omega = G * v
  mod.tilde.Omega.eigen.values = tilde.Omega %>% eigen(only.values = T) %>% `[[`('values') %>% Mod
  Omega = tilde.Omega + diag(tilde.Omega %>% nrow) * (min(mod.tilde.Omega.eigen.values) + u)
  Omega
}

get.Sigma.chiquet = function(n, u=0.3, v=0.1) {
  inv.Omega = generate.pa.G(n) %>% get.Omega.chiquet(u, v) %>% inv
  inv.Omega %*% t(inv.Omega)
}

get.Omega.amplify = function(G, u, v) {
  tilde.Omega = G - diag(G %>% nrow) # making self-limit
  amplified.Omega = tilde.Omega %*% t(tilde.Omega)
  eigen.values = amplified.Omega %>% eigen(only.values = T) %>% `[[`('values')
  Omega = amplified.Omega + diag(tilde.Omega %>% nrow) * (min(eigen.values) + u)
  # here we could return Omega
  Omega
}

get.coupled.Mu.Omega = function(G, u, v) {
  tilde.Omega = G - diag(G %>% nrow) # making self-limit
  amplified.Omega = tilde.Omega %*% t(tilde.Omega)
  eigen.values = amplified.Omega %>% eigen(only.values = T) %>% `[[`('values')
  Omega = amplified.Omega + diag(tilde.Omega %>% nrow) * (min(eigen.values) + u)
  # here we could return Omega
  list(mu=eigen.values, omega=Omega)
}


sample.squencing = function(n.species, 
                            n.sequencing, # number of sequencing performed
                            sequencing.depth, # depth of each sequencing
                            u=0.3, v=0.1,
                            preferential.attaching.power=1
                            ) {
  G = generate.pa.G(n.species, preferential.attaching.power)
  coupled.mu.omega = get.coupled.Mu.Omega(G, u, v)
  abundance.mu = coupled.mu.omega$mu
  abundance.sigma = coupled.mu.omega$omega %>% inv
  
  # to avoid numeric error
  abundance.sigma = forceSymmetric(abundance.sigma) %>% as.matrix
  
  groundtruth.abundance = rmvnorm(1, mean=abundance.mu, sigma=abundance.sigma) %>% exp
  # print(groundtruth.abundance)
  exp.a = groundtruth.abundance
  # print(exp.a)
  groundtruth.proportions = exp.a / sum(exp.a)
  # print(groundtruth.proportions)
  
  synthetic.sequencing.data = rmultinom(n.sequencing, sequencing.depth, prob=groundtruth.proportions)
  list(
    G = G,
    omega = coupled.mu.omega$omega,
    groundtruth.abundance=groundtruth.abundance, 
    synthetic.sequencing.data=synthetic.sequencing.data
  )
  # output: {n.sequencing}-columns of possible sequencing results, 
  # with each sequencing having {senquencing.depth} counts of counts
}



