#' Generate an unweighted asymmetric interaction matrix from a graph
#'
#' An unweighted asymmetric interaction matrix, denoted as (G), to present
#' underlying type of taxa interactions as either commensalistic (0), 
#' competitive (-1), and mutalistic (+1). 
#' This function randomly rewrites edges denoted in an adjacency
#' matrix by randomly switch some (+1) to (-1).
#' 
#' @param graph igraph Graph object
#' @return a generated asymmetric interaction matrix
#' @examples
#' g = igraph::sample_gnm(n=5, m=5)
#' unweighted_interaction_matrix = SPLANG:::generate.asymmetric.G(graph=g)
#' unweighted_interaction_matrix
#' @importFrom dplyr `%>%` 
#' @importFrom igraph as_adjacency_matrix
#' @export
generate.asymmetric.G = function(graph) {
  
  adj.matrix = graph %>% as_adjacency_matrix %>% as.matrix
  
  n.species = adj.matrix %>% nrow
  
  n.edges = sum(adj.matrix)
  
  possible.interactions = c(-1,1)
  
  sampled.interaction = sample(possible.interactions, n.edges, replace=T)
  
  adj.matrix[adj.matrix>0] = sampled.interaction
  
  # return
  adj.matrix
}


#' Calculate coupled mean abundance and a precision matrix
#'
#' Calculate coupled mean abundance (mu) and a precision matrix (Omega) from 
#' the underlying unweighted asymmetric interaction matrix (G).
#' 
#' @param G underlying unweighted asymmetric interaction matrix
#' @param u offset parameter 
#' @param v scale parameter
#' @return a named list of coupled expected abundance (mu) and precision matrix (Omega)
#' @examples
#' graph = igraph::sample_gnm(n=5, m=5) 
#' G = SPLANG:::generate.asymmetric.G(graph)
#' coupled.mu.Omega = SPLANG:::get.coupled.mu.Omega(G, u=0.1, v=0.3)
#' coupled.mu.Omega
#' @importFrom dplyr `%>%` 
#' @importFrom igraph as_adjacency_matrix
get.coupled.mu.Omega = function(G, u, v) {
  tilde.Omega = (G - diag(G %>% nrow)) * v # making self-limit
  amplified.Omega = tilde.Omega %*% t(tilde.Omega)
  eigen.values = amplified.Omega %>% eigen(only.values = T) %>% `[[`('values')
  Omega = amplified.Omega + diag(tilde.Omega %>% nrow) * (min(eigen.values) + u)
  
  # return
  list(mu=eigen.values, omega=Omega)
}


#' Generate abundance samples and underlying taxa interaction network
#'
#' This function will firstly generate an underlying taxa interaction network,
#' then generate synthetic abundance samples with specified number of sequencings
#' performed and the depth of each sequencing.
#' 
#' 
#' @param n.species number of taxa, an integer.
#' @param n.sequencing number of sequencing performed, an integer.
#' @param sequencing.depth depth of each sequencing, an integer.
#' @param u offset parameter, a float number.
#' @param v scale parameter, a non-negative float number.
#' @param network.type type of underlying network, either 
#'    preferential attaching ("pa"), or Erdos-Renyi ("nm")
#' @param m.edges number of edges if network type is Erdos-Renyi N-M, 
#'    the default is `round(n.species * (n.species - 1) *.8)`,
#'    i.e. randomly choose 80% of all possible edges
#'    only used if `network.type="nm"`. The `floor(m.edge)` will be used if
#'    `m.edge` is not an integer.
#' @param power the power of preferential attachment, the default is one, 
#'    i.e. linear preferential attachment.
#'    This parameter is meaningful only when the `network.type="pa"`, and it will
#'    be ignored otherwise.
#' @return a named-list of:<br>
#'    - underlying unweighted interaction network (`G`) with shape `n.species` \eqn{\times}{x} `n.species`;<br>
#'    - partial correlation matrix (`omega`) with shape `n.species` \eqn{\times}{x} `n.species`;<br>
#'    - ground truth abundance (`groundtruth.abundance`) with shape 1 \eqn{\times}{x} `n.species`;<br>
#'    - synthetic sequencing data (`synthetic.sequencing.data`) as a matrix with
#'    `n.species` rows and `n.sequencing` columns, with each row representing
#'    `n.sequencing` results of the i-th species. The result is a counting ranging
#'    from 0 to `sequencing.depth`.
#'    
#' @examples
#' # generate abundance sequencing for preferential-attaching typed underlying network
#' sample.sequencing(5, 10, 100, network.type = 'pa')
#' 
#' # generate abundance sequencing for Erdo-Renyi typed underlying network with 
#' # 80% of all possible edges
#' sample.sequencing(5, 10, 100, network.type = 'nm')
#'
#' # generate abundance sequencing for Erdo-Renyi typed underlying network with 
#' # 50% of all possible edges
#' sample.sequencing(
#'   n.species=5, 
#'   n.sequencing=10, 
#'   sequencing.depth=100, 
#'   network.type = 'nm',
#'   m.edges=(5 * (5 - 1) * .5) # use the floor value if not an integer
#'   )
#' 
#' @importFrom dplyr `%>%`
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph sample_gnm
#' @importFrom igraph sample_pa
#' @importFrom Matrix forceSymmetric
#' @importFrom MASS ginv
#' @importFrom mvtnorm rmvnorm
#' @importFrom stats rmultinom
#' @export
sample.sequencing = function(n.species, 
                            n.sequencing,
                            sequencing.depth,
                            u=0.1, v=0.3,
                            network.type=c('nm', 'pa'),
                            m.edges = round(n.species * (n.species - 1) *.8),
                            power=1
                            ) {
  network.type = match.arg(network.type)
  graph = switch(
    network.type,
    nm = sample_gnm(n.species, m.edges, directed = T),
    pa = sample_pa(n.species, power, directed = F)
  )

  G = graph %>% generate.asymmetric.G
  coupled.mu.omega = get.coupled.mu.Omega(G, u, v)
  abundance.mu = coupled.mu.omega$mu
  abundance.sigma = coupled.mu.omega$omega %>% ginv
  
  # to avoid numeric error
  abundance.sigma = forceSymmetric(abundance.sigma) %>% as.matrix
  
  
  groundtruth.abundance = rmvnorm(
    n=1,mean=abundance.mu, sigma=abundance.sigma) %>% exp
  
  exp.a = groundtruth.abundance
  
  groundtruth.proportions = exp.a / sum(exp.a)
  
  synthetic.sequencing.data = rmultinom(
    n.sequencing, sequencing.depth, prob=groundtruth.proportions)
  
  # return
  list(
    G = G,
    omega = coupled.mu.omega$omega,
    groundtruth.abundance=groundtruth.abundance, 
    synthetic.sequencing.data=synthetic.sequencing.data
  )
}
