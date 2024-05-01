## SPLANG: A synthetic Poisson-lognormal-based abundance and network generative model for microbial interaction inference algorithms

Weicheng Qian $^{1}$, Kevin G. Stanley $^{2}$, Zohaib Aziz $^{1}$, Umair Aziz $^{1}$, and Steven D. Siciliano $^{3}$

$^{1}$ University of Saskatchewan, Computer Science, Saskatoon, S7N5C9, Canada  
$^{2}$ University of Victoria, Computer Science, Victoria, V8W282, Canada  
$^{3}$ University of Saskatchewan, Soil Science, Saskatoon, S7N5C9, Canada  

\*kevinstanley@uvic.ca  
\+these authors contributed equally to this work  

This repository contains the generator code and the analysis code for the paper "SPLANG: A synthetic Poisson-lognormal-based abundance and network generative model for microbial interaction inference algorithms".

<br/>

## Use SPLANG

### Installation

```R
## Install devtools if needed.
# install.packages("devtools")
devtools::install_github(repo="Quar/SPLANG", ref="dev", subdir="splang", build_vignettes=TRUE)
```

### Quick Start Guide

```R
# After installation
vignette('quickstart', package='SPLANG')
```

### View Docstring

```R
# After loading package
?sample.sequencing
```

### Contribution

Thank you for your interest in contributing to SPLANG! The R package is located
under the `splang` folder.


## Scripts Used for Analyses in the Paper

### Network Generator

The `./generator` folder contains scripts to generate network and observed abundance.

### Benchmark

The `./benchmark` folder contains scripts to benchmark MA, MIC, PC, PLN algorithms in parallel.


### Analysis

The `./ranal` folder contains R scripts to analyze benchmark results and generate plots.

The `./result` folder contains scripts to parse benchmark results stored in .Rdata to CSVs.
