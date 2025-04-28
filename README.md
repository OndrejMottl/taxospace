

# taxospace

<!-- badges: start -->

[![R-CMD-check](https://github.com/OndrejMottl/taxospace/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OndrejMottl/taxospace/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/OndrejMottl/taxospace/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OndrejMottl/taxospace?branch=main) <!-- badges: end -->

The goal of **taxospace** is to to classify any sting of taxa to a taxonomic rank.

The **taxospace** package uses the {[taxize package](https://docs.ropensci.org/taxize/index.html)} to get the most probable taxa name, which is then aling to the GBIF taxonomy backbone.

## Installation

You can install the development version of taxospace from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("OndrejMottl/taxospace")
```

## Classify taxa

``` r
# Attach the package
library(taxospace)

# get classification of a single taxa
classification_homo_sapiens <-
  get_classification("Homo sapiens")

classification_homo_sapiens$classification
#> [[1]]
#> # A tibble: 7 × 3
#>   name         rank         id
#>   <chr>        <chr>     <int>
#> 1 Animalia     kingdom       1
#> 2 Chordata     phylum       44
#> 3 Mammalia     class       359
#> 4 Primates     order       798
#> 5 Hominidae    family     5483
#> 6 Homo         genus   2436435
#> 7 Homo sapiens species 2436436

# Or a vector with several taxa
data_classification <-
  get_classification(c("Betula", "Canis lupus"))

data_classification$classification
#> [[1]]
#> # A tibble: 6 × 3
#>   name          rank         id
#>   <chr>         <chr>     <int>
#> 1 Plantae       kingdom       6
#> 2 Tracheophyta  phylum  7707728
#> 3 Magnoliopsida class       220
#> 4 Fagales       order      1354
#> 5 Betulaceae    family     4688
#> 6 Betula        genus   2875008
#> 
#> [[2]]
#> # A tibble: 7 × 3
#>   name        rank         id
#>   <chr>       <chr>     <int>
#> 1 Animalia    kingdom       1
#> 2 Chordata    phylum       44
#> 3 Mammalia    class       359
#> 4 Carnivora   order       732
#> 5 Canidae     family     9701
#> 6 Canis       genus   5219142
#> 7 Canis lupus species 5219173
```
