# NicheBarcoding

This is a quick guide to getting started with the two main functions.
Complete examples can also be found in the help documentation of each functions.
Users can also read the manual to learn more.

## Installation

You can install the stable released version of `{NicheBarcoding}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Yangcq-Ivy/NicheBarcoding")
```

## Functions

This package provides three primary verbs:

-   `NBSI` and `NBSI2` functions execute the main identification
    integrating both DNA barcoding and ecological niche modeling.
-   `extractSpeInfo`, `niche.Model.Build`, `pseudo.present.points` 
    and `pseudo.absent.points` functions are the extractable intermediate
    steps of `NBSI` and `NBSI2` that execute information extraction,
    niche modeling and pseudo points generation.
-   `monophyly.prop`, `spe.mantel.test` and `niche.PCA` functions 
    execute the analysis of the characteristics of reference or/and query 
    datasets, including the phylogenetic monophyletic proportion, the
    correlation between interspecific pairwise genetic distance and 
    ecological distance, and the principal component analysis of ecological
    niche between datasets.


## Usage

``` r
rm(list=ls())
library(NicheBarcoding)
```

Load the example bioclimatic layers first for the subsequent processes

``` r
data(en.vir)
data(bak.vir)
```

or if you want to download the complete bioclimatic layers from online 
[`worldclim`](https://www.worldclim.org/), run:

``` r
envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
en.vir<-raster::brick(envir)

# Generate random background points
back<-dismo::randomPoints(mask=en.vir,n=5000,ext=NULL,extf=1.1,
                          excludep=TRUE,prob=FALSE,
                          cellnumbers=FALSE,tryf=3,warn=2,
                          lonlatCorrection=TRUE)
bak.vir<-raster::extract(en.vir,back)
```
