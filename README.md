
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ageproR

<!-- badges: start -->
<!-- badges: end -->

ageproR is a R-package designed to handle input data to and from Jon
Brodizak’s AGEPRO (Age Structured Projection Model).

***ageproR** is still in early development. Code base may change without
warning prior to first stable release.*

**If you using AGEPRO for production, please use:**

- [Jon Brodziak’s original AGEPRO program (source
  code)](https://github.com/PIFSCstockassessments/AGEPRO)
- [GUI interface for AGEPRO (installer &
  source)](https://github.com/PIFSCstockassessments/AGEPRO-GUI)

## Installation

You can install the development version of ageproR from
[PIFSCstockassessments GitHub
Repository](https://github.com/PIFSCstockassessments/ageproR) with:

``` r

install.packages("remotes")
remotes::install_github("PIFSCstockassessments/ageproR")

# Alternative method via `pak`
install.packages("pak")
pak::pkg_install("PIFSCstockassessments/ageproR")
```

## AGEPRO input file

*Note: **ageproR** is currently incompatible with the supported AGEPRO
input file format* (`AGEPRO VERSION 4.0` & `AGEPRO VERSION 4.2`).
*Included example AGEPRO input file* (`inst/test-example4.inp`) *is used
to demonstrate it’s implemented features.*

## Vignettes

Hawaiian Uku Projection Projection base example (TODO)

<!-- Do not edit below. This adds the Disclaimer and NMFS footer. -->

------------------------------------------------------------------------

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an ‘as is’ basis and the user assumes responsibility for its
use. DOC has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed by
all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.”

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" width="200" style="height: 75px !important;"  alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
