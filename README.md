
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ageproR

<!-- badges: start -->
<!-- badges: end -->

ageproR is a R-package designed to handle input data to and from Jon
Brodizak’s AGEPRO (Age Structured Projection Model).

***ageproR** is still in early development. Code base may change without
warning prior to first stable release.*

If you use AGEPRO for production, please use:

- [Jon Brodziak’s original AGEPRO program (source
  code)](https://github.com/PIFSCstockassessments/AGEPRO)
- [GUI interface for AGEPRO (installer &
  source)](https://github.com/PIFSCstockassessments/AGEPRO-GUI)

## Installation

You can install the development version of ageproR from
[PIFSCstockassessments GitHub
Repository](https://github.com/PIFSCstockassessments/ageproR) with:

``` r
# install.packages("remotes")
remotes::install_github("PIFSCstockassessments/ageproR")
```

## Reading AGEPRO input files

Note: **ageproR** is currently incompatible with the supported AGEPRO
input file format (`AGEPRO VERSION 4.0` & `AGEPRO VERSION 4.2`).
Included example AGEPRO input file (`inst/test-example4.inp`) is used to
demonstrate it’s current implementation.

Loading a AGEPRO Input File:

``` r
library(ageproR)
## basic example code
test <- agepro_inp_model$new()

test$read_inp("inst/test-example4.inp")
#> Check Version
#> line 1:
#> inp_line:AGEPRO VERSION 4.2
#> line 2: [CASEID]
#> Read Case ID at line 2 ...
#> Line 3: Case ID: Test EXAMPLE4 - Uku Projection Base (2019-2026)
#> line 4: [GENERAL]
#> → Line 5 ...
#> • First Year in Projection: 2019
#> • Last Year in Projection: 2026
#> • First Age Class: 1
#> • Last Age Class: 32
#> • Number of Population Simulations: 1000
#> • Number of Fleets: 4
#> • Number of Recruitment Model(s): 3
#> • Discards are present: FALSE
#> • Calculation Engine Random Number Seed: 300
#> line 6: [RECRUIT]
#> → Setting Recruitment data for 2019 - 2026 ...
#> → Line 7 : Recruit/SSB Scaling Factors & max recruit obs ...
#> 1000, 1, and 500
#> → Line 8: Recruitment model number: 5, 3, and 3
#> ℹ Reading Recruitment Probabaility
#> → Line 9: Recruitment probabaility for year 2019 : 0.6, 0.2, and 0.2
#> → Line 10: Recruitment probabaility for year 2020 : 0.6, 0.2, and 0.2
#> → Line 11: Recruitment probabaility for year 2021 : 0.6, 0.2, and 0.2
#> → Line 12: Recruitment probabaility for year 2022 : 0.6, 0.2, and 0.2
#> → Line 13: Recruitment probabaility for year 2023 : 0.6, 0.2, and 0.2
#> → Line 14: Recruitment probabaility for year 2024 : 0.6, 0.2, and 0.2
#> → Line 15: Recruitment probabaility for year 2025 : 0.6, 0.2, and 0.2
#> → Line 16: Recruitment probabaility for year 2026 : 0.6, 0.2, and 0.2
#> ℹ Recruitment Probability:
#> [[1]]
#> 2019 2020 2021 2022 2023 2024 2025 2026 
#>  0.6  0.6  0.6  0.6  0.6  0.6  0.6  0.6 
#> 
#> [[2]]
#> 2019 2020 2021 2022 2023 2024 2025 2026 
#>  0.2  0.2  0.2  0.2  0.2  0.2  0.2  0.2 
#> 
#> [[3]]
#> 2019 2020 2021 2022 2023 2024 2025 2026 
#>  0.2  0.2  0.2  0.2  0.2  0.2  0.2  0.2
#> → Reading recruitment model #5
#> → Line 17 ...
#> Beverton-Holt Curve w/ Lognormal Error
#> • Alpha: 81.1
#> • Beta: 157.2
#> • Variance: 0.1521
#> → Reading recruitment model #3
#> → Line 18: Observed points : 71...
#> → Line 19 Observations ...
#> # A tibble: 71 × 1
#>    recruit
#>      <dbl>
#>  1    71.3
#>  2    65.4
#>  3    66.4
#>  4    62.8
#>  5    55.9
#>  6    53.4
#>  7    48.1
#>  8    47.6
#>  9    50.7
#> 10    48.4
#> 11    48.8
#> 12    51.3
#> 13    51.7
#> 14    52.2
#> 15    50.7
#> 16    55.5
#> 17    55.5
#> 18    61.7
#> 19    59.7
#> 20    56.1
#> 21    66.6
#> 22    71.2
#> 23    71.5
#> 24    98.9
#> 25    90.3
#> 26    62.5
#> 27    54.9
#> 28    70.2
#> 29    88.8
#> 30    65.4
#> 31    61.7
#> 32    73.8
#> 33    63.5
#> 34    75.5
#> 35    62.8
#> 36   109. 
#> 37    85.6
#> 38    73.1
#> 39    51.2
#> 40    52.6
#> 41    69.8
#> 42   133. 
#> 43    54.0
#> 44    60.0
#> 45    64.2
#> 46    58.0
#> 47    54.4
#> 48    65.1
#> 49    85.4
#> 50    74.5
#> 51    64.4
#> 52    65.1
#> 53    59.0
#> 54    59.7
#> 55    65.9
#> 56    94.6
#> 57   142. 
#> 58   101. 
#> 59    81.5
#> 60    70.9
#> 61    73.8
#> 62    92.8
#> 63    91.5
#> 64   111. 
#> 65   102. 
#> 66    89.8
#> 67   149. 
#> 68   109. 
#> 69    70.9
#> 70    69.7
#> 71    72.8
#> → Reading recruitment model #3
#> → Line 20: Observed points : 18...
#> → Line 21 Observations ...
#> # A tibble: 18 × 1
#>    recruit
#>      <dbl>
#>  1    59.7
#>  2    65.9
#>  3    94.6
#>  4   142. 
#>  5   101. 
#>  6    81.5
#>  7    70.9
#>  8    73.8
#>  9    92.8
#> 10    91.5
#> 11   111. 
#> 12   102. 
#> 13    89.8
#> 14   149. 
#> 15   109. 
#> 16    70.9
#> 17    69.7
#> 18    72.8
#> line 22: [BOOTSTRAP]
#> → Line 23:
#> Number of Bootstraps: 100
#> Population Scale Factor (BootFac): 1000
#> → Line 24:
#> ! Bootstrap file path does not exist in system: "C:\\Users\\Jon.Brodziak\\Documents\\AGEPRO\\Example\\Example1.BSN"
#> There were warnings raised when reading this file:
#> simpleWarning: 'C:\Users\Jon.Brodziak\Documents\AGEPRO\Example\Example1.BSN' does not exist. 
#> Please provide a vaild bootstrap filepath when saving to input file for the AGEPRO calcuation engine.
#> Finished reading to file.
```

To set (or correct) bootstrap filename

``` r
test$set_bootstrap_filename("inst/Example1.BSN")
#> ✔ Bootstrap file: "inst/Example1.BSN"
```

## Links

Impl

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
