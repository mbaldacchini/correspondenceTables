<!-- badges: start -->
[![last commit](https://img.shields.io/github/last-commit/eurostat/correspondenceTables?style=flat)](https://github.com/eurostat/correspondenceTables/commits/)
[![R build
status](https://github.com/eurostat/correspondenceTables/workflows/R-CMD-check/badge.svg)](https://github.com/eurostat/correspondenceTables/actions)
[![dependencies](https://tinyverse.netlify.com/badge/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN version](https://www.r-pkg.org/badges/version/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![CRAN status](https://badges.cranchecks.info/flavor/r-release-linux-x86_64/correspondenceTables.svg)](https://cran.r-project.org/web/checks/check_results_correspondenceTables.html)
[![license](https://img.shields.io/badge/license-EUPL-success)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![weekly downloads](https://cranlogs.r-pkg.org/badges/last-week/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![monthly downloads](https://cranlogs.r-pkg.org/badges/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
[![all downloads](https://cranlogs.r-pkg.org/badges/grand-total/correspondenceTables)](https://CRAN.R-project.org/package=correspondenceTables)
<!-- badges: end -->

# CorrespondenceTables

Empowering the seamless creation and refinement of correspondence tables between two statistical classifications, such as NACE, CPA, CN, and more.
This toolkit not only streamlines the process of generating and updating these tables but also provides functionality for retrieving classification and correspondence tables.
It includes features for conducting quality control on classifications and offers a comprehensive analysis of correspondence tables with the ability to aggregate them effectively.

## Installation

'correspondenceTables' can be installed from [CRAN](https://CRAN.R-project.org/package=correspondenceTables) by 

```R
install.packages("correspondenceTables")
```

or use the development version from GitHub

```R
devtools::install_github("eurostat/correspondenceTables", build_vignettes = TRUE)
```

## Background

This package serves as a tool to assist you in creating and updating a correspondence table between two classifications, such as NACE, CPA, CN, and others. It enables the retrieval of classification tables and correspondence tables. Additionally, it facilitates quality control on a classification and allows for the analysis and aggregation of correspondence tables.

## Content

The package contains 11 functions:

1. aggregateCorrespondenceTable	Aggregate values from classification A to classification B
2. analyseCorrespondenceTable	Perform analysis on correspondence tables
3. classificationList	List available classification schemes from CELLAR or FAO
4. classificationQC	Perform quality control on a classification
5. correspondenceTableList	List available correspondence tables from online services
6. dataStructure	Retrieve a classification structure (levels and concepts) from a supported service
7. newCorrespondenceTable	Correspondence table creation
8. prefixList	Build SPARQL PREFIX declarations for an endpoint
9. retrieveClassificationTable	Retrieve a full classification table from CELLAR or FAO
10. retrieveCorrespondenceTable	Download a correspondence (mapping) table between two classifications
11. updateCorrespondenceTable	Correspondence table creation

## Examples

For the examples see the vignettes.
```R
browseVignettes("correspondenceTables")
```
