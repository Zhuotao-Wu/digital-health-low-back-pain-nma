# Comparative effectiveness of digital health interventions for low back pain: systematic review and network meta-analysis of randomised controlled trials

## Overview

This repository contains the R code and analytical outputs for a frequentist network meta-analysis. It includes the primary analysis, two sensitivity analyses, and a subgroup analysis.

The study-level input dataset is not included in this public repository. The analytical code, input-data specification, software environment information, and generated results are provided to support transparent reporting and evaluation of the analysis.

## Analyses

The repository contains code for:

1. Primary network meta-analysis.
2. Sensitivity analysis excluding studies judged to be at high risk of bias.
3. Sensitivity analysis restricted to outcomes reported using means and standard deviations.
4. Subgroup analysis.

Seven outcomes are evaluated:

- Pain intensity
- Physical function
- Pain-related fear avoidance
- Health-related quality of life
- Anxiety
- Depression
- Self-efficacy

Where data are available, analyses are conducted at four follow-up periods: post-intervention, short term, mid term, and long term. Outcome–time combinations with insufficient data are skipped automatically.


## Outputs

Depending on data availability and network connectivity, the workflows generate:

- Network plots in SVG and JPEG formats.
- Forest plots in SVG and JPEG formats.
- League tables in Excel format.
- P-score tables in Excel format.
- Model estimates, heterogeneity statistics, and consistency assessments in Excel format.
- A record of the R session and package versions.

The files in `results/` are generated outputs and do not replace the private input dataset.

## Data availability

The study-level dataset used for this network meta-analysis is not publicly available. It may be made available by the corresponding author on reasonable request, subject to approval by the study team and any applicable data-use conditions.


## Reporting

The accompanying systematic review and network meta-analysis should be reported in accordance with PRISMA 2020 and the applicable extension for network meta-analyses.


## Contact

For questions about the analysis or requests concerning the dataset, contact:

C Chen chenchan@scu.edu.cn
