
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script master_script_kela65.R         ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: The order in which R scripts should be run (kela65).
rm(list=ls())

# R version 4.2.1.
# Install and load the following packages:
library(here)                 # Relative file paths.
library(data.table)           # mutating and aggregating data
library(tictoc)               # timing 
library(fixest)               # Fast fixed effects estimation.
library(ggplot2)              # Creating plots.
library(viridis)              # accessible color palette.
library(patchwork)            # Combining plots.
library(stargazer)            # Save as tex file.

writeLines(capture.output(sessionInfo()), 'sessionInfo_kela65.txt')

###
###


# Data extraction and cleaning:
#source(file = "master_script_datainfra.R")

# Extract study population and construct socioeconomic features.
source(file = here('scripts', '1_study_population.R'))
# Running time: 2 mins

# Construct outcomes and panel data for DID analyses.
source(file = here('scripts', '2_panel_data.R'))
# Running time: 20 mins

# Plot trends plots and DID plots.
source(file = here('scripts', '3_did_results.R'))
# Running time: 7 mins

# End.
