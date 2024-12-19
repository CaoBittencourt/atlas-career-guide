modular::project.options('atlas')
getOption("atlas.db") |> setwd()
library(dplyr)
library(tidyr)
read.csv('output/skill_sets.csv') -> skill_set_mtx
read.csv('output/education.csv') -> education
read.csv('output/labor.csv') -> labor

skill_set_mtx
education
labor
