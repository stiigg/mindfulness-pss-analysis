# Installs required CRAN packages
need <- c(
  "tidyverse","readxl","janitor","lubridate","broom","broom.mixed","stringr",
  "skimr","naniar","effectsize","parameters","performance","see","patchwork",
  "mice","lme4","lmerTest","splines","ggeffects","marginaleffects","TOSTER",
  "ggdist","cowplot","scales","modelsummary","yaml"
)
inst <- setdiff(need, rownames(installed.packages()))
if(length(inst)) install.packages(inst, dependencies = TRUE)
message("Packages ready.")
