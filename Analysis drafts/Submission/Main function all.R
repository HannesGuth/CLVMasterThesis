# DESCRIPTION

# Call the main functions

################################################################

install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source(paste0(getwd(), "/Main function one.r"))
source(paste0(getwd(), "/Main function periods.r"))