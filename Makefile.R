outputs <- c("HVC_1701.csv")
file.remove(outputs)

# script 0 only has to be run once on a machine, it simply installs all required packages
#source("00_installPackages.R")

## run all scripts
source("01_PGN_to_CSV.R")   ## can take up to a minute to run because of reading a big dataset
source("02_Manipulation.R")    ## analyse global terrorism using plots
source("03_rchess_package.R")     ## show terrorism on a world map
