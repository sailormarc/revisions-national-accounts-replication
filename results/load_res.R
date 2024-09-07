library(dplyr)

# load paper replications
# Time horizon: same as in paper
# Definition of final revision: same as in paper 
load("results/res_pth_fr1.RData")

# load time updated paper replication
# Time horizon: up to updated time horizon
# Definition of final revision: same as in paper 
load("results/res_cth_fr1.RData")

# load paper replications with different final revision
# Time horizon: same as in paper
# Definition of final revision: instead of paper definition, final vintage
load("results/res_pth_fr2.RData")
