### Load what we need:

rm(list=ls())
pkgs <- c("readr","tibble","dplyr","magrittr",
          "survival","purrr","tidyr","rlang",
          "furrr","stringr","pseudo")

invisible(purrr::walk(pkgs,library,character.only=T))

source("util functions.R")
source("calibration functions.R")
source("save and load functions.R")
source("dgm functions.R")
source("aggregating functions.R")


### Global Parameters to use ####################

Run_parallel <- 3 #Will be set to the max of this and Cores - 1

n <- 10000 #Population size
N <- 100 #number of sims
nt <- 100 #Number of time points

# Vector of what parameters to try (full list)
g.list <- (-4:4)/2
b.list <- setdiff(g.list,0) 
e.list <- (-1:1)/2


## We're going to prioritise these first:

g.list <- (-2:2)/2
b.list <- 1
e.list <- 0.5


###############################################


#Sometimes the GLMs give warnings, these aren't informative anymore
options(warn=-1)

memory.limit(20000)
#Pseudo function requires large matrices,
# so we need to increase the RAM eaten by R

#Limit Cores to try to use
Run_parallel %<>% min(parallel::detectCores()-1)

#Load previous results (if relevant) & get avg time
Done <- Load_Done("All Results")

avg.time <- mean(Done$time.taken,na.rm=T)/1000

# Random, Schandom! Let's set the seed anyway
set.seed(Sys.time())

#Figure out which simulations haven't been run yet
# Generate a value to be used as a seed here for replicability
Sims.tbd <- expand(tibble(),
               b = round(b.list,2),
               g = round(g.list,2),
               e = round(e.list,2),
               iter = 1:N) %>% 
  anti_join(Done,by=c("b","g","e","iter")) %>%
  mutate(seed = round(runif(n(),0,2^31-1)))

# How long will this take?
ETA <- nrow(Sims.tbd)*avg.time/Run_parallel


if(nrow(Sims.tbd) > 0)
{
  #Print some preliminary diagnositics
  
  cat("\nAverage Time per simulation: ",round(avg.time,4)," seconds",
      "\nNumber of simulations to run: ",nrow(Sims.tbd),
      "\nTotal Estimated Time: ", round(ETA,4)," seconds",
      "\nEstimated Time of Completion: ",as.character(Sys.time() + ETA),"\n\n\n")
  
  #Set the parallel processing plan:
  if(Run_parallel==1) plan(sequential) else plan(multiprocess,workers=Run_parallel) 
  
  Sims.tbd %>%
    sample_n(nrow(.)) %>% 
    arrange(iter) %>% 
    split(1:nrow(.)) %>% 
    future_map_lgl(Gen.Results_try,nt=nt,
                   .options=future_options(packages=pkgs),
                   .progress=T) %>%
    any
} else Sims <- NULL

# Aggregate the data into Performance Metrics

Get_All_PM("All Results","Aggregate Results")




