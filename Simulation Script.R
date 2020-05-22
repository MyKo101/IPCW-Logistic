### Load what we need:

rm(list=ls())
if(Sys.info()[1] == "Linux")
  cluster <- T else cluster <- F


pkgs <- c("readr","tibble","dplyr","magrittr",
          "survival","purrr","tidyr","rlang",
          "furrr","stringr","pseudo")
if(!cluster) pkgs <- c(pkgs,"ggplot2")

invisible(purrr::walk(pkgs,library,character.only=T))


source("util functions.R")
source("calibration functions.R")
source("save and load functions.R")
source("dgm functions.R")
source("aggregating functions.R")
if(!cluster) source("plot functions.R")


### Global Parameters to use ####################

Run_parallel <- 60 #Will be set to the max of this and Cores - 1

n <- 10000 #Population size
N <- 100 #number of sims
nt <- 100 #Number of time points

# Vector of what parameters to try (full list)
g.list <- (-4:4)/2
b.list <- (-4:4)/2
e.list <- (-1:1)/2


## We're going to prioritise these first:

#g.list <- -1:1
#b.list <- -1:1
#e.list <- -1:1/2


###############################################


#Sometimes the GLMs give warnings, these aren't informative anymore
options(warn=-1)

memory.limit(20000)
#Pseudo function requires large matrices,
# so we need to increase the RAM eaten by R


#Load previous results (if relevant) & get avg time
Done <- Load_Done_Summary("Aggregate Results")

if(cluster)
{
  Done <- unique(bind_rows(Done,Load_Done("All Results",nt=nt)))
}

# output done table
with(Done,table(b,g,e))

avg.time <- mean(Done$time.taken,na.rm=T)/1000

# Random, Schandom! Let's set the seed anyway
set.seed(Sys.time())

#Figure out which simulations haven't been run yet
# Generate a value to be used as a seed here for replicability

N0 <- 100
Sims.tbd <- expand(tibble(),
               b = round(b.list,2),
               g = round(g.list,2),
               e = round(e.list,2),
               iter = 1:N) %>% 
  anti_join(Done,by=c("b","g","e","iter")) %>% 
  mutate(seed = round(runif(n(),0,2^31-1))) %>% 
  left_join(Done %>%
              group_by(b,g,e) %>%
              summarise(n=n()),
            by=c("b","g","e")) %>% 
  mutate(n = replace_na(n,0)) %>%
  filter(n < N0) %>% 
  group_by(b,g,e) %>%
  slice(1:(N0-n)) %>%
  ungroup

#How many Cores to use?
Use_Cores <- min(Run_parallel,parallel::detectCores()-1,nrow(Sims.tbd))

# How long will this take?
ETA <- nrow(Sims.tbd)*avg.time/Use_Cores


if(nrow(Sims.tbd) > 0)
{
  #Print some preliminary diagnositics
  
  cat("\nAverage Time per simulation: ",round(avg.time,4)," seconds",
      "\nNumber of simulations to run: ",nrow(Sims.tbd),
      "\nNumber of Cores to use: ",Use_Cores,
      "\nTotal Estimated Time: ", round(ETA,4)," seconds",
      "\nEstimated Time of Completion: ",as.character(Sys.time() + ETA),"\n\n\n")
  
  #Set the parallel processing plan:
  if(Use_Cores==1) plan(sequential) else plan(multiprocess,workers=Use_Cores) 
  
  Sims.tbd %>%
    sample_n(nrow(.)) %>% 
    arrange(iter) %>% 
    split(1:nrow(.)) %>% 
    future_map_lgl(Gen.Results_try,nt=nt,
                   .options=future_options(packages=pkgs),
                   .progress=T) %>%
    any
}


if(!cluster)
{
# Aggregate the data into Performance Metrics

Get_All_PM("All Results","Aggregate Results",nt)


#Generate ALL plots
plots.CIL <- Make_All_MainPlots("Aggregate Results","MainPlots","None")
plots.slopes <- Make_All_MainPlots("Aggregate Results","MainPlots","Only")
plots.all <- Make_All_MainPlots("Aggregate Results","MainPlots","All")

Thesis.plot.dir <- "C:/Users/mbrxsmbc/Documents/Thesis/MyThesis/figure/IPCW_Logistic"

tibble(b=1,g=c(-1,0,1),e=0.5) %>%
  mutate(fn = paste0("Plot_b(",b,")_g(",g,")_e(",e,").png")) %>%
  group_by_all %>%
  expand(src=c("Main","Slope")) %>%
  mutate(dir=if_else(src == "Main","None","Only")) %>%
  mutate(old_file=paste0("MainPlots/",dir,"/Main",fn),
         new_file=paste0(Thesis.plot.dir,"/",src,fn)) %>%
  ungroup %>%
  split(1:nrow(.)) %>%
  map(~{file.remove(.$new_file);return(.)}) %>%
  map(~file.copy(.$old_file,.$new_file))

}



