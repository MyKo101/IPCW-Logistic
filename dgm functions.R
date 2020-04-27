## Data Generating Functions


rPHtimes <- function(LP=0,k=0)
{
  # Generates random survival times based on a Linear Predictor
  # and an exponent (k) with a hazard function
  # h(t) = e^(LP) * t^k
  
  if(k==-1) stop("Baseline hazard cannot equal 1/t")
  
  
  if((length(k) %% length(LP) != 0) & (length(LP) %% length(k) != 0))
    stop("Lengths of k and LP are not multiples of one another")
  
  n <- max(length(LP),length(k))
  
  (-(k+1)*log(runif(n))*(exp(-LP)))^(1/(k+1))
}

Get.KM <- function(.tbl,time,event,t0)
{
  #Generates a Kaplan-Meier curve for the data
  # with time and event variables passed into the Surv function
  # in .tbl at the times specified in t.vect
  
  Frm.s <- paste0("Surv(",substitute(time),",",substitute(event),") ~ 1")
  Frm <- as.formula(Frm.s)
  
  
  
  .tbl %>%
    survfit(Frm,data=.) %>%
    summary %$%
    tibble(time = time,
           surv = surv,
           se = std.err) %>%
    bind_rows(tibble(time=t0,surv=NA,se=NA)) %>%
    arrange(time) %>%
    fill(surv,se) %>% 
    mutate(surv = replace_na(surv,1),
           se = replace_na(se,0)) %>%
    filter(time %in% t0) %>%
    unique %>%
    transmute(time,
              Observed = 1-surv,
              Observed.se = se)
}

Get.Pseudo <- function(.tbl,time,event,t0)
{
  # Pipeable wrapper of pseudosurv(time,event,t.vect)
  #
  # Runs the pseudosurv function on the .tbl
  # where time and event are the relevant variables
  # and t.vect is the times we want to find
  
  x <- pull(.tbl,!!enquo(time))
  e <- pull(.tbl,!!enquo(event))
  
  pseudosurv(x,e,t0) %>%
    extract2("pseudo") %>%
    as_tibble %>%
    set_names(paste0("time_",1:length(t0))) %>%
    mutate_all(~(1-.)) %>%
    mutate(id = .tbl$id) %>%
    select(id,everything())
}





### Result generating function
#
# To be acted upon a one row tibble
# containing all relevant parameters


Gen.Results <- function(X,nt=100)
{
  #X must contain the following parameters:
  # b - Beta: linear predictor for the event-of-interest
  # g - Gamma: linear predictor for the censoring event
  # e - Eta: exponent of the hazard function
  # iter - Current iteration
  # seed - a random seed
   
  time.start <- Sys.time()
  
  #Extract the parameters
  be <- X$b
  ga <- X$g
  et <- X$e
  
  set.seed(X$seed)
  
  #Generate the population
  df <- tibble(id=1:n,
               z = rnorm(n),
               t = rPHtimes(be*z,et),
               c = rPHtimes(ga*z,0)) %>%
    mutate(x = pmin(t,c),
           e = x == t) %>%
    select(id,z,x,e)
  
  #Create the Modelling/Censoring functions
  F_P <- function(t,z) Drop.Extremes(1-exp(-exp(be*z)*(t^(et+1))/(et+1)))
  F_O <- function(t,z) Drop.Extremes(miscal(F_P(t,z),-0.2))
  F_U <- function(t,z) Drop.Extremes(miscal(F_P(t,z),0.2))
  
  G <- function(t,z) exp(-exp(ga*z)*t)
  
  #Get the times to assess at
  t.vect <- df %>%
    filter(e) %>%
    pull("x") %>%
    quantile(c(0.25,0.75)) %>%
    seq_between(nt)
  
  #Note that the KM curve here is *observed* (i.e. failures)
  # so it is actally 1-KM
  KM_Curve <- Get.KM(df,x,e,t.vect)
  
  
  #Get the Pseudo values. This is where the large matrix comes from
  Pseudo_All <- Get.Pseudo(df,x,e,t.vect)
  
  Res <- NULL
  
  for(it in 1:nt)
  {
    t0 <- t.vect[it]
    
    #Get the current population
    cdf <- df %>%
      mutate(Observed = (t0 >= x & e),
             Censored = !(t0 <= x | e),
             IPCW = 1/G(pmin(x,t0),z))
    
    #Split the population into the outcomes predicted by the three models
    cdf_P <- cdf %>% mutate(Expected = F_P(t0,z))
    cdf_U <- cdf %>% mutate(Expected = F_U(t0,z))
    cdf_O <- cdf %>% mutate(Expected = F_O(t0,z))
    
    #Pull out the current KM and pseudo values
    cKM <- KM_Curve %>% filter(time == t0)
    cPseudo <- Pseudo_All %>%
      select(id,!!paste0("time_",it)) %>%
      set_names(c("id","Pseudo_F"))
    
    #Get the calibration results for all three models
    Res <- list(
      Get_Calib_All(cdf_P,cKM,cPseudo,"_P"),
      Get_Calib_All(cdf_U,cKM,cPseudo,"_U"),
      Get_Calib_All(cdf_O,cKM,cPseudo,"_O")
    ) %>%
      reduce(full_join,by="Method") %>%
      mutate(it = it) %>%
      bind_rows(Res)
    
    
  }
  
  #Save the results.
  
  .out <- Res %>% 
    pivot_wider(names_from=c("Method"),values_from = c(starts_with("Est"),starts_with("SE"))) %>%
    Save_Sim_Result(X,time.start,"All Results") %>%
    Save_Errors(X,"Errors")
  
  #The output is whether or not the run was completely successful
  return(.out)
  
  
}


Gen.Results_try <- function(X,nt=100)
{
  #A wrapper function around Gen.Results
  # to catch any errors
  tryCatch(
    {
      res <- Gen.Results(X,nt)
    },
    error = function(e)
    {
      res <- tibble(All=NA) %>%
        Save_Errors(X,"Errors")
    }
    
  )
  return(res)
}
