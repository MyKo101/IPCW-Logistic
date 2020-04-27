
Save_Sim_Result <- function(res,X,time.start,dir=".")
{
  time.end <- time.start %>%
    difftime(Sys.time(),units="s") %>%
    as.numeric %>%
    abs %>%
    multiply_by(1000) %>%
    round
    
  .filename <- paste0(dir,"/Result",
                      "_b(",X$b,")",
                      "_g(",X$g,")",
                      "_e(",X$e,")",
                      "_i(",X$iter,")",
                      "_s(",X$seed,")",
                      "_t(",time.end,")",
                      ".csv")
  
  res %>% 
    arrange(it) %>%
    select(-it) %>%
    write_csv(.filename)
    
  return(res)
}

Save_Errors <- function(res,X,dir=".")
{
  #Checks the results for any errors (i.e. NAs)
  .out <- F
  if(any(is.na(res)))
  {
    #If there are errors, it will save them in a separate directory
    .filename <- paste0(dir,"/Result",
                        "_b(",X$b,")",
                        "_g(",X$g,")",
                        "_e(",X$e,")",
                        "_i(",X$iter,")",
                        "_s(",X$seed,")",
                        ".csv")
    
    which(is.na(res),arr.ind=T) %>%
      as_tibble() %>%
      transmute(it = row,
                Var= names(res)[col]) %>%
      write_csv(.filename)
    
    .out <- T
  }
  
  #Output is whether or not an error was found
  return(.out)
}

Load_Done <- function(dir=".")
{
  ff <- list.files(dir)
  if(length(ff) > 0) 
  {
    .out <- ff %>%
      tibble(ff=.) %>%
      mutate(b = "b",g="g",e="e",iter="i",time.taken="t") %>%
      mutate_at(vars(b,g,e,iter,time.taken),
                ~paste0("(?<=",.,"\\().+?(?=\\))") %>%
                  str_extract(ff,.) %>%
                  as.numeric) %>%
      mutate(fn = paste0(dir,"/",ff)) %>%
      mutate(n = map_int(as.list(fn),~nrow(read_csv(.,col_types = cols())))) %>%
      filter(n==nt) %>%
      select(b,g,e,iter,time.taken)
      
    
    
  } else 
  {
    .out <- tibble(g=numeric(),
                   b=numeric(),
                   e=numeric(),
                   iter=numeric(),
                   time.taken=numeric())
  }
  return(.out)
}

Load_Errors <- function(dir=".")
{
  ff <- list.files(dir)
  
  if(length(ff) > 0) 
  {
    .out <- ff %>%
      tibble(ff=.) %>%
      mutate(b = "b",g="g",e="e",iter="i",seed="s") %>%
      mutate_at(vars(b,g,e,iter,seed),
                ~paste0("(?<=",.,"\\().+?(?=\\))") %>%
                  str_extract(ff,.) %>%
                  as.numeric) %>%
      select(-ff)
  } else {
    .out <- tibble(g=numeric(),
                   b=numeric(),
                   e=numeric(),
                   iter=numeric(),
                   seed=numeric())
    
  }
  
  
  return(.out)
  
}

Load_Sim_Results <- function(X,dir=".")
{
  #Loads the sim results matching the
  #  b, g & e variables in the X tibble
  
  if(nrow(X) > 1) X <- X[1,]

  ff <- list.files(dir)
  
  regex.search <- paste0("_b(",X$b,")",
                         "_g(",X$g,")",
                         "_e(",X$e,")")
  
  ff <- ff[grepl(regex.search,ff,fixed=T)]
  
  ff %>%
    paste0(dir,"/",.) %>%
    map_dfr(~read_csv(.,col_types = cols()) %>%
          mutate(it = 1:nrow(.)),
          .id="iter") %>%
    pivot_longer(cols=c(-it,-iter),
                 names_to=c("Est","Model","Method"),names_sep="_",
                 values_to=c("Value")) %>%
    pivot_wider(names_from="Est",
                values_from="Value")
  
}

Save_Aggregate_Results <- function(X,All_dir=".",Agg_dir=".")
{
  Y <- Load_Sim_Results(X,All_dir) %>%
    Get_PMs %>%
    select(-N) %>%
    pivot_longer(cols=c(ends_with("Est"),ends_with("SE")),
                 names_to=c("Measure","Est"),names_sep="_",
                 values_to="Value") %>%
    pivot_wider(names_from=c("Method","Measure","Est"),
                names_sep="_",
                values_from="Value")
  
  
  .filename <- paste0("Aggregate",
                      "_b(",X$b,")",
                      "_g(",X$g,")",
                      "_e(",X$e,")",
                      ".csv")
  
  write_csv(Y,paste0(Agg_dir,"/",.filename))
  
  return(X)
}

  
  
  
  
  



