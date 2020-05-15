
Save_Sim_Result <- function(res,X,time.start,All_dir=".")
{
  time.end <- time.start %>%
    difftime(Sys.time(),units="s") %>%
    as.numeric %>%
    abs %>%
    multiply_by(1000) %>%
    round
    
  .filename <- paste0(All_dir,"/Result",
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

Save_Errors <- function(res,X,Err_dir=".")
{
  #Checks the results for any errors (i.e. NAs)
  .out <- F
  if(any(is.na(res)))
  {
    #If there are errors, it will save them in a separate directory
    .filename <- paste0(Err_dir,"/Result",
                        "_b(",X$b,")",
                        "_g(",X$g,")",
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

Load_Done <- function(All_dir=".",nt)
{
  ff <- list.files(All_dir)
  if(length(ff) > 0) 
  {
    .out <- ff %>%
      tibble(ff=.) %>%
      mutate(b = "b",g="g",e="e",iter="i",time.taken="t") %>%
      mutate_at(vars(b,g,e,iter,time.taken),
                ~paste0("(?<=",.,"\\().+?(?=\\))") %>%
                  str_extract(ff,.) %>%
                  as.numeric) %>%
      mutate(fn = paste0(All_dir,"/",ff)) %>%
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
Load_Sim_Results <- function(X,All_dir=".")
{
  #Loads the sim results matching the
  #  b, g & e variables in the X tibble
  
  if(nrow(X) > 1) X <- X[1,]

  ff <- list.files(All_dir)
  
  regex.search <- paste0("_b(",X$b,")",
                         "_g(",X$g,")",
                         "_e(",X$e,")")
  
  ff <- ff[grepl(regex.search,ff,fixed=T)]
  
  ff %>%
    paste0(All_dir,"/",.) %>%
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
  cat("\n\nAggregating b=",X$b," g=",X$g," e=",X$e)
  Y <- Load_Sim_Results(X,All_dir) %>%
    mid_frac_by(0.9,it,Method,Model) %>%
    Get_PMs %>%
    filter(N>5) %>%
    select(-N) %>%
    pivot_longer(cols=c(ends_with("Est"),ends_with("SE")),
                 names_to=c("Measure","Est"),names_sep="_",
                 values_to="Value") %>%
    pivot_wider(names_from=c("Method","Measure","Est"),
                names_sep="_",
                values_from="Value")
  cat("\nAggregated b=",X$b," g=",X$g," e=",X$e)

  if(nrow(Y)> 1)
  {
    .filename <- paste0("Aggregate",
                        "_b(",X$b,")",
                        "_g(",X$g,")",
                        "_e(",X$e,")",
                        ".csv")
    
    write_csv(Y,paste0(Agg_dir,"/",.filename))
  
  }
  cat("\nSaved b=",X$b," g=",X$g," e=",X$e)
  return(X)
}

Load_Done_Summary <- function(Agg_dir=".")
{
  Agg_dir %>%
    paste0("/00-Done.csv") %>%
    read_csv(col_types=cols())
  
}

Save_Plot<-function(b,g,e,p,Plot_dir=".")  
{
  .filename <- paste0("MainPlot",
                      "_b(",b,")",
                      "_g(",g,")",
                      "_e(",e,")",
                      ".png") %>%
    paste0(Plot_dir,"/",.)
  
  
  ggsave(.filename,p,dpi=500,width=20,height=10,units="cm")
  
  return(.filename)
  
}

Save_Plot_tibble <- function(X,Plot_dir=".")
{
  Save_Plot(X$b,X$g,X$e,X$p[[1]],Plot_dir=Plot_dir)
}
  

Save_Plot_ranges <- function(Agg_dir=".")
{
  list.files(Agg_dir) %>%
    .[grep("^Aggregate",.)] %>%
    paste0(Agg_dir,"/",.) %>%
    map_dfr(~read_csv(.,col_types=cols()) %>%
              pivot_longer(cols=-c(Model,it),
                           names_to=c("Method","Measure","Est"),
                           names_sep="_",
                           values_to="values") %>%
              mutate(Slope = if_else(grepl(".Slope",Method),"Only","None")) %>%
              select(Measure,Slope,values) %>%
              group_by(Measure,Slope) %>%
              summarise(file.min=min(values,na.rm=T),
                        file.max=max(values,na.rm=T))
              ) %>% 
    bind_rows(mutate(.,Slope = "All")) %>%
    bind_rows(tibble(Measure = c("Coverage","Coverage","Bias","EmpSE"),
                     file.min = c(0,1,0,0),
                     file.max = c(0,1,0,0)) %>% 
                group_by_all %>%
                expand(Slope = c("None","All","Only"))) %>%
    group_by(Measure,Slope) %>%
    filter(file.min >-10 & file.max < 10) %>%
    summarise(min = min(file.min),max=max(file.max)) %>%
    mutate(abs.max = pmax(abs(min),abs(max)),
           min = if_else(Measure == "Bias",-abs.max,min),
           max = if_else(Measure == "Bias",abs.max,max)) %>%
    select(-abs.max) %>%
    write_csv(paste0(Agg_dir,"/00-Ranges.csv"))
}
  



