
ggMainplot <- function(tbl,.range=NULL)
{
  if(is.null(.range))
  {
    .range <- tibble(Est=c(1,0,0,0),
                     Measure=c("Coverage","Coverage","Bias","EmpSE"))
  }
  
  .range %<>%
    group_by_all %>%
    expand(Model = c("Over-prediction","Under-prediction","Perfect")) %>%
    ungroup %>%
    mutate(it=50)
    
  ggplot(tbl) +
    geom_line(aes(it,Est,col=Method)) + 
    geom_ribbon(aes(it,ymin=LL,ymax=UL,fill=Method),alpha=0.2)+
    geom_blank(aes(it,Est),data=.range) + 
    facet_grid(rows=vars(Measure),cols=vars(Model),
               scales="free") +
    xlab("Time point") + 
    ylab("Simulation Estimate")
    
  
}

ggMainPlot_blank <- function(slope)
{
  
  Ranges <- read_csv(paste0(Agg_dir,"/00-Ranges.csv"),
                     col_types=cols()) %>%
    filter(Slope == slope) %>%
    select(-Slope) %>%
    pivot_longer(c(min,max),
                 names_to="minmax",
                 values_to="Est") %>%
    select(Measure,Est)
  
  Methods <- switch(slope,
                    None = c("KM","Weighted","Unweighted","Pseudo"),
                    Only = c("Weighted.Slope","Unweighted.Slope",
                             "Pseudo.Slope"),
                    All = c("KM","Weighted","Unweighted","Pseudo",
                            "Weighted.Slope","Unweighted.Slope",
                            "Pseudo.Slope"))
  
  tbl <- tibble(Method=Methods,
                Est=0,SE=0,LL=0,UL=0) %>%
    group_by_all %>%
    expand(Model=c("Over-prediction","Perfect","Under-prediction"),
           Measure=c("Bias","Coverage","EmpSE"),
           it=c(0,100))
  
  p <- ggMainplot(tbl,.range)
  
  return(p)
}

Prepare_Plot <- function(filename,Methods,Agg_dir)
{
  filename %>%
    paste0(Agg_dir,"/",.) %>%
    read_csv(col_types=cols()) %>%
    pivot_longer(col=-c(Model,it),
                 names_to=c("Method","Measure","Est"),names_sep="_",
                 values_to="Value") %>%
    filter(Method %in% Methods) %>%
    pivot_wider(names_from=Est,values_from=Value) %>%
    mutate(LL = Est - qnorm(0.975)*SE,
           UL = Est + qnorm(0.975)*SE) %>%
    mutate(Model = recode(Model,
                          U = "Under-prediction",
                          O = "Over-prediction",
                          P = "Perfect")) %>%
    mutate(Method=recode(Method,Pseudo="PO",Unweighted="LU",Weighted="LO",
                         Pseudo.Slope="POS",Unweighted.Slope="LUS",
                         Weighted.Slope="LOS"))
  
}

Make_MainPlot <- function(b,g,e,Agg_dir=".",slope="None")
{
  #slope values:
  #   None = Only Calibration-in-the-large results
  #   All = All results
  #   Only = Slope results only
  
  Methods <- switch(slope,
                    None = c("KM","Weighted","Unweighted","Pseudo"),
                    Only = c("Weighted.Slope","Unweighted.Slope",
                             "Pseudo.Slope"),
                    All = c("KM","Weighted","Unweighted","Pseudo",
                            "Weighted.Slope","Unweighted.Slope",
                            "Pseudo.Slope"))
  
  Ranges <- read_csv(paste0(Agg_dir,"/00-Ranges.csv"),
                     col_types=cols()) %>%
    filter(Slope == slope) %>%
    select(-Slope) %>%
    pivot_longer(c(min,max),
                 names_to="minmax",
                 values_to="Est") %>%
    select(Measure,Est)

  ff <- list.files(Agg_dir)
  
  regex.search <- paste0("_b(",b,")",
                         "_g(",g,")",
                         "_e(",e,")")
  
  ff <- ff[grepl(regex.search,ff,fixed=T)]
  
  if(length(ff) == 1)
  {
    tbl <- Prepare_Plot(ff,Methods,Agg_dir)
    
    p <- ggMainplot(tbl,Ranges)
    
    return(p)
    
    
  } else return(NULL)
}

Make_MainPlot_tibble <- function(X,Agg_dir=".",slope="None")
{
  p <- Make_MainPlot(X$b,X$g,X$e,Agg_dir=Agg_dir,slope=slope)
  
  tibble(b=X$b,g=X$g,e=X$e,p=list(p))
}

Make_All_MainPlots <- function(Agg_dir=".",Plot_dir=".",slope="None")
{
  Load_Done_Summary(Agg_dir) %>% 
    group_by(b,g,e) %>%
    summarise(n=n()) %>%
    filter(n>5) %>%
    ungroup %>% 
    split(1:nrow(.)) %>%
    map_chr(~Make_MainPlot_tibble(.,Agg_dir=Agg_dir,slope=slope) %>%
              Save_Plot_tibble(Plot_dir=paste0(Plot_dir,"/",slope)))
  
  ggMainPlot_blank(slope) %>%
    Save_Plot(b="n",g="n",e="n",p=.,Plot_dir=paste0(Plot_dir,"/",slope))
    
  
}


  options(warn=2)
for(i in 1:length(p.tib))
{
  cp <- p.tib[[i]]
  cat("\nb = ",cp$b,"\tg = ",cp$g,"\te = ",cp$e)
  print(cp$p)
  Sys.sleep(10)
}
  
  
  
  
  
  
  
  
  
  
  

