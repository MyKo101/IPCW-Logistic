
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

Prepare_Plot <- function(filename,Methods,dir)
{
  filename %>%
    paste0(dir,"/",.) %>%
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
                          P = "Perfect"))
  
}

Make_MainPlot <- function(b,g,e,dir=".",slope="None",anim=F)
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
  
  Ranges <- read_csv(paste0(dir,"/00-Ranges.csv"),
                     col_types=cols()) %>%
    filter(Slope == slope) %>%
    select(-Slope) %>%
    pivot_longer(c(min,max),
                 names_to="minmax",
                 values_to="Est") %>%
    select(Measure,Est)

  ff <- list.files(dir)
  
  regex.search <- paste0("_b(",b,")",
                         "_g(",g,")",
                         "_e(",e,")")
  
  ff <- ff[grepl(regex.search,ff,fixed=T)]
  
  if(length(ff) == 1)
  {
    tbl <- Prepare_Plot(ff,Methods,dir)
    
    if(!anim)
    {
      p <- ggMainplot(tbl,Ranges)
      return(p)
    } else return(tbl)
      
    
    
    
    
  } else return(NULL)
}

Make_MainPlot_tibble <- function(X,dir=".",slope="None")
{
  p <- Make_MainPlot(X$b,X$g,X$e,dir=dir,slop=slope,anim=F)
  
  tibble(b=X$b,g=X$g,e=X$e,p=list(p))
}

Make_All_MainPlots <- function(Agg_dir=".",Plot_dir=".",slope="None")
{
  Load_Done_Summary(Agg_dir) %>%
    filter(n>2) %>%
    split(1:nrow(.)) %>%
    map_chr(~Make_MainPlot_tibble(.,dir=Agg_dir) %>%
              Save_Plot_tibble(dir=paste0(Plot_dir,"/",slope)))
  
}


  
  
  
  
  
  
  
  
  
  
  
  
  

