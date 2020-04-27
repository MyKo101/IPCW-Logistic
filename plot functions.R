
ggMainplot <- function(tbl)
{
  ggplot(tbl) +
    geom_line(aes(it,Est,col=Method)) + 
    geom_ribbon(aes(it,ymin=LL,ymax=UL,fill=Method),alpha=0.2) + 
    facet_grid(rows=vars(Measure),cols=vars(Model),
               scales="free")
  
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
           UL = Est + qnorm(0.975)*SE)
  
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
      p <- ggMainplot(tbl)
      p
    } else return(tbl)
      
    
    
    
    
  } else return(NULL)
}



  
  
  
  
  
  
  
  
  
  
  
  
  

