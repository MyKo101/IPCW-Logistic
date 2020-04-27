

Get_PMs <- function(.res)
{
  .res %>%
    mutate(.slope = grepl(".Slope",Method,fixed=T),
           True_value = case_when(.slope ~ 1,
                                  Model == "P" ~ 0,
                                  Model == "U" ~ -0.2,
                                  Model == "O" ~ 0.2),
           LL = Est - qnorm(0.975)*SE,
           UL = Est + qnorm(0.975)*SE) %>%
    group_by(Model,Method,it) %>%
    summarise(N = n(),
              Bias_Est = mean(Est - True_value,na.rm=T),
              EmpSE_Est = sd(Est - Bias_Est,na.rm=T),
              Coverage_Est = mean( LL <= True_value & True_value <= UL ,na.rm=T),
              
              Bias_SE = EmpSE_Est/sqrt(N),
              EmpSE_SE = EmpSE_Est/sqrt(2*(N-1)),
              Coverage_SE = Coverage_Est*(1-Coverage_Est)/N
              ) %>%
    ungroup
}

Get_All_PM <- function(All_dir=".",Agg_dir=".")
{
  Done <- Load_Done(All_dir) %>%
    group_by(b,g,e) %>%
    summarise(n=n())
  
  write_csv(Done,paste0(Agg_dir,"/00-Done.csv"))
  
  Done %<>%
    ungroup %>%
    filter(n > 2) %>%
    split(1:nrow(.)) %>% 
    map_dfr(~Save_Aggregate_Results(.,All_dir,Agg_dir))
  
  return(Done)
  
  
}

