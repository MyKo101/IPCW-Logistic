

Get_Calib_log.reg <- function(.tbl,weighted=F)
{
  #Gets the logistic regression calibration
  # Note that weighted*(IPCW-1)+1 will return 1 when weighted == F
  
  Methods <- paste0(c("Unweighted","Weighted")[weighted+1],
                                c("",".Slope"))
  
  #Create the first model, which calculates just the intercept
  res1 <- tidy(glm(Observed ~ 1,
                   offset=logit(Expected),
                   weights=weighted*(IPCW-1)+1,
                   family=binomial(),
                   data=.tbl))
  
  
  #Create the second model which calculates just the slope
  res2 <- tidy(glm(Observed ~ logit(Expected)-1,
                   offset=rep(res1$estimate,nrow(.tbl)),
                   weights=weighted*(IPCW-1)+1,
                   family=binomial(),
                   data=.tbl))
  
  #create output results
  res <- bind_rows(res1,res2) %>%
    mutate(Method = Methods) %>%
    select(Method,estimate,std.error)
  
  return(res)
}

Get_Calib_KM <- function(.tbl,.KM)
{
  #Gets the KM calibration
  # .KM is a single row in the Kaplan-Meier table
  
  #Get Expected and SE (from .tbl)
  Expected <- mean(.tbl$Expected)
  Expected.se <- sd(.tbl$Expected)/sqrt(nrow(.tbl))
  
  #Get Observed and SE (from KM)
  Observed <- .KM$Observed
  Observed.se <- .KM$Observed.se
  
  
  #create a tibble to store and output these results
  res <- tibble(Method = "KM",
                estimate = Expected - Observed,
                std.error = sqrt(Expected.se^2 + Observed.se^2))
  return(res)
}

Get_Calib_Pseudo <- function(.tbl,.Pseudo)
{
  
  Methods <- c("Pseudo","Pseudo.Slope")
  
  cData <- .tbl %>%
    full_join(.Pseudo,by="id") %>%
    mutate(lnH = cloglog(Expected))
  
  
  #Create the first model, which calculates just the intercept
  res1 <- tidy(glm(rescale.val(Pseudo_F) ~ 1,
                   offset = lnH,
                   data=cData,
                   family=gaussian(link="cloglog")))
  
  
  #Create the second model which calculates just the slope
  res2 <- tidy(glm(rescale.val(Pseudo_F) ~ lnH - 1,
                   offset=rep(res1$estimate,nrow(.tbl)),
                   data=cData,
                   family=gaussian(link="cloglog")))
  
  
  
  #create output results
  res <- bind_rows(res1,res2) %>%
    mutate(Method = Methods) %>%
    select(Method,estimate,std.error)
  
  return(res)
  
}



Get_Calib_All <- function(.tbl,.KM,.Pseudo,suffix)
{
  nm <- suffix %>%
    paste0(c("Est","SE"),.) %>%
    c("Method",.)
  
  res <- bind_rows(
    Get_Calib_log.reg(filter(.tbl,!Censored),T),
    Get_Calib_log.reg(filter(.tbl,!Censored),F),
    Get_Calib_KM(filter(.tbl,!Censored),.KM),
    Get_Calib_Pseudo(.tbl,.Pseudo)
  ) %>%
    set_names(nm)
  return(res)
  
}
