
logit <- function(p) log(p/(1-p))
unlogit <- function(p) exp(p)/(exp(p) + 1)

cloglog <- function(x) make.link("cloglog")$linkfun(x)
uncloglog <- function(x) make.link("cloglog")$linkinv(x)



miscal <- function(p,m) unlogit(logit(p) + m)

seq_between <- function(x,len) seq(x[1],x[2],length.out=len) 

Drop.Extremes <- function(x,raw=T)
{
  #Reduces extreme values in the [0,1] range
  # such that if they are less than 10^(-6) away from
  # the ends, it rescales them to be capped at 10^(-7)
  
  #The raw variable says whether this should actually be done or not
  # (i.e. should I return the raw values?)
  
  if(!raw) x else
    case_when(x < 10^(-6) ~ 0.5*x+10^(-7),
              x > 1-10^(-6) ~ 1- 0.5*(1-x)-10^(-7),
              T ~ x)
}

rescale.val <- function(x)
{
  #Used for rescaling the Pseudo values to be between 0 and 1
  
  ((x-min(x))/(max(x)-min(x))) %>%
     Drop.Extremes
}






  