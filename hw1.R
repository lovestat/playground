
library(tidyverse)

is.as <- function(x){
  
  if (!is.numeric(x) || any(x > 999) || any(x <= 0 || any(x %% 1 != 0))) stop("`x` is not a right input")
  
  is.as.unit <- function(x.unit){
    x.unit %>% 
      as.character() %>% 
      strsplit("") %>% 
      unlist() %>% 
      as.integer() %>% 
      `^`(ceiling(log10(x.unit))) %>% 
      sum() %>% 
      identical(x.unit)
  }
  map_lgl(x, is.as.unit) %>% unlist()
}

is.armstrong(x = -2)
is.armstrong(x = 1011)
is.armstrong(x = c(pi, 6))
is.armstrong(x = "a")
