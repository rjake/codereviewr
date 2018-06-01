library(tidyverse)
a <- mpg %>% mutate(length = nchar(model)) %>% filter(length < 10) %>% group_by(manufacturer) %>% count(model) %>% ungroup()
b <- function(x) { if(1 == 1){ 1}  else { 2} } 
n <- c(2, 3, 5) 
s <- c("aa", "bb", "cc", "dd", "ee") 
b <- c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x <- list(n, s, b, 3) 
