library(tidyverse)
model_name <- "a4"
a <- mpg %>% mutate(ideal = model_name) %>% filter(model == ideal) %>% group_by(manufacturer) %>% count(cyl) %>% ungroup()
fn <- function(x) { if(1 == 1){ 1}  else { 2} } 
p <- paste0("here is one string of text", fn, "another string of text")
x <- list(c(2, 3, 5), c("aa", "bb", "cc", "dd", "ee"), c(TRUE, FALSE, TRUE, FALSE, FALSE), 3) 
