library(tidyverse)
model_name <- "pickup"
mpg <- mpg
models <- mpg %>% mutate(model_new = ifelse(str_detect(model, model_name), model_name, str_extract(model, "\\w+"))) %>% filter(year == 1999) %>% group_by(model_new) %>% count(sort = T) %>% ungroup()
over_5 <- function(x) { x > 5} 
models$over_5 <- over_5(models$n)
edit_models <- models %>% mutate(over_10 = over_5(n/2))
make_list <- list(edit_models$model_new, c(1999, 2000), TRUE) 
