code_review <- function(...) {
  code_details <-
    code_to_df(...) %>%
    mutate(line = row_number()) %>%
    mutate(
      creation =
        ifelse(str_detect(raw, "\\<\\-"),
          str_replace(raw, "(.*) \\<\\-.*", "is(\\1)"),
          NA
        ),
      is = ""
    )

  parse_df <- map_dfr(1:nrow(code_details), identify_code_step)
  
  prep_plot <-
    parse_df %>% 
    mutate(obj = ifelse(str_detect(final, "obj"), "obj", final),
           plot = ifelse(str_detect(final, "lib|obj|func|loop"), "plot", "detail")) %>% 
    group_by(step) %>% 
    mutate(dir = ifelse(row_number() == 1, "output", "input")) %>% 
    ungroup()
  
  prep_plot
  
  get_outputs <-
    prep_plot %>% 
    filter(dir == "output") %>%
    select(step, output = text, create = obj)
  
  get_inputs <-
    prep_plot %>% 
    filter(dir == "input",
           plot == "plot") %>% 
    select(step, input = text, type = obj) %>% 
    left_join(get_outputs %>% select(input = output, input_step = step)) %>% 
    left_join(code_details %>% select(input_step = line, input_detail = text, input_is = is))
  
  
  order_outputs <- 
    c("list", "loop", "constant", "function", "table", "single column", "unused", "library") %>%
    .[. %in% code_details$is]
  
  order_inputs <- 
    as.character(c("list", "loop", "constant", "function", "table", "unused")) %>%
    .[. %in% get_inputs$input_is]
  
  
  final_plot <-
    get_outputs %>% 
    full_join(get_inputs, by = "step") %>% 
    left_join(code_details %>% select(step = line, detail = text, is)) %>% 
    mutate(is = factor(is, ordered = T) %>% fct_relevel(order_outputs),
           input_is = factor(input_is, ordered = T) %>% fct_relevel(order_inputs)) %>% 
    mutate(is_color = 
             case_when(is == "library" ~ "khaki1",
                       is == "constant" ~ "skyblue1",
                       is == "table" ~ "turquoise3",
                       is == "function" ~ "violet",
                       is == "single column" ~ "chocolate1",
                       is == "loop" ~ "grey85",
                       is == "list" ~ "grey85",
                       TRUE ~ "grey85")) %>% 
    mutate(input_is_color = 
             case_when(input_is == "library" ~ "khaki1",
                       input_is == "constant" ~ "skyblue1",
                       input_is == "table" ~ "turquoise3",
                       input_is == "function" ~ "violet",
                       input_is == "single column" ~ "chocolate1",
                       input_is == "loop" ~ "grey85",
                       input_is == "list" ~ "grey85",
                       TRUE ~ "grey85"))
  
  get_curve <-
    final_plot %>% 
    filter(step != input_step) %>% 
    mutate(curve = ifelse((input_is == "table"), "up", "down"))
  
  ggplot(final_plot, aes(x = (step), y = is)) +
    geom_curve(data = filter(get_curve, curve == "up"),
               aes(x = (input_step), y = input_is, 
                   xend = step, yend = is,
                   color = input_is_color),
               curvature = -0.25,
               size = 1) +
    geom_curve(data = filter(get_curve, curve == "down"),
               aes(x = (input_step), y = input_is, 
                   xend = step, yend = is,
                   color = input_is_color),
               curvature = 0.25,
               size = 1) +
    geom_label(data = distinct(final_plot, output, is, step, is_color),
               aes(label = output, fill = is_color),
               nudge_y = 0.1, label.padding = unit(0.4, "lines")) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_discrete(limits = order_outputs) +
    scale_x_continuous(limits = c(0.5, nrow(code_details) * 1.1), 
                       breaks = seq_len(nrow(code_details)),
                       labels = seq_len(nrow(code_details))) +
    labs(title = paste("Analyis of", file_name), 
         subtitle = paste(lubridate::today(), "\n"),
         x = "step",
         y = "") +
    theme_classic()
}
