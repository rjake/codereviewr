#' Prep dataframe to plot
#'
#' @param ... file path, passed to code_to_df()
#'
#' @importFrom dplyr mutate group_by ungroup row_number
#' @importFrom stringr str_detect str_remove_all
#' @importFrom purrr map_dfr
#' @export
#'
#' @examples
prep_plot <- function(...) {
  code_df <-
    #code_to_df("inst/test.Rmd") %>%
    code_to_df(...) %>% 
    mutate(
      creation =
        ifelse(
          str_detect(raw, "\\<-"),
          str_remove_all(raw, "\\n") %>% 
            str_remove_all(" \\<-.*") %>% 
            paste0("is(", ., ")"),
          NA
        )
    )

  parse_df <- map_dfr(1:nrow(code_df), identify_code_step, code_details = code_df)

  parse_df %>% 
    mutate(
      obj = ifelse(str_detect(final, "obj"), "obj", final),
      plot = ifelse(str_detect(final, "lib|obj|func|loop"), "plot", "detail")
    ) %>% 
    group_by(step) %>% 
    mutate(dir = ifelse(row_number() == 1, "output", "input")) %>% 
    ungroup()
}  


#' Create chart of dependencies
#'
#' @param ... file path, passed to code_to_df()
#' 
#' @importFrom dplyr group_by slice ungroup mutate select filter left_join rename full_join contains case_when rowwise
#' @importFrom stringr str_detect str_remove_all
#' @importFrom ggplot2 ggplot aes geom_curve geom_label scale_color_identity scale_fill_identity scale_y_continuous scale_x_continuous labs theme_classic
#' @export
#'
#' @examples
plot_output <- function(...) {
  plot_df <- prep_plot(...)
  
  step_is <-
    plot_df %>% 
    group_by(step) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(is = str_remove_all(final, ".* | ")) %>% 
    #left_join(code_to_df("inst/test.Rmd")) %>% 
    select(step, is)
  
  get_outputs <-
    plot_df %>% 
    filter(dir == "output") %>%
    select(step, output = text, create = obj) %>% 
    left_join(step_is %>% select(step, is))
  
  get_inputs <-
    plot_df %>% 
    filter(dir == "input",
           plot == "plot") %>% 
    select(step, input = text, type = obj) %>% 
    left_join(get_outputs %>% select(input = output, input_step = step)) %>% 
    left_join(step_is %>% rename(input_step = step, input_is = is))
  
  
  object_types <- c(
    "list", "loop", "constant", "function", "table", "column", "unused", "library", "NA"
  )
  
  final_plot <-
    get_outputs %>% 
    full_join(get_inputs, by = "step") %>% 
    #left_join(step_is, by = "step") %>% 
    select(step, output, is, contains("input")) %>% 
    # mutate(
    #   is = factor(is) %>% fct_relevel(object_types),
    #   input_is = factor(input_is) %>% fct_relevel(object_types)
    # ) %>% 
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
                       TRUE ~ "grey85")) %>% 
    rowwise() %>% 
    mutate(
      is = which(object_types == is),
      input_is = ifelse(is.na(input_is), NA, which(object_types == input_is))
    ) %>% 
    ungroup()
  
  get_curve <-
    final_plot %>% 
    filter(step != input_step) %>% 
    mutate(curve = ifelse((input_is == "table"), "up", "down"))
  
  
  ggplot(final_plot, aes(x = step, y = is)) +
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
    scale_y_continuous(breaks = seq_along(object_types), labels = object_types) +
    scale_x_continuous(limits = c(0.5, max(final_plot$step) * 1.1), 
                       breaks = 1:max(final_plot$step),
                       labels = 1:max(final_plot$step)) +
    labs(#title = paste("Analyis of", ...), 
         subtitle = paste(as.Date(Sys.time()), "\n"),
         x = "step",
         y = "") +
    theme_classic()
}
