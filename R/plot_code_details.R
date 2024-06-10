#' Create chart of dependencies
#'
#' @param ... file path, passed to code_to_df()
#' 
#' @importFrom dplyr group_by slice ungroup mutate select filter left_join rename full_join contains case_when rowwise
#' @importFrom tidyr separate_longer_delim
#' @importFrom stringr str_detect str_remove_all
#' @importFrom ggplot2 ggplot aes geom_point geom_curve geom_label unit scale_color_identity scale_fill_identity scale_y_continuous scale_x_continuous labs theme_classic
#' @importFrom forcats fct_reorder
#' @importFrom purrr map map_int
#' @export
#' @examples
#' code_lineage <- 
#'   code_to_df(file) |> 
#'   identify_code_steps() |>
#'   track_lineage(
#'     operation_types = c("assignment", "update"), 
#'     object_type_exclusion = c("function", "constant"),
#'     code_exclusion_regex = "<- (switch|system)|^board"
#'   )
#'   
#' plot_code_details(code_lineage)
#' 
plot_code_details <- function(code_lineage) {
  object_as_factor <- 
    fct_inorder(df$object_name) |> 
    fct_rev() |> 
    levels()
  
  df <- 
    code_lineage |> 
    #head(10) |> 
    mutate(
      object_name = factor(object_name, levels = object_as_factor),
      used_object_name = factor(used_object_name, levels = object_as_factor),
      color =
        case_when(
          operation == "assignment" ~ "skyblue1",
          operation == "update" ~ "grey85"
        )
    )
  
  
  plot_df <- 
    dplyr::bind_rows(
      # # inputs
      # df |> 
      #   distinct(object_name) |> 
      #   filter(str_detect(object_name, "^(asset_all|helix|raw|rocqi|special)")) |> 
      #   dplyr::transmute(
      #     source = "input",
      #     target = object_name,
      #     group = source
      #   ),
      # everything
      df |> 
        drop_na(used_object_name) |> 
        filter(
          !str_detect(used_object_name, "branch|collapse|label|regex|shorten"),
          #!str_detect("^(asset|table)_node_stats|all_deps$")
        ) |>
        dplyr::transmute(
          source = object_name,
          target = used_object_name,
          group = "steps"
        )
      ,
      # # outputs
      # df |>
      #   distinct(object_name) |>
      #   filter(str_detect(object_name, "^(asset|table)_node_stats|all_deps$")) |>
      #   dplyr::transmute(
      #     source = object_name,
      #     target = "output",
      #     group = target
      #   )
    )
  
  
  g_d3 <- 
    plot_df |> 
    igraph::graph_from_data_frame() |>
    networkD3::igraph_to_networkD3()
  
  
  g_d3$links$value <- 1
  
  g_d3$nodes <-
    g_d3$nodes |>
    mutate(
      group =
        case_when(
          str_detect(name, "^(input|asset_all|helix|raw|rocqi|special)") ~ "input",
          str_detect(name, "^(output|(asset|table)_node_stats|all_deps)$") ~ "output",
          .default = "other"
        ),
      size = ifelse(group == "other", 1, 25)
    )
  
  networkD3::forceNetwork(
    # mappings
    Links = g_d3$links,
    Nodes = g_d3$nodes,
    Source = "source",
    Target = "target",
    NodeID = "name",
    Group = "group",
    Nodesize = "size",
    Value = "value",
    # arguments
    arrows = TRUE,
    charge = -300,
    fontSize = 15,
    colourScale = networkD3::JS('d3.scaleOrdinal(["grey", "purple", "orange"]);'),
    linkColour = "#999999",
    linkDistance = 100,
    #nodeColour = "black",
    opacity = 1,
    opacityNoHover = 1,
    zoom = TRUE
  )
}

other_attempts <- function(variables) {
  plot_df |> 
    igraph::graph_from_data_frame() |> 
    plot()
  
  
  plot_df |> 
    networkD3::simpleNetwork(
      linkDistance = 150,
      charge = -300,
      linkColour = "#999999",
      fontSize = 15,
      nodeColour = "black",
      opacity = 1,
      zoom = TRUE
    )
  
  
  end_lineage <-
    df |>
    transmute(
      step_type = "end",
      x = step,
      y = step,
      object_name,
      used_object_name
    ) |>
    drop_na() |> 
    mutate(
      line_id = row_number()
    )
  
  start_lineage <- 
    df |>
    transmute(
      step_type = "start",
      x = step,
      y = step,
      object_name,
      used_object_name = object_name
    ) |> 
    inner_join(
      end_lineage |> 
        select(
          used_object_name,
          line_id
        )
    ) |> 
    distinct()
  
  
  step_lineage <- 
    bind_rows(
      start_lineage,
      end_lineage
    ) |> 
    # filter(used_object_name == "raw_lineage") |> 
    arrange(line_id) |> 
    rowwise() |> 
    mutate(
      jitter = sample(-10:10 / 100, size = 1)
    ) |> 
    ungroup()
  
  
  ggplot() +
    #geom_point()
    scale_y_reverse() +
    guides(color = "none", fill = "none") +
    geom_step(
      data = step_lineage,
      aes(x = x, y = y+jitter, group = line_id, color = object_name),
      direction = "mid"
    )
  +
    geom_point(
      data = distinct(step_lineage, x, y, object_name),
      aes(x = x, y = y+jitter, color = object_name)
    )
  
  
  geom_label(
    data = distinct(df, step, object_name, operation),
    aes(x = step, y = step, label = object_name, fill = object_name),
    #hjust = 0,
    #nudge_y = 0.1, label.padding = unit(0.1, "lines")
  )
  
  geom_curve(
    data = drop_na(df, step_created),
    aes(
      x = step,
      y = step,
      xend = step_created,
      yend = step_created
    ),
    curvature = -0.25
  ) +
    geom_label(
      data = ~distinct(.x, step, object_name, operation),
      aes(x = step, y = step, label = object_name, fill = operation),
      hjust = 0.25,
      #nudge_y = 0.1, label.padding = unit(0.1, "lines")
    )
  
  
  geom_curve(
    data = filter(get_curve, curve == "up"),
    aes(
      x = (input_step), 
      y = input_is,
      xend = step, 
      yend = is,
      color = input_is_color
    ),
    curvature = -0.25,
    size = 1
  ) +
    geom_curve(
      data = filter(get_curve, curve == "down"),
      aes(
        x = (input_step), y = input_is,
        xend = step, yend = is,
        color = input_is_color
      ),
      curvature = 0.25,
      size = 1
    ) +
    geom_label(
      data = distinct(final_plot, output, is, step, is_color),
      aes(label = output, fill = is_color),
      nudge_y = 0.1, label.padding = unit(0.4, "lines")
    ) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_y_continuous(breaks = seq_along(object_types), labels = object_types) +
    scale_x_continuous(
      limits = c(0.5, max(final_plot$step) * 1.1),
      breaks = 1:max(final_plot$step),
      labels = 1:max(final_plot$step)
    ) +
    labs( # title = paste("Analyis of", ...),
      subtitle = paste(as.Date(Sys.time()), "\n"),
      x = "step",
      y = ""
    ) +
    theme_classic()
}

