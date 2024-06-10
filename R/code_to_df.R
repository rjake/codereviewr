#' Convert R code to dataframe
#'
#' @param file file to evaluate, if missing, a window will open to select file 
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' file <- system.file(package = "codereviewr", "R/test.R")
#' code_to_df(file)
#' }
code_to_df <- function(file) {
  # file <- "inst/test.R"

  # create temp file
  temp_folder <- tempdir(check = TRUE)
  temp_R <- tempfile(tmpdir = temp_folder, fileext = ".R")

  # choose file if not given
  file_to_parse <- ifelse(missing(file), file.choose(), file)

  # identify if file is an Rmd
  is_rmd <- grepl("\\.Rmd$", file_to_parse, ignore.case = TRUE)

  # if file is Rmd use knitr::purl(), otherwise use file name
  if (is_rmd) {
    x <- purl(file_to_parse, output = temp_R, quiet = TRUE)
  } else {
    x <- file_to_parse
  }

  # parse code
  r_code <- as.character(parse(x))

  tibble(
    line = seq_along(r_code),
    raw_code = trimws(r_code)
  )
}


#' Identify what is happening in code step
#'
#' @param code_df parsed data from code_to_df()
#' @importFrom dplyr mutate row_number na_if select
#' @importFrom tidyr drop_na
#' @importFrom stringr str_detect str_replace_all str_extract
#' @importFrom purrr map2_chr keep
#' @importFrom glue glue_collapse glue
#' @export
#' @examples
#' file <- system.file(package = "codereviewr", "R/test.R")
#' code_df <- code_to_df(file) 
#' identify_code_steps(code_df)
identify_code_steps <- function(code_df, ...) {
  code_df |> 
    mutate(
      operation =
        case_when(
          str_detect(raw_code, "^library\\(") ~ "library",
          str_detect(raw_code, "^[\\w\\.]+[\\$\\[]+") ~ "update",
          str_detect(raw_code, "^[\\w\\.:]+\\(") ~ "action",  # ex. plot
          str_detect(raw_code, "^[\\w\\.]+\\s*<-") ~ "assignment",
          str_detect(raw_code, "^(for|while)\\b") ~ "loop",
          str_detect(raw_code, "^(if)\\b") ~ "if",
          .default = NA_character_
        ),
      object_type = 
        case_when(
          str_detect(raw_code, "^[\\w\\.]+\\s*<-\\s*function\\(") ~ "function",
          str_detect(raw_code, '^[\\w\\.]+\\s*<-\\s*(["\\d]|c\\(|list\\()') ~ "constant"
        )
    ) |>
    drop_na(operation) |> 
    mutate(
      step = row_number(),
      search_code = 
        raw_code |> # replace quotes
        str_replace_all('"[^"]+"', '""') |> 
        str_replace_all("'[^']+'", '""'),
      object_name = 
        ifelse(
          operation == "assignment",
          str_extract(search_code, "^[\\w\\.]+"),
          NA_character_
        )
      ) |> 
    mutate(
      avail_objects = 
        map2_chr(
          .x = object_name, 
          .y = step - 1,
          ~object_name[1:.y] |> 
            keep(~!is.na(.x)) |> 
            glue_collapse("|")
        ),
      regex = 
        ifelse(
          avail_objects == "", 
          NA_character_,
          glue(x = avail_objects, "\\b({x})\\b")
        ),
      used_object_name =
        map2_chr(
          .x = search_code, 
          .y = regex,
          ~str_extract_all(.x, .y) |> 
            unlist() |> 
            unique() |> 
            paste(collapse = ",")
        ) |> 
        na_if("NA") |> 
        na_if("")
    ) |> 
    select(
      -c(avail_objects, regex, search_code)
    ) |> 
    relocate(raw_code, .after = everything())
}

#' @importFrom dplyr group_by slice ungroup mutate select filter left_join rename full_join contains case_when rowwise summarise
#' @importFrom tidyr separate_longer_delim
#' @examples
#' code_steps <- 
#'   code_to_df(file) |> 
#'   identify_code_steps()
#'   
#' track_lineage(
#'   code_steps, 
#'   operation_types = c("assignment", "update"), 
#'   object_type_exclusion = c("function", "constant"),
#'   code_exclusion_regex = "<- (switch|system)|^board"
#' )
#'
track_lineage <- function(code_steps, 
                          operation_types = NULL, 
                          object_type_exclusion = NULL,
                          code_exclusion_regex = NULL
                          ) {
  df <- 
    code_steps |> 
    select(
      step,
      operation,
      object_name,
      object_type,
      used_object_name,
      raw_code
    )
  
  if (!is.null(operation_types)) {
    df <- 
      df |> 
      filter(operation %in% operation_types) |> 
      mutate(step = row_number())
  }
  
  if (!is.null(object_type_exclusion)) {
    df <- 
      df |> 
      filter(!object_type %in% object_type_exclusion) |> 
      mutate(step = row_number())
  }
  
  if (!is.null(code_exclusion_regex)) {
    df <- 
      df |> 
      filter(!str_detect(raw_code, code_exclusion_regex)) |> 
      mutate(step = row_number())
  }
  
  df |> 
    separate_longer_delim(used_object_name, ",") |> 
    filter(
      # remove any objects that were excluded with above if statements
      is.na(used_object_name) | used_object_name %in% df$object_name
    ) |> 
    distinct() |> 
    left_join(
      df |> 
        select(
          used_object_name = object_name,
          step_created = step,
          used_object_type = object_type
        ) |> 
        filter(
          .by = used_object_name,
          step_created == min(step_created)
        ),
      by = join_by(used_object_name)
    ) |> 
    relocate(used_object_type, step_created, .after = used_object_name)
}
