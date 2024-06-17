#' Title
#' 
#' @param code_details df of code to run through
#' @param get_line step number from code_df
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter rowwise mutate ungroup distinct case_when select row_number
#' @importFrom stringr str_detect str_extract_all str_replace str_extract
#' @importFrom tidyr unnest
#' @importFrom utils capture.output getParseData ls.str lsf.str
#' @export
#'
asses_code_step <- function(code_details, get_line) {
  
  # run the line
  x <- parse(text = code_details$raw[get_line])

  hidden_step <- capture.output(eval(x, envir = .GlobalEnv))

  # identify functions
  get_fx_pkg <-
    tibble(pkg = search()) |>
    filter(str_detect(pkg, "package")) |>
    rowwise() |>
    mutate(fx = list(as.character(ls.str(pkg)))) |>
    ungroup() |>
    unnest(fx) |>
    filter(str_detect(tolower(fx), "^[a-z]")) |>
    distinct(fx) |>
    pull(fx) # mutate(src = "pkg")

  get_fx_local <-
    tibble(fx = list(as.character(lsf.str(search())))) |>
    unnest(fx) |>
    pull(fx) # mutate(src = "local")

  # get_fx <- c(get_fx_pkg, get_fx_local)

  get_objects <-
    tibble(obj = list(as.character(ls.str(search())))) |>
    unnest(obj) |>
    filter(!obj %in% get_fx_local) |>
    pull(obj)

  # make dataframe of found functions
  local_fx_list <- paste0(paste0(get_fx_local, "\\("), collapse = "|")

  fx_found <- str_detect(code_details$raw[get_line], local_fx_list)

  if (fx_found) {
    fx_df <-
      tibble(
        step = get_line,
        text = (str_extract_all(code_details$raw[get_line], local_fx_list)),
        final = "local function"
      ) |>
      rowwise() |>
      unnest(text) |>
      mutate(text = str_replace(text, "\\(", ""))
  }

  # pattern to identify single column manipulation
  assignment_pattern <- "^\\w+\\$\\w+ \\<-" # ex: "mpg$cyl <- 2"

  # single column
  if (str_detect(code_details$raw[get_line], assignment_pattern)) {
    df <-
      tibble(
        step = get_line,
        text = str_extract(code_details$raw[get_line], "\\w+\\$\\w+"),
        final = "manipulation single column"
      ) |>
      rbind(
        tibble(
          step = get_line,
          text = str_extract(code_details$raw[get_line], "\\w+"),
          final = "obj dependency"
        )
      )

    # loops
  } else if (str_detect(code_details$raw[get_line], "^for|^while")) {
    df <-
      tibble(
        step = get_line,
        text = paste(str_extract(code_details$raw[get_line], "\\w+"), "loop"),
        final = "loop"
      )

    # libraries
  } else if (str_detect(code_details$raw[get_line], "^library")) {
    df <-
      tibble(
        step = get_line,
        text = code_details$raw[get_line],
        final = "load library"
      )

    # functions
  } else if (str_detect(code_details$raw[get_line], "\\<- function\\(")) {
    df <-
      tibble(
        step = get_line,
        text = str_extract(code_details$raw[get_line], "\\w+"),
        final = "function"
      )

    # main funciton
  } else {
    # get_line = 2
    get_type <-
      # code_details$creation is an 'is()' statement
      eval(parse(text = code_details$creation[get_line]))

    x <-
      get_type |>
      paste0(collapse = "|")

    is_x <-
      case_when(
        str_detect(x, "^list") ~ "list",
        str_detect(x, "tbl|DT") ~ "table",
        str_detect(x, "vector|POSIX") ~ "constant",
        TRUE ~ as.character(get_type[1])
      )

    df <-
      getParseData(
        parse(text = code_details$raw[get_line])
      ) |>
      as_tibble() |>
      mutate(step = get_line) |>
      select(step, parent:text) |>
      filter(
        terminal == T,
        str_detect(token, "SYMBOL")
      ) |>
      filter((!str_detect(token, "SYMBOL_FUNCTION_CALL") | text %in% get_fx_local)) |>
      # group_by(text) |> mutate(first = first(token)) |> ungroup() |>
      mutate(
        final =
          case_when(
            row_number() == 1 ~ paste("new object | ", is_x),
            str_detect(token, "SUB") ~ "new column",
            text %in% get_fx_pkg ~ "from libary",
            text %in% get_fx_local ~ "local function",
            # ls("package:datasets")
            text %in% get_objects ~ "obj dependency",
            TRUE ~ ""
          )
      ) |>
      filter(final != "") |>
      select(step, text, final) |>
      distinct()
  }

  if (fx_found) {
    df <- rbind(df, fx_df)
  }

  return(df)
}
