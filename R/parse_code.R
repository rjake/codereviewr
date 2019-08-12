#' Convert R code to dataframe
#'
#' @param file
#'
#' @description A data frame of all assignments and libraries
#' @importFrom knitr purl
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' system.file(package = "codereviewr", "R/test.R") %>% 
#'  code_to_df()
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
    raw = trimws(r_code)
  )
}
