#' searchPDFcontent
#'
#' @param my_search character string text to search
#' @param df the dataframe with the content information
#' @param my_path (optional) character string of path #TODO
#' @param my_filename (optional) character string of filename #TODO
#'
#' @return tibble
#' @export
searchPDFcontent <- function(my_search, df, my_path = NULL, my_filename = NULL) {
  ## Search:
  if (!is.null(my_path)) {
    df <- df %>% filter(str_detect(path, my_path))
  }
  if (!is.null(my_filename)) {
    df <- df %>% filter(str_detect(path, my_filename))
  }
  
  usethis::ui_todo("Searching the contents...")
  output <- df %>% filter(str_detect(txt, my_search))
  #%>% select(path, txt, size) %>% arrange(size)
  usethis::ui_done("Done!")
  
  if (nrow(output) < 1) {
    usethis::ui_warn("Nothing found")
    # TODO: suggestions <- FindSimilarWords(df, my_search)
    # usethis::ui_info("Refresh the app and try {suggestions} ?")
    return(NULL)
  }
  
  return(output)
}


# TODO:
FindSimilarWords <- function(df, x) {
  all_words <- flatten_chr(df$txt) %>%
    str_split(boundary("word")) %>%
    flatten_chr %>%
    unique %>%
    sort
  # all_words[stringsim(x, all_words, method = "hamming") > 0.8]
  # Hamming distance (a and b must have same nr of characters).
  all_words[stringsim(x, all_words, method = "osa") > 0.8] %>%
    head()
  # Optimal string aligment, (restricted Damerau-Levenshtein distance).
}

