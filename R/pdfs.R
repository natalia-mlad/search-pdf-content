#' Get PDF txt using poppler
#'
#' See https://www.mankier.com/1/pdftotext for more info.
#'
#' @param tpath the path of the pdf file
#' @param page which pages of the pdf to convert to plain text.
#' All pages are converted by default.
#' @param enc encoding to be assumed for the readLines function.
#' It is used to mark character strings as known to be in Latin-1 or UTF-8:
#' it is not used to re-encode the input. Default is "UTF-8".
#'
#' @export
GetPDFtxt <- function(tpath, page = NULL, enc = "UTF-8") {
  # Create the temp output file to save the results of 'pdftotext':
  tfile <- tempfile(fileext = ".txt")
  # pdftotext:
  my_cmd <- c()
  if (!is.null(page)) {
    my_cmd <- paste(my_cmd,  shQuote("-f"), shQuote(page),
                    shQuote("-l"), shQuote(page))
  }
  my_cmd <- paste(my_cmd, shQuote("-enc"), shQuote(enc),
                  shQuote(normalizePath(tpath)), shQuote(tfile))
  # TODO:
  system2("pdftotext", my_cmd, stderr = FALSE)
  # Read the Results:
  output <- suppressWarnings(readLines(tfile, encoding = enc))
  # Tidy up & Output:
  file.remove(tfile) #unlink(tfile)
  return(output)
}


#' FindPDFs
#'
#' @param my_path directory path to search
#' @param max_size (character string) max size of the pdf file to consider; 5MB by default
#'
#' @export
FindPDFs <- function(my_path, max_size = "5MB") {
  stopifnot(length(my_path) == 1 & isTRUE(dir_exists(my_path)))
  stopifnot(length(max_size) == 1 & !is.na(fs_bytes(max_size)))

  dir_ls(my_path, glob = "*.pdf", recurse = TRUE, all = FALSE, fail = FALSE) %>%
    file_info() %>% #tidy_my_data() %>% remove_identical_cols() %>%
    filter(!is.na(size) & size != 0) %>%
    filter(file.exists(path) == TRUE) %>%
    filter(nchar(path) < 259) %>%
    filter(size <= fs_bytes(max_size))
}


#' GetPDFtxt_tibble
#'
#' @param my_path directory path
#' @param max_size 5MB default
#'
#' @return tibble
#' @export
GetPDFtxt_tibble <- function(my_path, max_size = "5MB") {
  # stopifnot(length(my_path) == 1 & isTRUE(dir_exists(my_path)))
  # stopifnot(length(max_size) == 1 & !is.na(fs_bytes(max_size)))
  # TODO: add max_size parameter
  my_path %>%
    file_info() %>%
    mutate(txt = map(path, possibly(
      GetPDFtxt, NA_real_
    )), .after = path) %>%
    filter(!is.na(txt)) %>%
    # tidy_my_data() %>%
    mutate(filename = path_file(path), .after = path) %>%
    select(path, filename, txt, size, modification_time,
           inode, blocks, access_time, change_time, birth_time)
}


#' SearchPDFs
#' alt: SearchPDFcontent??
#'
#' Please note, can be somewhat slow.
#'
#' @param my_path directory path to search
#' @param search_string character string to search for in the contents of the PDFs
#' @param max_size (character string) max size of the pdf file to consider; "5MB" by default.
#'
#' @return tibble
#' @export
SearchPDFs <- function(my_path, search_string, max_size = "5MB") {
  usethis::ui_todo("Finding all the PDFs...")
  pdf_files_info <- FindPDFs(my_path = my_path, max_size = max_size)

  usethis::ui_done("Done! {nrow(pdf_files_info)} PDFs found.")
  # TODO: notify user of time estimation
  pdf_files_info <- pdf_files_info %>%
    mutate(txt = map(path, possibly(GetPDFtxt, NA_real_)), .after = path)

  # TODO: Save
  # filename <- filenamer::filename("all_items", ext = "RDS", subdir = F) %>%
  #   as.character(.) %>% paste0("data/zotero/", .)
  # filename <- paste0("all_items_", Sys.time(), ".RDS") %>% path_sanitize() %>%
  #   str_replace_all(" ", "_") %>% paste0("data/zotero/", .)
  # print(set_fdate(filename, "2011-01-05"))
  ##
  # filename <- my_path %>% #path_sanitize()%>%
  #   str_remove(paste0(path_home(), "/")) %>%
  #   str_remove_all(" |[.]|,|-|/") %>%
  #   paste0("PDFs-", .) %>%
  #   filenamer::filename(ext = "RDS") %>% #subdir = F, path = NULL, tag = NULL, date = NULL, time = NULL,
  #   as.character() #%>% paste0("data/zotero/", .)
  # saveRDS(pdf_files_info, file = filename) #"data-raw/pdf-search1.RDS")

  # TODO: notify user of search string action
  output <- pdf_files_info %>%
    filter(str_detect(txt,  search_string)) #%>% select(path, txt, size)

  # Warning message:
  #   In stri_detect_regex(string, pattern, negate = negate, opts_regex = opts(pattern)) :
  #   argument is not an atomic vector; coercing

  if (nrow(output) <= 10) {
    walk(output$path, file_show)
  }
  # TODO otherwise ask?

  return(output)
}
