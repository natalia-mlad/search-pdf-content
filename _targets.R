# 1. Set-Up ####
# packages:
library(targets)
# library(tarchetypes)

# options:
tar_option_set(packages = c("fs", "tidyverse"))
# memory = "transient", garbage_collection = TRUE)

# functions:
source("R/pdfs.R")

# 2. List of Targets -------------------------------------------------
# TODO: use date-modified + path to figure out which content needs updating in the DBs (similar to the zotdb approach)
list(
  # PDF selection:
  tar_target(max_size, "5MB"),
  tar_target(my_dir1,
             path_home("OneDrive/PhD Psychology/01 - Papers, Books, Theses"),
             format = "file"),
  tar_target(my_dir2,
             path_home("OneDrive/PDFs"),
             format = "file"),
  
  # find all files:
  tar_target(
      pdf_dir,
      c(my_dir1, my_dir2) %>%
        dir_ls(glob = "*.pdf", all = F, recurse = T, fail = F) %>%
        file_info() %>%
        # TODO: inform the user explicitly about the files "lost"
        filter(!is.na(size) & size != 0) %>%
        filter(file.exists(path) == TRUE) %>%
        filter(nchar(path) < 259) %>%
        filter(size <= fs_bytes(max_size)) %>%
        pull(path)#, format = "file"
  ),
  # tar_files(
  #   pdf_dir01,
  #   path_home("OneDrive/PhD Psychology/01 - Papers, Books, Theses/00 - TOP SOURCES") %>%
  #     dir_ls(glob = "*.pdf", all = T, recurse = T, fail = F)
  # ),
  
  # dynamic branching approach:
  tar_target(
    all_pdf_contents,
    GetPDFtxt_tibble(pdf_dir),
    # GetPDFtxt(pdf_dir01),
    pattern = map(pdf_dir), # dynamic branching
    # iteration = "list",
    error = "null",
    format = "feather", # from the arrow package
    storage = "worker",
    retrieval = "worker"
  ),

  # 'all at once' approach:
  tar_target(
    all_pdf_contents2,
    pdf_dir %>%
      file_info() %>%
      mutate(txt = map(
        path, possibly(GetPDFtxt, NA_real_)
      ), .after = path) %>%
      # TODO: inform the user explicitly about the unread files
      filter(!is.na(txt)) %>%
      mutate(filename = path_file(path), .after = path) %>%
      select(path, filename, txt, size, modification_time,
             inode, blocks, access_time, change_time, birth_time)
  )
)
