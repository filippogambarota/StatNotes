#' update_index
#' @description update the \code{index.md} website
#' @export
#'
update_index <- function(){
  index_file <- "index.md"
  fs::file_delete(index_file)
  fs::file_create(index_file)
  index_file %>% write_line(create_header())
  rmd_metadata <- lapply(get_all_rmd(), get_metadata)
  html_files <- get_all_html()
  tree <- get_tree(".", ".Rmd")

  for(i in seq_along(html_files)){
    index_file %>% write_line(ins_h(rmd_metadata[[i]]$title, 2))
    index_file %>% write_line(format_link(rmd_metadata[[i]]$title, html_files[i]))
  }
  success(cli::col_green("Website updated!"))
}

#' create_header
#'
create_header <- function(){
  intro <- "This repository contains my notes and snippets about statistics, simulations, data visualization etc. \n"
  intro <- c(intro,
             paste0(get_date(), "\n"),
             "*** \n",
             ins_h("Content", 1))
  return(intro)
}

#' write_line
#'
write_line <- function(file = index_file, txt){
  write(txt, file, append = T)
}

#' format_link
#'
format_link <- function(title, file){
  sprintf("- [%s](%s) \n", title, file)
}

#' get_name
#'

get_name <- function(file){
  tools::file_path_sans_ext(basename(file))
}

#' ins_h
#'
ins_h <- function(txt, lvl){
  paste(paste(rep("#", lvl), collapse = ""), txt, "\n")
}

#' get_all_rmd
#'
get_all_rmd <- function(path = "_posts", absolute = TRUE){
  list.files(here::here(path), recursive = TRUE, full.names = absolute, pattern = ".Rmd")
}

#' get_all_html
#'
get_all_html <- function(absolute = FALSE){
  all_rmd <- get_all_rmd(absolute)
  stringr::str_replace_all(all_rmd, ".Rmd", ".html")
}

#' init_local_database
#'
init_local_database <- function(){
  dir.create()
}

#' get_os
#'
get_os <- function(){
  return(.Platform$OS.type)
}

#' init_db_folder
#'
init_db_folder <- function(folder_name = ".local-db"){
  if(!dir.exists(folder_name)){
    dir.create(folder_name) # create the hidden folder for temp files
    os <- get_os() # get the current os

    if(os == "windows"){
      shell(paste("attrib +h", folder_name)) # make the directory hidden in windows
    }
    success("db folder created!")
  }
  return(folder_name)

}

#' success
#'
success <- function(msg){
  cli::cli_alert_success(msg)
}

#' msg
#'
msg <- function(msg){
  cli::cli_alert_info(msg)
}

#' update_db
#' @export
update_db <- function(){
  db_folder <- init_db_folder()
  all_rmd <- get_all_rmd()
  db_file <- file.path(here::here(db_folder, "db.rds"))
  db <- data.frame(
    file = basename(all_rmd),
    path = all_rmd,
    md5 = unname(tools::md5sum(all_rmd))
  )
  saveRDS(db, db_file)
  success(cli::col_blue("Database updated!"))
  invisible(db_file)
}

#' which_modified
#'
which_modified <- function(force = FALSE){
  to_knit <- get_all_rmd()
  if(!force){
    md5_now <- unname(tools::md5sum(to_knit))
    db <- readRDS(".local-db/db.rds")
    to_knit <- db$path[!md5_now == db$md5]
  }
  return(to_knit)
}

#' knit_file
#'
knit_file <- function(file){
  rmarkdown::render(file, quiet = T)
  success(paste(basename(file), "updated!"))
}

#' update_files
#' @param force logical that indicate if knit again the files
#' @export
#'
update_files <- function(force = FALSE){
  to_knit <- which_modified(force)
  if(length(to_knit) != 0){
    out <- lapply(to_knit, knit_file)
    db_folder <- update_db()
  }else{
    msg("Nothing to update!")
  }
}

#' get_metadata
#'
get_metadata <- function(file){
  rmarkdown::yaml_front_matter(file)
}

#' get_tree
#'
get_tree <- function(path, pattern = ""){
  dirs <- list.dirs(path, full.names = FALSE, recursive = TRUE)
  tree <- lapply(dirs, list.files)
  names(tree) <- dirs
  tree <- tree[sapply(tree, function(x) any(grepl(pattern, x)))]
  return(tree)
}

#' get_date
#'
get_date <- function(){
  sprintf("*Last update: %s*", Sys.Date())
}
