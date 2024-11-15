library(tidyverse)
library(rprojroot)

climate_root <- rprojroot::is_rstudio_project

climate_datapath <- function(filename) {
    file.path(climate_root$find_file("Data"), filename)
}

climate_script_path <- function(filename) {
    file.path(climate_root$find_file("Scripts"), filename)
}

#' Ensure that a directory exists.
#' 
#' Creates the directory if needed otherwise does nothing.
#' 
#' @param path Path to the directory
ensure_directory = function(path) {
    if (!dir.exists(path))
        dir.create(path)
    path
}

#' Ensure that a file path is vacant
#' 
#' Deletes any existing file.
#' 
#' @param path Path to the file
ensure_file_vacant <- function(path) {
    ensure_directory(dirname(path))
    if (file.exists(path)) {
        file.remove(path)
    }
    path
}

