## -----------------------------------------------------
## Southern Oscillation Data Source
## -----------------------------------------------------

soi_directory <- climate_datapath("SOI")

soi_bom_url <- "ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html"

soi_bom1876_path <- file.path(soi_directory, "soiplaintext.html")

soi_download_bom1876 = function() {
    
    ensure_file_vacant(soi_bom1876_path)
    
    cat("----------------------------------------------\n")
    cat("Downloading SOI from 1876 onward from the BOM\n")
    download.file(soi_bom_url, soi_bom1876_path, mode="wb")
    cat("----------------------------------------------\n")
}

soi_load_bom1876 <- function() {
    page <- xml2::read_html(soi_bom1876_path)
    text <- page %>% rvest::html_node("body") %>% rvest::html_node("pre") %>% rvest::html_text()
    col_names <- c("year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    col_widths <- fwf_widths(c(9, rep(8,11), 7), col_names=col_names)
    col_types <- cols(col_integer(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number(), col_number())
    df <- read_fwf(text, col_widths, col_types = col_types, skip  = 2)
    months <- col_names[which(col_names == "Jan"):which(col_names == "Dec")]
    df <- df %>%
        gather(month, index, Jan:Dec) %>%
        mutate(month = factor(month, levels=months, ordered=TRUE)) %>%
        mutate(SOI.Source = "BOM") %>%
        mutate(Date = make_date(year, month)) %>%
        select(Year = year, Month = month, Date, SOI = index, SOI.Source) %>%
        filter(!is.na(SOI))
    df 
}

soi_load <- function() soi_load_bom1876()
