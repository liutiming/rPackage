#' Read CSV files
#'
#' @param filename a string that indicates the path/name of the file
#' 
#' @return a dataframe tbl from the csv file
#' 
#' @details will return error if the no file of specified name exists 
#' @importFrom readr::read_csv,dplyr::tbl_df
#' 
#' @example fars_read("file.txt")
#'


fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'make a file name
#' @param year a numeric used for name 
#' @return a string as a name for bz2 file that takes in year
#' @details error if year is not a numeric
#' @example make_filename(2009)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' read files with name in the range years and return a dataframe with column month and year
#' @param years a range of years  
#' @return a dataframe of month and year where the file contains such name exists 
#' @details warning will return if the file of the year does not exist
#' @importFrom dplyr::mutate, dplyr::select
#' @example fars_read_years(2016)
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' summarize the number of year and month 
#' @param years a range of years  
#' @return return a data frame which contains a column of the number of columns with the same year and month
#' @importFrom dplyr::bind_rows, dplyr::group_by, dplyr::summarize, tidyr::spread
#' @example fars_summarize_years(2016)
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' plot the accidents of a particular state in a particular year
#' @param stat.num the number of a state
#' @param years a range of years   
#' @return return a graph of the location of the accidents of a particular state in a particular year
#' @importFrom dplyr::filter,maps::map, graphics::points
#' @example fars_summarize_years(2016)
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
