#' Read the CSV files without a visible progression
#'
#' The "fars_read" function reads a CSV file, given a file name, without showing a progress line.
#' Throws an error line if the file doesn't exist
#' @param filename possibly with the directory
#' @return a tibble
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples  
#' \dontrun{\code{fars_read("accident_2013.csv.bz2")}}
#' @export
fars_read <- function(filename){
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make a file name
#'
#' The "make_filename" function creats a file name based on the year that is provided as a parameter
#' @param year integer
#' @return string, file name with bz2 zipped extension
#' @examples 
#' \dontrun{\code{make_filename(2013)}}
#' @export
make_filename <- function(year){
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Reads data in batch mode
#'
#' The "fars_read_years" function reads a range of valid years, calls the make_filename function and reads data in batch mode.
#' If a valid year is not given, the function throws an error message
#'
#' @param  year_range a vector with a range of intergers
#' @return a list of tibble objects given the range of valid years
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
#' @examples 
#' \dontrun{\code{fars_read_years(2013:2014)}}
#' @export
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

#' Summarize data given the range of years
#'
#' The "fars_summarize_years" function counts the occurance by years and months, calls the "fars_read_years" function
#' @param year_range a vector with a range of intergers
#' @return a wide format tibble object, given the range of valid years, of the count of occurance by years and months
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#' @examples 
#' \dontrun{\code{fars_summarize_years(2013:2014)}}
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draws a map for a given state number in the data and a valid year
#'
#' The "fars_map_state" function draws a map for a given state number in the data and a valid year
#' It throws an error "invalid STATE number:" if an invalid state number is given.
#' If no data is available, it throws a message "no accidents to plot".
#' @param State_number integer
#' @param Year integer
#' @return a map
#' @importFrom maps map
#' @importFrom graphics points
#' @examples 
#' \dontrun{\code{fars_map_state(1, 2013)}}
#' @export
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