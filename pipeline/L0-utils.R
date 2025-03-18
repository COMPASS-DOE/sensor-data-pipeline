# L0-utils.R
# Utility functions, and test code, used exclusively by L0.qmd
# BBL March 2024

#' Read a raw Campbell datalogger data file
#'
#' @param filename Fully-qualified filename of a raw Campbell datalogger file
#' @param quiet Print diagnostic messages? Logical
#' @param ... Other parameters to pass on to \code{\link{read_csv}}
#' @description This function reads a local file of raw sapflow data,
#' extracts the logger number from the header, and uses
#' \code{\link[readr]{read_csv}} to parse the file into a data frame.
#' @author Stephanie Pennington and Ben Bond-Lamberty
#' @return A \code{\link[tibble]{tibble}} with the data.
#' @export
#' @importFrom readr read_csv read_lines col_character
#' @seealso \code{\link{process_sapflow_dir}}
read_datalogger_file <- function(filename, quiet = FALSE, ...) {

    # Parse line one to extract logger and table names
    dat <- read_lines(filename)
    header_split <- strsplit(dat[1], ",")[[1]]
    header_split <- gsub("\"", "", header_split) # remove quotation marks
    format_name <- header_split[1] # first field of row 1
    logger_name <- header_split[2] # second field of row 1
    table_name <- header_split[length(header_split)]

    # We have no time zone information, so read the timestamp as character
    if(length(list(...))) {
        x <- read_csv(I(dat[-c(1, 3, 4)]), ...)
    } else {
        x <- read_csv(I(dat[-c(1, 3, 4)]),
                      # don't want timestamp parsed to a datetime at this point
                      col_types = list(TIMESTAMP = col_character()))
    }
    info <- tibble(Logger = rep(logger_name, nrow(x)),
                   Table = rep(table_name, nrow(x)),
                   Format = rep(format_name, nrow(x)))
    as_tibble(cbind(info, x))
}
