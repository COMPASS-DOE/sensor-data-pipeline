# metadata-utils.R
# Helper functions for generating L1 and L2 metadata
# BBL June 2025

# Get information about files in a folder and insert into metadata
# (a character vector)
md_insert_fileinfo <- function(folder, md) {
    files <- list.files(path = folder, pattern = "csv$", full.names = TRUE)
    message("\tFound ", length(files), " data files")
    file_info <- c()
    # Build up information about files...
    for(f in files) {
        fdata <- readLines(f) # just for a quick line count
        file_info <- c(file_info,
                       basename(f),
                       paste("Rows:", length(fdata) - 1),
                       paste("md5:", digest::digest(f, file = TRUE)),
                       "")
    }
    # ...and insert into metadata
    # We used the head(-1) to drop the final empty line, just to keep things pretty
    file_info_pos <- grep("[FILE_INFO", md, fixed = TRUE)
    md <- append(md, head(file_info, -1), after = file_info_pos)
    md[-file_info_pos]
}
