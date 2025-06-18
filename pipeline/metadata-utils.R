# metadata-utils.R
# Helper functions for generating both L1 and L2 metadata
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

# Read the variable metadata table and return a formatted extract from it
md_variable_info <- function(variable_md_file) {
    var_md <- read.csv(variable_md_file)
    paste(sprintf("%-20s", c("research_name", var_md$research_name)),
          sprintf("%-10s", c("Units", var_md$final_units)),
          sprintf("%-12s", c("Bounds", paste0(var_md$low_bound, ", ", var_md$high_bound))),
          c("Description", var_md$description))
}

# Insert site information into metadata
# There MUST be an informational file named <site>.txt
md_insert_siteinfo <- function(site, site_files_folder, md) {
    site_md_file <- file.path(site_files_folder, paste0(site, ".txt"))
    if(!file.exists(site_md_file)) {
        stop("Couldn't find file ", site_md_file, " in ", site_files_folder)
    }
    site_md_for_insert <- readLines(site_md_file)

    # Insert site information
    site_info_pos <- grep("[SITE_INFO]", md, fixed = TRUE)
    md <- append(md, site_md_for_insert, after = site_info_pos)
    md[-site_info_pos]
}

md_insert_miscellany <- function(md, na_string, time_zone, version) {
    message("Inserting NA code, time zone, and version strings")

    # The NA code is an in-line replacement
    md <- gsub("[NA_STRING]", na_string, md, fixed = TRUE)
    # The time zone is an in-line replacement
    md <- gsub("[TIMEZONE]", time_zone, md, fixed = TRUE)

    gsub("[VERSION]", version, md, fixed = TRUE)
}
