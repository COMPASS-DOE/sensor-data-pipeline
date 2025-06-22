# metadata-utils.R
# Helper functions for generating both L1 and L2 metadata
# BBL June 2025

# Get information about files in a folder and insert into metadata
# (a character vector)
md_insert_fileinfo <- function(folder, md, filename_spacing) {
    files <- list.files(path = folder, pattern = "csv$", full.names = TRUE)
    if(length(files) == 0) stop("No files found in ", folder, " - this is bad")
    message("\tFound ", length(files), " data files")

    # Build up information about files...
    file_info <- data.frame(File = basename(files), Rows = NA, MD5 = NA)
    for(f in seq_len(length(files))) {
        fdata <- readLines(files[f]) # just for a quick line count
        file_info$Rows[f] <- length(fdata) - 1
        file_info$MD5[f] <- digest::digest(files[f], file = TRUE)
    }
    # ...and insert into metadata
    file_info_txt <- paste(sprintf(paste0("%-", filename_spacing, "s"),
                                   c("Filename", file_info$File)),
                           sprintf("%10s", c("Rows", file_info$Rows)),
                           sprintf("%-10s", c("MD5", file_info$MD5)))
    file_info_pos <- grep("[FILE_INFO]", md, fixed = TRUE)
    md <- append(md, file_info_txt, after = file_info_pos)
    md[-file_info_pos]
}

# Read the variable metadata table and return a formatted extract from it
md_variable_info <- function(variable_md_file,
                             only_these = NULL, derived_vars = NULL) {
    var_md <- read.csv(variable_md_file)
    our_columns <- c("research_name", "final_units", "low_bound",
                     "high_bound", "description")
    var_md <- var_md[our_columns]

    # Two extra parameters used by L2.qmd
    if(!is.null(only_these)) {
        var_md <- var_md[var_md$research_name %in% only_these,]
    }
    if(!is.null(derived_vars)) {
        derived_vars$low_bound <- derived_vars$high_bound <- NA
        var_md <- rbind(var_md, derived_vars[our_columns])
    }
    # Format things into a character vector for insertion into the metadata
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
    message("\tInserting NA code, time zone, and version strings")

    # The NA code is an in-line replacement
    md <- gsub("[NA_STRING]", na_string, md, fixed = TRUE)
    # The time zone is an in-line replacement
    md <- gsub("[TIMEZONE]", time_zone, md, fixed = TRUE)

    gsub("[VERSION]", version, md, fixed = TRUE)
}

md_readme_substitutions <- function(readme_fn, ver, rd, obs, commit, tz) {
    if(!file.exists(readme_fn)) stop("Couldn't find ", readme_fn)
    readme <- readLines(readme_fn)
    readme <- gsub("[VERSION]", ver, readme, fixed = TRUE)
    readme <- gsub("[DATESTAMP]", rd, readme, fixed = TRUE)
    readme <- gsub("[OBSERVATIONS]", obs, readme, fixed = TRUE)
    readme <- gsub("[GIT_COMMIT]", commit, readme, fixed = TRUE)
    readme <- gsub("[TIMEZONE]", tz, readme, fixed = TRUE)
    readme
}

check_drop_sort_columns <- function(dat, column_md, column_metadata_file) {
    # Check for metadata columns that are missing...
    if(!all(column_md$Column %in% colnames(dat))) {
        stop("Column metadata file ", column_metadata_file,
             " has entries not in data: ", setdiff(column_md$Column, colnames(dat)))
    }
    # ...order and remove columns not in the metadata...
    dat <- dat[column_md$Column]
    # ...and sort rows
    dat[order(dat$TIMESTAMP),]
}
