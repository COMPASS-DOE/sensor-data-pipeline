# metadata-utils.R
# Helper functions for generating both L1 and L2 metadata
# BBL June 2025

# Get information about files in a folder and insert into metadata
# (a character vector)
md_insert_fileinfo <- function(folder, md, level) {
    if(level == "L1") {
        filename_spacing <- 55
    } else if(level == "L2") {
        filename_spacing <- 45
    } else stop("Unknown level ", level)

    files <- list.files(path = folder, pattern = "(csv|parquet)$", full.names = TRUE)
    if(length(files) == 0) stop("No files found in ", folder, " - this is bad")
    message("\tFound ", length(files), " data files")

    # Build up information about files...
    file_info <- data.frame(File = basename(files), Values = NA, Dropped = NA, MD5 = NA)
    for(f in seq_along(files)) {

        if(grepl("parquet$", files[f])) {
            fdata <- read_parquet(files[f])
        } else if(grepl("csv$", files[f])) {
            fdata <- read_csv(files[f], show_col_types = FALSE)
        } else {
            stop("Don't know how to read ", f)
        }

        file_info$Values[f] <- sum(!is.na(fdata$Value))
        if(level == "L2") {
            file_info$Dropped[f] <- sum(fdata$N_drop, na.rm = TRUE)
        }
        file_info$MD5[f] <- substr(digest::digest(files[f], file = TRUE),1, 8)
    }
    # ...and insert into metadata; format differ slightly by level
    file_info_txt <- paste(sprintf(paste0("%-", filename_spacing, "s"),
                                   c("Filename", file_info$File)),
                           sprintf("%10s", c("Values", file_info$Values)))
    if(level == "L2") {
        file_info_txt <- paste(file_info_txt,
                               sprintf("%10s", c("Dropped", file_info$Dropped)))
    }
    file_info_txt <- paste(file_info_txt,
                           sprintf("%-10s", c("MD5", file_info$MD5)))

    file_info_pos <- grep("[FILE_INFO]", md, fixed = TRUE)
    md <- append(md, file_info_txt, after = file_info_pos)
    md[-file_info_pos]
}

# Combine datalogger and derived metadata into a single table
# We need this for plotting
combine_variable_metadata <- function(var_md, derived_vars) {
    var_md$Type <- "DLR"
    our_columns <- c("research_name", "Type", "final_units",
                    "low_bound", "high_bound", "description")
    var_md <- var_md[our_columns]
    derived_vars$low_bound <- derived_vars$high_bound <- NA
    derived_vars$Type <- "DRV"
    rbind(var_md, derived_vars[our_columns])
}

# Format the variable metadata for insertion into documentation
md_variable_info <- function(var_md,
                             L1_or_L2,
                             only_these = NULL) {

    # Kludge: L2 requests only certain variables, but we ALSO want derived ones
    if(!is.null(only_these)) {
        var_md <- var_md[var_md$research_name %in% only_these |
                             var_md$Type == "DRV",]
    }

    # Format things into a character vector for insertion into the metadata
    if(L1_or_L2 == "L1") {
        paste(sprintf("%-20s", c("research_name", var_md$research_name)),
              sprintf("%-10s", c("Units", var_md$final_units)),
              sprintf("%-12s", c("Bounds", paste0(var_md$low_bound, ", ", var_md$high_bound))),
              c("Description", var_md$description))
    } else if(L1_or_L2 == "L2") {
        paste(sprintf("%-20s", c("research_name", var_md$research_name)),
              sprintf("%-5s", c("Type", var_md$Type)),
              sprintf("%-10s", c("Units", var_md$final_units)),
              c("Description", var_md$description))
    } else stop("Unkown L1_or_L2")
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
