# Helper functions

library(lubridate)
library(readr)

# Constants used in this file and elsewhere in the system
GIT_COMMIT <- substr(system("git rev-parse HEAD", intern = TRUE), 1, 7)

# A date way far in the future, used by valid_entries()
MAX_DATE <- ymd_hms("2999-12-31 11:59:00")

# Data NA (not available) strings to use on writing
NA_STRING_L1 <- ""
NA_STRING_L2 <- ""

# Small helper functions to make the various steps obvious in the log
if(!exists("LOGFILE")) LOGFILE <- ""
log_info <- function(msg, logfile = LOGFILE) {
    cat(format(Sys.time()), msg, "\n", file = logfile, append = TRUE)
}
log_warning <- function(msg, logfile = LOGFILE) {
    log_info(paste("WARNING:", msg), logfile = logfile)
}
new_section <- function(name, logfile = LOGFILE, root = ROOT) {
    log_info("")
    log_info("===================================================")
    log_info(name)
}


# Copy quarto html output and log result
copy_output <- function(from, to, overwrite = TRUE) {
    if(file.copy(from, to, overwrite = overwrite)) {
        log_info(paste("html output copied to", outfile))
    } else {
        log_warning("Error copying html output")
    }
}


# Read a vector of CSV files with the same structure and bind data
read_csv_group <- function(files, col_types = NULL, quiet = FALSE, ...) {
    # Warnings are not allowed here, as this usually means a column format
    # problem that we want to fix immediately
    oldwarn <- options()$warn
    options(warn = 2)

    # File-reading function
    readf <- function(fn, quiet, ...) {
        if(!quiet) message("\tReading ", basename(fn))
        x <- read_csv(fn, col_types = col_types, ...)
        x
    }
    # Read all files, bind data frames, and return
    dat <- do.call("rbind", lapply(files, readf, quiet, ...))
    options(warn = oldwarn)
    dat
}

# File data into sub-folders based on what level data it is:
# L1_normalize outputs
#   Folders are site_plot_year_month
#   Filenames are Site_logger_table_year_month_hash
# L1 outputs
#   Folders are site_year
#   Filenames are site_plot_timeperiod_researchname_L1_version
# L2 outputs
#   Folders are site_year
#   Filenames are site_timeperiod_table_L2_version

# The data (x) should be a data frame with a POSIXct 'TIMESTAMP' column
# This is used to split the data for sorting into <yyyy>_<mm> folders
# Assumption: x represents a single site and plot
# Returns a list of filenames written (names) and number of data lines (values)
write_to_folders <- function(x, root_dir,
                             data_level,
                             site, plot,
                             logger, table, # only provided for L1_normalize
                             variable_metadata, # only provided for L1
                             version = "???",
                             derived_tempfile = FALSE,
                             quiet = FALSE, write_plots = TRUE) {
    # Sanity checks
    if("Plot" %in% names(x)) {
        stopifnot(length(unique(x$Plot)) == 1)
    }
    if("Site" %in% names(x)) {
        stopifnot(length(unique(x$Site)) == 1)
    }
    stopifnot("TIMESTAMP" %in% names(x))
    stopifnot("research_name" %in% names(x))

    # Prep: identify years and research names, along with current date
    years <- year(x$TIMESTAMP)
    research_names <- x$research_name
    nowyr <- year(Sys.Date())
    nowmo <- month(Sys.Date())
    vversion <- paste0("v", version)

    # Loop by years and research names
    lines_written <- list()
    for(y in unique(years)) {
        if(is.na(y)) {
            stop(data_level, " invalid year ", y)
        }

        for(rn in unique(research_names)) {
            write_this_plot <- FALSE

            # Isolate the data to write
            dat <- x[y == years & rn == research_names,]
            if(!nrow(dat)) {
                message("No data for ", y, "_", rn, " - skipping")
                next
            }

            # Sanity checks
            if(is.na(rn)) {
                stop(data_level, " invalid research_name ", rn)
            }
            if(y > nowyr) {
                stop("I am being asked to write ", nrow(dat), " rows of future data: ",
                     paste(site, logger, table, y))
            }

            # Construct folder and file names based on data_level
            time_period <- paste(format(min(dat$TIMESTAMP), format = "%Y%m%d"),
                                 format(max(dat$TIMESTAMP), format = "%Y%m%d"),
                                 sep = "-")
            if(data_level == "L1_normalize") {
                folder <- file.path(root_dir, paste(site, plot, y, rn, sep = "_"))
                # Add a short hash to end of filename to ensure we don't
                # overwrite anything that's already there (e.g. another
                # month's data)
                short_hash <- substr(digest::digest(dat, algo = "md5"), 1, 6)
                filename <- paste(logger, table, y, rn, short_hash, sep = "_")
                na_string <- NA_STRING_L1
            } else if(data_level == "L1") {
                # Isolate this research name's metadata
                vmd <- variable_metadata[variable_metadata$research_name == rn,]

                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste(site, plot, time_period, rn, data_level, vversion, sep = "_")
                na_string <- NA_STRING_L1
                write_this_plot <- TRUE
                p <- ggplot(x, aes(TIMESTAMP, Value, group = paste(Instrument_ID, Sensor_ID))) +
                    geom_line(na.rm = TRUE) +
                    facet_wrap(~research_name, scales = "free") +
                    ylab(paste0(vmd$research_name, " (", vmd$final_units, ")")) +
                    theme(axis.text = element_text(size = 10),
                          strip.text = element_text(size = 10))

                # If any data are out of bounds, show those bounds
                if(any(x$F_OOB, na.rm = TRUE)) {
                    p <- p + geom_hline(yintercept = vmd$low_bound,
                                        linetype = 2, color = "blue",
                                        na.rm = TRUE) +
                        geom_hline(yintercept = vmd$high_bound,
                                   linetype = 2, color = "blue",
                                   na.rm = TRUE) +
                        ggtitle(filename,
                                subtitle = "Dashed lines show instrument bounds")
                } else {
                    p <- p + ggtitle(filename)

                }
            } else if(data_level == "L2_qaqc") {
                folder <- file.path(root_dir, paste(site, y, sep = "_"))
                filename <- paste(site, plot, y, rn, data_level, vversion, sep = "_")
                na_string <- NA_STRING_L2
            } else if(data_level == "L2") {
                filename <- paste(site, plot, y, rn, data_level, vversion, sep = "_")
                na_string <- NA_STRING_L2
                if(derived_tempfile) {
                    # The L2.qmd step has a derived variables step that needs to
                    # save its as a results as in a temporary place
                    folder <- file.path(root_dir, "derived-tempfiles/")
                } else {
                    # Isolate this research name's metadata
                    vmd <- variable_metadata[variable_metadata$research_name == rn,]

                    folder <- file.path(root_dir, paste(site, y, sep = "_"))

                    write_this_plot <- TRUE
                    p <- ggplot(x, aes(TIMESTAMP, Value, group = paste(Instrument_ID, Sensor_ID))) +
                        geom_line(aes(y = Value_GF_MAC), linetype = 2, color = "lightgrey", na.rm = TRUE) +
                        geom_line(na.rm = TRUE) +
                        #facet_wrap(~research_name, scales = "free") +
                        ylab(paste0(vmd$research_name, " (", vmd$final_units, ")")) +
                        theme(axis.text = element_text(size = 10),
                              strip.text = element_text(size = 10))
                }
            } else {
                stop("Unkown data_level ", data_level)
            }
            filename <- paste0(filename, ".csv")

            # Create folder, if needed
            if(!dir.exists(folder)) {
                if(!quiet) message("Creating ", basename(folder))
                dir.create(folder, showWarnings = FALSE)
            }

            # Convert timestamp to character to ensure that observations
            # at midnight have seconds written correctly
            if(is.POSIXct(dat$TIMESTAMP)) {
                dat$TIMESTAMP <- format(dat$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
            }

            # Write data
            if(!quiet) message("Writing ", nrow(dat), "/", nrow(x),
                               " rows of data to ",
                               basename(folder), "/", filename)

            fqfn <- file.path(folder, filename)
            if(file.exists(fqfn)) message("\tNOTE: overwriting existing file")
            # We were using readr::write_csv for this but it was
            # randomly crashing on GA (Error in `vroom write()`: ! bad value)
            write.csv(dat, fqfn, row.names = FALSE, na = na_string)
            if(!file.exists(fqfn)) {
                stop("File ", fqfn, "was not written")
            }

            # The L2.qmd step has a derived variables step that needs to
            # know where its file went to
            if(derived_tempfile) return(fqfn)

            # Write basic QA/QC plot
            # We use cairo_pdf to better handle Unicode chars in axis labels
            if(write_plots && write_this_plot) {
                fn_p <- gsub("csv$", "pdf", fqfn)
                ggsave(fn_p, plot = p, width = 12, height = 8, device = cairo_pdf)
            }

            lines_written[[fqfn]] <- nrow(dat)
        } # for m
    } # for y
    invisible(lines_written)
}


# Reset the system by removing all intermediate files in L0, L1_normalize,
# L1, L2, and Logs folders
reset <- function(root = here::here("pipeline/data_TEST")) {
    message("root is ", root)

    remove_items <- function(dir, pat = "(txt|pdf|csv|html)$") {
        items <- list.files(file.path(root, dir), include.dirs = TRUE,
                            recursive = TRUE,
                            pattern = pat, full.names = TRUE)

        # Don't remove READMEs or R files
        # items <- items[basename(items) != "README.md"]
        # items <- items[grep("\\.R$", items, invert = TRUE)]

        message("Removing ", length(items), " items in ", dir)
        file.remove(items)
    }

    remove_items("L0/")
    remove_items("L1_normalize/")
    # remove L1_normalize folders
    remove_items("L1_normalize/", pat = "[A-Z]{3}_[A-Z]+_[0-9]{4}_[a-z-]+")
    remove_items("L1/")
    # remove L1 folders
    remove_items("L1/", pat = "[A-Z]{3}_[0-9]{4}")
    # remove L1 code examples
    remove_items("L1/code_examples/", pat = "")
    remove_items("L1/", pat = "code_example")

    remove_items("L2_qaqc/")
    remove_items("L2/")
    remove_items("Logs/")

    message("All done.")
}


# The design links might not be stable over time; for example, if a tree
# dies, its sensor might get reassigned to a new tree. In this case the
# design_link table will have two entries, one for the old assignment and
# one for the new. We know which one to use by the "valid_through" column,
# which give a end date for a design link--or, most commonly, it will be
# empty (NA) indicating that there is no end date.
#
# So, when the design_link table is merged with a data table, if a reassignment
# has occurred, some data rows will get repeated with the different possible
# design links.
#
# This function uses the object (i.e. group identifier; typically, Logger+
# Table+Loggernet_variable), timestamp, and valid_through timestamps to identify
# which rows to keep (correct design_link assignment) and which to drop.
valid_entries <- function(objects, times, valid_through) {
    # Nothing to do if there are no valid_through entries
    if(all(is.na(valid_through))) return(rep(TRUE, length(objects)))

    # Any NA valid_through entries apply into the far future
    valid_through[is.na(valid_through)] <- MAX_DATE
    past_valid_time <- times > valid_through

    # Create a data frame to aggregate and then merge, below
    x <- data.frame(num = seq_along(objects), obj = objects, time = times, vu = valid_through)
    # Compute the minimum valid_through entry for each object and time that is
    # not past the valid_through point; this is the 'controlling' value
    y <- aggregate(vu ~ obj + time, data = x[!past_valid_time,], FUN = min)
    names(y)[3] <- "controlling"

    # Figure out controlling valid_through for each object/time
    z <- merge(x, y, all.x = TRUE)
    z <- z[order(z$num),] # ensure in correct original order
    # An NA controlling entry means there is none
    valids <- z$vu == z$controlling
    valids[is.na(valids)] <- FALSE

    valids
}

# Test code for valid_entries()
test_valid_entries <- function() {
    # Sample data. We have two objects (sensors) at time points 1:3
    test_data <- data.frame(obj = c(1, 1, 1, 2, 2, 2), time = c(1, 2, 3, 1, 2, 3))
    # Object 2 changes its design link after time 2
    test_dt <- data.frame(obj = c(1,2,2),
                          dl = c("A", "B", "C"),
                          valid_through = c(NA, 2, NA))
    # Merge the 'data' with the 'design link table'
    x <- merge(test_data, test_dt)
    # Call valid_entries. It figures out that all the object 1 entries should be
    # retained, but 1 of 2 entries in each timestep should be dropped for object 2.
    # This is because there are two design_table entries for it (see above); the
    # first ends at time point 2, and the second is indefinite after that.
    valid_entries(x$obj, x$time, x$valid_through)

    # No shifting objects
    ret <- valid_entries(c(1, 1, 1), c(1, 2, 3), c(NA, NA, NA))
    stopifnot(all(ret))
    # One object, shift time is never reached
    ret <- valid_entries(c(1, 1, 1, 1), c(1, 1, 2, 2), c(4, NA, 4, NA))
    stopifnot(ret == c(TRUE, FALSE, TRUE, FALSE))
    # One object, shift time is in the past
    ret <- valid_entries(c(1, 1, 1, 1), c(3, 3, 4, 4), c(2, NA, 2, NA))
    stopifnot(ret == c(FALSE, TRUE, FALSE, TRUE))
    # One object, shifts
    ret <- valid_entries(c(1, 1, 1, 1), c(2, 2, 3, 3), c(2, NA, 2, NA))
    stopifnot(ret == c(TRUE, FALSE, FALSE, TRUE))
    # One objects, shifts twice (valid_throughs at 1 and 2)
    ret <- valid_entries(objects = rep(1, 9),
                         times = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                         valid_through = c(1, 2, NA, 1, 2, NA, 1, 2, NA))
    stopifnot(ret == c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
    # Two objects, only one shifts
    ret <- valid_entries(objects = c(1, 1, 1, 2, 2, 2, 2, 2, 2),
                         times = c(1, 2, 3, 1, 1, 2, 2, 3, 3),
                         valid_through = c(NA, NA, NA, 2, NA, 2, NA, 2, NA))
    stopifnot(ret == c(TRUE, TRUE, TRUE, # obj 1
                       TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)) # obj 2
    # There's a valid_through but no new entry
    ret <- valid_entries(objects = c(1, 1),
                         times = c(1, 2),
                         valid_through = c(1, 1))
    stopifnot(ret == c(TRUE, FALSE))
}
test_valid_entries()
