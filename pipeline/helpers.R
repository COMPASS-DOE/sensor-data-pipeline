# Helper functions

library(lubridate)
library(readr)
#library(arrow)

# Constants used in this file and elsewhere in the system
GIT_COMMIT <- substr(system("git rev-parse HEAD", intern = TRUE), 1, 7)

# A date way far in the future, used by valid_entries()
MAX_DATE <- ymd_hms("2999-12-31 11:59:00")

# Data NA (not available) strings to use on writing
NA_STRING_L1 <- ""
NA_STRING_L2 <- ""

# Types of files written by each step
FILE_TYPES <- c("L1_normalize"  = ".csv",
                "L1"            = ".csv",
                "L2_qaqc"       = ".parquet",
                "L2"            = ".parquet")

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

# Identify which elements of a vector can be converted to numeric
# I tried using a regex, but it's very complicated and prone to edge case failure
# So just do the conversion and test what resulted in NA
can_convert_to_numeric <- function(x) {
    suppressWarnings(
        y <- as.numeric(x)
    )
    return(is.na(x) | !is.na(y))
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

# Helper function for a helper function: make the L1 plot
# for write_to_folders()
make_L1_plot <- function(x, vmd, filename) {
    # Above a certain number of rows, our plot sizes get very large
    # with no visual benefit (can't see that many points)
    very_large_cutoff <- 100000
    if(nrow(x) > very_large_cutoff) {
        pct <- round(very_large_cutoff / nrow(x) * 100, 0)
        x <- x[sample(nrow(x), very_large_cutoff),]
        filename <- paste0(filename, " (subsampled ", pct, "%)")
    }

    # Color points if out of bounds, service, or potential outlier
    x$bad <- as.integer(as.logical(x$F_OOB) | as.logical(x$F_OOS)) # out
    x$bad[as.logical(x$F_MAD)] <- 2
    x$bad[is.na(x$bad)] <- 0
    x$bad <- factor(x$bad, levels = 0:2)
    p <- ggplot(x, aes(TIMESTAMP, Value, color = bad,
                       group = paste(Instrument_ID, Sensor_ID))) +
        geom_line(na.rm = TRUE) +
        scale_color_manual(values = c("black", "red", "orange"), breaks = 0:2) +
        facet_wrap(~research_name, scales = "free") +
        ylab(paste0(vmd$research_name, " (", vmd$final_units, ")")) +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 10),
              plot.subtitle = element_text(size = 8),
              legend.position = "none") +
        ggtitle(filename,
                subtitle = "Orange = possible outlier; red = out of bounds or service; dashed lines are instrument bounds")

    # If graph covers more than couple months, axis
    # labels should be "Jan 2024", "Feb 2024", etc.
    if(max(x$TIMESTAMP) - min(x$TIMESTAMP) > period("2 months")) {
        p <- p + scale_x_datetime(date_breaks = "1 month",
                                  date_labels =  "%b %Y")
    }

    # If any data are out of bounds, use variable metadata to show bounds
    if(any(as.logical(x$F_OOB), na.rm = TRUE)) {
        p <- p + geom_hline(yintercept = vmd$low_bound,
                            linetype = 2,
                            na.rm = TRUE) +
            geom_hline(yintercept = vmd$high_bound,
                       linetype = 2,
                       na.rm = TRUE) +
            ggtitle(filename)
    }
    return(p)
}

make_L2_plot <- function(x, vmd, filename) {
    # Above a certain number of rows, our plot sizes get very large
    # with no visual benefit (can't see that many points)
    very_large_cutoff <- 150000
    if(nrow(x) > very_large_cutoff) {
        pct <- round(very_large_cutoff / nrow(x) * 100, 0)
        x <- x[sample(nrow(x), very_large_cutoff),]
        filename <- paste0(filename, " (subsampled ", pct, "%)")
    }

    x$gf <- is.na(x$Value) & !is.na(x$Value_MAC) # flag for gap-fill or not
    p <- ggplot(x, aes(TIMESTAMP, Value_MAC, color = gf,
                       alpha = gf,
                       group = paste(Instrument_ID, Sensor_ID))) +
        geom_line(na.rm = TRUE) +
        scale_color_manual(values = c("black", "blue")) +
        # make gap-fill lines partially transparent, so as not to obscure data
        scale_alpha_manual(values = c(1.0, 0.6)) +
        ylab(paste0(vmd$research_name, " (", vmd$final_units, ")")) +
        ggtitle(filename,
                subtitle = "Blue = available inferred data based on mean annual cycle") +
        theme(axis.text = element_text(size = 10),
              strip.text = element_text(size = 10),
              plot.subtitle = element_text(size = 8),
              legend.position = "none")

    # If graph covers more than couple months, axis
    # labels should be "Jan 2024", "Feb 2024", etc.
    if(max(x$TIMESTAMP) - min(x$TIMESTAMP) > period("2 months")) {
        p <- p + scale_x_datetime(date_breaks = "1 month",
                                  date_labels =  "%b %Y")
    }
    return(p)
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

            is_numeric_data <- all(can_convert_to_numeric(dat$Value))

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
                if(is_numeric_data){
                    write_this_plot <- TRUE
                    dat$Value <- as.numeric(dat$Value)
                    p <- make_L1_plot(dat, vmd, filename)
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
                    # save its results in a temporary place
                    folder <- file.path(root_dir, "derived-tempfiles/")
                } else {
                    folder <- file.path(root_dir, paste(site, y, sep = "_"))

                    if(is_numeric_data){

                        write_this_plot <- TRUE
                        # Isolate this research name's metadata
                        vmd <- variable_metadata[variable_metadata$research_name == rn,]
                        p <- make_L2_plot(x, vmd, filename)
                    }
                } # else derived_tempfile
            } else {
                stop("Unkown data_level ", data_level)
            }
            frmt <- FILE_TYPES[data_level]
            data_filename <- paste0(filename, frmt)

            # Create folder, if needed
            if(!dir.exists(folder)) {
                if(!quiet) message("Creating ", basename(folder))
                dir.create(folder, showWarnings = FALSE)
            }

            # Write data
            if(!quiet) message("\tWriting ", nrow(dat), "/", nrow(x),
                               " rows of data to ",
                               basename(folder), "/", data_filename)

            fqfn <- file.path(folder, data_filename)
            if(file.exists(fqfn)) message("\tNOTE: overwriting existing file")
            if(frmt == ".csv") {
                # Convert timestamp to character to ensure that observations
                # at midnight have seconds written correctly
                if(is.POSIXct(dat$TIMESTAMP)) {
                    dat$TIMESTAMP <- format(dat$TIMESTAMP, "%Y-%m-%d %H:%M:%S")
                }
                # We were using readr::write_csv for this but it was
                # randomly crashing on GA (Error in `vroom write()`: ! bad value)
                write.csv(dat, fqfn, row.names = FALSE, na = na_string)
            } else if(frmt == ".parquet") {
                arrow::write_parquet(dat, fqfn)
            } else {
                stop("Unknown format ", frmt)
            }
            if(!file.exists(fqfn)) {
                stop("File ", fqfn, "was not written")
            }

            # The L2.qmd step has a derived variables step that needs to
            # know where its file went to
            if(derived_tempfile) return(fqfn)

            # Write basic QA/QC plot
            # We use cairo_pdf to better handle Unicode chars in axis labels
            if(is_numeric_data && write_plots && write_this_plot) {
                plot_filename <- file.path(folder, paste0(filename, ".pdf"))
                if(!quiet) message("\tWriting plot to ",
                                   basename(folder), "/", plot_filename)

                ggsave(plot_filename, plot = p, width = 10, height = 7, device = cairo_pdf)
            }

            lines_written[[data_filename]] <- nrow(dat)
        } # for m
    } # for y
    invisible(lines_written)
}


# Reset the system by removing all intermediate files in L0, L1_normalize,
# L1, L2, and Logs folders
reset <- function(root = here::here("pipeline/data_TEST")) {
    message("root is ", root)

    remove_items <- function(dir, pat = "(txt|pdf|png|csv|html)$") {
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
    remove_items("L1/code_examples_L1/", pat = "(R|md)$")

    remove_items("L2_qaqc/")
    remove_items("L2/")
    # remove L2 code examples
    remove_items("L2/code_examples_L2/", pat = "(R|md)$")
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
# The reason this isn't trivial (i.e., why we can't just filter out any
# entries with a timestamp greater than the valid_through value) is that
# *before* we hit the valid_through time point, there are two 'ok' entries
# and we want to take the one with the minimum valid_through. Ugh!
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

# A simple mechanism to allow multiple timers that accumulate time
# For diagnosing slow parts of L1, etc.
timer_tracker <- c()
timer_starts <- c()

start_timer <- function(timer) {
    if(timer %in% names(timer_starts)) stop("Timer ", timer, " is already running")
    timer_starts[timer] <<- as.double(Sys.time())
}

stop_timer <- function(timer) {
    if(!timer %in% names(timer_starts)) stop("Timer ", timer, " is not running")

    tm <- as.double(Sys.time()) - timer_starts[timer]
    timer_starts <<- timer_starts[-which(names(timer_starts) == timer)]
    if(!timer %in% names(timer_tracker)) {
        timer_tracker[timer] <<- tm
    } else {
        timer_tracker[timer] <<- timer_tracker[timer] + tm
    }
}
