# L1_normalize-utils.R
# Utility functions, and test code, used exclusively by L1_normalize.qmd
# BBL March 2024

library(lubridate)

# Expand a string: look for patterns like \{x,y,z\} within a possibly
# larger string, and break them apart at the commas
# So "Hello {A,B2,C}" -> c("Hello A", "Hello B2", "Hello C")
# It also handles numerical sequences: "x{1:3}" -> c("x1", "x2", "x3")
# Comma expansions are performed before colon expansions:
# "{A,B{1:3},C}" -> c("A", "B1", "B2", "B3", "C")

#' Expand a string based on comma or colon patterns
#'
#' @param s String
#' @param expand_comma Look for comma expansions ("\{X,Y,...\}")? Logical
#' @param expand_colon Look for colon expansions ("\{X:Y\}")? Logical
#' @param quiet Be quiet or print diagnostic messages? Logical
#' @export
#' @return The expanded string, as a character vector
#' @description look for patterns like "\{x,y,z\}" within a possibly
# larger string, and break them apart at the commas
# So "Hello {A,B2,C}" -> c("Hello A", "Hello B2", "Hello C")
# It also handles numerical sequences: "x\{1:3\}" -> c("x1", "x2", "x3")
# Comma expansions are performed before colon expansions:
# "\{A,B\{1:3\},C\}" -> c("A", "B1", "B2", "B3", "C")
#' @examples
#' expand_string("{A,B2,C}")
#' expand_string("B0{1:3}")
expand_string <- function(s, expand_comma = TRUE, expand_colon = TRUE, quiet = TRUE) {
    if(!quiet) message(s)
    if(is.na(s)) return(s)

    # Look for 1+ "words" (groups of characters followed by a comma)
    # followed by commas (and perhaps white space), and then a final word
    if(expand_comma) {
        COMMA_PATTERN <- "\\{(.+,)+.+\\}"
        matches <- regexpr(COMMA_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ",")[[1]]
            subs[1] <- gsub("^\\{", "", subs[1]) # get rid of beginning...
            subs[length(subs)] <- gsub("\\}$", "", subs[length(subs)]) # ...and end curly braces
            s <- rep(s, length(subs))
            newmatches <- regexpr(COMMA_PATTERN, s)
            regmatches(s, newmatches) <- trimws(subs)
            # Recurse once to look for possible colon expansions
            s <- unlist(sapply(s, expand_string,
                               expand_comma = FALSE, expand_colon = expand_colon,
                               USE.NAMES = FALSE))
            return(s)
        }
    }

    if(expand_colon) {
        # Look for two numbers separated by a colon, with optional white space
        COLON_PATTERN <- "\\{\\s*\\d+\\s*:\\s*\\d+\\s*\\}"
        matches <- regexpr(COLON_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ":")[[1]]
            subs <- gsub("[\\{\\}]", "", subs) # get rid of curly braces
            subs <- seq.int(from = subs[1], to = subs[2])
            s <- rep(s, length(subs))
            newmatches <- regexpr(COLON_PATTERN, s)
            regmatches(s, newmatches) <- subs
        }
    }

    s
}


#' Expand a data frame by looking for string expansion patterns
#'
#' @param df The data frame to expand
#'
#' @return The expanded data frame.
#' @description Expand a data frame: look for patterns like x,y,z within
#' entries and, for that row, replicate it and use expand_string to break
#' apart the x,y,z. Multiple expansions within a row are OK as long as
#' they're the same length.
#' @export
expand_df <- function(df) {
    results <- list()
    for(i in seq_len(nrow(df))) {
        dfr <- df[i,,drop=FALSE]  # row we're working on
        # Figure out max expansion (may be 1) and replicate row that many times
        expand_lens <- sapply(dfr, function(x) length(expand_string(x)))
        new_df <- dfr[rep(1, max(expand_lens)),,drop=FALSE]

        # Can't have mismatches (except for 1-length)
        # E.g. "x,y" in one cell and "x,y,z" in another
        if(length(setdiff(unique(expand_lens), 1)) > 1) {
            stop("Row ", i, " has mismatched expansion entries")
        }

        # For each column, expand its entry string as needed
        if(nrow(new_df) > 1) {
            for(col in seq_along(new_df)) {
                new_df[col] <- expand_string(new_df[[1, col]])
            }
        }
        results[[i]] <- new_df
    }

    do.call("rbind", results)
}

library(testthat)
#test_that("expand_string works", {
# No expansion
expect_identical("hello", expand_string("hello"))

# Comma separator
expect_identical(c("A", "B"), expand_string("{A,B}"))
expect_identical(c("CA", "CB"), expand_string("C{A,B}"))
expect_identical(c("AD", "BD"), expand_string("{A,B}D"))
expect_identical("A,B", expand_string("A,B")) # no braces
expect_identical("{A,B}", expand_string("{A,B}", expand_comma = FALSE))

# Colon separator
expect_identical(c("1", "2"), expand_string("{1:2}"))
expect_identical(c("C1", "C2"), expand_string("C{1:2}"))
expect_identical(c("1D", "2D"), expand_string("{1:2}D"))
expect_identical("1:2", expand_string("1:2")) # no braces
expect_identical("{A:B}", expand_string("{A:B}")) # no numbers
expect_identical("{1:2}", expand_string("{1:2}", expand_colon = FALSE))

# Nesting
expect_identical(c("A", "B1", "B2", "C"), expand_string("{A,B{1:2},C}"))

# Misc
expect_message(expand_string("AAA", quiet = FALSE), regexp = "AAA")
#})

#test_that("expand_df works", {
# No expansion
x <- data.frame(x = 1)
expect_identical(expand_df(x), x)
x <- dplyr::tibble(x = 1)
expect_identical(expand_df(x), x)

# Comma separator
x <- data.frame(C1 = "{A,B}", C2 = "1")
y <- expand_df(x)
expect_identical(nrow(y), 2L)
expect_identical(y$C1, c("A", "B"))
expect_identical(y$C2, c("1", "1")) # replicates

x <- data.frame(C1 = "{A,B}", C2 = "{C,D}")
y <- expand_df(x)
expect_identical(y$C2, c("C", "D")) # sequences

# Colon separator
x <- data.frame(C1 = "{1:2}", C2 = "1")
y <- expand_df(x)
expect_identical(nrow(y), 2L)
expect_identical(y$C1, c("1", "2"))
expect_identical(y$C2, c("1", "1")) # replicates

x <- data.frame(C1 = "{2:1}", C2 = "{C,D}")
y <- expand_df(x)
expect_identical(y$C1, c("2", "1")) # descending
expect_identical(y$C2, c("C", "D")) # sequences

# Nesting

#})


#' Perform unit conversion based on a data frame of data and units table
#'
#' @param dat Data frame with \code{research_name} and \code{value} columns
#' @param ut Data frame with \code{research_name}, \code{conversion}, and \code{new_unit} columns
#' @param quiet Print progress messages?
#'
#' @return The data frame with new columns \code{value_conv} and \code{units}.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' x <- data.frame(value = 1:3, research_name = c("a", "a", "b"))
#' y <- data.frame(research_name = c("a", "b"), conversion = c("x * 1", "(x * 2) - 1"), new_unit = "")
#' unit_conversion(x, y)
unit_conversion <- function(dat, ut, quiet = FALSE) {
    if(!all(c("research_name", "conversion", "new_unit") %in% names(ut))) {
        stop("The units table data frame isn't structured correctly")
    }

    dat_conv <- list()

    # Isolate the various research_name entries one by one, find corresponding
    # conversion string, evaluate it against the `value` data column
    for(rn in unique(dat$research_name)) {
        d <- dat[dat$research_name == rn,] # filter
        x <- d$value # this `x` name is crucial; used by conversion strings
        which_ut <- which(ut$research_name == rn)
        # Isolate conversion string
        if(length(which_ut) == 0) {
            conv <- "<not found>"
            d$value_conv <- NA_real_
            d$units <- NA_character_
        } else if(length(which_ut) == 1) {
            conv <- ut$conversion[which_ut]
            # ...and evaluate it
            out <- try(eval(parse(text = conv)))
            if(is.numeric(out) & length(out) == nrow(d)) {
                d$value_conv <- out
            } else {
                stop("Error evaluating conversion string for research_name ", rn)
            }
            d$units <- ut$new_unit[which_ut]
        } else {
            stop("Multiple conversions for ", rn)
        }

        if(!quiet) message("\t\tUC: ", rn, " n=", nrow(d), ", conv=", conv)

        dat_conv[[rn]] <- d
    }
    if(length(dat_conv)) {
        do.call("rbind", dat_conv)
    } else {
        data.frame()
    }
}

#test_that("unit_conversion works", {
x <- data.frame(value = 1:3, research_name = c("a", "a", "b"))

# Empty unit conversion table - should be all NA
empty <- data.frame(research_name = "", conversion = "", new_unit = "")
z <- unit_conversion(x, empty, quiet = TRUE)
expect_true(all(is.na(z$value_conv)))

# Empty empty
z <- unit_conversion(data.frame(), empty)
expect_s3_class(z, "data.frame")
expect_identical(nrow(z), 0L)

# Bad units table
expect_error(unit_conversion(x, cars), regexp = "isn't structured correctly")

# Multiple conversions for a research_name
y <- data.frame(research_name = c("a", "a"), conversion = c("x * 1", "(x * 2) - 1"), new_unit = "")
expect_error(unit_conversion(x, y), regexp = "Multiple conversions")

# Error in conversion string
# Suppress the output
f <- file(tempfile(), "a")
sink(f, type = "message")
# evaluation error
y <- data.frame(research_name = c("a", "b"), conversion = c(" * 1", "(x * 2) - 1"), new_unit = "")
expect_error(unit_conversion(x, y), regexp = "Error evaluating conversion string")
# tries to return non-numeric
y <- data.frame(research_name = c("a", "b"), conversion = c("cars", "(x * 2) - 1"), new_unit = "")
expect_error(unit_conversion(x, y), regexp = "Error evaluating conversion string")
#  sink() # Seems like testthat removes the sink

# Respects quiet
y <- data.frame(research_name = c("a", "b"), conversion = c("x * 1", "(x * 2) - 1"), new_unit = "")
expect_silent(unit_conversion(x, y, quiet = TRUE))
#})


# We pass an oos_df data frame to oos()
# This is a table of out-of-service windows
# It MUST have `oos_begin` and `oos_end` timestamps; optionally,
# it can have other columns that must be matched as well
# (e.g., site, plot, etc)

# The second thing we pass is the observation data frame

# This function returns a logical vector, of the same length as the data_df
# input, that becomes F_OOS
oos <- function(oos_df, data_df) {
    oos_df <- as.data.frame(oos_df)

    # Make sure that any 'extra' condition columns (in addition to the
    # oos window begin and end) are present in the data d.f.
    non_ts_fields <- setdiff(colnames(oos_df), c("oos_begin", "oos_end"))
    if(!all(non_ts_fields %in% colnames(data_df))) {
        stop("Not all out-of-service condition columns are present in data!")
    }
    # For speed, compute the min and max up front
    min_ts <- min(data_df$TIMESTAMP)
    max_ts <- max(data_df$TIMESTAMP)

    oos_final <- rep(FALSE, nrow(data_df))

    for(i in seq_len(nrow(oos_df))) {
        # First quickly check: is there any overlap in timestamps?
        timestamp_overlap <- min_ts <= oos_df$oos_end[i] &&
            max_ts >= oos_df$oos_begin[i]
        #     message("timestamp_overlap = ", timestamp_overlap)
        if(timestamp_overlap) {
            oos <- data_df$TIMESTAMP >= oos_df$oos_begin[i] &
                data_df$TIMESTAMP <= oos_df$oos_end[i]
            # There are timestamp matches, so check other (optional)
            # conditions in the oos_df; they must match exactly
            # For example, if there's a "Site" entry in oos_df then only
            # data_df entries with the same Site qualify to be o.o.s
            for(f in non_ts_fields) {
                matches <- data_df[,f] == oos_df[i,f]
                #               message("f = ", f, " ", oos_df[i,f], ", matches = ", sum(matches))
                oos <- oos & matches
            }

            # The out-of-service flags for this row of the oos_df table
            # are OR'd with the overall flags that will be returned below
            # I.e., if ANY of the oos entries triggers TRUE, then the
            # datum is marked as out of service
            oos_final <- oos_final | oos
        }
    }
    return(oos_final)
}

# Test code for oos()
test_oos <- function() {
    data_df <- data.frame(TIMESTAMP = 1:3, x = letters[1:3], y = 4:6)

    # No other conditions beyond time window
    oos_df <- data.frame(oos_begin = 1, oos_end = 1)
    stopifnot(oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
    oos_df <- data.frame(oos_begin = 4, oos_end = 5)
    stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
    oos_df <- data.frame(oos_begin = 0, oos_end = 2)
    stopifnot(oos(oos_df, data_df) == c(TRUE, TRUE, FALSE))
    oos_df <- data.frame(oos_begin = 0, oos_end = 3)
    stopifnot(oos(oos_df, data_df) == c(TRUE, TRUE, TRUE))

    # x condition - doesn't match even though timestamp does
    oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "b")
    stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))
    # x condition - matches and timestamp does
    oos_df <- data.frame(oos_begin = 1, oos_end = 1, x = "a")
    stopifnot(oos(oos_df, data_df) == c(TRUE, FALSE, FALSE))
    # x condition - some match, some don't
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b")
    stopifnot(oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
    # x and y condition
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "b", y = 5)
    stopifnot(oos(oos_df, data_df) == c(FALSE, TRUE, FALSE))
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, x = "a", y = 5)
    stopifnot(oos(oos_df, data_df) == c(FALSE, FALSE, FALSE))

    # Error thrown if condition column(s) not present
    oos_df <- data.frame(oos_begin = 1, oos_end = 2, z = 1)
    out <- try(oos(oos_df, data_df), silent = TRUE)
    stopifnot(class(out) == "try-error")
}
test_oos()

# Read all out-of-service files and check their formatting
read_oos_data <- function(oos_dir) {
    message("Dir is ", oos_dir)
    oos_files <- list.files(oos_dir, pattern = "\\.csv$", full.names = TRUE)
    message("Files: ", length(oos_files))

    # Read files and check for required columns
    oos_data <- lapply(oos_files, function(f) {
        message(f)
        x <- read_csv(f, col_types = list(oos_begin = col_character(),
                                          oos_end = col_character()))
        required <- c("Site", "oos_begin", "oos_end")

        if(!all(required %in% names(x))) {
            missing <- setdiff(required, names(x))
            stop("Required column(s) ", paste(missing, collapse = ","),
                 " not present in ", f)
        }
        x
    })

    # Make list names
    names(oos_data) <- sapply(oos_files, function(x) {
        gsub(".csv", "", basename(x), fixed = TRUE)
    })
    return(oos_data)
}
