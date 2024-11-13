# out-of-service.R

library(lubridate)


# We read in a version of the Aquatroll Calibration/Removal Log
# (in Monitoring Documents on the COMPASS Google Drive) and restructure it
# into a form ready for out-of-service calculations in L1_normalize.qmd
prep_troll_oos_table <- function(troll) {
    # If no time_pulled given, assume 6 AM
    tp <- troll$Time_pulled
    tp[is.na(tp) | tp == ""] <- "06:00"

    # If no time_replaced given, assume 6 PM
    tr <- troll$Time_replaced
    tr[is.na(tr) | tr == ""] <- "18:00"

    # If no date_replaced given, assume ongoing
    dr <- troll$Date_replaced
    dr[is.na(dr) | dr == ""] <- "12/31/2999"

    # Calculate out of service windows
    troll$oos_begin <- mdy(troll$Date_pulled, tz = "EST") + hm(tp)
    troll$oos_end <- mdy(dr, tz = "EST") + hm(tr)
    # Per Peter R., we throw out all data for 24 hours after replacement
    troll$oos_end <- troll$oos_end + 60 * 60 * 24

    # The L1 data have character, not PosixCT, timestamps, so for correct
    # comparisons in oos() below need to have that be true here too
    troll$oos_begin <- as.character(troll$oos_begin)
    troll$oos_end <- as.character(troll$oos_end)

    # Rename columns to match the design table entries
    troll$Instrument_ID <- troll$Troll
    troll$Plot <- troll$Location

    # Return a data frame with the needed columns: the oos begin and end,
    # as well as the additional columns to match
    troll[c("Site", "Plot", "Instrument_ID", "oos_begin", "oos_end")]
}

# Restructure the EXO calibration/deployment log
# (https://docs.google.com/spreadsheets/d/1y08XAOzzpLZbIOMJquVUv-jSHXhHJpF9neG5kcJpnjQ/edit#gid=913708372)
prep_exo_oos_table <- function(exo) {
    # Out of service until deployment
    exo$oos_begin <-ymd(paste0(exo$year, "-01-01"), tz = "EST")
    exo$oos_end <- force_tz(exo$deployed + hm("21:00"), tzone = "EST")

    # Out of service after retrieval
    exo2 <- tibble(
        Site = exo$Site,
        oos_begin = force_tz(exo$retrieved + hm("03:00"), tzone = "EST"),
        oos_end = ymd(paste0(exo$year, "-12-31"), tz = "EST")
    )

    # And, out of service when out of water
    exo3 <- tibble(
        Site = exo$Site,
        oos_begin = force_tz(exo$out_of_water + hm("03:00"), tzone = "EST"),
        oos_end = force_tz(exo$out_of_water + hm("21:00"), tzone = "EST")
    )

    exo <- exo[c("Site", "oos_begin", "oos_end")]
    exo <- rbind(exo, exo2, exo3)

    # The L1 data have character, not PosixCT, timestamps, so for correct
    # comparisons in oos() below need to have that be true here too
    exo$oos_begin <- as.character(exo$oos_begin)
    exo$oos_end <- as.character(exo$oos_end)

    exo[!is.na(exo$oos_begin),]
}

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
