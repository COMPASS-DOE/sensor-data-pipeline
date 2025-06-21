# L2-utils.R
# Utility functions, and test code, used exclusively by L1.qmd
# BBL June 2025

library(testthat)

# Fill a gap in a vector x, based on the values immediately before
# and after the gap and the mean annual cycle (mac)
# Returns a numeric vector exactly as long as the gap
fill_gap <- function(x, gap_begin, gap_end, mac) {
    if(!gap_end >= gap_begin) {
        stop("I am seeing ", gap_begin, " ", gap_end, " in ", length(x))
    }
    stopifnot(gap_end >= gap_begin)
    stopifnot(gap_begin > 0 && gap_begin <= length(x))
    stopifnot(gap_end > 0 && gap_end <= length(x))
    stopifnot(length(x) == length(mac))

    # x and mac are numeric vectors of equal length
    # gap_begin and gap_end are the indices of the first and last NAs of the gap

    if(gap_begin > 1) {
        left_adjust <- x[gap_begin - 1] - mac[gap_begin - 1]
    } else {
        left_adjust <- 0
    }
    #message("left_adjust ", left_adjust)
    if(gap_end < length(x)) {
        right_adjust <- x[gap_end + 1] - mac[gap_end + 1]
    } else {
        right_adjust <- 0
    }
    #message("right_adjust ", right_adjust)

    # Calculate adjustments for the gap points plus the pre- and post-gap
    # good data points that make up the calculation of left_adjust and right_adjust
    adj <- seq(left_adjust, right_adjust, length.out = gap_end - gap_begin + 3)
    # ...but then drop those out-of-gap adjustment values
    adj <- adj[c(-1, -length(adj))]
    # Return the adjusted mean annual cycle values to fill the gap
    mac[gap_begin:gap_end] + adj
}

# Test code
expect_identical(fill_gap(c(1, 2, NA), 3, 3, 1:3), 3)
expect_identical(fill_gap(c(1, 2, NA), 3, 3, c(1, 2, NA)), NA_real_)
expect_equivalent(fill_gap(c(1, NA, NA, 4), 2, 3, 1:4), 2:3)
expect_equivalent(fill_gap(c(2, NA, NA, 5), 2, 3, 1:4), 3:4)

expect_error(fill_gap(1:2, 3, 2), regexp = "I am seeing")
expect_error(fill_gap(1:2, 1, 1, 1), regexp = "length")
expect_error(fill_gap(1:2, -1, 2), regexp = "gap_begin")


# Find the gaps (NAs) in a vector and return a data frame listing their
# start and end positions
find_gaps <- function(x) {
    rle_x <- rle(is.na(x))
    # Compute endpoints of run
    end <- cumsum(rle_x$lengths)
    start <- c(1, head(end, -1) + 1)
    data.frame(start, end)[rle_x$values,] # return only TRUE value gaps
}

# Test code
expect_equivalent(find_gaps(c(1, NA, 3)), data.frame(start = 2, end = 2))
expect_equivalent(find_gaps(c(1, NA, NA, 3)), data.frame(start = 2, end = 3))
expect_equivalent(find_gaps(c(1, NA)), data.frame(start = 2, end = 2))
expect_equivalent(find_gaps(c(1, NA, 3, NA, 5)),
                  data.frame(start = c(2, 4), end = c(2, 4)))
expect_equivalent(find_gaps(NA), data.frame(start = 1, end = 1))

expect_identical(nrow(find_gaps(c(1, 2))), 0L) # 0-row d.f. if no gaps
expect_identical(nrow(find_gaps(c(1))), 0L) # 0-row d.f. if no gaps


# Fill all the gaps in a vector based on the mean annual cycle
fill_all_gaps <- function(x, mac) {
    stopifnot(length(x) == length(mac))

    gaps <- find_gaps(x)
    gapfilled <- rep(NA_real_, length(x))

    for(i in seq_len(nrow(gaps))) {
        thisgap <- gaps$start[i]:gaps$end[i]
        gapfilled[thisgap] <- fill_gap(x, gaps$start[i], gaps$end[i], mac)
        # Just to make the graphs look good, return the boundary points too
        # This code (these two ifs) should not be in production code
        if(gaps$start[i] > 1)
            gapfilled[gaps$start[i] - 1] <- x[gaps$start[i] - 1]
        if(gaps$end[i] < length(x))
            gapfilled[gaps$end[i] + 1] <- x[gaps$end[i] + 1]
    }
    return(gapfilled)
}
