# flag-database.R
#
# The synoptic data are subject to a variety of checks, both automated
# and human. These result in "flags" noting potential problems:
# measurements out of instrument range, algorithmic outliers,
# ones flagged by human QA/QC, etc.

library(DBI)

# Open the database, optionally initializing (erasing) it
fdb_open <- function(root, init = FALSE) {
    fn <- file.path(root, "flag-db.sqlite")
    if(init && file.exists(fn)) {
        file.remove(fn)
    }
    FDB <<- dbConnect(RSQLite::SQLite(), fn)
}

# Add flag data to the database
# Each site gets its own table, which is created if necessary
fdb_add_flags <- function(site, flag_data, fdb = FDB) {
    if(site %in% dbListTables(fdb)) {
        # We don't want to create identical rows
        # Given our small data volumes, seems simplest to use R's duplicated()
        old_data <- dbReadTable(fdb, site)
        new_data <- rbind(old_data, flag_data)
        new_data <- new_data[!duplicated(new_data),]
        dbWriteTable(fdb, site, new_data, overwrite = TRUE)
        rows <- nrow(new_data) - nrow(old_data)
    } else {
        message("\tCreating new site ", site)
        dbWriteTable(fdb, site, flag_data)
        rows <- nrow(flag_data)
    }
    message("\tAdded ", rows, " rows")
}

# Get flag data for a single site
# If there are no flags for the site, return NULL
fdb_get_flags_site <- function(site, fdb = FDB) {
    if(site %in% dbListTables(fdb)) {
        dbReadTable(fdb, site)
    } else {
        message("No flags for site ", site)
        NULL
    }
}

# Get flag data for one or more IDs in a site
# Supplying an invalid site is an error
fdb_get_flags_ids <- function(site, ids, fdb = FDB) {
    # Using paste() is theoretically dangerous in this context (see
    # https://rsqlite.r-dbi.org/articles/rsqlite#queries) but I couldn't
    # get the table name to work as a parameter
    dbGetQuery(fdb,
               paste('SELECT * FROM', site, 'WHERE ID IN (?)'),
               params = list(ids))
}

# Delete flag data for one or more IDs in a site
# Supplying an invalid site is an error
fdb_delete_ids <- function(site, ids, fdb = FDB) {
    # Using paste() is theoretically dangerous in this context (see
    # https://rsqlite.r-dbi.org/articles/rsqlite#queries) but I couldn't
    # get the table name to work as a parameter
    res <- dbSendStatement(fdb,
               paste('DELETE FROM', site, 'WHERE ID IN (?)'),
               params = list(ids))
    message("Deleted ", dbGetRowsAffected(res), " rows")
    dbClearResult(res)
}

# Close the connection
fdb_cleanup <- function(fdb = FDB) {
    dbDisconnect(fdb)
}


# ============= Test code =============

# Create test database
test_db <- dbConnect(RSQLite::SQLite(), ":memory:")
test1 <- data.frame(ID = 1:3, Flag_type = letters[1:3])
fdb_add_flags("site1", test1, fdb = test_db)

# Pull out data for an entire site (table)
x <- fdb_get_flags_site("site1", fdb = test_db)
stopifnot(identical(x, test1))

# Adding duplicate data shouldn't add new entries
fdb_add_flags("site1", test1, fdb = test_db)
y <- fdb_get_flags_site("site1", fdb = test_db)
stopifnot(identical(x, y))

# No flags for a site should return NULL
x <- fdb_get_flags_site("another_site", fdb = test_db)
stopifnot(is.null(x))

# Search by Site-ID
x <- fdb_get_flags_ids("site1", ids = 1:2, fdb = test_db)
stopifnot(identical(x, subset(test1, ID %in% 1:2)))

# Remove IDs
fdb_delete_ids("site1", ids = 1, fdb = test_db)
x <- fdb_get_flags_site("site1", fdb = test_db)
stopifnot(!1 %in% x$ID)

# Clean up
fdb_cleanup(fdb = test_db)

