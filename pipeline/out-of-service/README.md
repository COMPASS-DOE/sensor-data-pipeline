# out-of-service

This folder holds out-of-service (OOS) tables that are
read by `L1_normalize.qmd` and used to add out-of-service flags to data.

Note that **the CSV filenames are important.** They are used as a 
pattern that's checked against the data 'Table' during L1_normalize
processing.

OOS tables are CSV files and _must_ have at least three columns:
* `Site` - Site name for which these OOS entries apply
* `oos_begin` - Timestamp (YYYY-MM-DD HH:MM:SS) of OOS start
* `oos_end` - Timestamp (YYYY-MM-DD HH:MM:SS) of OOS end

They may however have additional columns. During processing, any data
that match these conditions of any row in any OOS file are marked with
`F_OOS` (flag out of service) of 1.
