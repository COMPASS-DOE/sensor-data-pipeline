# out-of-service

This folder holds out-of-service (OOS) tables that are
read by `L1_normalize.qmd` and used to add out-of-service flags to data.

OOS tables are CSV files and _must_ have at least four columns:
* `Site` - Site name for which these OOS entries apply
* `Table` - Datalogger "Table" name for which these OOS entries apply
* `oos_begin` - Timestamp (YYYY-MM-DD HH:MM:SS) of OOS start
* `oos_end` - Timestamp (YYYY-MM-DD HH:MM:SS) of OOS end

During processing, any data that match these conditions of any row in any
OOS file are marked with `F_OOS` (flag out of service) of 1.
