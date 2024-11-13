# design_table_README.md

The design table, `design_table.csv`, is the central piece of metadata
in the entire COMPASS-FME data processing system, mapping datalogger data variables
to metadata columns that encode the physical location, measurement type, and sometimes
individual corresponding to that variable. It is read and used by `L1_normalize.qmd`.

Design table fields:

| Name               | Description                |
| ------------------ | -------------------------- |
| Site               | Site abbreviation          |
| Plot               | Plot abbreviation          |
| Logger             | Datalogger name            |
| Table              | Datalogger table name      |
| loggernet_variable | Datalogger variable name   |
| instrument         | Name of instrument group   |
| which              | Which instrument group     |
| individual         | Experimental individual    |
| valid_through      | Expiry date of design link |
| research_name      | Type of measurement        |
| Note               | Note                       |

The table can have empty lines; this can be visually useful, for example to 
separate information about different tables.

The `instrument` column is the grouping name, typically the name of the measurement
instrument (e.g. "TEROS12" or "Sapflow"); the `which` column designates a 
specific instrument within a plot, if there's more than one.

The `individual` column can designate a specific, named sensor (e.g., the TEROS
probes in the TEMPEST plots) or an experimental individual (the sapflow trees).

The `valid_through` column is used when a sensor is reassigned, for example if a tree
dies and we reassign its sapflux sensor to a new tree, and encodes the last valid
date for a given design link. In this case the loggernet variable has _two_ entries
(rows): the original assignment, with a YYYY-MM-DD `valid_through` entry, and the new assignment,
with a blank `valid_through` entry.

The TEMPEST sapflow assignments are based on:
COMPASS FME Task 2 -> TEMPEST -> Sensor Networks & Infrastructure -> Monitoring Documents -> TEMPEST Sap Flow Monitoring

The Synoptic Sap Flow Monitoring, TEROS Monitoring, and Aquatroll Monitoring documents live here:
COMPASS FME Task 1 -> 1.2 Synoptic -> Sensor Maintenance
