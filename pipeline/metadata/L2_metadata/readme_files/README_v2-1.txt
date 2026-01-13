COMPASS-FME Level 2 sensor data
Version: [VERSION] (BETA)
Date: [DATESTAMP]
Observations: [OBSERVATIONS]
Git commit: [GIT_COMMIT]

DESCRIPTION
—----------------------------------
Level 2 (L2) data consist of sensor observations from the COMPASS-FME
synoptic sites, TEMPEST, and DELUGE. Compared to the L1 data, these are
more consistent (always 15-minute timestamps for the entire year);
better QA/QC’d (out of bounds, out of service, and extreme outlier
values are removed); and more complete, with a gap-filled time series
available alongside the main observations, and additional derived
(calculated) variables. L2 data are intended to be rapidly and easily
usable in analyses and simulations. However, algorithmic outlier
identification always carries the risk of removing valid data, and Level
1 data may be more suitable for analyses that focus on variability or
extreme events.

CONTACT
—----------------------------------
Project: https://compass.pnnl.gov
Data lead: Stephanie Pennington, stephanie.pennington@pnnl.gov

HOW TO CITE THESE DATA
—----------------------------------
Pennington, Bittencourt Peixoto, Bond-Lamberty, Cheng, LaGorga,
Machado-Silva, Peresta, Phillips, Regier, Rich, Sandoval, Stearns, Ward,
Wilson, Weintraub, Megonigal, and Bailey (2024). COMPASS-FME Level 2
Sensor Data (version [VERSION] released [DATESTAMP]), downloaded
YYYY-MM-DD, https://compass.pnnl.gov.

DATA STRUCTURE
—----------------------------------
Data are organized into {SITE}_{YEAR} folders, with Parquet (a high
performance, space efficient format; see https://parquet.apache.org)
files in each folder for each plot and output variable at that site.

The data file naming convention is
{SITE}_{PLOT}_{YEAR}_{OUTPUT VARIABLE}_L2_{VERSION}.parquet

Sites include CRC (Crane Creek), DLG (DELUGE experiment), GCW (GCReW),
GWI (Goodwin Island), MSM (Moneystump Marsh), OWC (Old Woman Creek), PTR
(Portage River), SWH (Sweet Hall Marsh), and TMP (TEMPEST experiment).
See site-specific metadata files in each folder.

Data timestamps are every 15 minutes, from January 1 00:00 to December
31 23:45. The `N_avg` column indicates how many Level 1 values were averaged
to produce the L2 value. Short (typically <= 1 hour, although this
varies by variable) data gaps are filled by linear interpolation. If you
want to exclude these interpolated values, only use data where the `N_avg`
column is >= 1.

DATA VERSIONS
—----------------------------------
COMPASS-FME L2 data releases use semantic versioning (https://semver.org).
This means that given a version number MAJOR-MINOR-PATCH, we increment the:
* MAJOR number when we make incompatible changes to the data structure;
* MINOR version when we add data in a backwards-compatible manner; and
* PATCH version when we fix documentation and the like.

Importantly, “backward compatible” does NOT mean that the data don’t
change, only that your scripts using L2 data will probably still work.

CHANGELOG
—----------------------------------
Version 2-1 release [DATESTAMP]
* The Level 2 data format has been tweaked; in particular see documentation about `Value_MAC`
* DELUGE (DLG) data are here! Well, the TEROS measurements anyway
* We now output times of various weather station minima and maxima; as a result, 'Value' may be non-numeric 
* TEMPEST AquaTROLL600 pressure is now corrected for atmospheric pressure; see site files 
* The `sonde-depth` variable has been removed, as it was unreliable and misleading
* New soil redox data streams at all CB sites
* Plot elevation information is now available is the site metadata files

Version 2-0 released 2025-07-07
* First release; covers late 2019 through June 2025 for TEMPEST and all synoptic sites
* This release has only a subset of the L1 output variables
