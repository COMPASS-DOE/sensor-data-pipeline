COMPASS-FME Level 1 sensor data
Version: [VERSION]
Date: [DATESTAMP]
Observations: [OBSERVATIONS]
Git commit: [GIT_COMMIT]

DESCRIPTION
—----------------------------------
This is the Level 1 (L1) data release [VERSION] for COMPASS-FME
environmental sensors located at field sites in the Lake Erie and
Chesapeake Bay regions. L1 data are close to raw, but are
units-transformed and have out-of-instrument-bounds and out-of-service
flags added. Duplicates and missing data are removed but otherwise these
data are not filtered, and have not been subject to any additional
algorithmic or human QA/QC. Any scientific analyses of L1 data should be
performed with care.

CONTACT
—----------------------------------
Project: https://compass.pnnl.gov
Data lead: Stephanie Pennington, stephanie.pennington@pnnl.gov

HOW TO CITE THESE DATA
—----------------------------------
Pennington, Bittencourt Peixoto, Bond-Lamberty, Cheng, LaGorga,
Machado-Silva, Peresta, Phillips, Regier, Rich, Sandoval, Stearns, Ward,
Wilson, Weintraub, Megonigal, and Bailey (2024). COMPASS-FME Level 1
Sensor Data (version [VERSION] released [DATESTAMP]), downloaded
YYYY-MM-DD, https://compass.pnnl.gov.

DATA STRUCTURE
—----------------------------------
Data are organized into {SITE_YEAR} folders, with up to 12 monthly
comma-separated value (CSV) files in each folder for each plot at that
site. Sites include CRC (Crane Creek), GCW (GCReW), GWI (Goodwin
Island), MSM (Moneystump Marsh), OWC (Old Woman Creek), PTR (Portage
River), SWH (Sweet Hall Marsh), and TMP (TEMPEST experiment). See
site-specific metadata files in each folder.

Data are normally logged at a 15 minute interval, but this is **not** 
guaranteed. In particular, there may be:
* Missing data points (due to offline sensors, for example);
* Multiple, numerically different observations for a given timestamp (rare);
* Less-than-15-minute time intervals, in particular during TEMPEST flood events.

DATA VERSIONS
—----------------------------------
COMPASS-FME L1 data releases use semantic versioning (https://semver.org). 
This means that given a version number MAJOR-MINOR-PATCH, we increment the: 
* MAJOR number when we make incompatible changes to the data structure; 
* MINOR version when we add data in a backwards-compatible manner; and 
* PATCH version when we fix documentation and the like.

Importantly, “backward compatible” does NOT mean that the data don’t
change, only that your scripts using L1 data will probably still work. 

CHANGELOG
—----------------------------------
Version 1-1 released 2024-08-05
* Covers late 2019 through July 2024 for TEMPEST and all synoptic sites
* TEMPEST redox data now available starting April 2024
* Now includes high-frequency (1 and 5 min interval) data from TEMPEST floods

Version 1-0 released 2024-05-29
* Covers late 2019 through April 2024 for TEMPEST and all synoptic sites
* Restructured for ease of use, with metadata (location, sensor ID, etc) in separate columns
* SWH plot naming reworked for new upland plot
* Mirroring TMP C to GCW UP for synoptic site consistency
* GCReW weather station variables are now included in GCW-W 
* Many fixes to variable units and bounds
* Out-of-service is valid for AquaTROLL and EXO

Version 0-9 released 2024-01-22
* Preliminary release covering all synoptic site and TEMPEST data collected to date
* Units and bounds (and thus OOB flags) are missing for some ClimaVue, AquaTROLL, and Sonde variables
* Some research_name assignments for may incorrect for ClimaVue
* Out-of-service is working only for AquaTROLL
* No TEMPEST 1- or 5-minute data included
