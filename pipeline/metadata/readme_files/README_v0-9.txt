COMPASS-FME Level 1 data
Version: [VERSION]
Date: [DATESTAMP]
Observations: [OBSERVATIONS]
Git commit: [GIT_COMMIT]

DESCRIPTION
—----------------------------------
Level 1 (L1) data are close to raw, but are units-transformed
and have out-of-instrument-bounds and out-of-service flags added.
Duplicates are removed but otherwise these data are not filtered,
and have not been subject to any additional algorithmic or human QA/QC.
Any use of L1 data for science analyses should be performed with care.

CONTACT
—----------------------------------
Project: https://compass.pnnl.gov
Data lead: Stephanie Pennington, stephanie.pennington@pnnl.gov

HOW TO CITE THESE DATA
—----------------------------------
Pennington, Rich, Cheng, and Bond-Lamberty (2024). COMPASS-FME Level 1 Sensor Data
(version [VERSION] released [DATESTAMP]), downloaded YYYY-MM-DD, https://compass.pnnl.gov.

CHANGELOG
—----------------------------------
Version 0-9 released 2023-01-22
* Preliminary release covering all synoptic site and TEMPEST data collected to date
* Units and bounds (and thus OOB flags) are missing for some ClimaVue, AquaTROLL, and Sonde variables
* Some research_name assignments for may incorrect for ClimaVue
* Out-of-service is working only for AquaTROLL
* No TEMPEST 1- or 5-minute data included
