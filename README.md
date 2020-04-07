
<!-- README.md is generated from README.Rmd. Please edit that file -->

# singstatR

The package contains my final project for GR 5072 - Modern Data
Structures.

The [Singstat Table
Builder](https://www.tablebuilder.singstat.gov.sg/publicfacing/mainMenu.action)
is a service provided by the Singapore Government’s Department of
Statistics to allow public access to government data. In their words,
“The SingStat Table Builder contains statistical data series from 60
public sector agencies providing a comprehensive statistical view of
Singapore’s economic and socio-demographic characteristics. Users may
build customised data tables and export these in different file
formats.”

singstatR is a R wrapper for the SingStat Table Builder Developer API.
It aims to help users access data from the SingStat developer API more
easily using R. All functions in singstatR start with `singstat_`.

The package has 3 basic functions that wrap around the developer API
endpoints. These are:

  - `singstat_resource` - A function to access the resourceId endpoint.
    This is a search, returning table IDs and names of SingStat tables
    matching the search query.
  - `singstat_metadata` - A function to access the metadata endpoint.
    This returns a summary of metadata on a table based on its
    resourceId.
  - `singstat_tabledata` - A function to access the tabledata endpoint.
    This returns a table from the table builder based on the resourceId
    and additional queries specified.

Read the vignette - singstatR.Rmd for an introduction to the API,
package and these functions.

Additional functions build on these to provide more powerful features or
objects better suited for data analysis:

  - `singstat_tidytable`
  - `singstat_search` (in progress)
  - `singstat_craft_table` (in progress)

Read the vignette - More\_SingstatR.Rmd for an exploration of these
functions.

**Progress:**

Completed function, vignette, tests for:

  - singstat\_endpoint
  - singstat\_resource
  - singstat\_metadata
  - singstat\_tabledata
  - singstat\_tidytable

To complete:

  - add singstat\_search
  - add singstat\_craft\_table
  - add search-metadata chain function
