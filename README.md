# College-Access-Desert-Database
This repository contains the data for "Shifting Sands: An Analysis of College Access Deserts from 2001 to 2019", presented at the Association for Education Finance & Policy's annual conference March 23-25, 2023.

## Funding Disclosure
This research was supported by a grant from the American Educational Research Association which receives funds for its "AERA-NSF Grants Program" from the National Science Foundation under NSF award NSF-DRL #1749275. Opinions reflect those of the author and do not necessarily reflect those AERA or NSF. 

## Database
File name: [CAD Classification_Output_20230105.csv](https://github.com/jakedwinfield/college-access-desert-database/blob/main/CAD%20Classification_Output_20230105.csv)

This file contains a database of college access deserts from 2001 to 2019, as constructed in the pre-print paper on the Open Science Framework.

### Variable Definitions
`FIPS` The county FIPS code

`ERS00` Commuting Zone designations constructed United States Department of Agriculture’s Economic Research Service, accessed from [Fowler's *Labor-sheds for Regional Analysis* webpage](https://sites.psu.edu/psucz/)

`OUT10` Commuting Zone designations constructed by [Fowler et al. (2016)](Https://doi.org/10.1007/s11113-016-9386-0), accessed from [Fowler's *Labor-sheds for Regional Analysis* webpage](https://sites.psu.edu/psucz/)

`AccessDesert.Sector.20XX` This family of variables identifies if a county, and corresponding commuting zone, was a college access desert that year. Two- and four- year colleges are identified using the IPEDS sector designations from 2001 to 2019. Fall enrollment data was used to calculate admissions rates (Fall 2001 data to calculate admissions rates for the AccessDesert.Sector.2001 variable).

`AccessDesert.Carnegie.20XX` This family of variables identifies if a county, and corresponding commuting zone, was a college access desert that year. Two- and four- year colleges are identified using the Carnegie Classifications from 2001 to 2019. Fall enrollment data was used to calculate admissions rates (Fall 2001 data to calculate admissions rates for the AccessDesert.Sector.2001 variable).

AccessDesert variables with a date range and the suffex `.All` (e.g., `AccessDesert.Carnegie.2000to2009.All`) indicate that a commuting zone was a college access desert for all of the years in the range - inclusive. The suffex `.Majority` indicates a commuting zone was a colege access desert for the majority of the years in the range of dates, while `.Any` indicates a commuting zone was a college access desert for any of the years in the provided range, inclusive.

## Code
File name: [IdentifyingCollegeAccessDeserts_20230312.R](https://github.com/jakedwinfield/college-access-desert-database/blob/main/IdentifyingCollegeAccessDeserts_20230312.R)

This .R script was used to create the above database file. 
