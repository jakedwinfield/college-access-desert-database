library(educationdata)
library(tidyverse)


# Pull data from Education Data Portal ------------------------------------

# Applicants, number admitted, year, county, public, two or four year
#Define two year based on Institutional category: 
#degree-granting associates and certificates and not primarily BA or above (3, 4)
#Four-year criteria: primarily BA or above (2)

years.list = c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
               2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)

directory.full <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = years.list, inst_control = 1)
)

admissions.full <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'admissions-enrollment',
  filter = list(year = years.list, sex = 99)
)


data.full <- dplyr::left_join(directory.full, admissions.full)

View(data.full)
VariableList = c( 'year', 'unitid', 'inst_name', 'state_abbr', 'county_name', 
                  'inst_control', 'institution_level', 'sector',
                  'county_fips', 'number_applied', 'number_admitted', 'offering_undergrad', 'degree_granting')

#Filter to variables needed to created College Access Deserts 
data.full <- data.full %>%
  select(VariableList)

#Notes on other geographies that may be useful
#CBSAs are not contiguous. In addition, BEA Economic areas are not consistently updated - haven't been since 04. 
#Also, important to note that CZs can span state boundaries which does have college access implications


# FIPS 2001-09 Merging ----------------------------------------------------

#IPEDS didn't collect county information/FIPS codes until 2009.
#Here I copy the dataset and merge for missing columns 
#This choice assumes that institutions are placebound

directory.2009 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2009, inst_control = 1)
)

directory.2009.fips <- directory.2009 %>%
  select('unitid', 'county_fips')

directory.2009.fips <- rename(directory.2009.fips, county_fips_2009 = county_fips)

data.full.fips <- left_join(data.full, directory.2009.fips, by = 'unitid')
data.full.fips <- rename(data.full.fips, county_fips_reported = county_fips)

sum(is.na(data.full.fips$county_fips_reported)) #17843 missing FIPS from original dataset

data.full.fips$county_fips_merge <- data.full.fips$county_fips_reported

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2009, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #978 missing FIPS after merging in 2009 data

#Repeating for 2010
directory.2010 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2010, inst_control = 1)
)

directory.2010.fips <- directory.2010 %>%
  select('unitid', 'county_fips')

directory.2010.fips <- rename(directory.2010.fips, county_fips_2010 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2010.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2010, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #924 missing FIPS after merging in 2010 data - 58 added

#Repeating for 2011
directory.2011 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2011, inst_control = 1)
)

directory.2011.fips <- directory.2011 %>%
  select('unitid', 'county_fips')

directory.2011.fips <- rename(directory.2011.fips, county_fips_2011 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2011.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2011, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #903 missing FIPS after merging in 2011 data - 21 added

#Repeating for 2012
directory.2012 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2012, inst_control = 1)
)

directory.2012.fips <- directory.2012 %>%
  select('unitid', 'county_fips')

directory.2012.fips <- rename(directory.2012.fips, county_fips_2012 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2012.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2012, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #903 missing FIPS after merging in 2012 data - 0 added


#Repeating for 2013
directory.2013 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2013, inst_control = 1)
)

directory.2013.fips <- directory.2013 %>%
  select('unitid', 'county_fips')

directory.2013.fips <- rename(directory.2013.fips, county_fips_2013 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2013.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2013, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #884 missing FIPS after merging in 2013 data - 19 added

#Repeating for 2014
directory.2014 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2014, inst_control = 1)
)

directory.2014.fips <- directory.2014 %>%
  select('unitid', 'county_fips')

directory.2014.fips <- rename(directory.2014.fips, county_fips_2014 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2014.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2014, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #884 missing FIPS after merging in 2014 data - 0 added

#Repeating for 2015
directory.2015 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2015, inst_control = 1)
)

directory.2015.fips <- directory.2015 %>%
  select('unitid', 'county_fips')

directory.2015.fips <- rename(directory.2015.fips, county_fips_2015 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2015.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2015, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #884 missing FIPS after merging in 2015 data - 0 added

#Repeating for 2016
directory.2016 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2016, inst_control = 1)
)

directory.2016.fips <- directory.2016 %>%
  select('unitid', 'county_fips')

directory.2016.fips <- rename(directory.2016.fips, county_fips_2016 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2016.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2016, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #877 missing FIPS after merging in 2016 data - 7 added

#Repeating for 2017
directory.2017 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2017, inst_control = 1)
)

directory.2017.fips <- directory.2017 %>%
  select('unitid', 'county_fips')

directory.2017.fips <- rename(directory.2017.fips, county_fips_2017 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2017.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2017, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #877 missing FIPS after merging in 2017 data - 0 added

#Repeating for 2018
directory.2018 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2018, inst_control = 1)
)

directory.2018.fips <- directory.2018 %>%
  select('unitid', 'county_fips')

directory.2018.fips <- rename(directory.2018.fips, county_fips_2018 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2018.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2018, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #877 missing FIPS after merging in 2018 data - 0 added

#Repeating for 2019
directory.2019 <- educationdata::get_education_data(
  level = 'college-university',
  source= 'ipeds',
  topic = 'directory',
  filter = list(year = 2019, inst_control = 1)
)

directory.2019.fips <- directory.2019 %>%
  select('unitid', 'county_fips')

directory.2019.fips <- rename(directory.2019.fips, county_fips_2019 = county_fips)

data.full.fips <- left_join(data.full.fips, directory.2019.fips, by = 'unitid')

data.full.fips <- data.full.fips %>%
  mutate(county_fips_merge = ifelse(is.na(county_fips_merge), county_fips_2019, county_fips_merge))
sum(is.na(data.full.fips$county_fips_merge)) #875 missing FIPS after merging in 2019 data - 2 added

write.csv(data.full.fips, file = 'College Access Desert Analysis/Data Full_WithFIPS_20220920.csv')



# Sector - Four-year ------------------------------------------------------
Institutional.Characteristics <- read_csv("Characteristics_20220920.csv")
data.full.fips <- read_csv('Data Full_WithFIPS_20220920.csv')

data.full.chars <- left_join(data.full.fips, Institutional.Characteristics, by = c('year', 'unitid'))

View(data.full.chars)

table(data.full.chars$institution_level, useNA = 'always') #0 missing cases, 1 value of missing/not reported
#1 = less than 2 years, 2 = 2 but less than 4, 4, four or more years

table(data.full.chars$sector, useNA = 'always') #no missing cases, no values of missing/not reported 
#In sector value of 7 would be excluded (public, less htan 2 year)

sjPlot::tab_xtab(var.row = data.full.chars$sector, var.col = data.full.chars$institution_level)
#Sector and institution level are near perfect aligned - I'll use SECTOR instead of I Level because it describes admin units

#Select 4 year colleges -  SECTOR =1, degree_granting == 1,  offering_undergrad == 1
CAD.Sector.FourYear <- data.full.chars %>%
  filter(sector == 1, degree_granting == 1,  offering_undergrad == 1)

View(CAD.Sector.FourYear)

#13048 institutions across years
sum(is.na(CAD.Sector.FourYear$county_fips_merge)) #67 missing FIPS after eliminating other sectors
sum(is.na(CAD.Sector.FourYear$number_applied)) #2735 missing admit number after eliminating other sectors


CAD.Sector.FourYear$admission.rate <- (CAD.Sector.FourYear$number_admitted / CAD.Sector.FourYear$number_applied)

CAD.Sector.FourYear <- CAD.Sector.FourYear %>%
  filter(admission.rate >= .75)
#4282 four year colleges that meet CAD criteria 

CAD.Sector.FourYear.CountyCount <- CAD.Sector.FourYear %>% 
  group_by(year) %>%
  count(county_fips_merge, name = 'CountByCounty.Sector.FourYear') 

CAD.Sector.FourYear.CountyCount$FIPS <- CAD.Sector.FourYear.CountyCount$county_fips_merge

View(CAD.Sector.FourYear.CountyCount)


# Sector - Two Year -------------------------------------------------------

#Select 2 year colleges
CAD.Sector.TwoYear <- data.full.fips %>%
  filter(sector == 4, degree_granting == 1,  offering_undergrad == 1)

CAD.Sector.TwoYear$admission.rate <- NA
CAD.Sector.TwoYear$admission.rate <- (CAD.Sector.TwoYear$number_admitted / CAD.Sector.TwoYear$number_applied)

CAD.Sector.TwoYear.CountyCount <- CAD.Sector.TwoYear %>% 
  filter(county_fips_merge > 0) %>%
  group_by(year) %>%
  count(county_fips_merge, name = 'CountByCounty.Sector.TwoYear')

CAD.Sector.TwoYear.CountyCount$FIPS <- CAD.Sector.TwoYear.CountyCount$county_fips_merge

View(CAD.Sector.TwoYear.CountyCount)


# Create Sector CAD - Four Year -------------------------------------------

#download files from https://sites.psu.edu/psucz/data/
County.Classification.2000 <- read_csv('https://sites.psu.edu/psucz/files/2018/09/counties00-2jljyw7.csv')
County.Classification.2010 <- read_csv('https://sites.psu.edu/psucz/files/2018/09/counties10-zqvz0r.csv')
View(County.Classification.2010)

#In classifications two fips codes don't match other public data. I verified the FIPS code by comparing 
# location and county name in the shapefiles (linked here https://sites.psu.edu/psucz/data/)
#to Wikipedia's list of County FIPS codes (https://en.wikipedia.org/wiki/List_of_United_States_FIPS_codes_by_county)

#PSU FIPS 46113 SHOULD BE 46102 (Ogala Lakota County, South Dakota)
#PSU FIPS 2270 SHOULD BE 2158 (Kusilvak Census Area, Alaska)
#This is because they changed names in 2015 (SD)
#I'll add a second row for these counties with the updated FIPS for analyses with these FIPS codes

County.Classification.a <- left_join(County.Classification.2010, County.Classification.2000, all.x = T, by = 'FIPS')
View(County.Classification.a)

County.Classification.ERS <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')
View(County.Classification.ERS)

#Merge college count by county
County.Classification.ERS <- left_join(County.Classification.ERS, CAD.Sector.FourYear.CountyCount, all.x = T, by = 'FIPS')


#Replaces NA values with 0 for counties without colleges
County.Classification.ERS$CountByCounty.Sector.FourYear <- replace(County.Classification.ERS$CountByCounty.Sector.FourYear,
                                                          is.na(County.Classification.ERS$CountByCounty.Sector.FourYear),
                                                          0)
table(County.Classification.ERS$CountByCounty.Sector.FourYear, useNA= 'always')

#Count four-year college by CZ

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2001) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2001 = sum(CountByCounty.Sector.FourYear))

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2002) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2002 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2003) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2003 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2004) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2004 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2005) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2005 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2006) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2006 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2007) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2007 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2008) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2008 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

CountByCZ.Sector.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2009) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.FourYear.2009 = sum(CountByCounty.Sector.FourYear)) %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

View(CountByCZ.Sector.FourYear)

#Now repeating for the 2010 grouping
CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2010) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2010 = sum(CountByCounty.Sector.FourYear)) 

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2011) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2011 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2012) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2012 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2013) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2013 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2014) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2014 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2015) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2015 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2016) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2016 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2017) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2017 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2018) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2018 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')

CountByCZ.Sector.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2019) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.FourYear.2019 = sum(CountByCounty.Sector.FourYear))  %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10')


View(CountByCZ.Sector.FourYear.OUT10)


#Merge count by CZ and CountyClassication.ERS

County.Classification.Merge <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Sector.FourYear.ERS00, by = 'ERS00')

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Sector.FourYear.OUT10, by = 'OUT10') 
  
# Create Sector CAD - Two Year --------------------------------------------
County.Classification.ERS <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')
View(County.Classification.ERS)

#Merge college count by county
County.Classification.ERS <- left_join(County.Classification.ERS, CAD.Sector.TwoYear.CountyCount, all.x = T, by = 'FIPS')

#Replaces NA values with 0 for counties without colleges
County.Classification.ERS$CountByCounty.Sector.TwoYear <- replace(County.Classification.ERS$CountByCounty.Sector.TwoYear,
                                                         is.na(County.Classification.ERS$CountByCounty.Sector.TwoYear),
                                                         0)
table(County.Classification.ERS$CountByCounty.Sector.TwoYear, useNA= 'always')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2001) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2001 = sum(CountByCounty.Sector.TwoYear))

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2002) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2002 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2003) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2003 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2004) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2004 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2005) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2005 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2006) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2006 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2007) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2007 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2008) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2008 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Sector.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2009) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Sector.TwoYear.2009 = sum(CountByCounty.Sector.TwoYear)) %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')

View(CountByCZ.Sector.TwoYear)

#Now repeating for the 2010 grouping
CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2010) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2010 = sum(CountByCounty.Sector.TwoYear)) 

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2011) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2011 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2012) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2012 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2013) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2013 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2014) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2014 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2015) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2015 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2016) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2016 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2017) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2017 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2018) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2018 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Sector.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2019) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Sector.TwoYear.2019 = sum(CountByCounty.Sector.TwoYear))  %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10')

#Merge count by CZ and CountyClassication.b

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Sector.TwoYear.ERS00, by = 'ERS00')


County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Sector.TwoYear.OUT10, by = 'OUT10') 

View(County.Classification.Merge)

# CAD Determination - Sector ----------------------------------------------
CAD.Classification <- County.Classification.Merge

#CAD value of 1 means it IS a college access dessert - values of 0 represent not a CAD

CAD.Classification <- CAD.Classification %>%
 replace(is.na(.), 0)

CAD.Classification$ERS00 <- na_if(CAD.Classification$ERS00, 0)
  
CAD.Classification <- CAD.Classification %>%
  mutate(AccessDesert.Sector.2001 = ifelse(Count.CZ.Sector.FourYear.2001 > 0 | Count.CZ.Sector.TwoYear.2001 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2002 = ifelse(Count.CZ.Sector.FourYear.2002 > 0 | Count.CZ.Sector.TwoYear.2002 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2003 = ifelse(Count.CZ.Sector.FourYear.2003 > 0 | Count.CZ.Sector.TwoYear.2003 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2004 = ifelse(Count.CZ.Sector.FourYear.2004 > 0 | Count.CZ.Sector.TwoYear.2004 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2005 = ifelse(Count.CZ.Sector.FourYear.2005 > 0 | Count.CZ.Sector.TwoYear.2005 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2006 = ifelse(Count.CZ.Sector.FourYear.2006 > 0 | Count.CZ.Sector.TwoYear.2006 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2007 = ifelse(Count.CZ.Sector.FourYear.2007 > 0 | Count.CZ.Sector.TwoYear.2007 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2008 = ifelse(Count.CZ.Sector.FourYear.2008 > 0 | Count.CZ.Sector.TwoYear.2008 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2009 = ifelse(Count.CZ.Sector.FourYear.2009 > 0 | Count.CZ.Sector.TwoYear.2009 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2010 = ifelse(Count.CZ.Sector.FourYear.2010 > 0 | Count.CZ.Sector.TwoYear.2010 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2011 = ifelse(Count.CZ.Sector.FourYear.2011 > 0 | Count.CZ.Sector.TwoYear.2011 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2012 = ifelse(Count.CZ.Sector.FourYear.2012 > 0 | Count.CZ.Sector.TwoYear.2012 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2013 = ifelse(Count.CZ.Sector.FourYear.2013 > 0 | Count.CZ.Sector.TwoYear.2013 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2014 = ifelse(Count.CZ.Sector.FourYear.2014 > 0 | Count.CZ.Sector.TwoYear.2014 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2015 = ifelse(Count.CZ.Sector.FourYear.2015 > 0 | Count.CZ.Sector.TwoYear.2015 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2016 = ifelse(Count.CZ.Sector.FourYear.2016 > 0 | Count.CZ.Sector.TwoYear.2016 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2017 = ifelse(Count.CZ.Sector.FourYear.2017 > 0 | Count.CZ.Sector.TwoYear.2017 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2018 = ifelse(Count.CZ.Sector.FourYear.2018 > 0 | Count.CZ.Sector.TwoYear.2018 >=2, 0, 1)) %>%
  mutate(AccessDesert.Sector.2019 = ifelse(Count.CZ.Sector.FourYear.2019 > 0 | Count.CZ.Sector.TwoYear.2019 >=2, 0, 1))
  
first.cases <- CAD.Classification[!duplicated(CAD.Classification$OUT10),]

sjPlot::tab_xtab(var.row = first.cases$AccessDesert.Sector.2013, var.col = first.cases$OUT10)
#There are 352 CZ that are CADs in 2013 

CAD.Classification$AccessDesert.Sector.2000to2009 <- 
  (CAD.Classification$AccessDesert.Sector.2001 + CAD.Classification$AccessDesert.Sector.2002 + 
     CAD.Classification$AccessDesert.Sector.2003 + CAD.Classification$AccessDesert.Sector.2004 +
     CAD.Classification$AccessDesert.Sector.2005 + CAD.Classification$AccessDesert.Sector.2006 + 
     CAD.Classification$AccessDesert.Sector.2007 + CAD.Classification$AccessDesert.Sector.2008 +
     CAD.Classification$AccessDesert.Sector.2009)

CAD.Classification$AccessDesert.Sector.2000to2009.Any <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2009 >= 1, 1, 0)

CAD.Classification$AccessDesert.Sector.2000to2009.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2009 >= 5, 1, 0)

CAD.Classification$AccessDesert.Sector.2000to2009.All <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2009 == 9, 1, 0)

CAD.Classification$AccessDesert.Sector.2010to2019 <- 
  (CAD.Classification$AccessDesert.Sector.2010 + CAD.Classification$AccessDesert.Sector.2011 + 
     CAD.Classification$AccessDesert.Sector.2012 + CAD.Classification$AccessDesert.Sector.2013 +
     CAD.Classification$AccessDesert.Sector.2014 + CAD.Classification$AccessDesert.Sector.2015 + 
     CAD.Classification$AccessDesert.Sector.2016 + CAD.Classification$AccessDesert.Sector.2017 +
     CAD.Classification$AccessDesert.Sector.2018 + CAD.Classification$AccessDesert.Sector.2019)

CAD.Classification$AccessDesert.Sector.2010to2019.Any <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2010to2019 >= 1, 1, 0)

CAD.Classification$AccessDesert.Sector.2010to2019.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2010to2019 >= 5, 1, 0)

CAD.Classification$AccessDesert.Sector.2010to2019.All <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2010to2019 == 10, 1, 0)

table(CAD.Classification$AccessDesert.Sector.2010to2019.All, useNA = 'always')

CAD.Classification$AccessDesert.Sector.2000to2019 <- CAD.Classification$AccessDesert.Sector.2000to2009 +
  CAD.Classification$AccessDesert.Sector.2010to2019

CAD.Classification$AccessDesert.Sector.2000to2019.Any <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2019 >= 1, 1, 0)

CAD.Classification$AccessDesert.Sector.2000to2019.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2019 >= 10, 1, 0)

CAD.Classification$AccessDesert.Sector.2000to2019.All <- 
  ifelse(CAD.Classification$AccessDesert.Sector.2000to2019 == 19, 1, 0)


# Carnegie - Four Year Colleges --------------------------------

#Read in CC classifications for 2000, 2005, 2010, 2015, and 2018
#from https://carnegieclassifications.acenet.edu/downloads.php 
#Chaange file locations to run code

Carnegie2000_PublicDataFile <- readxl::read_excel("College Access Desert Analysis/Carnegie Classification Data/2000_edition_data.xlsx", 
                                            sheet = "Data")

#UNITID for 2005 file are being read in as dates from XLS file, so I am cohercing R to read it as numeric
Carnegie2005_PublicDataFile <- readxl::read_excel("College Access Desert Analysis/Carnegie Classification Data/cc2005_public_file_021110.xls", 
                                          sheet = "Data", col_types = c("numeric", 
                                                                        "text", "text", "text", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric"))

Carnegie2010_PublicDataFile <- readxl::read_excel("College Access Desert Analysis/Carnegie Classification Data/cc2010_classification_data_file.xls", 
                                         sheet = "Data")

Carnegie2015_PublicDataFile <- readxl::read_excel("College Access Desert Analysis/Carnegie Classification Data/CCIHE2015-PublicDataFile.xlsx", 
                                         sheet = "Data")

Carnegie2018_PublicDataFile <- readxl::read_excel("College Access Desert Analysis/Carnegie Classification Data/CCIHE2018-PublicDataFile.xlsx", 
                                       sheet = "Data")

Carnegie_2000 <- Carnegie2000_PublicDataFile %>%
  select(unitid = UNITID, CC2000)
Carnegie_2005 <- Carnegie2005_PublicDataFile %>%
  select(unitid = UNITID, BASIC2005)
Carnegie_2010 <- Carnegie2010_PublicDataFile %>%
  select(unitid = UNITID, BASIC2010)
Carnegie_2015 <- Carnegie2015_PublicDataFile %>%
  select(unitid = UNITID, BASIC2015)
Carnegie_2018 <- Carnegie2018_PublicDataFile %>%
  select(unitid = UNITID, BASIC2018)


#4-year CC-basic classifications for analysis
#2001 to 2004: 15, 16, 21, 22, 31, 32, 33
#2005 to 2014: 15, 16, 17, 18, 19, 20, 21, 22, 23
#2015-2017: 15, 16, 17, 18, 19, 20, 21, 22, 23
#2018 to 2019: 15, 16, 17, 18, 19, 20, 21, 22, 23

Carnegie_Working <- data.full.fips

#Calculate admit rate

Carnegie_Working$admission.rate <- (Carnegie_Working$number_admitted / Carnegie_Working$number_applied)

#Select institutions with following CCBasic Classifications

CAD.Carnegie.FourYear <- Carnegie_Working %>%
  filter(year < 2005 & admission.rate >= 0.75) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2000, by = 'unitid') %>%
  filter(CC2000 %in% c(15, 16, 21, 22, 31, 32, 33))

CAD.Carnegie.FourYear <- Carnegie_Working %>%
  filter(year %in% c(2005, 2006, 2007, 2008, 2009) & admission.rate >= 0.75) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2005, by = 'unitid') %>%
  filter(BASIC2005 %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23)) %>%
  full_join(CAD.Carnegie.FourYear)

CAD.Carnegie.FourYear <- Carnegie_Working %>%
  filter(year %in% c(2010, 2011, 2012,2013, 2014) & admission.rate >= 0.75) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2010, by = 'unitid') %>%
  filter(BASIC2010 %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23))%>%
  full_join(CAD.Carnegie.FourYear)

CAD.Carnegie.FourYear <- Carnegie_Working %>%
  filter(year %in% c(2015, 2016, 2017) & admission.rate >= 0.75) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2015, by = 'unitid') %>%
  filter(BASIC2015 %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23))%>%
  full_join(CAD.Carnegie.FourYear)

CAD.Carnegie.FourYear <- Carnegie_Working %>%
  filter(year %in% c(2018, 2019) & admission.rate >= 0.75) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2018, by = 'unitid') %>%
  filter(BASIC2018 %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23))%>%
  full_join(CAD.Carnegie.FourYear)

#Count by county, grouped by year
CAD.Carnegie.FourYear.CountyCount <- CAD.Carnegie.FourYear %>% 
  group_by(year) %>%
  count(county_fips_merge, name = 'CountByCounty.Carnegie.FourYear') 

CAD.Carnegie.FourYear.CountyCount$FIPS <- CAD.Carnegie.FourYear.CountyCount$county_fips_merge

#View(CAD.Carnegie.FourYear.CountyCount)

# Carnegie - Two Year colleges --------------------------------------------
#Two year classifications:
#2001 to 2004: 40, 60
#2005 to 2014: 1, 2, 3, 4, 5, 6, 7, 11, 12, 33
#2015-2017: 1, 2, 3, 4, 5, 6, 7, 8 , 9, 14, 33
#2018 - 2019: 1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 33

CAD.Carnegie.TwoYear <- Carnegie_Working %>%
  filter(year < 2005) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2000, by = 'unitid') %>%
  filter(CC2000 %in% c(40, 60))

CAD.Carnegie.TwoYear <- Carnegie_Working %>%
  filter(year %in% c(2005, 2006, 2007, 2008, 2009)) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2005, by = 'unitid') %>%
  filter(BASIC2005 %in% c(1, 2, 3, 4, 5, 6, 7, 11, 12, 33)) %>%
  full_join(CAD.Carnegie.TwoYear)

CAD.Carnegie.TwoYear <- Carnegie_Working %>%
  filter(year %in% c(2010, 2011, 2012,2013, 2014)) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2010, by = 'unitid') %>%
  filter(BASIC2010 %in% c(1, 2, 3, 4, 5, 6, 7, 11, 12, 33))%>%
  full_join(CAD.Carnegie.TwoYear)

CAD.Carnegie.TwoYear <- Carnegie_Working %>%
  filter(year %in% c(2015, 2016, 2017)) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2015, by = 'unitid') %>%
  filter(BASIC2015 %in% c(1, 2, 3, 4, 5, 6, 7, 8 , 9, 14, 33))%>%
  full_join(CAD.Carnegie.TwoYear)

CAD.Carnegie.TwoYear <- Carnegie_Working %>%
  filter(year %in% c(2018, 2019)) %>%
  filter(offering_undergrad == 1, degree_granting ==1) %>%
  left_join(Carnegie_2018, by = 'unitid') %>%
  filter(BASIC2018 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 33))%>%
  full_join(CAD.Carnegie.TwoYear)

#Count by county, grouped by year
CAD.Carnegie.TwoYear.CountyCount <- CAD.Carnegie.TwoYear %>% 
  group_by(year) %>%
  count(county_fips_merge, name = 'CountByCounty.Carnegie.TwoYear') 

CAD.Carnegie.TwoYear.CountyCount$FIPS <- CAD.Carnegie.TwoYear.CountyCount$county_fips_merge

View(CAD.Carnegie.TwoYear.CountyCount)

# Create Carnegie CAD - Four Year -----------------------------------------

#download files from https://sites.psu.edu/psucz/data/
County.Classification.2000 <- read_csv('https://sites.psu.edu/psucz/files/2018/09/counties00-2jljyw7.csv')
County.Classification.2010 <- read_csv('https://sites.psu.edu/psucz/files/2018/09/counties10-zqvz0r.csv')
View(County.Classification.2000)

County.Classification.a <- left_join(County.Classification.2010, County.Classification.2000, all.x = T, by = 'FIPS')
View(County.Classification.a)

County.Classification.ERS <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')
View(County.Classification.ERS)

#Merge college count by county
County.Classification.ERS <- left_join(County.Classification.ERS, CAD.Carnegie.FourYear.CountyCount, all.x = T, by = 'FIPS')


#Replaces NA values with 0 for counties without colleges
County.Classification.ERS$CountByCounty.Carnegie.FourYear <- replace(County.Classification.ERS$CountByCounty.Carnegie.FourYear,
                                                                   is.na(County.Classification.ERS$CountByCounty.Carnegie.FourYear),
                                                                   0)
table(County.Classification.ERS$CountByCounty.Carnegie.FourYear, useNA= 'always')

#Count four-year college by CZ

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2001) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2001 = sum(CountByCounty.Carnegie.FourYear))

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2002) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2002 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2003) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2003 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2004) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2004 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2005) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2005 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2006) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2006 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2007) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2007 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2008) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2008 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.FourYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2009) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.FourYear.2009 = sum(CountByCounty.Carnegie.FourYear)) %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

View(CountByCZ.Carnegie.FourYear.ERS00)

#Now repeating for the 2010 grouping
CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2010) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2010 = sum(CountByCounty.Carnegie.FourYear)) 

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2011) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2011 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2012) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2012 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2013) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2013 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2014) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2014 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2015) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2015 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2016) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2016 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2017) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2017 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2018) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2018 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.FourYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2019) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.FourYear.2019 = sum(CountByCounty.Carnegie.FourYear))  %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10')


View(CountByCZ.Carnegie.FourYear.OUT10)


#Merge count by CZ and CountyClassication.ERS

County.Classification.Merge <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Carnegie.FourYear.ERS00, by = 'ERS00')

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Carnegie.FourYear.OUT10, by = 'OUT10') 


# Create Carnegie CAD - Two Year ------------------------------------------

County.Classification.ERS <- County.Classification.a %>%
  select('FIPS', 'ERS00', 'OUT10')
View(County.Classification.ERS)

#Merge college count by county
County.Classification.ERS <- left_join(County.Classification.ERS, CAD.Carnegie.TwoYear.CountyCount, all.x = T, by = 'FIPS')

#Replaces NA values with 0 for counties without colleges
County.Classification.ERS$CountByCounty.Carnegie.TwoYear <- replace(County.Classification.ERS$CountByCounty.Carnegie.TwoYear,
                                                                  is.na(County.Classification.ERS$CountByCounty.Carnegie.TwoYear),
                                                                  0)
table(County.Classification.ERS$CountByCounty.Carnegie.TwoYear, useNA= 'always')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2001) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2001 = sum(CountByCounty.Carnegie.TwoYear))

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2002) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2002 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2003) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2003 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2004) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2004 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2005) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2005 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2006) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2006 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2007) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2007 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2008) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2008 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

CountByCZ.Carnegie.TwoYear.ERS00 <- County.Classification.ERS %>%
  filter(year == 2009) %>%
  group_by(ERS00) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2009 = sum(CountByCounty.Carnegie.TwoYear)) %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')

View(CountByCZ.Carnegie.TwoYear.ERS00)

#Now repeating for the 2010 grouping
CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2010) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2010 = sum(CountByCounty.Carnegie.TwoYear)) 

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2011) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2011 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2012) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2012 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2013) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2013 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2014) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2014 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2015) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2015 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2016) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2016 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2017) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2017 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2018) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2018 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

CountByCZ.Carnegie.TwoYear.OUT10 <- County.Classification.ERS %>%
  filter(year == 2019) %>%
  group_by(OUT10) %>%
  summarise(Count.CZ.Carnegie.TwoYear.2019 = sum(CountByCounty.Carnegie.TwoYear))  %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10')

#Merge count by CZ and CountyClassication.b

County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Carnegie.TwoYear.ERS00, by = 'ERS00')


County.Classification.Merge <- County.Classification.Merge %>%
  full_join(CountByCZ.Carnegie.TwoYear.OUT10, by = 'OUT10') 

View(County.Classification.Merge)


# CAD Determination - Carnegie --------------------------------------------

#CAD value of 1 means it IS a college access dessert - values of 0 represent not a CAD

CAD.Classification <- CAD.Classification %>%
  left_join(County.Classification.Merge)

CAD.Classification <- CAD.Classification %>%
  replace(is.na(.), 0)

CAD.Classification$ERS00 <- na_if(CAD.Classification$ERS00, 0)

CAD.Classification <- CAD.Classification %>%
  mutate(AccessDesert.Carnegie.2001 = ifelse(Count.CZ.Carnegie.FourYear.2001 > 0 | Count.CZ.Carnegie.TwoYear.2001 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2002 = ifelse(Count.CZ.Carnegie.FourYear.2002 > 0 | Count.CZ.Carnegie.TwoYear.2002 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2003 = ifelse(Count.CZ.Carnegie.FourYear.2003 > 0 | Count.CZ.Carnegie.TwoYear.2003 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2004 = ifelse(Count.CZ.Carnegie.FourYear.2004 > 0 | Count.CZ.Carnegie.TwoYear.2004 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2005 = ifelse(Count.CZ.Carnegie.FourYear.2005 > 0 | Count.CZ.Carnegie.TwoYear.2005 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2006 = ifelse(Count.CZ.Carnegie.FourYear.2006 > 0 | Count.CZ.Carnegie.TwoYear.2006 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2007 = ifelse(Count.CZ.Carnegie.FourYear.2007 > 0 | Count.CZ.Carnegie.TwoYear.2007 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2008 = ifelse(Count.CZ.Carnegie.FourYear.2008 > 0 | Count.CZ.Carnegie.TwoYear.2008 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2009 = ifelse(Count.CZ.Carnegie.FourYear.2009 > 0 | Count.CZ.Carnegie.TwoYear.2009 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2010 = ifelse(Count.CZ.Carnegie.FourYear.2010 > 0 | Count.CZ.Carnegie.TwoYear.2010 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2011 = ifelse(Count.CZ.Carnegie.FourYear.2011 > 0 | Count.CZ.Carnegie.TwoYear.2011 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2012 = ifelse(Count.CZ.Carnegie.FourYear.2012 > 0 | Count.CZ.Carnegie.TwoYear.2012 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2013 = ifelse(Count.CZ.Carnegie.FourYear.2013 > 0 | Count.CZ.Carnegie.TwoYear.2013 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2014 = ifelse(Count.CZ.Carnegie.FourYear.2014 > 0 | Count.CZ.Carnegie.TwoYear.2014 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2015 = ifelse(Count.CZ.Carnegie.FourYear.2015 > 0 | Count.CZ.Carnegie.TwoYear.2015 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2016 = ifelse(Count.CZ.Carnegie.FourYear.2016 > 0 | Count.CZ.Carnegie.TwoYear.2016 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2017 = ifelse(Count.CZ.Carnegie.FourYear.2017 > 0 | Count.CZ.Carnegie.TwoYear.2017 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2018 = ifelse(Count.CZ.Carnegie.FourYear.2018 > 0 | Count.CZ.Carnegie.TwoYear.2018 >=2, 0, 1)) %>%
  mutate(AccessDesert.Carnegie.2019 = ifelse(Count.CZ.Carnegie.FourYear.2019 > 0 | Count.CZ.Carnegie.TwoYear.2019 >=2, 0, 1))

CAD.Classification$AccessDesert.Carnegie.2000to2009 <- 
  (CAD.Classification$AccessDesert.Carnegie.2001 + CAD.Classification$AccessDesert.Carnegie.2002 + 
     CAD.Classification$AccessDesert.Carnegie.2003 + CAD.Classification$AccessDesert.Carnegie.2004 +
     CAD.Classification$AccessDesert.Carnegie.2005 + CAD.Classification$AccessDesert.Carnegie.2006 + 
     CAD.Classification$AccessDesert.Carnegie.2007 + CAD.Classification$AccessDesert.Carnegie.2008 +
     CAD.Classification$AccessDesert.Carnegie.2009)

CAD.Classification$AccessDesert.Carnegie.2000to2009.Any <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2009 >= 1, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2000to2009.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2009 >= 5, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2000to2009.All <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2009 == 9, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2010to2019 <- 
  (CAD.Classification$AccessDesert.Carnegie.2010 + CAD.Classification$AccessDesert.Carnegie.2011 + 
     CAD.Classification$AccessDesert.Carnegie.2012 + CAD.Classification$AccessDesert.Carnegie.2013 +
     CAD.Classification$AccessDesert.Carnegie.2014 + CAD.Classification$AccessDesert.Carnegie.2015 + 
     CAD.Classification$AccessDesert.Carnegie.2016 + CAD.Classification$AccessDesert.Carnegie.2017 +
     CAD.Classification$AccessDesert.Carnegie.2018 + CAD.Classification$AccessDesert.Carnegie.2019)

CAD.Classification$AccessDesert.Carnegie.2010to2019.Any <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2010to2019 >= 1, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2010to2019.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2010to2019 >= 5, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2010to2019.All <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2010to2019 == 10, 1, 0)

table(CAD.Classification$AccessDesert.Carnegie.2010to2019.All, useNA = 'always')

CAD.Classification$AccessDesert.Carnegie.2000to2019 <- CAD.Classification$AccessDesert.Carnegie.2000to2009 +
  CAD.Classification$AccessDesert.Carnegie.2010to2019

CAD.Classification$AccessDesert.Carnegie.2000to2019.Any <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2019 >= 1, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2000to2019.Majority <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2019 >= 10, 1, 0)

CAD.Classification$AccessDesert.Carnegie.2000to2019.All <- 
  ifelse(CAD.Classification$AccessDesert.Carnegie.2000to2019 == 19, 1, 0)

View(CAD.Classification)

kept_variables <- c("FIPS", "ERS00", 'OUT10',
                    "AccessDesert.Sector.2001",                "AccessDesert.Sector.2002",               
                     "AccessDesert.Sector.2003",                "AccessDesert.Sector.2004",               
                     "AccessDesert.Sector.2005",                "AccessDesert.Sector.2006",               
                     "AccessDesert.Sector.2007",                "AccessDesert.Sector.2008",               
                     "AccessDesert.Sector.2009",                "AccessDesert.Sector.2010",               
                     "AccessDesert.Sector.2011",                "AccessDesert.Sector.2012",               
                     "AccessDesert.Sector.2013",                "AccessDesert.Sector.2014",               
                     "AccessDesert.Sector.2015",                "AccessDesert.Sector.2016",               
                     "AccessDesert.Sector.2017",                "AccessDesert.Sector.2018",               
                     "AccessDesert.Sector.2019",                "AccessDesert.Sector.2000to2009",         
                     "AccessDesert.Sector.2000to2009.Any",      "AccessDesert.Sector.2000to2009.Majority",
                     "AccessDesert.Sector.2000to2009.All",      "AccessDesert.Sector.2010to2019",         
                     "AccessDesert.Sector.2010to2019.Any",      "AccessDesert.Sector.2010to2019.Majority",
                     "AccessDesert.Sector.2010to2019.All",      "AccessDesert.Sector.2000to2019",         
                     "AccessDesert.Sector.2000to2019.Any",      "AccessDesert.Sector.2000to2019.Majority",
                     "AccessDesert.Sector.2000to2019.All",
                     "AccessDesert.Carnegie.2001",                "AccessDesert.Carnegie.2002",               
                    "AccessDesert.Carnegie.2003",                "AccessDesert.Carnegie.2004",               
                    "AccessDesert.Carnegie.2005",                "AccessDesert.Carnegie.2006",               
                    "AccessDesert.Carnegie.2007",                "AccessDesert.Carnegie.2008",               
                     "AccessDesert.Carnegie.2009",                "AccessDesert.Carnegie.2010",               
                     "AccessDesert.Carnegie.2011",                "AccessDesert.Carnegie.2012",               
                     "AccessDesert.Carnegie.2013",                "AccessDesert.Carnegie.2014",               
                     "AccessDesert.Carnegie.2015",                "AccessDesert.Carnegie.2016",               
                     "AccessDesert.Carnegie.2017",                "AccessDesert.Carnegie.2018",               
                     "AccessDesert.Carnegie.2019",                "AccessDesert.Carnegie.2000to2009",         
                     "AccessDesert.Carnegie.2000to2009.Any",      "AccessDesert.Carnegie.2000to2009.Majority",
                     "AccessDesert.Carnegie.2000to2009.All",      "AccessDesert.Carnegie.2010to2019",         
                     "AccessDesert.Carnegie.2010to2019.Any",      "AccessDesert.Carnegie.2010to2019.Majority",
                     "AccessDesert.Carnegie.2010to2019.All",      "AccessDesert.Carnegie.2000to2019",         
                     "AccessDesert.Carnegie.2000to2019.Any",      "AccessDesert.Carnegie.2000to2019.Majority",
                     "AccessDesert.Carnegie.2000to2019.All")
CAD.Classification.Output <- CAD.Classification %>%
  select(kept_variables)


# Add Changed FIPS for AK and SD ------------------------------------------
View(CAD.Classification.Output)

#CAD.Classification.Output <- read.csv("CAD Classification_Output_20220929.csv")

#Create mini-dataframe, one for each of the two counties that changed FIPS codes because of name changes in 2015
CAD.ChangeFIPS.a <- subset(CAD.Classification.Output, subset = CAD.Classification.Output$FIPS == '46113')
CAD.ChangeFIPS.b <- subset(CAD.Classification.Output, subset = CAD.Classification.Output$FIPS == '2270')

CAD.ChangeFIPS.a$FIPS <- '46102'
CAD.ChangeFIPS.b$FIPS <- '2158'

CAD.Classification.Output <- rbind(CAD.Classification.Output, CAD.ChangeFIPS.a)
CAD.Classification.Output <- rbind(CAD.Classification.Output, CAD.ChangeFIPS.b)

write.csv(CAD.Classification.Output, file = 'CAD Classification_Output_20230105.csv')
