# Download and parse ERIC data

This repository downloads and parses published NHS Estates Returns 
Information Collection data.  

The raw data is available from here:  
https://digital.nhs.uk/data-and-information/publications/statistical/estates-returns-information-collection  

This will download all of the files from each year, and then combine the results.
Historical data (from 1999/00 to 2013/14) is in a slightly different format. 

It will output:

- eric_combined.RDS - results all questions (from 2014/15)
- historic_eric_combined.RDS - results for all questions (1999/00 to 2013/14)
- trust_type_lookup.RDS - trust_type by year (from 1999/00)
- trust_type_lookup.csv - trust_type by year (from 1999/00)  

The data in eric_combined and historic_eric_combined has not been looked at, 
but this could be a useful starting point.  