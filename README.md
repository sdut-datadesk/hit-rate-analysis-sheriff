# Hit rate and stop: San Diego County Sheriff's Department
By: [Lauryn Schroeder](https://www.sandiegouniontribune.com/sdut-lauryn-schroeder-staff.html) and [Lyndsay Winkley](https://www.sandiegouniontribune.com/sdut-lyndsay-winkley-staff.html)

This repository contains data and code for the analysis [reported and published](XXXXXX) by *The San Diego Union-Tribune* on March XXXXX, 2021.

### About

The Racial and Identity Profiling Act of 2015 (RIPA) requires nearly all California law enforcement agencies to submit demographic data on all
detentions and searches. The Union-Tribune obtained in January stop data from the San Diego County Sheriff's Department under the California Public Records Act.

The Union-Tribune collected this data to analyze stops conducted by the sheriff's department. In particular, the code calculates hit rates -- or search yield rates -- which show how many searches resulted in officers finding contraband.

When analyzing the reason for searches, criminal justice experts often categorize the basis or reason for a search into two groups: discretionary and non-discretionary. The code also analyzes trends in discretionary and non-discretionary searches.

In discretionary searches, it’s up to the officer whether there is a reason to search an individual. This group includes consent searches, where an officer requests to search and an individual agrees, as well as searches prompted by a person being suspected of a crime, suspected to have a weapon, or when contraband is visible.

Non-discretionary searches are when a search is required by law, such as when the basis for search was incident to arrest, vehicle inventory or a search warrant.

### Methodology / Notes

The sheriff department's data contains all pedestrian and traffic stops from July 2018 through June 2020. The original data tables are compiled in four, newline-deliminated JSON files, with nested data frames for each individual involved in each stop. Since more than one individual can be involved a stop (deputies are required to record the ethnicity of drivers and passengers) the Union-Tribune opted to analyze the race of each person involved, which is the same technique used by RIPA officials.

In some circumstances, deputies list more than one perceived race for an individual involved in traffic stops. 

Individuals who were perceived by deputies as Hispanic and any other race, were included in Hispanic totals. Individuals perceived as more than one race were categorized as those with two or more race. The remaining race categories were left the same.

There can be more than one reason for a search to take place. If both a non-discretionary and discretionary reason was listed, the Union-Tribune categorized the search as non-discretionary, since the search would take place regardless of other circumstances.

### The SDUT repository contains the following:

- `JAN_-_JUN_2019.txt` - Stop data from San Diego Sheriff's Department. Contains stops from January through June 2019.
- `JAN_-_JUN_2020.txt` - Stop data from San Diego Sheriff's Department. Contains stops from January through June 2020.
- `JUL_-_DEC_2018.txt` - Stop data from San Diego Sheriff's Department. Contains stops from July through December 2018.
- `JUL-DEC_2019.txt` - Stop data from San Diego Sheriff's Department. Contains stops from July through December 2019.
- `hit-rate-analysis-sheriff.R` - Import and analysis R script documenting findings published by the Union-Tribune.

### Sourcing
Please link and source [*The San Diego Union-Tribune*](https://www.sandiegouniontribune.com/) when referencing any analysis or findings in published work.

### Questions / Feedback

Email Lauryn Schroeder at [lauryn.schroeder@sduniontribune.com](mailto:lauryn.schroeder@sduniontribune.com) or Lyndsay Winkley at [lyndsay.winkley@sduniontribune.com](mailto:lyndsay.winkley@sduniontribune.com).
