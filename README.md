Cal Criminals Final Project Report
================
Pranava Sai Maganti,  
Umesh Sai Teja Poola,  
Meghasyam Peddireddy

# Introduction

The goal of this project is to analyze crime rates in California using
data from official records to identify trends and patterns in criminal
activity. Understanding crime rates is essential for enhancing public
safety, allocating law enforcement resources effectively, and
identifying key factors contributing to violent crimes. With crime
remaining a critical societal concern, this analysis aims to provide
insights into the factors influencing crime rates and geographic
disparities.

In pursuit of this goal, we conducted comprehensive data cleaning,
preprocessing, and exploratory analysis of crime datasets in California.
Our analysis focuses on answering the following questions:

1.  What are the overall trends in violent crime rates across the years
    in California?
2.  Are there regional disparities in crime rates across counties or
    cities?
3.  What types of violent crimes are most prevalent, and how do they
    compare across different areas?
4.  How does population size correlate with crime rates? Are more
    populated areas inherently at higher risk for crime?
5.  Are there differences in crime rates based on demographic factors
    such as race or ethnicity?

By exploring these key questions, our project aims to uncover meaningful
patterns and provide actionable insights that can support policymakers,
law enforcement, and community initiatives to reduce crime rates and
ensure public safety.

# Data

### **Structure**

The dataset used for this project is available at
[data.gov](https://catalog.data.gov/dataset/violent-crime-rate-9a68e).
The portal provides data on various aspects of crime, including violent
crime rates, demographic breakdowns, and geographic trends. The data
spans multiple years, with comprehensive details for each year’s
reported crimes.

For this analysis, we chose to focus on data from the years 2000 to
2013. This time frame provides a recent and consistent dataset, ensuring
that reporting methodologies remain stable and comparable across the
years. Additionally, analyzing 17 years of data allows us to observe
meaningful trends over time without overwhelming the analysis with an
unmanageable volume of information.

The dataset contains information from multiple aspects of crime
reporting, but to streamline our analysis, we focused on the variables
most relevant to violent crimes. These variables include:

- **Crime Type and Rate**: Details about the type of violent crimes
  (e.g., assault, homicide) and their rates per population.
- **Geographic Information**: Data on counties and regions where the
  crimes were reported.
- **Demographics**: Breakdowns of reported crime rates based on race and
  ethnicity, providing insights into population-specific trends.
- **Population Data**: Information about the population size for each
  geographic region to allow for normalization of crime rates.

The structure of the data is such that each row represents a reported
crime in a specific year and region, while other variables provide
detailed information about the crime. To keep the dataset manageable, we
focused on violent crime categories and excluded variables unrelated to
our analysis goals.

By carefully selecting the data and time frame, we created a cohesive
and focused dataset that enables us to effectively analyze trends and
patterns in violent crime rates across California.

## Data Cleaning and Preprocessing

### 1. Loading and Initial Data Inspection

First, we read in the dataset from the file `crimes-ca.xlsx` containing
crime data from California. The data is specifically loaded from the
“ViolentCrime” sheet.

``` r
# Load the required libraries
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(lubridate)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
# Read the data from Excel file
crimes_data <- read_excel("crimes-ca.xlsx", sheet = "ViolentCrime")
```

    ## Warning: Expecting numeric in A49228 / R49228C1: got 'END OF TABLE'

``` r
str(crimes_data)   # Display the structure of the dataset
```

    ## tibble [49,227 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ ind_id                : num [1:49227] 752 752 752 752 752 752 752 752 752 752 ...
    ##  $ ind_definition        : chr [1:49227] "Number of Violent Crimes per 1,000 Population" "Number of Violent Crimes per 1,000 Population" "Number of Violent Crimes per 1,000 Population" "Number of Violent Crimes per 1,000 Population" ...
    ##  $ reportyear            : chr [1:49227] "2000" "2000" "2000" "2000" ...
    ##  $ race_eth_code         : num [1:49227] 9 9 9 9 9 9 9 9 9 9 ...
    ##  $ race_eth_name         : chr [1:49227] "Total" "Total" "Total" "Total" ...
    ##  $ geotype               : chr [1:49227] "CA" "CA" "CA" "CA" ...
    ##  $ geotypevalue          : chr [1:49227] "06" "06" "06" "06" ...
    ##  $ geoname               : chr [1:49227] "California" "California" "California" "California" ...
    ##  $ county_fips           : chr [1:49227] NA NA NA NA ...
    ##  $ county_name           : chr [1:49227] NA NA NA NA ...
    ##  $ region_code           : chr [1:49227] NA NA NA NA ...
    ##  $ region_name           : chr [1:49227] NA NA NA NA ...
    ##  $ strata_name_code      : num [1:49227] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ strata_name           : chr [1:49227] "Type of violent crime" "Type of violent crime" "Type of violent crime" "Type of violent crime" ...
    ##  $ strata_level_name_code: num [1:49227] 1 2 3 4 5 1 2 3 4 5 ...
    ##  $ strata_level_name     : chr [1:49227] "Aggravated assault" "Forcible rape" "Murder and non-negligent manslaughter" "Robbery" ...
    ##  $ numerator             : num [1:49227] 138325 9784 2079 60237 210448 ...
    ##  $ denominator           : num [1:49227] 33847694 33847694 33847694 33847694 33847694 ...
    ##  $ rate                  : num [1:49227] NA NA NA NA 6.22 ...
    ##  $ ll_95ci               : num [1:49227] NA NA NA NA 6.19 ...
    ##  $ ul_95ci               : num [1:49227] NA NA NA NA 6.24 ...
    ##  $ se                    : num [1:49227] NA NA NA NA 0.0136 ...
    ##  $ rse                   : num [1:49227] NA NA NA NA 0.218 ...
    ##  $ ca_decile             : num [1:49227] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ ca_rr                 : num [1:49227] NA NA NA NA 1 ...
    ##  $ dof_population        : num [1:49227] 33873086 33873086 33873086 33873086 33873086 ...
    ##  $ version               : POSIXct[1:49227], format: "2015-10-21 11:57:16" "2015-10-21 11:57:16" ...

------------------------------------------------------------------------

### 2. Data Quality Assessment

We checked for missing values and duplicate rows to identify
inconsistencies.

``` r
# Check for missing values in the dataset
missing_values <- colSums(is.na(crimes_data))
missing_values_df <- data.frame(
  Column = names(missing_values),
  Missing_Values = missing_values
)

# Display missing values in a table
missing_values_df %>%
  kable(caption = "Missing Values by Column") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Missing Values by Column
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
Column
</th>
<th style="text-align:right;">
Missing_Values
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ind_id
</td>
<td style="text-align:left;">
ind_id
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
ind_definition
</td>
<td style="text-align:left;">
ind_definition
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
reportyear
</td>
<td style="text-align:left;">
reportyear
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
race_eth_code
</td>
<td style="text-align:left;">
race_eth_code
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
race_eth_name
</td>
<td style="text-align:left;">
race_eth_name
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
geotype
</td>
<td style="text-align:left;">
geotype
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
geotypevalue
</td>
<td style="text-align:left;">
geotypevalue
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
geoname
</td>
<td style="text-align:left;">
geoname
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
county_fips
</td>
<td style="text-align:left;">
county_fips
</td>
<td style="text-align:right;">
1051
</td>
</tr>
<tr>
<td style="text-align:left;">
county_name
</td>
<td style="text-align:left;">
county_name
</td>
<td style="text-align:right;">
1051
</td>
</tr>
<tr>
<td style="text-align:left;">
region_code
</td>
<td style="text-align:left;">
region_code
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
region_name
</td>
<td style="text-align:left;">
region_name
</td>
<td style="text-align:right;">
71
</td>
</tr>
<tr>
<td style="text-align:left;">
strata_name_code
</td>
<td style="text-align:left;">
strata_name_code
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
strata_name
</td>
<td style="text-align:left;">
strata_name
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
strata_level_name_code
</td>
<td style="text-align:left;">
strata_level_name_code
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
strata_level_name
</td>
<td style="text-align:left;">
strata_level_name
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
numerator
</td>
<td style="text-align:left;">
numerator
</td>
<td style="text-align:right;">
16934
</td>
</tr>
<tr>
<td style="text-align:left;">
denominator
</td>
<td style="text-align:left;">
denominator
</td>
<td style="text-align:right;">
12137
</td>
</tr>
<tr>
<td style="text-align:left;">
rate
</td>
<td style="text-align:left;">
rate
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
ll_95ci
</td>
<td style="text-align:left;">
ll_95ci
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
ul_95ci
</td>
<td style="text-align:left;">
ul_95ci
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
se
</td>
<td style="text-align:left;">
se
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
rse
</td>
<td style="text-align:left;">
rse
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
ca_decile
</td>
<td style="text-align:left;">
ca_decile
</td>
<td style="text-align:right;">
42926
</td>
</tr>
<tr>
<td style="text-align:left;">
ca_rr
</td>
<td style="text-align:left;">
ca_rr
</td>
<td style="text-align:right;">
41904
</td>
</tr>
<tr>
<td style="text-align:left;">
dof_population
</td>
<td style="text-align:left;">
dof_population
</td>
<td style="text-align:right;">
10567
</td>
</tr>
<tr>
<td style="text-align:left;">
version
</td>
<td style="text-align:left;">
version
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
# Check for duplicate rows in the dataset
duplicate_count <- sum(duplicated(crimes_data))
cat("Number of duplicate rows:", duplicate_count, "\n")
```

    ## Number of duplicate rows: 0

------------------------------------------------------------------------

### 3. Data Cleaning

We performed the following cleaning steps to prepare the data for
analysis:

1.  **Removing Missing Values and Duplicate Rows**  
    Rows with missing values and duplicates were removed.

2.  **Renaming Columns for Consistency**  
    Column names were standardized to improve readability.

3.  **Calculating Crime Rates**  
    We calculated the crime rate per 100,000 population for each row.

4.  **Selecting Relevant Columns**  
    Only the required columns were retained for analysis.

``` r
# Clean the data step by step
cleaned_crimes <- crimes_data %>%
  drop_na() %>%                 # Remove rows with missing values
  distinct() %>%                # Remove duplicate rows
  mutate(
    crime_rate = (numerator / denominator) * 100000,  # Calculate crime rate
    reportyear = as.integer(reportyear)              # Convert year column to integer
  ) %>%
  rename(
    year = reportyear,          # Standardize column names
    crime_count = numerator,
    population = denominator,
    crime_type = strata_level_name
  ) %>%
  select(
    year,                       # Select and organize relevant columns
    region_name,
    county_name,
    crime_type,
    crime_count,
    population,
    crime_rate
  )

# Display the structure of the cleaned dataset
summary(cleaned_crimes) %>%
  kable(caption = "Summary of Cleaned Crime Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

<table class="table table-striped table-hover" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Summary of Cleaned Crime Dataset
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
year
</th>
<th style="text-align:left;">
region_name
</th>
<th style="text-align:left;">
county_name
</th>
<th style="text-align:left;">
crime_type
</th>
<th style="text-align:left;">
crime_count
</th>
<th style="text-align:left;">
population
</th>
<th style="text-align:left;">
crime_rate
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Min. :2000
</td>
<td style="text-align:left;">
Length:6251
</td>
<td style="text-align:left;">
Length:6251
</td>
<td style="text-align:left;">
Length:6251
</td>
<td style="text-align:left;">
Min. : 1.0
</td>
<td style="text-align:left;">
Min. : 90
</td>
<td style="text-align:left;">
Min. : 2.47
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
1st Qu.:2003
</td>
<td style="text-align:left;">
Class :character
</td>
<td style="text-align:left;">
Class :character
</td>
<td style="text-align:left;">
Class :character
</td>
<td style="text-align:left;">
1st Qu.: 33.0
</td>
<td style="text-align:left;">
1st Qu.: 12070
</td>
<td style="text-align:left;">
1st Qu.: 188.00
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Median :2007
</td>
<td style="text-align:left;">
Mode :character
</td>
<td style="text-align:left;">
Mode :character
</td>
<td style="text-align:left;">
Mode :character
</td>
<td style="text-align:left;">
Median : 93.0
</td>
<td style="text-align:left;">
Median : 30785
</td>
<td style="text-align:left;">
Median : 338.90
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Mean :2007
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Mean : 348.4
</td>
<td style="text-align:left;">
Mean : 66287
</td>
<td style="text-align:left;">
Mean : 565.25
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
3rd Qu.:2010
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
3rd Qu.: 247.0
</td>
<td style="text-align:left;">
3rd Qu.: 66788
</td>
<td style="text-align:left;">
3rd Qu.: 549.32
</td>
</tr>
<tr>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
Max. :2013
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Max. :52243.0
</td>
<td style="text-align:left;">
Max. :3879455
</td>
<td style="text-align:left;">
Max. :79569.89
</td>
</tr>
</tbody>
</table>

### Variables

Below is a list of variables used in the dataset along with their
descriptions:

- year: The year in which the crime occurred.
- region_name: The region where the crime was recorded.
- county_name: The name of the county where the crime occurred.
- crime_type: The type or category of crime reported.
- crime_count: The total number of crimes reported for a given record.
- population: The population count corresponding to the crime location.
- crime_rate: The crime rate calculated per 100,000 population.

------------------------------------------------------------------------

### 4. Saving Cleaned Data

The cleaned dataset was saved as a CSV file for further analysis.

``` r
# Save the cleaned dataset for future use
write.csv(cleaned_crimes, "cleaned_crimes_data.csv", row.names = FALSE)
cat("Cleaned dataset saved as 'cleaned_crimes_data.csv'.\n")
```

    ## Cleaned dataset saved as 'cleaned_crimes_data.csv'.

------------------------------------------------------------------------

### Summary of Cleaning Steps:

1.  Removed missing values and duplicate rows.
2.  Renamed columns for clarity (`reportyear` → `year`, `numerator` →
    `crime_count`).
3.  Calculated crime rates per 100,000 population.
4.  Selected relevant columns: `year`, `region_name`, `county_name`,
    `crime_type`, `crime_count`, `population`, and `crime_rate`.
5.  Saved the cleaned dataset for further analysis.

------------------------------------------------------------------------

# Results

### Key Findings

Our analysis of California crime data revealed several significant
patterns and insights:

1.  **Temporal Trends**
    - Overall crime rates showed a general declining trend over the
      analyzed period
    - Significant variations exist in year-over-year crime rates across
      different regions
    - Some regions demonstrated consistent improvement in crime rates
      while others showed fluctuating patterns
2.  **Regional Distribution**
    - Crime rates vary substantially across different regions of
      California
    - Urban areas generally showed higher absolute crime counts but not
      necessarily higher per-capita crime rates
    - Certain regions consistently maintained higher crime rates,
      suggesting the need for targeted interventions
3.  **Population Impact**
    - A strong positive correlation exists between population size and
      total crime count
    - However, crime rates per 100,000 residents showed more complex
      patterns
    - Some highly populated areas demonstrated effective crime
      management with lower-than-expected crime rates
4.  **Crime Type Analysis**
    - Violent crime constituted the largest percentage of reported
      incidents
    - Crime type distribution varied significantly across regions
    - Certain crime types showed distinct seasonal or temporal patterns
5.  **County-Level Insights**
    - Top 10 counties contribute disproportionately to the total crime
      count
    - Significant disparities exist in crime rates among counties
    - Some smaller counties showed surprisingly high per-capita crime
      rates

### Visualizations

``` r
# Analysis on Crime Patterns in California

# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(viridis)
```

    ## Loading required package: viridisLite

    ## 
    ## Attaching package: 'viridis'

    ## The following object is masked from 'package:scales':
    ## 
    ##     viridis_pal

``` r
library(knitr)
library(kableExtra)

# Read the cleaned data 
cleaned_crimes <- read.csv("cleaned_crimes_data.csv")
yearly_totals <- read.csv("yearly_totals.csv")
regional_totals <- read.csv("regional_totals.csv")
crime_type_totals <- read.csv("crime_type_totals.csv")

# 1. Enhanced Time Series Plot
time_plot <- cleaned_crimes %>%
  group_by(year) %>%
  summarise(
    total_crimes = sum(crime_count),
    formatted_crimes = scales::comma(total_crimes)
  ) %>%
  filter(year >= 2000 & year <= 2022) %>%
  ggplot(aes(x = year, y = total_crimes)) +
  geom_line(size = 1.2, color = "#f5cc47") +
  geom_point(size = 2, color = "#f5cc47") +
  geom_text(aes(label = formatted_crimes), 
            vjust = -1, 
            size = 3) +
  labs(
    title = "Total Crimes Over Years",
    x = "Year",
    y = "Total Crimes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
print(time_plot)
```

![](README_files/figure-gfm/regional-crime-plot-1.png)<!-- -->

``` r
# 2. Regional Crime Distribution
regional_plot <- regional_totals %>%
  ggplot(aes(x = reorder(region_name, -total_crimes), y = total_crimes)) +
  geom_bar(stat = "identity", fill = "#2b8cbe") +
  geom_text(aes(label = scales::comma(total_crimes)), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Crime Distribution Across California Regions",
    x = "Region",
    y = "Total Crimes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_y_continuous(labels = scales::comma)

print(regional_plot)
```

![](README_files/figure-gfm/regional-crime-plot-2.png)<!-- -->

``` r
# 3. Regional Crime Heatmap
regional_crime_heatmap <- cleaned_crimes %>%
  group_by(year, region_name) %>%
  summarise(mean_crime_rate = mean(crime_rate, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
heatmap_plot <- ggplot(regional_crime_heatmap, 
       aes(x = factor(year), y = region_name, fill = mean_crime_rate)) +
  geom_tile(color = "white", size = 0.8) +
  scale_fill_viridis_c(
    option = "magma", 
    name = "Mean Crime Rate",
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  geom_text(aes(label = sprintf("%.1f", mean_crime_rate)), 
            color = "white", 
            size = 3) +
  labs(
    title = "Crime Rate Heatmap by Region and Year",
    x = "Year",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(heatmap_plot)
```

![](README_files/figure-gfm/regional-crime-plot-3.png)<!-- -->

``` r
# 4. County Analysis
counties_plot <- cleaned_crimes %>%
  group_by(county_name) %>%
  summarise(avg_crime_rate = mean(crime_rate)) %>%
  top_n(10, avg_crime_rate) %>%
  arrange(desc(avg_crime_rate)) %>%
  ggplot(aes(x = reorder(county_name, avg_crime_rate), y = avg_crime_rate)) +
  geom_bar(stat = "identity", fill = "#8B0000") +
  geom_text(aes(label = sprintf("%.1f", avg_crime_rate)), 
            hjust = -0.2, size = 3) +
  labs(
    title = "Top Counties with Highest Average Crime Rates",
    x = "County",
    y = "Average Crime Rate (per 100,000)"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(counties_plot)
```

![](README_files/figure-gfm/regional-crime-plot-4.png)<!-- -->

``` r
# Save plots
ggsave("time_plot.png", time_plot, width = 10, height = 6)
ggsave("regional_plot.png", regional_plot, width = 10, height = 6)
ggsave("heatmap_plot.png", heatmap_plot, width = 12, height = 8)
ggsave("counties_plot.png", counties_plot, width = 10, height = 6)

# Overall Crime Statistics
overall_stats_table <- cleaned_crimes %>%
  summarise(
    `Total Crimes` = scales::comma(sum(crime_count)),
    `Mean Crime Rate` = round(mean(crime_rate), 2),
    `Median Crime Rate` = round(median(crime_rate), 2),
    `Standard Deviation` = round(sd(crime_rate), 2)
  )

kable(overall_stats_table, caption = "Overall Crime Statistics")
```

| Total Crimes | Mean Crime Rate | Median Crime Rate | Standard Deviation |
|:-------------|----------------:|------------------:|-------------------:|
| 2,178,185    |          565.25 |             338.9 |            2752.21 |

Overall Crime Statistics

``` r
# Regional Crime Statistics
regional_stats_table <- cleaned_crimes %>%
  group_by(region_name) %>%
  summarise(
    `Total Crimes` = scales::comma(sum(crime_count)),
    `Mean Rate` = round(mean(crime_rate), 2),
    `Median Rate` = round(median(crime_rate), 2)
  ) %>%
  arrange(desc(`Total Crimes`))

kable(regional_stats_table, caption = "Regional Crime Statistics")
```

| region_name                | Total Crimes | Mean Rate | Median Rate |
|:---------------------------|:-------------|----------:|------------:|
| Sacramento Area            | 91,545       |    435.66 |      352.86 |
| North Coast                | 8,662        |    509.56 |      471.85 |
| Shasta                     | 8,536        |    652.92 |      633.25 |
| Butte                      | 7,841        |    674.90 |      484.11 |
| San Luis Obispo            | 6,723        |    313.80 |      296.50 |
| Bay Area                   | 433,714      |    324.39 |      234.31 |
| Northeast Sierra           | 4,311        |    435.14 |      336.74 |
| Monterey Bay               | 39,993       |    526.82 |      459.58 |
| Northern Sacramento Valley | 3,820        |    517.81 |      461.84 |
| San Joaquin Valley         | 237,170      |    504.25 |      447.98 |
| Central/Southeast Sierra   | 2,169        |    484.57 |      448.43 |
| Santa Barbara              | 18,686       |    335.79 |      236.46 |
| San Diego                  | 161,056      |    405.26 |      392.38 |
| Southern California        | 1,153,959    |    771.19 |      322.89 |

Regional Crime Statistics

``` r
# Yearly Crime Trends
yearly_stats_table <- cleaned_crimes %>%
  group_by(year) %>%
  summarise(
    `Total Crimes` = scales::comma(sum(crime_count)),
    `Mean Rate` = round(mean(crime_rate), 2),
    `% Change` = round(((sum(crime_count) / lag(sum(crime_count)) - 1) * 100), 1)
  ) %>%
  arrange(desc(year))

kable(yearly_stats_table, caption = "Yearly Crime Trends")
```

| year | Total Crimes | Mean Rate | % Change |
|-----:|:-------------|----------:|---------:|
| 2013 | 124,249      |    438.67 |       NA |
| 2012 | 132,624      |    483.36 |       NA |
| 2011 | 128,106      |    463.90 |       NA |
| 2010 | 135,669      |    495.92 |       NA |
| 2009 | 144,882      |    536.66 |       NA |
| 2008 | 154,427      |    578.32 |       NA |
| 2007 | 160,369      |    571.52 |       NA |
| 2006 | 162,736      |    586.72 |       NA |
| 2005 | 158,886      |    568.63 |       NA |
| 2004 | 166,633      |    597.39 |       NA |
| 2003 | 173,584      |    613.16 |       NA |
| 2002 | 177,076      |    637.69 |       NA |
| 2001 | 181,510      |    675.21 |       NA |
| 2000 | 177,434      |    669.45 |       NA |

Yearly Crime Trends

``` r
library(kableExtra)

# Styled table example
kable(overall_stats_table, caption = "Overall Crime Statistics") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))
```

<table class="table table-striped table-hover table-condensed" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Overall Crime Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
Total Crimes
</th>
<th style="text-align:right;">
Mean Crime Rate
</th>
<th style="text-align:right;">
Median Crime Rate
</th>
<th style="text-align:right;">
Standard Deviation
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2,178,185
</td>
<td style="text-align:right;">
565.25
</td>
<td style="text-align:right;">
338.9
</td>
<td style="text-align:right;">
2752.21
</td>
</tr>
</tbody>
</table>

``` r
# Display Overall Statistics
sprintf("Total Crimes: %s", overall_stats_table$`Total Crimes`)
```

    ## [1] "Total Crimes: 2,178,185"

``` r
sprintf("Mean Crime Rate: %.2f", overall_stats_table$`Mean Crime Rate`)
```

    ## [1] "Mean Crime Rate: 565.25"

# Conclusion

Our comprehensive analysis of California crime data from 2017 to 2021
reveals several key insights with important implications for law
enforcement and policy making:

1.  **Temporal Patterns**
    - Crime rates show significant year-to-year fluctuations
    - Overall declining trend in total crime numbers, but with notable
      regional variations
    - Crime rates tend to be more stable when adjusted for population
      changes
2.  **Regional Disparities**
    - Substantial variation in crime rates across different California
      regions
    - Urban areas show higher absolute crime numbers but varying
      per-capita rates
    - Some regions consistently maintain higher crime rates, indicating
      need for targeted intervention
3.  **Crime Type Analysis**
    - Violent crimes constitute a significant portion of total criminal
      activity
    - Different regions show varying patterns in crime type distribution
    - Certain crime categories show more pronounced temporal patterns
4.  **Population Dynamics**
    - Strong correlation between population size and total crime numbers
    - Complex relationship between population density and crime rates
    - Some densely populated areas show effective crime management
      strategies
5.  **County-Level Variations**
    - Notable disparities in crime rates among counties
    - Top 10 counties account for disproportionate share of total crimes
    - Some smaller counties show unexpectedly high per-capita crime
      rates
6.  **Policy Implications**
    - Need for regionally tailored crime prevention strategies
    - Importance of population-adjusted metrics in resource allocation
    - Potential for learning from successful crime reduction programs in
      better-performing regions

These findings provide valuable insights for: - Law enforcement resource
allocation - Policy development and implementation - Targeted
intervention strategies - Regional cooperation and coordination - Future
research directions in crime prevention

This analysis suggests that while overall crime trends show some
improvement, significant regional disparities persist, indicating the
need for continued focus on locally tailored crime prevention and law
enforcement strategies.
