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

### **Data Processing**

- Explain any steps taken to clean or preprocess the data:
  - Handling missing values
  - Transformations or filtering
  - Feature engineering, if applicable

------------------------------------------------------------------------

# Methods

If applicable, describe the methods or techniques you used in the
analysis: - Statistical methods - Machine learning models - Tools and
libraries (e.g., `dplyr`, `ggplot2`, etc.)

------------------------------------------------------------------------

# Results

### Key Findings

Present your results with appropriate visuals and summaries: - Use
tables or plots to communicate findings.

#### Example Plot

``` r
# Example Code for a Plot (Replace with your actual code)
library(ggplot2)

data(mpg) # Sample dataset
ggplot(mpg, aes(x=class, fill=drv)) +
    geom_bar() +
    labs(title="Example Plot: Count of Vehicles by Class",
         x="Vehicle Class", y="Count")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->