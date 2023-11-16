Camera Traffic Detectors
================

# Camera Traffic Detectors

## Overview

The dataset under examination has been meticulously maintained and
updated by the Arterial Management Division of the City of Austin
Transportation & Public Works Department. This comprehensive dataset
provides in-depth information about various traffic detectors
strategically positioned at signalized intersections across Austin,
Texas. As of October 4, 2023, this dataset comprises a total of 5,759
rows and 15 columns, with each row corresponding to a unique traffic
detector.

Columns in the dataset, which are not limited to detector_id,
detector_type, detector_status, detector_direction, detector_movement,
location_name, and atd_location_id, jointly offer detailed insights into
each traffic detector. These include the types of detectors, their
operational statuses, the directions and movements they service, their
physical locations, and unique identifiers for their locations.

The first hypothesis seeks to explore the potential relationship between
detector types (Loop or Gridsmart) and their operational statuses (OK,
Broken, Removed, or Inactive). The assumption here is that due to
advanced technology, Gridsmart detectors might demonstrate superior
operational stability and lower failure rates compared to Loop
detectors. The null hypothesis for this scenario posits no significant
relationship between detector types and their operational statuses.

For the second hypothesis, the focus is on examining the relationship
between detector types and the direction of traffic they monitor. It’s
hypothesized that the distribution of detector types might vary across
different traffic directions, potentially due to installation
preferences or technological suitability for specific traffic flows. The
corresponding null hypothesis asserts that there is no significant
relationship between the types of detectors and the traffic directions
they monitor.

Through the rigorous testing of these hypotheses, the analysis aims to
unearth crucial relationships and patterns within the dataset, thereby
offering valuable insights that the City of Austin could leverage to
enhance the efficiency and reliability of its traffic detection and
management systems. The results of the analyses will be depicted through
clear, accessible visualizations crafted using various data
visualization tools and techniques. This approach ensures that the
findings are easily understandable by audiences with varied technical
expertise, with a strong commitment to accuracy and thoroughness in
interpreting the data within the dataset.

## Link to Data

[Data
Set](https://data.austintexas.gov/Transportation-and-Mobility/Camera-Traffic-Counts/sh59-i6y9)

``` r
Traffic_Counts<- read.csv("Traffic_Detectors.csv")
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

\#Visualization Number One

``` r
incident_summary <- Traffic_Counts %>%
  group_by(detector_status) %>%
  summarise(incident_count = sum(signal_id, na.rm = TRUE)) %>%
  arrange(-incident_count)

ggplot(incident_summary, aes(x = detector_status, y = incident_count, fill = detector_status)) +
  geom_bar(stat = "identity", show.legend = FALSE, width=0.6) +
  labs(
    title = "Traffic Incidents by Equipment Status",
    x = "Equipment Status",
    y = "Number of Traffic Incidents"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
```

![](Readme_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\#Visualization Number Two

``` r
type_by_direction <- Traffic_Counts %>%
  group_by(detector_type, detector_direction) %>%
  summarise(count = n()) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'detector_type'. You can override using the
    ## `.groups` argument.

``` r
ggplot(type_by_direction, aes(x = detector_direction, y = count, fill = detector_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Count of Detector Types by Direction",
    x = "Detector Direction",
    y = "Count of Detectors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Readme_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

\#Visualization Number 3

``` r
  Traffic_Counts %>%
     group_by(detector_direction) %>%
     summarise(average_incident_count = mean(signal_id)) %>%
     ggplot(aes(x = detector_direction, y = average_incident_count, fill = detector_direction)) +
     geom_bar(stat = "identity", show.legend = FALSE) +
     labs(title = "Average Incident Count by Detector Direction",
          x = "Detector Direction",
          y = "Average Incident Count") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

    ## Warning: Removed 2 rows containing missing values (`position_stack()`).

![](Readme_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#Visualization Number 4

``` r
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

ggplot(Traffic_Counts, aes(x = LOCATION, y = as.factor(signal_id), fill = LOCATION)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Signal ID by Location",
       x = "Location Groups",
       y = "Frequency") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(),
        legend.position = "none") +
  coord_cartesian(xlim = c(0, 1100)) 
```

![](Readme_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

\#Regression or Classification
