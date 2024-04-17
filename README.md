# Montecincla-Sholicola-occupancy
Occupancy Study of Two Endemic Bird Species in Southern Western Ghats, India

This is an R Script for carrying out occupancy analyses of data gathered from the Masters Dissertation of Paul Pop (author of this script) in 2018. 
In 2018, the analysis was run in the PRESENCE software version 12.10.Over 350 different models were run in the co-occurrence and single-species models at that time. However, the analyses is redone here (in 2024) due to overfitting models and issues surrounding large SE and 95% CI errors during the 2018 analyses. This script will be updated with the link to the associated article once the pre-print or the peer-reviewed paper has been published.

The occupancy analyses is carried out using the package RPresence v. 2.13.60. Other packages used (besides the base packages and the ones automatically loaded via namespace): ggplot2_3.4.4, ggcorrplot_0.1.4.1, ggbeeswarm_0.7.2, dplyr_1.1.4, tidyr_1.2.0, data.table_1.14.2, gridExtra_2.3, janitor_2.2.0, stringr_1.4.0, tibble_3.1.6, stringr_1.4.0, officer_0.6.5, flextable_0.9.5, & knitr_1.37(optional).

The codes used in section 2, 3, and 6 of this script has been adapted largely from the RStudio RPresence tutorials called OccupancyTuts (https://doi.org/10.1111/2041-210X.14285), as well as documentation for the package RPresence.

This script doesn't assume any prior knowledge of R by the user.
Install RStudio in your system to run this script.
