################################################################################

# Script Title: Plotting total counts of Black-capped Chickadees in the Breeding
# Bird Survey Data for Route 212 "Point Grey"

# Script Author: Rory Macklin (macklin@zoology.ubc.ca)

# Date: September 27, 2022

################################################################################

# Open "groundhog" package to conduct package version control.

library(groundhog)

# Load required packages using "groundhog" to access versions most recent to
# 2022-09-25.

groundhog.library(tidyverse, date = "2022-09-25")
groundhog.library(visreg, date = "2022-09-25")

# Read cleaned dataset created in "BBS-Data-Download-Filter.R" into environment 
# for modeling and plotting.

PointGrey_BCCH <- read_csv(file = "./Data/Clean_Data/BBS_filtered/PointGrey_BCCH.csv")

# Use ggplot2 to plot counts over time in a scatter plot.

PointGrey_BCCH_Plot <- ggplot(data = PointGrey_BCCH, mapping = aes(x = Year, y = SpeciesTotal)) +
  geom_point() +
  xlab("Year") +
  ylab("Count of BCCH") +
  theme_classic()

PointGrey_BCCH_Plot

# Use base "glm" function to fit a Generalized Linear Model with a log-linked 
# quasipoisson distribution (appropriate for overdispersed count data). Much of
# the background for this analysis was found at Dr. Dolph Schluter's University
# of British Columbia course "Quantitative methods in ecology and evolution",
# accessed at https://www.zoology.ubc.ca/~schluter/R/Model.html on 2022-09-27.

PointGrey_BCCH_GLM <- glm(data = PointGrey_BCCH, SpeciesTotal ~ Year, family = quasipoisson(link = "log"))

# Use base "summary" and visreg package "visreg" commands to visually and 
# numerically assess the characteristics of the model.

summary(PointGrey_BCCH_GLM)
visreg(PointGrey_BCCH_GLM, xvar = "Year", ylim = range(PointGrey_BCCH$SpeciesTotal), rug = 2, scale = "response") 
