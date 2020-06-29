# ttestapp (in development)

https://lavonne-hoang.shinyapps.io/apps/

The goal of this app is explore the robustness of assumptions for a two-sample t-test, specifically the Normality and constant variance assumptions (when using pooled standard deviation). 

## Using the app

### Tab 1
The user chooses the shape of two population distributions and the ratio of the standard deviation of population 1 to the standard deviation of population 2. The two population distributions are displayed. 

### Tab 2
A visualization showing the sampling distributions of the difference in means for 8 simulation runs. For each simulation, sample data is drawn from the populations with the criteria specified on Tab 1. 

### Tab 3
For each simulation, a 95% confidence interval for the difference in means is calculated. Red intervals are those that do not contain the true difference in means. If the assumptions are met, or the test is robust to violations in the assumptions, then about 95% of the intervals should contain the true difference in means. 

## Acknowledgements
This app was originally designed and created by Lavonne Hoang. The app is based on Section 3.2 of *Statistical Sleuth*. 



