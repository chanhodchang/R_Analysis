# R_Analysis
Using R to create statistical analysis about data for an Auto Dealership.

![Dealership](Images/Dealership.jpg)

## Summary
Used multiple R statistical tests to measure different production issues to see design flaws are at fault for poor mpg and PSI.

## Resources
- Data Sources: MechaCar_mpg.csv, Suspension_Coil.csv
- Software: R 3.6.2, RStudio 1.2.5001

## Analysis
### MPG Regression

Coefficients:

|(Intercept) | vehicle.length | vehicle.weight | spoiler.angle | ground.clearance | AWD|
|------------|----------------|----------------|---------------|------------------|----|
| -1.040e+02 |   6.267e+00    |     1.245e-03  |  6.877e-02    |  3.546e+00 |-3.411e+00|

Coefficients:

<table style="width:100%">
  <tr>
    <th></th>
    <th>Estimate</th>
    <th>Std. Error</th>
    <th>t value</th>
    <th>Pr(>|t|)</th>
    <th></th>
  </tr>
  <tr>
    <td>(Intercept)</td>
    <td>-1.040e+02</td>
    <td>1.585e+01</td>
    <td>-6.559</td>
    <td>5.08e-08</td>
    <td>***</td>
  </tr>
  <tr>
    <td>vehicle.length</td>
    <td>6.267e+00</td>
    <td>6.553e-01</td>
    <td>9.563</td>
    <td>2.60e-12</td>
    <td>***</td>
  </tr>
 <tr>
    <td>vehicle.weight</td>
    <td>1.245e-03</td>
    <td>6.890e-04</td>
    <td>1.807</td>
    <td>0.0776</td>
    <td>.</td>
  </tr>
 <tr>
    <td>spoiler.angle</td>
    <td>6.877e-02</td>
    <td>6.653e-02</td>
    <td>1.034</td>
    <td>0.3069</td>
    <td></td>
  </tr>
 <tr>
    <td>ground.clearance</td>
    <td>3.546e+00</td>
    <td>5.412e-01</td>
    <td>6.551</td>
    <td>5.21e-08</td>
    <td>***</td>
  </tr>
 <tr>
    <td>AWD</td>
    <td>-3.411e+00</td>
    <td>2.535e+00</td>
    <td>-1.346</td>
    <td>0.1852</td>
    <td></td>
  </tr>
</table>

---

Signif. codes: 0: `***`, 0.001:  `**`, 0.01: `*`, 0.05: `.`, 0.1: ` `, 1

Residual standard error: 8.774 on 44 degrees of freedom
<br>Multiple R-squared:  0.7149,	Adjusted R-squared:  0.6825 
<br>F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11

- When looking at the linear regression model that was created based on mpg, it showed that the Intercept, vehicle.length, and ground.clearance all are very unlikely to provide random amounts of variance. This is shown by looking at the Pr(>|t|) values and all three have a significance code of 0.
- The slope of the linear model cannot be considered to be zero. The reason why is by looking at the p-value from the regression model. It shows that that the p-value is 5.35e-11 which is alot smaller than the assumed significance level of 0.05%. This means that the null hypothesis is rejected which then translates that the slope of the linear model is not zero.
- This linear regression model does help to show the relationships with mpg to the other variables. Since the null hypothesis is rejected that means that there is a correlation between several of the variables and the mpg. It shows that vehicle.length and ground.clearance does have a significant correlation with mpg. And that something does need to change with those variables to improve mpg. 

### Suspension Coil Summary

* mean(suspension_coil$PSI)
  * [1] 1498.78

* median(suspension_coil$PSI)
  * [1] 1500

* var(suspension_coil$PSI)
  * [1] 62.29356

* sd(suspension_coil$PSI)
  * [1] 7.892627

- Looking at the variance for the suspension coil PSI, the value is 62.29 pounds per inch which does not exceed the 100 pounds per inch limit.
- The current manufacturing data does match these requirements because the variance for the PSI shows that it is below 100. 

### Suspension Coil T-Test
One Sample t-test

data:  log10(suspension_coil$PSI)
<br>t = 0, df = 149, p-value = 1

alternative hypothesis: true mean is not equal to 3.175732

95 percent confidence interval:
<br> 3.175361 3.176103

sample estimates:
<br>mean of x 
<br> 3.175732 

- Looking at the p-value that was resulted from the One Sample T-Test, the value is 1 which much higher than the the significance level of 0.05%. This means that there is not enough evidence to reject the null hypothesis. This means that the suspension coil's PSI result is statistically similar to the mean population results of 1,500 PSI.

### Own Study

- A test that can be useful to measure are different car options. When looking at different car models being sold, each company has different options and price points for the car. It would be useful to see if customers have a preference for certain colors or if premier options that come stocked with better wheels and engines would be a good reason for customers to purchase. 
- Some questions that could be asked is "What color would be most popular with the customers?" and another would be "Would a premium option car generate as much or more revenue than a base model car?"
- An ANOVA test can be used to see the distribution means of the different color options for the MechaCar. Since ANOVA uses mean from multiple samples, these tests can be useful to measure colors. There are probably gonna be more color options for the cars to have. 
- Two-Sample T-Test can be used to measure the distribution between the normal option MechaCar and a potential premium option MechaCar. This can be useful since the the two-sample measures means from two variables.