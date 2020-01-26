#Using read to read the csv file
> MechaCar_MPG_Table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
> #Adding the library in order to use liner model parameters 
> library(tidyverse)
> #Multiple Regresion (MPG Regression)
> #Changed excel Colum name as the space was creating issue while reading the file 
> lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_MPG_Table)
> lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle +ground_clearance+ AWD,data=MechaCar_MPG_Table)

Call:
lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ground_clearance + AWD, data = MechaCar_MPG_Table)

Coefficients:
     (Intercept)    vehicle_length    vehicle_weight     spoiler_angle  ground_clearance               AWD  
      -1.040e+02         6.267e+00         1.245e-03         6.877e-02         3.546e+00        -3.411e+00  

> #Creating summary statistics 
> summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_MPG_Table))

Call:
lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
    ground_clearance + AWD, data = MechaCar_MPG_Table)

Residuals:
     Min       1Q   Median       3Q      Max 
-19.4701  -4.4994  -0.0692   5.4433  18.5849 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -1.040e+02  1.585e+01  -6.559 5.08e-08 ***
vehicle_length    6.267e+00  6.553e-01   9.563 2.60e-12 ***
vehicle_weight    1.245e-03  6.890e-04   1.807   0.0776 .  
spoiler_angle     6.877e-02  6.653e-02   1.034   0.3069    
ground_clearance  3.546e+00  5.412e-01   6.551 5.21e-08 ***
AWD              -3.411e+00  2.535e+00  -1.346   0.1852    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.774 on 44 degrees of freedom
Multiple R-squared:  0.7149,	Adjusted R-squared:  0.6825 
F-statistic: 22.07 on 5 and 44 DF,  p-value: 5.35e-11

> #To create the multiplelinear regrssion model we  will consider multiple independent variables.
> # Considering three independent variable , we will be creating a line plot and coeffieciets and then ploting points on scatter plot
> MPG_variable1 <- lm(mpg ~ vehicle_length,MechaCar_MPG_Table)
> #adding Intercepts 
> variable1_yvals <- MPG_variable1$coefficients['vehicle_length']*MechaCar_MPG_Table$vehicle_length + MPG_variable1$coefficients['(Intercept)']
> #now plotting scatter plot 
> #importing ggplot2
> plt1 <- ggplot(MechaCar_MPG_Table,aes(x=vehicle_length,y=mpg))
> plt1 + geom_point() + geom_line(aes(y=variable1_yvals), color = "red")
> #variable 2
> MPG_variable2 <- lm(mpg ~ vehicle_weight,MechaCar_MPG_Table)
> variable2_yvals<-  MPG_variable2$coefficients['vehicle_weight']*MechaCar_MPG_Table$vehicle_weight +  MPG_variable2$coefficients['(Intercept)']
> plt2 <- ggplot(MechaCar_MPG_Table,aes(x=vehicle_weight,y=mpg))
> plt2 + geom_point() + geom_line(aes(y=variable2_yvals), color = "red")
> #plaotting Variable 3
> 
> MPG_variable3 <- lm(mpg ~ spoiler_angle,MechaCar_MPG_Table)
> variable3_yvals3 <- MPG_variable3$coefficients['spoiler_angle']*MechaCar_MPG_Table$spoiler_angle + MPG_variable3$coefficients['(Intercept)']
> plt3 <- ggplot(MechaCar_MPG_Table,aes(x=spoiler_angle,y=mpg))
> plt3 + geom_point() + geom_line(aes(y=variable3_yvals3), color = "red")
> # Considering other two Variable as well 
> MPG_variable4 <- lm(mpg ~ ground_clearance,MechaCar_MPG_Table)
> variable4_yvals4 <-MPG_variable4$coefficients['ground_clearance']*MechaCar_MPG_Table$ground_clearance + MPG_variable4$coefficients['(Intercept)']
> plt4 <- ggplot(MechaCar_MPG_Table,aes(x=ground_clearance,y=mpg))
> plt4 + geom_point() + geom_line(aes(y=variable4_yvals4), color = "red")
> #Last variable in data set 
> MPG_variable5 <- lm(mpg ~ AWD,MechaCar_MPG_Table)
> variable5_yvals5 <- MPG_variable5$coefficients['AWD']*MechaCar_MPG_Table$AWD + MPG_variable5$coefficients['(Intercept)']
> plt5 <- ggplot(MechaCar_MPG_Table,aes(x=AWD,y=mpg)) 
> plt5 + geom_point() + geom_line(aes(y=variable5_yvals5), color = "red")
> #Suspension coil summary 
> # Creating Summary statistics 
> #reading the  CSV file First 
> suspension_table <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) 
>
> suspension_table <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) 
> view(suspension_table)
> library(tidyverse)
> summarize_demo <- suspension_table %>% group_by(PSI) %>% summarize(Mean_Mileage=mean(PSI))
>  Summary_Table <- suspension_table %>% group_by(PSI) %>% summarize(Mean= mean(suspension_table$PSI), Median= median(suspension_table$PSI), Variance= var(suspension_table$PSI), Standard_Deviation= sd(suspension_table$PSI))
> view(Summary_Table)
> #Creating Random sample for T-test on suspension coil 
> plt <- ggplot(suspension_table,aes(x=PSI))
> plt + geom_density()
> sample_table <- suspension_table %>% sample_n(50)
> #To make sample more normal we will using log 10
> lt <- ggplot(sample_table,aes(x=log10(PSI)))
> #visulaising the plot 
> lt+ geom_density()
> #using Welch two sample test as per module
> t.test((sample_table$PSI),mu=mean(suspension_table$PSI))

	One Sample t-test

data:  (sample_table$PSI)
t = -0.71949, df = 49, p-value = 0.4753
alternative hypothesis: true mean is not equal to 1498.78
95 percent confidence interval:
 1495.746 1500.214
sample estimates:
mean of x 
  1497.98 

> sample_table1 <- suspension_table %>% sample_n(50)
> sample_table2 <- suspension_table %>% sample_n(50)
> t.test(log10(sample_table$PSI),log10(sample_table2$PSI))

	Welch Two Sample t-test

data:  log10(sample_table$PSI) and log10(sample_table2$PSI)
t = -1.3472, df = 97.896, p-value = 0.181
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.0015607458  0.0002985442
sample estimates:
mean of x mean of y 
 3.175500  3.176131 
