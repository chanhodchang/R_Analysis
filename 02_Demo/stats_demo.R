# Visualize distribution using density plot
ggplot(mtcars,aes(x=wt))+geom_density()
?shapiro.test()
shapiro.test(mtcars$wt)

?sample_n()
# Import used car dataset
population_table<-read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F)
# Import dataset into ggplot2
plt<-ggplot(population_table,aes(x=log10(Miles_Driven)))
# Visualize distribution using density plot
plt+ geom_density()

# Randomly sample 50 data points
sample_table <- population_table %>% sample_n(50)
# Import dataset into ggplot2
plt<-ggplot(sample_table,aes(x=log10(Miles_Driven)))
# Visualize distribution using density plot
plt+geom_density()

?t.test()
# Compare sample versus population means
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven)))

# Generate 50 randomly sampled data
sample_table <- population_table %>% sample_n(50)
# Generate another 50 randomly sampled data
sample_table2 <- population_table %>% sample_n(50)
# Compare means of two samples
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven))

# Import dataset
mpg_data <- read.csv('mpg_modified.csv')
# Select only data points where the year is 1999
mpg_1999 <- mpg_data %>% filter(year==1999)
# Select only data points where year is 2008
mpg_2008 <- mpg_data %>% filter(year==2008)
# Compare the mean difference between two samples
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T)

?aov()
# Filter columns from mtcars dataset
mtcars_filt <- mtcars[,c('hp','cyl')]
# Convert numeric column to factor
mtcars_filt$cyl <- factor(mtcars_filt$cyl)
# Compare means across multiple levels
aov(hp~cyl,data=mtcars_filt)
summary(aov(hp~cyl,data=mtcars_filt))

?cor()
head(mtcars)
# Import dataset into ggplot2
plt<- ggplot(mtcars,aes(x=hp,y=qsec))
# Create scatter plot
plt + geom_point()
# Calculate correlation coefficient
cor(mtcars$hp,mtcars$qsec)

# Read dataset
used_cars<- read.csv('used_car_data.csv',stringsAsFactors = F)
head(used_cars)
# Import dataset into ggplot2
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price))
# Create scatter plot
plt +geom_point()
# Calculate correlation coefficient
cor(used_cars$Miles_Driven,used_cars$Selling_Price)

# Convert data frame into numeric matrix
used_matrix <- as.matrix(used_cars[,c('Selling_Price','Present_Price','Miles_Driven')])
cor(used_matrix)

?lm()
# Create a linear model
summary(lm(hp ~ qsec,mtcars))
# Create linear model
model <- lm(hp ~ qsec,mtcars)
# Determine y-axis values from linear model
yvals <- model$coefficients['qsec']*mtcars$qsec + model$coefficients['(Intercept)']
# Import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=qsec,y=hp))
# Plot scatter and linear models
plt +geom_point() + geom_line(aes(y=yvals), color='red')

# Generate multiple linear regression model
lm(hp ~ mpg + disp + drat + wt + qsec, data=mtcars)
# Generate summary statistics
summary(lm(hp ~ mpg + disp + drat + wt + qsec, data=mtcars))

?chisq.test()
# Generate contingency table
table(mpg$class,mpg$year)
# Generate contingency table
tbl <- table(mpg$class,mpg$year)
# Compare categorical distributions
chisq.test(tbl)
