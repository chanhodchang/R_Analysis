# Multiple Linear Regression with MechaCar_mpg
mecha_mpg <- read.csv('MechaCar_mpg.csv')
head(mecha_mpg)
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_mpg)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_mpg))

# Statistical summary with Suspension_Coil.csv
suspension_coil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)
head(suspension_coil)
# Find the mean
mean(suspension_coil$PSI)
# Find the median
median(suspension_coil$PSI)
# Find the variance
var(suspension_coil$PSI)
# Find the standard deviation
sd(suspension_coil$PSI)
# Use T-Test
t.test(log10(suspension_coil$PSI), mu=mean(log10(suspension_coil$PSI)))
