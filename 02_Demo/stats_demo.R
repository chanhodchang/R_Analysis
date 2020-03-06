# Visualize distribution using density plot
ggplot(mtcars,aes(x=wt))+geom_density()
?shapiro.test()
shapiro.test(mtcars$wt)
