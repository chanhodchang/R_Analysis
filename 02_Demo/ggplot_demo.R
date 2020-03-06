?ggplot()
head(mpg)
# Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=class))
# plot a bar plot
plt + geom_bar()

?geom_bar()
# Create summary table
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n())

# import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=manufacturer, y=Vehicle_Count))
# plot a bar plot
plt + geom_col()
# Plot bar plot with labels
plt + geom_col() + xlab('Manufacturing Company') + ylab('Number of Vehicles in Dataset') + 
  # rotate the x-axis label 45 degrees
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Create summary table
mpg_summary2 <- subset(mpg,manufacturer=='toyota') %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy))

# Import dataset into ggplot2
plt <- ggplot(mpg_summary2,aes(x=cyl,y=Mean_Hwy))
plt + geom_line()
# Add line plot with labels
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30))

# Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty))
# Add scatter plot with labels
plt + geom_point() + xlab('Engine Size (L)') + ylab('City Fuel-Efficiency (MPG)')

# Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class))
# Add scatter plot with labels
plt + geom_point() + labs(x='Engine Size (L)', y='City Fuel-Efficiency (MPG)', color="Vehicle Class")

# Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv))
# Add scatter plot with mulptiple aesthetics
plt + geom_point() + labs(x='Engine Size (L)', y='City Fuel-Efficiency (MPG)', color='Vehicle Class',shape='Type of Drive')

# Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv,size=cty))
plt+geom_point()+labs(x='Engine Size (L)',y='City Fuel-Efficiency (MPG)',color='Vehicle Class',shape='Type of Drive',size='City Fuel-Efficiency (MPG)')

# Import dataset into ggplot2
plt<-ggplot(mpg,aes(y=hwy))
# Add boxplot
plt+geom_boxplot()

# Import dataset into ggplot2
plt<-ggplot(mpg,aes(x=manufacturer, y=hwy))
# Add boplot and rotate x-axis labels 45 degrees
plt+geom_boxplot()+ theme(axis.text.x=element_text(angle=45,hjust=1))

# Import dataset into ggplot2
plt<-ggplot(mpg,aes(x=manufacturer, y=hwy,color=manufacturer))
plt+geom_boxplot(notch=FALSE)+theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(x='Manufacturer',y='Highway Fuel-Efficiency (MPG)',color='Manufacturer')

# Create summary table
mpg_summary3 <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy))
# Import dataset into ggplot2
plt<-ggplot(mpg_summary3,aes(x=class,y=factor(year),fill=Mean_Hwy))
mpg_summary3<-mpg %>%group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy))
# Create heatmap with labels
plt+geom_tile()+labs(x="Vehicle Class",y='Vehicle Year',fill='MEan highway (MPG)')

# Create summary table
mpg_summary4 <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy)) 
# Import dataset into ggplot2
plt <- ggplot(mpg_summary4, aes(x=model,y=factor(year),fill=Mean_Hwy)) 
# Add heatmap with labels
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + 
  #rotate x-axis labels 90 degrees
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) 

# Import dataset into ggplot2
plt<-ggplot(mpg,aes(x=manufacturer,y=hwy))
# Add boxplot 
plt+geom_boxplot()+
  # Rotate x-axis labels 45 degrees
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  #overlay scatter plot on top
  geom_point() 

# Create summary table
mpg_summary5<-mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ))
# Import dataset into ggplot2
plt<-ggplot(mpg_summary5,aes(x=class,y=Mean_Engine))
# Add scatter plot
plt+geom_point(size=4)+labs(x='Vehicle Class',y='Mean Engine Size')

mpg_summary6<-mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ))
# Import dataset into ggplot2
plt<-ggplot(mpg_summary6,aes(x=class,y=Mean_Engine))
# Add scatter plot
plt+geom_point(size=4)+labs(x='Vehicle Class',y='Mean Engine Size') + 
  # Overlay with error bars
  geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine))

# Convert to long format
mpg_long<-mpg %>% gather(key='MPG_Type',value='Rating',c(cty,hwy))
head(mpg_long)

# Import dataset into ggplot2
plt<-ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type))
# Add boxplot with labels rotated 45 degrees
plt+geom_boxplot()+theme(axis.text.x=element_text(angle=45,hjust=1))

?facet_wrap
# Import dataset into ggplot2
plt<-ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type))
# Create multiple boxplots, one for each MPG type
plt+geom_boxplot()+facet_wrap(vars(MPG_Type)) +
  # Rotate x-axis labels
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='none')+xlab('Manufacturer')
