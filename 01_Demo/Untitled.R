demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
library(jsonlite)
demo_table2 <- fromJSON(txt = 'demo.json')
x <- c(3, 3, 2, 2, 5, 5, 8, 8, 9)
x[3]
demo_table[3,'Year']
demo_table[3,3]
demo_table$'Vehicle_Class'
demo_table$'Vehicle_Class'[2]
filter_table <- demo_table2[demo_table2$price > 10000,]
demo_table2$price > 10000

?subset()
# Filter by price and drivetrain
filter_table2 <- subset(demo_table2, price > 10000 & drive == '4wd' & 'clean' %in% title_status)

?sample()
sample(c('cow', 'deer', 'pig', 'chicken', 'duck', 'sheep', 'dog'), 4)
demo_table[sample(1:nrow(demo_table), 3),]
library(tidyverse)

?mutate()
# add columns to original data frame
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE)
# Create a summary table by condition
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer))
# Create a summary table by multiple conditions
summarize_demo2 <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n())

?gather()
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)
long_table <- gather(demo_table3,key='Metric',value='Score',buying_price:popularity)
long_table <- demo_table3 %>% gather(key='Metric',value='Score',buying_price:popularity)

?spread()
wide_table <- long_table %>% spread(key='Metric', value='Score')
all.equal(demo_table3,wide_table)

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
