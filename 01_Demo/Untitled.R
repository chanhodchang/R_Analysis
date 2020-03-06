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

