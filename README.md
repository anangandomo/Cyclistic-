## Introduction
This case study is from the Data Analysts Google Certification capstone course. This case study is based on a fictional bike-sharing company called Cyclistic. Cyclistic has two types of customers: Members and Casual riders. The Casual riders consist of single-ride and full-day passes, and Members consist of people with annual memberships. The Cyclistic believes that maximizing Members is best for the company.

#### Business Task
Analyze the differences between causal riders and riders with a membership.

## Prepare

#### Data
The data is credible because the data is  first-party data collected from the bikes, and the sample size is in the thousands. The data is public data from Motivate International Inc. The range of the data is from May 2020 to April 2021.

#### Loading the packages needed
```{r}
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(ggplot2)
```


#### Importing files to Rstudio
Import the 12 files into Rstudio, and name them as their corresponding months for easier use.

#### Combine the 12 months into 1 data frame
```{r}
all_months <- bind_rows(january,february,march,april,may,june,july,august,september,october,november,december)
```

#### Make new columns for date, ride_length, day_of_week,month
```{r}
all_months$date<- as.Date(all_months$started_at) # need to make a date column because the mutate function doesn't work on characters.
all_months$month<-format(as.Date(all_months$started_at),"%m")
all_months<- all_months %>%
  mutate(ride_length= ended_at- started_at) %>%
  mutate(day_of_week= weekdays(as.Date(all_months$date)))
```
## Process

#### Clean Data
Check for negative values for ride_length and duplicates
```{r}
all_months <- all_months%>%
  filter(ride_length > 0) %>%
  unique()
```

#### Make sample size of the data set because there is a lot of data
```{r}
sample<- sample_n(all_months,65398,replace = FALSE)

```
99% confidence level, .5% margin of error

## Analysis 
First I found out the total number of rides by each type of rider: Member(38417), Casual(26981)
```{r}
sample%>% 
  group_by(member_casual,) %>% 
  summarise(rides=n()) %>%
  arrange(member_casual)
```

Comparing the average duration of the rides by rider type find out that the casual riders ride for around 1.4 times longer than members.
![image](https://github.com/anangandomo/Cyclistic-/assets/139901070/e68b1d57-4431-475d-bc05-7c080ab48a02)

```{r}
sample %>%
  group_by(member_casual) %>%
  summarise(total_ride=n(), average_duration=round(mean(ride_length)),0) %>%
  arrange(member_casual) %>%
  ggplot(aes(x=member_casual,y=average_duration,fill= member_casual))+ geom_col(position = "dodge")+ 
  geom_text(mapping = aes(label=average_duration),size=3.5,vjust=1)
```
Comparing the number of rides by weekday shows that rides occur on Saturday, and casual riders mostly ride on the weekends.
![image](https://github.com/anangandomo/Cyclistic-/assets/139901070/a027b179-2660-4642-b719-7789c60c71a3)
```{r}
sample$day_of_week<-ordered(sample$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
sample%>% 
  group_by(member_casual,weekday=day_of_week) %>% 
  summarise(rides=n()) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday,y=rides,fill=member_casual))+ geom_col(position = "dodge")
```

Comparing the number of rides per month by rider type shows that most rides occur during the summertime.
![image](https://github.com/anangandomo/Cyclistic-/assets/139901070/be7b5dae-b91a-41a2-9743-4a4d3e473653)

```{r}
sample%>% 
  group_by(month,member_casual) %>% 
  summarise(rides=n(),average_duration=mean(ride_length)) %>%
  arrange(month,member_casual) %>% 
  ggplot(aes(x=month,y=rides,fill=member_casual))+ geom_col(position = "dodge")
```

### Conclusion
From the analysis, casual users are more likely using bikes for leisure and pleasure. I say that because most of the casual rides occur on weekends when people are off work and have time to relax, and they also occur during the summer when the weather is perfect for bike rides. To further emphasize my point, the average duration of the casual riders is 1.4 times longer than the riders with memberships.

### My recommendations
* Placing ads around local parks and bike trails would be a great way to reach casual riders. 
* The campaign should be from June to September as that is when casual riders are at their peak.
* To appetize the casual riders make a discounted membership plan that is only valid on weekends, and during the summer have a discount on the membership as well.
* Making a family membership will be good for casual riders that ride with family members.




