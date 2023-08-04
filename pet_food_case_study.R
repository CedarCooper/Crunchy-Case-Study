# add tidyverse
install.packages("tidyverse")
library(dplyr)
# renamed column, then noticed some data is more than 1 month
monthly_pet_food_2018_2023 <- monthly_pet_food_2018_2023 %>% 
  rename("Month" = "Month(s)")
# removed column 3, which was "Period" and held no useful info
pet_food_2018_2023 <- select(pet_food_2018_2023, 1,2,4:5)
# remove rows that are not month specific, new df
monthly_pet_food_2018_2023 <- pet_food_2018_2023[-(67:77),]
View(monthly_pet_food_2018_2023)
# remove year from the Month column
monthly_pet_food_2018_2023$Month <- gsub("[0-9]+","",as.character(monthly_pet_food_2018_2023$Month))
# Added spendings column for 2 decimal places
monthly_pet_food_2018_2023$Spendings <- round(monthly_pet_food_2018_2023$Value, digit=2)
# now remove Value column
monthly_pet_food_2018_2023 <- select(monthly_pet_food_2018_2023, 1,2,3,5)
# now find the avg for each year and present that
# avg_2018
avg_2018 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2018, "Spendings"])), digits=2)
# avg_2019
avg_2019 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2019, "Spendings"])), digits=2)
# avg_2020
avg_2020 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2020, "Spendings"])), digits=2)
# avg_2021
avg_2021 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2021, "Spendings"])), digits=2)
# avg_2022
avg_2022 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2022, "Spendings"])), digits=2)
# avg_2023 - note not as many data points
avg_2023 <- round(mean(data.matrix(monthly_pet_food_2018_2023[monthly_pet_food_2018_2023$Year == 2023, "Spendings"])), digits=2)
# now time to plot
library(ggplot2)
# basic smooth line
ggplot(data=monthly_pet_food_2018_2023) +
  geom_smooth(mapping = aes(x=Year, y=Spendings), color="#6aa84f",  fill="#528638") +
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill="NA"),  plot.title = element_markdown(size=14), text = element_text(family = "mono")) +
  labs(title='US Pet Food Spending per Month', subtitle = '2018 - 2023') +
  scale_y_continuous(labels=function(x) paste0("$",x))
# add averages
ggplot(data=monthly_pet_food_2018_2023) + 
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black", fill="NA"), plot.title = element_markdown(size=14), text = element_text(family = "mono")) +
  geom_smooth(mapping = aes(x=Year, y=Spendings), color="#6aa84f", fill="#528638") +
  labs(title='US Pet Food Spending per Month', subtitle = '2018 - 2023') +
  scale_y_continuous(labels=function(x) paste0("$",x)) +
  annotate("rect",xmin = 2018-.05, xmax = 2018+.05, ymin= avg_2018-1, ymax = avg_2018+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2018, y=avg_2018+3, label=paste0("$",avg_2018), cex=3) +
  annotate("rect",xmin = 2019-.05, xmax = 2019+.05, min= avg_2019-1, ymax = avg_2019+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2019, y=avg_2019+3, label=paste0("$",avg_2019), cex=3) +
  annotate("rect",xmin = 2020-.05, xmax = 2020+.05, ymin= avg_2020-1, ymax = avg_2020+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2020, y=avg_2020+3, label=paste0("$",avg_2020), cex=3) +
  annotate("rect",xmin = 2021-.05, xmax = 2021+.05, ymin= avg_2021-1, ymax = avg_2021+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2021, y=avg_2021+3, label=paste0("$",avg_2021), cex=3) +
  annotate("rect",xmin = 2022-.05, xmax = 2022+.05, ymin= avg_2022-1, ymax = avg_2022+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2022, y=avg_2022+3, label=paste0("$",avg_2022), cex=3) +
  annotate("rect",xmin = 2023-.05, xmax = 2023+.05, ymin= avg_2023-1, ymax = avg_2023+1, alpha=0.2, color="#0d54ff", fill="blue") +
  annotate("text", x=2023, y=avg_2023+3, label=paste0("$",avg_2023,"0"), cex=3) 
# save dataset
write_csv(monthly_pet_food_2018_2023, file="Documents/Case Studies/pet_purchases_case_study/monthly_pet_food_2018-2023.csv")
