#Question 1: To find the mean and median of life expectancy, I used the summary command and the human.development data
summary(human.development$life_expectancy)
mean(human.development$life_expectancy)
median(human.development$life_expectancy)
#To find the mean and median of the gross national income, I used the summary command and the human.development data.
Summary(human.development)
mean(human.development$gni_per_capita)
median(human.development$gni_per_capita)

#Question 2: I arrange the life_expectancy data in descending order to know the number of countries above the mean and median years.
human.development <- human.development %>%
  arrange(desc(life_expectancy))

#Question 3: To find the highest and lowest average years of schooling, first I create a data with country and average years of schooling only.
Avgyearsofschooling <- human.development %>%
  select(country, mean_years_schooling)
#I rank the new average years of schooling data in descending order.
Descorder <- Avgyearsofschooling %>%
  arrange(desc(mean_years_schooling))
#To find the 4th highest country with average years of schooling, I used the the descending average years of school data and search for the country in the 4 th row which is the 4th highest.
Descorder[4, ]
#To find the 5th lowest country with average years of schooling, I select the descending average years of school data and request for the 183 row which matches the country with the 5 th lowest average years of schooling
Descorder[183, ]

#Question 4a: To find the standard deviation and variance of average years of school, I  used the summary command.
summary(human.development)
sd(human.development$mean_years_schooling)
var(human.development$mean_years_schooling)
mean(human.development$mean_years_schooling)

#Question 4b: To find the standard deviation and variance of expected years of schooling, I  used the summary command
summary(human.development)
sd(human.development$expected_years_schooling)
var(human.development$expected_years_schooling)
mean(human.development$expected_years_schooling)


#Part two
#Question 1: First I create a column in the human.development and add the geographical regions of each country and import as human.dev.georegions
#I create the mean for human development based on geographical regions using the group by and summary commands.
meanbygeoregions <- hum.dev.georegions %>%
  group_by(region) %>%  
  summarise(mean = mean(hdi_value, na.rm = TRUE))

meanLifeExpec <- hum.dev.georegions %>%
  group_by(region) %>%  
  summarise(mean = mean(life_expectancy, na.rm = TRUE))

#arranging the mean hDI in descending order
meanbygeoregionbyorder <- meanbygeoregions %>%
  arrange(desc(mean))
#To find the highest and lowest average human development score by regions, I used the ordered means by regions data and searched for row 1 (highest) and 23 (lowest) regions
meanbygeoregionbyorder[1, ]
meanbygeoregionbyorder[23, ]
#Exporting the mean hdi value by region descending order file
write.table(meanbygeoregionbyorder, file = "orderofmeanHDI.csv", row.names = F, sep=",")

#Question 2:Finding the standard deviation of life expectancy by region using the summary  and group command
sdforlifeEXbyregions <- hum.dev.georegions %>%
  group_by(region) %>%  
  summarise(sd = sd(life_expectancy, na.rm = TRUE))

#arranging the standard deviation of life expectancy of region in descending order
Lifeexpectancyofregionsinorder <- sdforlifeEXbyregions %>%
  arrange(desc(sd))
#To find the highest and lowest standard deviation of life expectancy score by regions, I used the ordered life expectancy by regions by regions data and searched for row 1 (highest) region
Lifeexpectancyofregionsinorder[1, ]

#Exporting the ordered  life expectancy by region file
write.table(Lifeexpectancyofregionsinorder, file = "orderedlifeExp.csv", row.names = F, sep=",")
#Calculating the mean life expectancy by regions, I used the group by and summary command. This is to compare the standard deviation and the mean life expectancy. 
meanLifeExpec <- hum.dev.georegions %>%
  group_by(region) %>%  
  summarise(mean = mean(life_expectancy, na.rm = TRUE))


#Question 3: To merge the women labor force data with the human development data by region, I used the inner_join command
LforceandHDImerge <- hum.dev.georegions %>%
  inner_join(laborforce, by = c("country" = "Country.Name"))
#Finding the average women labor force using the group and summary command
aveWoLforce <- LforceandHDImerge %>%
group_by(region) %>%  
  summarise(mean = mean(womenlaborforce, na.rm = TRUE))
#Exporting the average women labor force by region file
write.table(aveWoLforce , file = "WoLforcebyegions.csv", row.names = F, sep=",")