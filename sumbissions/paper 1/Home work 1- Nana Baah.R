#Question 1: Creating histogram for course evaluation of instructors with bin width of 0.1
ggplot(data = GHVevals, mapping = aes(x = courseevaluation)) +
  geom_histogram(binwidth = 0.1, color = "white", fill ="orange")


#Question 2:Creating a facet for female which is the variable name for gender in the data set
ggplot(data = GHVevals, mapping = aes(x = courseevaluation, col = factor(female))) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ female) +
  scale_color_manual(name = "gender", values = c("blue", "red"), labels = c("male", "female")) + 
  labs(x = "course evaluation", title = "Graphical relationship of gender and instrcutors assessment")
                       
#Question 3: Creating plot for age and average beauty
ggplot(data = GHVevals, mapping = aes(x = age, y = btystdave)) +
  geom_point()

#Creating labels for the graph
ggplot(data = GHVevals, mapping = aes(x = age, y = btystdave)) +
  geom_point() +
  labs(x = "instructors age", y = "average beauty rating", title = "Plot showing instructors age and average beauty rating")

#Question 4" Plotting the relationship between average beauty on course evaluation
ggplot(data = GHVevals, mapping = aes(x = btystdave, y = courseevaluation)) +
  geom_point() +
  labs(x = "average beauty", y = "course evaluation", title = "Average beauty assessment influence on course evaluation")
#Increasing the transparency of data with an alpha of 0.2
ggplot(data = GHVevals, mapping = aes(x = btystdave, y = courseevaluation)) +
  geom_point(alpha = 0.2) +
  labs(x = "average beauty", y = "course evaluation", title = "Average beauty assessment influence on course evaluation")

#Question 5: Creating plot based on gender and beauty on course evaluation
ggplot(data = GHVevals, mapping = aes(x= btystdave, y = courseevaluation, col = factor(female))) +
  geom_point() +
  scale_color_manual(name = "gender", values = c("green", "red"), labels = c("female", "male")) +
  labs(x = "beauty", y = "course evlaution", title = "Relationship grap of Instructors gender and beauty on course evalaution")

#Question 6: Creating a stacked bar for racial distribution among tenured and non-tenured track professors
ggplot(data = GHVevals, mapping = aes(x = factor(tenured), fill = factor(minority))) +
  geom_bar() +
  scale_x_discrete(name = "Instructors", labels = c("0" = "non-tenured", "1" = "tenured")) +
  scale_fill_manual(name = "Racial status", labels = c("majority", "minority"), values = c("red", "green")) +
    labs(x = "instructors", title = "Racial distribution among tenured and non-tenured track instructors")

#Question 7: Plotting the gender distribution of instructors
ggplot(data = GHVevals, mapping = aes(x = factor(tenured), fill = factor(female))) +
  geom_bar() +
  scale_x_discrete(name = "Instructors", labels = c("0" = "non-tenured", "1" = "tenured")) +
  scale_fill_manual(name = "Gender", labels = c("Male", "female"), values = c("orange", "green")) +
  labs(x = "Instructors", title = "Gender distribution of tenured and non-tenured instructors")
  