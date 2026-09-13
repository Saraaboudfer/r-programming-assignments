# r-programming-assignments
Sara Aboudfer
LIS4370
Repository for R Programming Assignments

Assignment 2 Blog post link: https://sara-r-programming.blogspot.com/2026/09/assignment-2-importing-data-and.html
Corrected myMean function:
##Assignment 2 Importing Data
> 
> ##Use vector
> assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)
> 
> ##Consider function
> myMean <- function(assignment2) {
+   return(sum(assignment2) / length(assignment2))
+ }
> 
> ##Find mean
> myMean(assignment2)
[1] 19.25


Assignment 3 Blog post link:https://sara-r-programming.blogspot.com/2026/09/assignment-3-analyzing-2016-poll-data.html
#Create vectors
Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC_poll   <- c(  4,      62,      51,    21,      2,        14,       15)
CBS_poll   <- c( 12,      75,      43,    19,      1,        21,       19)

#Create data frame
df_polls <- data.frame(Name, ABC_poll,CBS_poll)

#Inspect data
str(df_polls)
head(df_polls)

#Summary Stats
mean(df_polls$ABC_poll)
mean(df_polls$CBS_poll)

median(df_polls$ABC_poll)
median(df_polls$CBS_poll)

range(df_polls$ABC_poll)
range(df_polls$CBS_poll)

#Add column for difference between CBS and ABC
df_polls$Diff <- df_polls$CBS_poll - df_polls$ABC_poll

print(df_polls)

##ggplot2 bar chart
library(ggplot2)
library(tidyr)

df_long <- pivot_longer(df_polls,
                        cols = c("ABC_poll", "CBS_poll"),
                        names_to = "Poll",
                        values_to = "Percent")

ggplot(df_long, aes(x = Name, y = Percent, fill = Poll)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Candidate Poll Comparison: ABC vs CBS",
       x = "Candidate",
       y = "Poll Percentage") +
  theme_minimal()
ggsave("poll_chart.png", width = 8, height = 5)
