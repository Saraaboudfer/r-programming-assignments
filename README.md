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

##Assignment 4 Visualizing and Interpreting Hospital Patient Data


##Assignment 4 Visualizing and Interpreting Hospital Patient Data
#Sara Aboudfer
#LIS4370

#1 Data Prep and Cleaning
Frequency     <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
BloodPressure <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176)
FirstAssess   <- c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1)    # bad=1, good=0
SecondAssess  <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)    # low=0, high=1
FinalDecision <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1)    # low=0, high=1

df_hosp <- data.frame(
  Frequency, BloodPressure, FirstAssess,
  SecondAssess, FinalDecision, stringsAsFactors = FALSE
)
# Inspect and handle NA:
summary(df_hosp)
df_hosp <- na.omit(df_hosp)

#2 Generate Basic Visualizations
boxplot(
  BloodPressure ~ FirstAssess,
  data = df_hosp,
  names = c("Good","Bad"),
  ylab = "Blood Pressure",
  main = "BP by First MD Assessment"
)

boxplot(
  BloodPressure ~ SecondAssess,
  data = df_hosp,
  names = c("Low","High"),
  ylab = "Blood Pressure",
  main = "BP by Second MD Assessment"
)

boxplot(
  BloodPressure ~ FinalDecision,
  data = df_hosp,
  names = c("Low","High"),
  ylab = "Blood Pressure",
  main = "BP by Final Decision"
)

##Histogram
hist(
  df_hosp$Frequency,
  breaks = seq(0, 1, by = 0.1),
  xlab = "Visit Frequency",
  main = "Histogram of Visit Frequency"
)

hist(
  df_hosp$BloodPressure,
  breaks = 8,
  xlab = "Blood Pressure",
  main = "Histogram of Blood Pressure"
)
<img width="347" height="248" alt="boxplot first assess" src="https://github.com/user-attachments/assets/9e37bbe0-99ba-4dca-a2d5-2a01a2467f61" />
<img width="347" height="248" alt="boxplot second assess" src="https://github.com/user-attachments/assets/062023f4-1d57-4803-8a79-f17e140f9e78" />
<img width="347" height="248" alt="boxplot final decision" src="https://github.com/user-attachments/assets/69055c21-d641-4429-ba62-2e31d97b5d1a" />
<img width="347" height="248" alt="histo frequency" src="https://github.com/user-attachments/assets/c69c3dd2-ae6e-4ca1-b256-9d70550ebdae" />
<img width="347" height="248" alt="histo blood pressure" src="https://github.com/user-attachments/assets/e9e9fc37-0b91-42dc-af2d-1e6d46e088d2" />
Link to blog post: https://sara-r-programming.blogspot.com/2026/09/assignment-4-visualizing-and.html
 
#Assignment 5 Matrix Algebra in R

##Assignment 5 Matrix Algebra in R
#LIS 4370
#Sara Aboudfer

#1. Create the matrices
A <- matrix(1:100,  nrow = 10)
B <- matrix(1:1000, nrow = 10)

#2. Inspect Dimensions
dim(A)  # should be 10 × 10
dim(B)  # 10 × 100 — not square

#3. Compute inverse and determinant
# For A
invA <- tryCatch(solve(A), error=function(e) e)
detA <- tryCatch(det(A), error = function(e) e)

# For B, use tryCatch to capture errors
invB <- tryCatch(solve(B), error = function(e) e)
detB <- tryCatch(det(B),   error = function(e) e)

#Results
print(invA)
print(detA)
print(invB)
print(detB)

Link to Blog Assignment 5: https://sara-r-programming.blogspot.com/2026/09/assignment-5-matrix-algebra-in-r.html

[Uploading assignm##Assignment 6
#LIS 4370
#Sara Aboudfer

#1. Matrix Addition and Subtraction
A <- matrix(c(2, 0, 1, 3), ncol = 2)
B <- matrix(c(5, 2, 4, -1), ncol = 2)
A
B
A + B
A - B

#2. Diagonal Matrix
D <- diag(c(4, 1, 2, 3))
D
#3. Construct 5x5 matrix
my_matrix <- matrix(0, nrow = 5, ncol = 5)
diag(my_matrix) <- 3
my_matrix[1, 2:5] <- 1
my_matrix[2:5, 1] <- 2
my_matrixent6.R…]()

Link for blog post: https://sara-r-programming.blogspot.com/2026/10/assignment-6-matrix-operations-and.html

