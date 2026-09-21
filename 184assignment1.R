section <- factor(section, levels = c("A", "B"))
students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

typeof(student_id)
class(section)
length(quiz1)

str(students)
dim(students)

str(score_matrix)
dim(score_matrix)

str(course_record)
length(course_record)

# a vector stores values of the same basic type,
#a list can hold different types of objects. 
#A matrix stores data in rows and columns and normally has one data type. 
#A factor is used for categorical values, and a data frame is a table where different columns can have different types.

#Part B

score_matrix["504", "quiz2"]
score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course

# [ ] is used to subset an object and usually keeps the object's structure.
# [[ ]] extracts the actual element from an object, while $ is a convenient
# way to access an element by its name.

#Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$execellent <- students$average >= 90

subset_students <- students[students$section == "A" & students$average >= 80,]

subset_students[, c("student_id", "section", "average")]

average_vector <- students$average
names(average_vector) <- students$student_id
average_vector

# These calculations are vectorized because R performs the calculations
# on the whole column at once instead of needing a loop for each student.

#problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

complete_cases <- complete.cases(measurements)
measurements_complete <- measurements[complete_cases, ]

measurements$sample_id[!complete_cases]

# x == NA does not work as a missing-value test because R treats
# comparisons involving NA as unknown. The is.na() function is used
# instead to specifically check for missing values.

#Partb

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)


levels(measurements$site)
levels(measurements$status)


measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7
              
selected <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]

selected[, c("sample_id", "site", "temp_c", "temp_f", "ph")]

mean(measurements$temp_c, na.rm = TRUE)

mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)

#PartC
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

A * B

A %*% B

dim(A * B)
dim(A %*% B)

# A * B multiplies each corresponding element of the two matrices.
# A %*% B does actual matrix multiplication, where rows from the first
# matrix are multiplied with columns from the second matrix and added together.

#Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#Part A
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# The function returns NA for a missing score, A for 90, B for 80 and 85,
# and C for 74 based on the default grading boundaries.

#partb
grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id

grades

# During the loop, i represents the position of the score currently being graded.

#part c-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  mean_score <- round(mean(x, na.rm = na.rm), digits)
  sd_score <- round(sd(x, na.rm = na.rm), digits)
  min_score <- round(min(x, na.rm = na.rm), digits)
  max_score <- round(max(x, na.rm = na.rm), digits)
  
  c(
    total_count = total_count,
    missing_count = missing_count,
    mean = mean_score,
    standard_deviation = sd_score,
    minimum = min_score,
    maximum = max_score
  )
}

summarize_scores(scores)

summarize_scores(scores, na.rm = TRUE, digits = 2)

summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#part 3c2
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# The graph should show the scores by their position in the vector.
# The missing score at position 3 will appear as a break in the plotted line.


