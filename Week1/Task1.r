# Question 1 a
# z-score normalization function
zscore <- function(x) {
  return((x - mean(x)) / sd(x))
}
# Load the dataset
data <- read.table("volley.txt", sep = "\t", header = FALSE)
# Rename the columns
colnames(data) <- c("X1", "X2", "X3", "X4")
# Apply z-score normalization to the column 1
data$X1 <- zscore(data$X1)
head(data)

# Question 1 b
# Min-Max Normalization function
minmax <- function(x, min_val, max_val) {
  for (i in 1:length(x)) {
    if (x[i] < min_val) {
      x[i] <- min_val
    }
    if (x[i] > max_val) {
      x[i] <- max_val
    }
  }
  return(x)
}
# Apply min-max normalization to the column 1
data$X4 <- minmax(data$X4, 10, 25)
head(data)

# Question 1 c
# Linear Scaling function
linear_scaling <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# Apply linear scaling to the column 1
data$X3 <- linear_scaling(data$X3)
head(data)


# Question 1 d
# Bucketization function
bucketize <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] > 175) {
      x[i] <- "Tall"
    } else if (x[i] > 160) {
      x[i] <- "Moderate"
    } else {
      x[i] <- "Short"
    }
  }
  x
}
# Apply bucketization to the column 2
data$X2 <- bucketize(data$X2)
head(data)
