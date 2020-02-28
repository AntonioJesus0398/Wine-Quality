setwd("D:/Escuela/4to/Estadística/1 er projecto/Wine-Quality")
wine_table = read.csv("./dataset/wine.csv")

get_mode <- function(x)
{
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

measures = function(x)
{
  list(
    Mean = mean(x),
    Median = median(x),
    Mode = get_mode(x),
    Variance = var(x),
    Standard_Deviation = sd(x),
    Coefficient_of_variation = sd(x)/mean(x) * 100
  )
}

display = function(x)
{
  boxplot(x, horizontal = TRUE, col = "orange")
  hist(x)
}

analize = function(x)
{
  print(measures(x))
  display(x)
}

analize(wine_table$quality)
analize(wine_table$pH)
analize(wine_table$alcohol)

#Exercise 2

#generate a normal population of size 500
p <- rnorm(500, 0, 1)

#generate 4 samples without replace
s1 <- sample(p, size = 300)
s2 <- sample(p, size = 40)
s3 <- sample(p, size = 30)
s4 <- sample(p, size = 20)

#generate 4 samples with replace
sr1 <- sample(p, size = 300, replace = TRUE)
sr2 <- sample(p, size = 40, replace = TRUE)
sr3 <- sample(p, size = 30, replace = TRUE)
sr4 <- sample(p, size = 20, replace = TRUE)


analize(s1)
analize(s2)
analize(s3)
analize(s4)

analize(sr1)
analize(sr2)
analize(sr3)
analize(sr4)

analize(p)

library(glue)

mean_confidence_interval = function(s, alpha)
{
  # s: sample
  # cl: confidence level

  n <- length(x)
  m <- mean(s)
  sd <- sd(s)

  if (n > 30)
  {
    z <- qnorm(1 - alpha/2)
    left <- m - z * sd/sqrt(n)
    right <- m + z * sd/sqrt(n)
  }
  else
  {
    t <- qt(1 - alpha/2, n - 1)
    left <- m - t * sd/sqrt(n)
    right <- m + t * sd/sqrt(n)
  }

  glue("Mean confidence interval with confidence level {alpha}:\nLeft bound: {left}\nRight bound: {right}")
}

variance_confidence_interval = function(s, alpha)
{
  # s: sample
  # cl: confidence level

  n <- length(x)
  v <- var(s)

  x1 <- qchisq(1 - alpha/2, n - 1)
  x2 <- qchisq(alpha/2, n - 1)

  left <- (n - 1) * v / x1
  right <- (n - 1) * v / x2

  glue("Variance confidence interval with confidence level {alpha}:\nLeft bound: {left}\nRight bound: {right}")
}

mean_confidence_interval(s1, 0.01)
variance_confidence_interval(s1, 0.01)

mean_confidence_interval(s2, 0.01)
variance_confidence_interval(s2, 0.01)

mean_confidence_interval(s3, 0.01)
variance_confidence_interval(s3, 0.01)

mean_confidence_interval(s4, 0.01)
variance_confidence_interval(s4, 0.01)

mean_confidence_interval(sr1, 0.01)
variance_confidence_interval(sr1, 0.01)

mean_confidence_interval(sr2, 0.01)
variance_confidence_interval(sr2, 0.01)

mean_confidence_interval(sr3, 0.01)
variance_confidence_interval(sr3, 0.01)

mean_confidence_interval(sr4, 0.01)
variance_confidence_interval(sr4, 0.01)


#Exercise #3

white_wine <- subset(wine_table, color == "white")
red_wine <- subset(wine_table, color == "red")

t.test(red_wine$fixed.acidity, white_wine$fixed.acidity, alternative = "greater", conf.level = 0.01)
