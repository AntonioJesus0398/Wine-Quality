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

display = function(x, title)
{
  boxplot(x, horizontal = TRUE, col = "orange", xlab = title)
  hist(x, main = paste("Histogram of ", title), xlab = title)
}

analize = function(x, title)
{
  print(measures(x))
  display(x, title)
}

analize(wine_table$quality, "wine quality")
analize(wine_table$pH, "wine pH")
analize(wine_table$alcohol, "wine alcohol")

#Exercise 2

#generate a normal population of size 500
p <- rnorm(500, 0, 1)

#generate 4 samples without replace
s300 <- sample(p, size = 300)
s40 <- sample(p, size = 40)
s30 <- sample(p, size = 30)
s20 <- sample(p, size = 20)

#generate 4 samples with replace
sr300 <- sample(p, size = 300, replace = TRUE)
sr40 <- sample(p, size = 40, replace = TRUE)
sr30 <- sample(p, size = 30, replace = TRUE)
sr20 <- sample(p, size = 20, replace = TRUE)


analize(s300, "s300")
analize(sr300, "sr300")

analize(s40, "s40")
analize(sr40, "sr40")

analize(s30, "s30")
analize(sr30, "sr30")

analize(s20, "s20")
analize(sr20, "sr20")

analize(p, "population")

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

mean_confidence_interval(s300, 0.01)
variance_confidence_interval(s300, 0.01)

mean_confidence_interval(sr300, 0.01)
variance_confidence_interval(sr300, 0.01)

mean_confidence_interval(s40, 0.01)
variance_confidence_interval(s40, 0.01)

mean_confidence_interval(sr40, 0.01)
variance_confidence_interval(sr40, 0.01)


mean_confidence_interval(s30, 0.01)
variance_confidence_interval(s30, 0.01)

mean_confidence_interval(sr30, 0.01)
variance_confidence_interval(sr30, 0.01)

mean_confidence_interval(s20, 0.01)
variance_confidence_interval(s20, 0.01)

mean_confidence_interval(sr20, 0.01)
variance_confidence_interval(sr20, 0.01)


#Exercise #3

white_wine <- subset(wine_table, color == "white")
red_wine <- subset(wine_table, color == "red")

x <- white_wine$fixed.acidity
y <- red_wine$fixed.acidity

v <- var.test(x, y, conf.level = 0.995)
v

if (v$p.value < 0.1)
{
  t.test(x, y, alternative = "less", conf.level = 0.995, var.equal = TRUE)
}else
{
  t.test(x, y, alternative = "less", conf.level = 0.995, var.equal = FALSE)
}
