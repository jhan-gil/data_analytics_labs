rm(list = ls()); gc()

# If needed...
#install.packages("readr", "nortest")

# Load libraries
library(readr)
library(nortest)

# Load data
df <- readr::read_csv("./epi_results_2024_pop_gdp.csv")

# Selected variables WWC.old and WWC.new
str(df)

# 1. Variable summaries
summary(df$WWC.old)
summary(df$WRS.old)


# 2. Boxplots
boxplot(df$WWC.old, df$WRS.old, names = c("WWC Old", "WWC New"))


# 3. Histograms with overlayed theoretical probability distributions
hist(df$WWC.old, probability = TRUE)
lines(density(df$WWC.old,bw="SJ"))

hist(df$WRS.old, probability = TRUE)
lines(density(df$WRS.old,bw="SJ"))


# 4. ECDFs
plot(ecdf(df$WWC.old), do.points=FALSE, verticals=TRUE)
plot(ecdf(df$WRS.old), do.points=FALSE, verticals=TRUE) 


# 5. QQ plot against the normal distribution
qqnorm(df$WWC.old); qqline(df$WWC.old)
qqnorm(df$WRS.old); qqline(df$WRS.old)


# 5. QQ plot against each other
qqplot(df$WWC.old, df$WRS.old) 


# 6. Normality statistical tests for each variable
shapiro.test(df$WWC.old)
shapiro.test(df$WRS.old)

ad.test(df$WWC.old)
ad.test(df$WRS.old)


# 7. A statistical test for whether the variables having identical distribution
ks.test(df$WWC.old, df$WRS.old)

