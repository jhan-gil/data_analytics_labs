library("ggplot2")
library("readr")



## read dataset
df <- read_csv("NY-House-Dataset.csv")


ggplot(df, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(df, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()



## Remove ouliers
q1 <- quantile(df$PRICE, 0.25)
q3 <- quantile(df$PRICE, 0.75)
iqr_val <- IQR(df$PRICE)

lower_bound <- q1 - 1.5 * iqr_val
upper_bound <- q3 + 1.5 * iqr_val

df2 <- df[df$PRICE<upper_bound & df$PRICE>lower_bound,]

df2['LOGPROPERTYSQFT'] <- log10(df2$PROPERTYSQFT)
df2['LOGPRICE'] <- log10(df2$PRICE)




## Model 1
print('Model 1 -------------------------------------')

# Fit model
lm1 <- lm(LOGPRICE~LOGPROPERTYSQFT + BEDS + BATH, data = df2)
summary(lm1)

ggplot(df2, aes(x = LOGPROPERTYSQFT, y = LOGPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lm1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)




## Model 2
print('Model 2 -----------------------------')

# Fit model
lm2 <- lm(LOGPRICE~LOGPROPERTYSQFT + BATH, data = df2)
summary(lm2)

ggplot(df2, aes(x = BATH, y = LOGPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lm2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)



## Model 3
print('Model 3 -----------------------')

# Fit model
lm3 <- lm(PRICE~LOGPROPERTYSQFT + BATH, data = df2)
summary(lm3)


## better scatter plot of 2 variables with best fit line
ggplot(df2, aes(x = BATH, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(lm3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


### THE END ###

