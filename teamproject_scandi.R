
#Import libraries
library(tidyverse)

#Rename column names to understand the terms easily
colnames(sustainablityexp)[5] <- 'energy_use'
colnames(sustainablityexp)[1] <- 'country_name'


# Remove the extra variables which aren't required for analysis
sustainablityexp <- sustainablityexp[-c(34,35),] 



## Create a ggplot to show the relationship
ggplot(data = sustainablityexp) +
  geom_point(mapping = aes(x=Time, y = energy_use, color = country_name))

ggplot(data = sustainablityexp,aes(x=Time, y = energy_use, group = country_name)) + 
  geom_line(aes(linetype = country_name)) +
  geom_point(aes(shape =country_name))


#Store the plot in p after formatting
p <- ggplot(data = sustainablityexp,aes(x=Time, y = energy_use, group = country_name, color = country_name))+
  geom_line() +
  geom_point(aes(shape = country_name)) + 
  scale_color_manual(values = c("#999999", "#E69F00", "#FF0000")) + 
  theme_minimal()

p

# Insert legend position
p <- p + theme(legend.position = "bottom")

p

# Insert title of the plot
p <- p + labs(title = "Plot of Energy use for Sweden, Norway and Finland during 2004-2014")

p

#Filter the region to Sweden
Sweden <- filter(sustainablityexp, country_name == "Sweden")
head(Sweden)

# Simple Linear regression
Sweden_slr <- lm(Sweden$energy_use ~ Sweden$Time)
summary(Sweden_slr)

#Plot the Linear regression model
plot(Sweden_slr)

## anova table
anova(Sweden_slr)

## confidence interval for slope for Sweden
confint(Sweden_slr, level = 0.95)

## confidence interval and prediction interval (year = 2014)
confint(Sustainability_Italy, level = 0.95)


#Defining target variable 'y' and response variable 'x'
y <- Sweden$energy_use
x <- Sweden$Time
Sweden_slr2 <- lm(y ~ x)
summary(Sweden_slr2)

# 
newx = data.frame(x = 2015)

predict.lm(Sweden_slr2, newx, interval = "confidence")
predict.lm(Sweden_slr2, newx, interval = "predict")

Sweden_res <- resid(Sweden_slr)
plot(Sweden_res ~ Sweden$Time, xlab = "Year", ylab = "Residual")
mean(Sweden_res)
qqnorm(Sweden_res, pch = 16, frame = FALSE)
qqline(Sweden_res, col = "red", lwd = 2)


