#Importing
library(vroom)
library(tidyverse)
bikes = vroom("BikeShare2/train.csv")
bikes

#datetime is a unique identifier, figure out how to parse because time
#could be a good predictor, increments by 1 hour
ggplot(bikes[1:100,], aes(x = datetime, y = count)) + geom_point()

#season
bikes = bikes |> mutate(season = as.factor(season),
                        holiday = as.factor(holiday),
                        workingday = as.factor(workingday),
                        weather = as.factor(weather))

#holiday, there are very few holidays

#workingday, might be redundancy issues with holidays
bikes |> group_by(workingday, holiday) |> count()

#weather, one of the categories has only 1 value
bikes |> group_by(weather) |> count()

#temp, numeric predictor

#atemp, huge colinearity problem
cor(bikes$temp, bikes$atemp)
ggplot(bikes, aes(x = temp, y = atemp)) + geom_point()

#humidity, didn't see a correlation problem

#windspeed, some speeds are rare
bikes |> group_by(windspeed) |> count()

#count, heavily right-skewed
ggplot(bikes, aes(x = count)) + geom_boxplot()
ggplot(bikes, aes(x = count)) + geom_histogram()

#correlation, temp and atemp, maybe windspeed and humidity
bikes |> select(6:9, 12) |> cor()


#Plots
ggplot(bikes, aes(x = count, y = season)) + geom_boxplot()

ggplot(bikes, aes(x = count, y = holiday)) + geom_boxplot()
ggplot(bikes, aes(x = count, fill = holiday)) + geom_density(alpha =  0.5)

ggplot(bikes, aes(x = count, y = workingday)) + geom_boxplot()
ggplot(bikes, aes(x = count, fill = workingday)) + geom_density(alpha =  0.5)

ggplot(bikes, aes(x = count, y = weather)) + geom_boxplot()
ggplot(bikes, aes(x = count, fill = weather)) + geom_density(alpha = 0.5)

ggplot(bikes, aes(x = temp, y = count)) + geom_point() + geom_smooth(method = "lm")
ggplot(bikes, aes(x = humidity, y = count)) + geom_point() + geom_smooth(method = "lm")
ggplot(bikes, aes(x = windspeed, y = count)) + geom_point() + geom_smooth(method = "lm")

#Do t-tests
t.test(count ~ holiday, bikes) # No difference
t.test(count ~ workingday, bikes) # No difference

#Model
bikes2 = bikes |> select(-c(datetime, casual, registered))
model = lm(count ~ ., bikes2)
summary(model)



#plot 1
plot1 = ggplot(bikes, aes(x = weather)) + 
  geom_bar() +
  labs(subtitle = "(The 4th weather category is severely underrepresented)")
plot1

#plot 2
plot2 = ggplot(bikes[1:72,], aes(x = datetime, y = count)) + 
  geom_point() + 
  labs(subtitle = "(Need to parse datetime since time has a significant effect)") +
  ylab("total rentals")
plot2

plot3 = ggplot(bikes, aes(x = season, y = count)) +
  geom_boxplot() +
  labs(subtitle = "(Season seems to be a good predictor)") +
  ylab("total rentals")
plot3

plot4 = ggplot(bikes, aes(x = temp, y = atemp)) + 
  geom_point() +
  labs(subtitle = "(Clear collinearity issue along with several strange points)")
plot4

library(patchwork)
(plot1 + plot2) / (plot3 + plot4)










#Fitting a regression model
library(tidymodels)

model = linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(formula = count ~ season + workingday + weather + temp + humidity + windspeed, data = bikes)

model

testData = vroom("BikeShare2/test.csv") |> 
  mutate(season = as.factor(season),
         workingday = as.factor(workingday),
         weather = as.factor(weather))


bike_predictions = predict(model, new_data = testData)

bike_predictions

#DO LOG PREDICTIONS

model2 = linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(formula = log(count) ~ season + workingday + weather + temp + humidity + windspeed, data = bikes)

bike_predictions2 = predict(model2, new_data = testData) |> exp()

bike_predictions2



#Formatting and submitting
kaggle_submission = bike_predictions2 |> 
bind_cols(testData) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")


