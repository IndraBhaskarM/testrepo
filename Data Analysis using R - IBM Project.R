library(rlang)
library(tidymodels)
library(tidyverse)
url <- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'

download.file(url, destfile = "noaa-weather-sample-data.tar.gz")


untar("noaa-weather-sample-data.tar.gz", tar = "internal")
NOAA_weather <- read_csv("noaa-weather-sample-data/jfk_weather_sample.csv", 
                         col_types= cols("DATE" = col_number(),
                                         "HOURLYDewPointTempF" = col_number()))

head(NOAA_weather)
glimpse(NOAA_weather)


subset_NOAA_weather <- select(NOAA_weather, c("HOURLYRelativeHumidity",
                                              "HOURLYDRYBULBTEMPF",
                                              "HOURLYPrecip",
                                              "HOURLYWindSpeed",
                                              "HOURLYStationPressure"))


head(subset_NOAA_weather, n = 10)

unique(subset_NOAA_weather$HOURLYPrecip)
subset_NOAA_weather$HOURLYPrecip[subset_NOAA_weather$HOURLYPrecip == "T"] <- 0
subset_NOAA_weather$HOURLYPrecip <- str_remove(subset_NOAA_weather$HOURLYPrecip, pattern = "s$")
NOAA_weather$HOURLYPrecip <- subset_NOAA_weather$HOURLYPrecip
unique(NOAA_weather$HOURLYPrecip)
glimpse(NOAA_weather)



NOAA_weather$HOURLYPrecip <- (as.numeric(NOAA_weather$HOURLYPrecip))
glimpse(NOAA_weather)



NOAA_weather2 <- NOAA_weather %>%
  rename(relative_humidity = HOURLYRelativeHumidity,
         dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
         precip = HOURLYPrecip, 
         wind_speed = HOURLYWindSpeed,
         station_pressure = HOURLYStationPressure)
glimpse(NOAA_weather2)
set.seed(1234)
NOAA_weather_split <- initial_split(NOAA_weather2, prop = 0.8)
train_data <- training(NOAA_weather_split)
test_data <- testing(NOAA_weather_split)
ggplot(train_data, aes(x = relative_humidity))+
  geom_histogram(color = "darkblue", fill = "lightblue")
ggplot(train_data, aes(x = dry_bulb_temp_f))+
  geom_histogram(color = "darkblue", fill = "lightblue")
ggplot(train_data, aes(x = precip))+
  geom_histogram(color = "darkblue", fill = "lightblue")
ggplot(train_data, aes(x = wind_speed))+
  geom_histogram(color = "darkblue", fill = "lightblue")
ggplot(train_data, aes(x = station_pressure))+
  geom_histogram(color = "darkblue", fill = "lightblue")
linear_model_humidity <- lm(precip ~ relative_humidity, data = train_data)
ggplot(train_data, aes(x = relative_humidity, y = precip)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")
linear_model_drybulbtempf <- lm(precip ~ dry_bulb_temp_f, data = train_data)

ggplot(train_data, aes(x = dry_bulb_temp_f, y = precip)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")
linear_model_windspeed <- lm(precip ~ wind_speed, data = train_data)
ggplot(train_data, aes(x = wind_speed, y = precip)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")
linear_model_stationpressure <- lm(precip ~ station_pressure, data = train_data)
ggplot(train_data, aes(x = station_pressure, y = precip)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")
summary(linear_model_humidity)
summary(linear_model_drybulbtempf)
summary(linear_model_windspeed)
summary(linear_model_stationpressure)
polynomial_relativehumidity <- lm(precip ~ poly(relative_humidity, 10, raw = TRUE), data = train_data)

ggplot(data = train_data, aes(relative_humidity, precip))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,10))
summary(polynomial_relativehumidity)
mlr_all <- lm(precip ~ relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure, data = train_data)

ggplot(train_data, aes(x = relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure, y = precip)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")





MLR <- linear_reg() %>%
  set_engine(engine = "lm")

train_fit <- MLR %>% 
  fit(precip ~ relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure, data = train_data)

MLR_results <- train_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$precip)
head(MLR_results)
polynomial <- linear_reg () %>%
  set_engine(engine = "lm")

train_fit2 <- polynomial %>% 
  fit(precip ~ poly(relative_humidity, 10, raw = TRUE), data = train_data)

poly_relative_humidity <- train_fit2 %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$precip)
head(poly_relative_humidity)
rsq_MLR <- rsq(MLR_results, truth = truth, estimate = .pred)

rsq_MLR
rsq_poly_humiditity <- rsq(poly_relative_humidity, truth = truth, estimate = .pred)

rsq_poly_humiditity
model_names <- c("Relative_humidity_poly", "MLR_all")
train_error <- c("0.05536", "0.04632")
test_error  <- c("0.06346145", "0.06314133")

comparison_df <- data.frame(model_names, train_error, test_error)
comparison_df




