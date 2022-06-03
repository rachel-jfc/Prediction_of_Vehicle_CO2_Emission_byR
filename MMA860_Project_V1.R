# installing the required libraries 
library(readxl)
library(tidyverse)
library(caret)
library(car)
library(estimatr)
library
# specifying the path for file
path <- "/Users/rachelchen/Desktop/MMA2023W/MMA860/Project/Data"
# set the working directory 
setwd(path)
# Create a function to read data
car_data <- function(path, year) {
  df <- read.csv(path,header = FALSE)
  df <- df[df$V1 == year,]
  df <- df[c(1:15)]
  return(df)
}
# Read data from 2018 to 2022
file22 <- car_data("MY2022 Fuel Consumption Ratings.csv", 2022)
file21 <- car_data("MY2021 Fuel Consumption Ratings.csv", 2021)
file20 <- car_data("MY2020 Fuel Consumption Ratings.csv", 2020)
file19 <- car_data("MY2019 Fuel Consumption Ratings.csv", 2019)
file18 <- car_data("MY2018 Fuel Consumption Ratings.csv", 2018)
# Combine datasets 
mydata <- rbind(file22, file21, file20, file19, file18)
view(mydata)

### Part 1: Data Cleaning
# Rename columns
names(mydata) <- c('year', 'make', 'model', 'class', 'engine_size', 'cylinders',
               'transmission', 'fuel_type', 'fuel_city', 'fuel_hwy', 'fuel_combLKM', 
               'fuel_combMPG', 'CO2', 'CO2rating', 'smog_rating')

# Remove columns not fit in the model
df <- select(mydata, -c(11:12, 14:15))
# Extract info from car model name
df$model_type <- ifelse(grepl("4WD",df$model), "4WD", 
                        ifelse(grepl("4X4",df$model), "4WD",
                               ifelse(grepl("AWD", df$model), "AWD",
                                      ifelse(grepl("FFV", df$model), "FFV",
                                             ifelse(grepl("SWB", df$model), "SWB",
                                                    ifelse(grepl("LWB", df$model), "LWB",
                                                           ifelse(grepl("EWB", df$model), "EWB", 
                                                                  "Other")))))))
view(df)
head(df)
summary (df)

# Remove column not fit in the model
df_model <- select(df, -model)  
# Check for duplicate rows
sum(duplicated(df_model))
df_model[duplicated(df_model),]
# Remove duplicate rows
df_model <- unique(df_model)
head(df_model)
summary(unique(df_model)) # 470 rows were removed
# Check for null values
is.null(df_model) # no null

summary(df_model)

df_model$year <- as.numeric(df_model$year)
df_model$engine_size <- as.numeric(df_model$engine_size)
df_model$cylinders <- as.numeric(df_model$cylinders)
df_model$fuel_city <- as.numeric(df_model$fuel_city)
df_model$fuel_hwy <- as.numeric(df_model$fuel_hwy)
df_model$CO2 <- as.numeric(df_model$CO2)

head(df_model)
write.csv(df_model, "df_model.csv", row.names = FALSE)

### Part 2: EDA
summary(df_model)
# Explore relationship between Engine Size and CO2
ggplot(df_model, aes(x=engine_size, y=CO2)) + 
  geom_point() + ggtitle("Engine Size vs. CO2 Emission")
# Explore relationship between Cylinders and CO2
ggplot(df_model, aes(x=cylinders, y=CO2)) + 
  geom_point() + ggtitle("Cylinders vs. CO2 Emission")
# Histogram of CO2 Emission
hist <- hist(df_model$CO2)
# Average CO2 Emission every year
ggplot(df_model, aes(x = factor(year), y = CO2)) + 
  geom_bar(stat = "summary", fun = "mean")
sapply(df_model, n_distinct)
unique(df_model$make)
unique(df_model$class)
unique(df_model$model_type)

### Part 3: Features Engineering 
summary(df_model)

df_model$year <- (2022-df_model$year)
df_model$fuel_d <- (df_model$fuel_city - df_model$fuel_hwy)
df_model = select(df_model, -c(fuel_city,fuel_hwy))
 
new_data <- dummyVars("~ .", df_model)
dataframe_dummyd <- data.frame(predict(new_data, df_model))
head(dataframe_dummyd)
view(dataframe_dummyd)

### Part 4: Fitting Model
reg <- lm(CO2~., df_model)
summary(reg)
linearHypothesis(reg, c("makeChrysler=0","makeDodge=0","makeGMC=0","makeInfiniti=0",
                        "makeJeep=0","makeLexus=0","makeMINI=0","makeRam=0","makeToyota=0",
                        "makeVolvo=0","classFull-size=0","classMid-size=0",
                        "classMinicompact=0","transmissionAS9=0",
                        "transmissionAV1=0","transmissionAV7=0","transmissionAV8=0",
                        "transmissionM5=0","fuel_typeX=0"))
# Remove makeDodge, GMC
linearHypothesis(reg, c("makeChrysler=0","makeInfiniti=0",
                        "makeJeep=0","makeLexus=0","makeMINI=0","makeRam=0","makeToyota=0",
                        "makeVolvo=0","classFull-size=0","classMid-size=0",
                        "classMinicompact=0","transmissionAS9=0",
                        "transmissionAV1=0","transmissionAV7=0","transmissionAV8=0",
                        "transmissionM5=0","fuel_typeX=0"))

reg2 <- lm(CO2~. -makeAcura-makeChrysler-makeInfiniti-makeJeep
           -makeLexus-makeMINI-makeRam-makeToyota-makeVolvo-classCompact-classFull.size
           -classMid.size -classMinicompact-transmissionA10-transmissionAS9-transmissionAV1
           -transmissionAV7-transmissionAV8-transmissionM5-fuel_typeD-fuel_typeX-model_type4WD 
           , dataframe_dummyd)
summary(reg2)


### Part 5: Testing & Tuning Model
par(mfrow=c(2,2))
plot(reg2)
plot(density(resid(reg2))) # the plot looks normal
df_model$predicted <- predict(reg2)
df_model$residuals <- residuals(reg2)
# Check for Heteroskedasticity
df_model %>% 
  gather(key = "iv", value = "x", -CO2, -predicted, -residuals) %>%  
  ggplot(aes(x = x, y = residuals)) +  
  geom_point(aes(y = residuals), shape = 1) +
  facet_grid(~ iv, scales = "free_x") + 
  theme_bw()
ncvTest(reg2) #rejected the null hypothesis of no heteroskedasticity
# Correct the inferences in the regression by using HCCME 
rob_reg <- lm_robust(CO2~. -makeAcura-makeChrysler-makeInfiniti-makeJeep
                     -makeLexus-makeMINI-makeRam-makeToyota-makeVolvo-classCompact-classFull.size
                     -classMid.size -classMinicompact-transmissionA10-transmissionAS9-transmissionAV1
                     -transmissionAV7-transmissionAV8-transmissionM5-fuel_typeD-fuel_typeX-model_type4WD,
                     dataframe_dummyd, se_type="HC2")
summary(rob_reg)

### Part 6: Making Predictions
# Train/Test Split
set.seed(123)
sample <- sample.int(n = nrow(dataframe_dummyd), size=floor(.60*nrow(dataframe_dummyd)), replace = F)
train <- dataframe_dummyd[sample,]
test <- dataframe_dummyd[-sample,]
pred <- predict(rob_reg,test)
summary(pred)
# test & train comparisons
data.frame( R2 = R2(pred, test$CO2),
            RMSE = RMSE(pred, test$CO2),
            MAE = MAE(pred, test$CO2))

write.csv(tidy(rob_reg), "coefs.csv")

### Part 7: Chow test: does two-seater respond more to change in Engine Size
chow <- dataframe_dummyd
chow$c1 <- chow$classTwo.seater * chow$engine_size
reg_chow <- lm(CO2~. -makeAcura-makeChrysler-makeInfiniti-makeJeep
               -makeLexus-makeMINI-makeRam-makeToyota-makeVolvo-classCompact-classFull.size
               -classMid.size -classMinicompact-transmissionA10-transmissionAS9-transmissionAV1
               -transmissionAV7-transmissionAV8-transmissionM5-fuel_typeD-fuel_typeX-model_type4WD,chow)
summary(reg_chow)
linearHypothesis(reg_chow, c("classTwo.seater = 0", "c1 = 0"))
  
