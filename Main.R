# Author: Eric Wilson
# Date Created: 6 December 2021
# Filename: Main.R

library(tidyverse)
library(readxl)
library(MASS)
library(car)
library(leaps)

ev <- read_xlsx('Electric Vehicle Data/Cheapestelectriccars-EVDatabase.xlsx', 
                sheet = 'Cheapestelectriccars- UTF8')

# Convert Character Vectors into useable numeric vectors
ev$BatterySize <- as.numeric(gsub(".* ([0-9]{2,3}[.]*[0-9]*) kWh", "\\1", ev$Subtitle))
libra_strip <- gsub(".([0-9]{2,3},[0-9]{3})", "\\1", ev$PriceinUK) # strip the libra symbol
ev$PriceinUK <- as.numeric(gsub(",", "", libra_strip))
ev$Acceleration <- as.numeric(gsub(" sec", "", ev$Acceleration))
ev$TopSpeed <- as.numeric(gsub(" km/h", "", ev$TopSpeed))
ev$Range <- as.numeric(gsub(" km", "", ev$Range))
ev$Efficiency <- as.numeric(gsub(" Wh/km", "", ev$Efficiency))
ev$FastChargeSpeed <- as.numeric(gsub(" km/h", "", ev$FastChargeSpeed))
ev$Drive <- as.factor(ev$Drive)
ev$PriceinGermany <- as.numeric(ev$PriceinGermany)

# Limit ev to complete records, German Prices
ev <- ev[complete.cases(cbind(ev$FastChargeSpeed, ev$PriceinGermany)),]

# Create graph of pairs
pairs(~ PriceinGermany + PriceinUK + Acceleration + TopSpeed + Range + Efficiency +
          FastChargeSpeed + BatterySize, data = ev)

# Create first model
model1 <- lm(PriceinGermany ~ Acceleration + TopSpeed + Range + Efficiency + FastChargeSpeed +
                 BatterySize, data = ev, na.action = na.omit)

# Create second model, with Drive as categorical data
model2 <- lm(PriceinGermany ~ Acceleration + TopSpeed + Range + Efficiency + FastChargeSpeed +
                 BatterySize + Drive, data = ev, na.action = na.omit)

# Transform numeric predictors
summary(powerTransform(cbind(ev$Acceleration, ev$TopSpeed, ev$Range, ev$Efficiency,
                             ev$FastChargeSpeed, ev$BatterySize)))

# Create model with transformed Data
model3 <- lm(PriceinGermany ~ sqrt(Acceleration) + (1/TopSpeed) + log(Range) +
                 log(Efficiency) + log(FastChargeSpeed) + log(BatterySize), data = ev)

# Create model with transformed Data, without FastChargeSpeed
model4 <- lm(PriceinGermany ~ sqrt(Acceleration) + (1/TopSpeed) + log(Range) +
                 log(Efficiency) + log(BatterySize), data = ev)

# Note: Lightyear one seems to be a pretty bad strong outlier

# Perform Best Subset Selection on Transformed Data
model3_subsets <- regsubsets(x = PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed)
                             + log(Range) +log(Efficiency) + log(FastChargeSpeed)
                             + log(BatterySize), data = ev)

# Tranform both predictors and response
summary(powerTransform(cbind(ev$PriceinGermany, ev$Acceleration, ev$TopSpeed, ev$Range,
                             ev$Efficiency, ev$FastChargeSpeed, ev$BatterySize)))

# Create model with transformed predictors and response
model5 <- lm(1/PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed) + log(Range) + 
                 log(Efficiency) + log(FastChargeSpeed) + log(BatterySize), data = ev)

# Best Subset on Fully Transformed Data
model5_subsets <- regsubsets(x = 1/PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed)
                             + log(Range) +log(Efficiency) + log(FastChargeSpeed)
                             + log(BatterySize), data = ev)

# Create subset model with transformed response
model6 <- lm(1/PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed) + log(Range) +
                 log(Efficiency) + log(BatterySize), data = ev)

# Include NumberOfSeats in Analysis
summary(powerTransform(cbind(PriceinGermany, Acceleration, TopSpeed, Range, Efficiency,
                             FastChargeSpeed, NumberofSeats, BatterySize) ~ 1, 
                       data = ev))

model7 <- lm(1/PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed) + log(Range) + 
                 log(Efficiency) + log(FastChargeSpeed) + log(NumberofSeats) +
                 log(BatterySize), data = ev)

model7_subsets <- regsubsets(x = 1/PriceinGermany ~ sqrt(Acceleration) + I(1/TopSpeed) + log(Range) + 
                                 log(Efficiency) + log(FastChargeSpeed) + log(NumberofSeats) +
                                 log(BatterySize), data = ev)
(model7_subsets.summary <- summary(model7_subsets))
(best_number_variables <- list("AIC" = which.min(model7_subsets.summary$cp), "BIC" = which.min(model7_subsets.summary$bic),
                              "R2adj" = which.max(model7_subsets.summary$adjr2)))
# Acceleration not recommended for use

# Simplest Model based on Best Subset
model8 <- lm(1/PriceinGermany ~ I(1/TopSpeed) + log(Range) + 
                 log(Efficiency) + log(BatterySize), data = ev)
summary(model8)