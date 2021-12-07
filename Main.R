# Author: Eric Wilson
# Date Created: 6 December 2021
# Filename: Main.R

library(tidyverse)
library(readxl)

ev <- read_xlsx('Electric Vehicle Data/Cheapestelectriccars-EVDatabase.xlsx', 
                sheet = 'Cheapestelectriccars- UTF8')

# Convert Character Vectors into useable numeric vectors
ev$BatterySize <- as.numeric(gsub(".*([0-9]{2}[.]*[0-9]*) kWh", "\\1", ev$Subtitle))
libra_strip <- gsub(".([0-9]{2,3},[0-9]{3})", "\\1", ev$PriceinUK) # strip the libra symbol
ev$PriceinUK <- as.numeric(gsub(",", "", libra_strip))
ev$Acceleration <- as.numeric(gsub(" sec", "", ev$Acceleration))
ev$TopSpeed <- as.numeric(gsub(" km/h", "", ev$TopSpeed))
ev$Range <- as.numeric(gsub(" km", "", ev$Range))
ev$Efficiency <- as.numeric(gsub(" Wh/km", "", ev$Efficiency))
ev$FastChargeSpeed <- as.numeric(gsub(" km/h", "", ev$FastChargeSpeed))
ev$Drive <- as.factor(ev$Drive)
ev$PriceinGermany <- as.numeric(ev$PriceinGermany)

# Create graph of pairs
pairs(~ PriceinGermany + PriceinUK + Acceleration + TopSpeed + Range + Efficiency +
          FastChargeSpeed + BatterySize, data = ev)

# Create first model
model1 <- lm(PriceinGermany ~ Acceleration + TopSpeed + Range + Efficiency + FastChargeSpeed +
                 BatterySize, data = ev, na.action = na.omit)

# Create second model, with Drive as categorical data
model2 <- lm(PriceinGermany ~ Acceleration + TopSpeed + Range + Efficiency + FastChargeSpeed +
                 BatterySize + Drive, data = ev, na.action = na.omit)