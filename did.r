library(haven)
library(plm)

# Traffic fatality rate with unemployment, speed limit, and seatbelt correlates
my_data <- read_dta("tfr w ue speed seatbelt.dta"); head(my_data)
my_data$time_data <- my_data$time

# Difference in difference model
# timexstatestatus interaction term is the variable of interest
did_model <- plm(tfr ~ timexstatestatus + statestatus + time_data + ue + speed + seatbeltlaw, index=c("statenum"), data = my_data, model = "random")
summary(did_model)
confint(did_model, level = 0.95)

# Illustration of the effect of recreational marijuana commercialization on traffic fatality rate
# Red color denotes a state that legalized recereational marijuana

# Non-RML states
plot(my_data$time_data[my_data$statestatus == 0], my_data$tfr[my_data$statestatus == 0], xlab="Time Period", ylab="Traffic Fatality Rate")

y0 <- did_model$coefficients[1] + did_model$coefficients[5] * mean(my_data$ue) + did_model$coefficients[6] * mean(my_data$speed)
y1 <- did_model$coefficients[1] + did_model$coefficients[3] + did_model$coefficients[5] * mean(my_data$ue) + did_model$coefficients[6] * mean(my_data$speed)

x <- c(0, 1)
y <- c(y0, y1)
lines(x, y)

# RML states
points(my_data$time_data[my_data$statestatus == 1], my_data$tfr[my_data$statestatus == 1], col="red")

y0 <- did_model$coefficients[1] + did_model$coefficients[3] +  + did_model$coefficients[5] * mean(my_data$ue) + did_model$coefficients[6] * mean(my_data$speed)
y1 <- did_model$coefficients[1] + did_model$coefficients[2] + did_model$coefficients[3] + did_model$coefficients[5] * mean(my_data$ue) + did_model$coefficients[6] * mean(my_data$speed)

x <- c(0, 1)
y <- c(y0, y1)
lines(x, y, col="red")
