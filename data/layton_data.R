# Install ggplot2

install.packages("tidyverse")
install.packages("ggplot")
library(ggplot2)

# Import .csv file
layton_readings <- read.csv("C:/Users/daine/OneDrive/Desktop/BYU/4 - Senior Year/Fall 2020/CE EN 594R/ceen594r_term_project/data/layton_readings.csv")

# Remove incomplete minutes
na.omit(layton_readings)

# Compare lane 1 EQ auto and manual counts
eq1 <- ggplot(layton_readings,aes(Manual_EQ_1.2,Auto_EQ_1)) + geom_jitter() + xlab("Left lane EQ manual volume (veh)") + ylab("Left lane EQ detector volume (veh)")

# Compare lane 2 EQ auto and manual counts
eq2 <- ggplot(layton_readings,aes(Manual_EQ_3,Auto_EQ_2)) + geom_jitter() + xlab("Right lane EQ manual volume (veh)") + ylab("Right lane EQ detector volume (veh)")

#Compare intermediate and excessive queue counts
q1 <- ggplot(layton_readings,aes(Auto_IQ_1,Auto_EQ_1)) + geom_jitter()
q2 <- ggplot(layton_readings,aes(Auto_IQ_2,Auto_EQ_2)) + geom_jitter()

#Linear regression for lane 1
lm_EQ_1 <- lm(formula = Manual_EQ_1.2 ~ Auto_EQ_1 + EQ_1_Occ, data = layton_readings)
summary(lm_EQ_1)

# Linear regression for lane 2
lm_EQ_2 <- lm(formula = Manual_EQ_3 ~ Auto_EQ_2 + EQ_2_Occ, data = layton_readings)
summary(lm_EQ_2)

# Compare lane 1 occupancy and volume
lm_qo_eq1 <- lm(formula = Auto_EQ_1 ~ EQ_1_Occ, data = layton_readings)
summary(lm_qo_eq1)
qo_eq1 <- ggplot(layton_readings,aes(EQ_1_Occ,Auto_EQ_1)) +geom_jitter() + xlab("Left lane EQ occupancy (%)") + ylab("Left lane EQ volume (veh)")
lm_qo_iq1 <- lm(formula = Auto_IQ_1 ~ IQ_1_Occ, data = layton_readings)
summary(lm_qo_iq1)

# Compare lane 2 occupancy and volume
lm_qo_eq2 <- lm(formula = Auto_EQ_2 ~ EQ_2_Occ, data = layton_readings)
summary(lm_qo_eq2)
qo_eq2 <- ggplot(layton_readings,aes(EQ_2_Occ,Auto_EQ_2)) + geom_jitter()+ xlab("Right lane EQ occupancy (%)") + ylab("Right lane EQ volume (veh)")
lm_qo_iq2 <- lm(formula = Auto_IQ_2 ~ IQ_2_Occ, data = layton_readings)
summary(lm_qo_iq2)

# Compare EQ occupancies to manual counts
lm_manual_qo_eq1 <- lm(formula = Manual_EQ_1.2 ~ EQ_1_Occ, data = layton_readings)
summary(lm_manual_qo_eq1)
lm_manual_qo_eq2 <- lm(formula = Manual_EQ_3 ~ EQ_2_Occ, data = layton_readings)
summary(lm_manual_qo_eq2)

#Compare occupancy to actual queue size and actual volume
lm_solution <- lm(formula = EQ_1_Occ ~ Manual_EQ_1.2 + Total.on.ramp)
summary(lm_solution)
solution <- ggplot(layton_readings,aes(x = EQ_1_Occ,y = Manual_EQ_1.2,color = Total.on.ramp)) + geom_jitter()