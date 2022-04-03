# BUS-462-Group-Project

model_attack <- lm(data = fwd, Value ~ Dribbling + Curve + Acceleration + Agility + Aggression)
summary(model_attack)

model_2 <- lm(data = mid, Value ~ Dribbling + Curve + Acceleration + Agility + Aggression )
summary(model_2)
