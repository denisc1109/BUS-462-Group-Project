#### PREAMBLE : ## Clearing mem buffers ####
#Reference: Class sessions
cat("\014")  # Clear Console
rm(list = ls(all.names = TRUE))# clear all

#Command to load the csv file
dt = fread("C:/Users/Jamie/Desktop/R Data/fifa21_male2.csv")

#check for missing values
colSums(is.na(dt))

require(data.table)
require(dplyr)

# Removes rows with empty cells and NA's for Position
# CITE: https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
dt = dt[!(dt$BP == ""), ]
dt = dt[!is.na(dt$BP), ]
# Removes rows with empty cells and NA's for Value
dt = dt[!(dt$Value == ""), ]
dt = dt[!is.na(dt$Value), ]

library(readr)

#remove euro sign out of value
dt$Value =(gsub("\\€", "", dt$Value))

#convert K and M to their respective numbers

dt$Value <- dplyr::case_when(
  stringr::str_detect(dt$Value, 'M') ~ readr::parse_number(dt$Value) * 1e6,
  stringr::str_detect(dt$Value, 'K') ~ readr::parse_number(dt$Value) * 1e3,
  TRUE ~ parse_number(dt$Value)
)
#change integers to value
dt$Value = as.integer(dt$Value)
#Check the data type to make sure
str(dt$Value)
#remove all players with value of 0
dt <- filter(dt, Value > 0)

#see how many unique positions there are in "best position (BP)" column
unique(dt$BP)

#New set with goalkeepers
gk <- filter(dt,BP == "GK" )
#New set with defenders
def <- filter(dt,BP == "CB"|BP == "RB"|BP == "LB"|BP == "LWB"|BP == "RWB")
#New set with midfielders
mid <- filter(dt,BP == "CM"|BP == "CAM"|BP == "CDM"|BP == "LM"|BP == "RM")
#New set with forwards
fwd <- filter(dt,BP == "LW"|BP == "RW"|BP == "CF"|BP == "ST")

#average value for all players
mean(dt$Value) #2,637,769

#Average value for goalkeepers
mean(gk$Value) #2,260,060

#Average value for defenders
mean(def$Value) #2,301,351

#Average value for midfielders
mean(mid$Value) #2,944,261    <- USE

#Average value for forwards
mean(fwd$Value) #2,761,147    <- USE


######################################
###Question 2#########################
######################################


require(stargazer)

# Kitchen sink model
fwd2= fwd[,c(26,27,28,29,30,32,33,34,35,36,38,39,40,41,42,44,45,46,47,48,50,51,52,53,54,57,58,59)] 
model.ks = lm(fwd$Value~.,data=fwd2)
stargazer(model.ks, type = "text") # Adjusted R2   0.414

fwd3= fwd[,c(26,27,28,29,30,32,33,35,36,38,39,40,41,44,45,46,47,48,50)] 
model.guess = lm(fwd$Value ~., data=fwd3)
stargazer(model.guess, type = "text") # Adjusted R2  0.409  

fwd4= fwd[,c(26,27,28,29,30)] 
model.guess2 = lm(fwd$Value~.,data=fwd4)
stargazer(model.guess2, type = "text") # Adjusted R2   0.362

# Pick this because highest adjusted R, tells us that finishing, curve, sprint speed are the 3 most important vars
fwd5= fwd[,c(27,28,30,33,39,44,45,48,50)] 
model.guess3 = lm(fwd$Value~.,data=fwd5)
stargazer(model.guess3, type = "text") # Adjusted R2   0.381


mid2 = mid[,c(26,29,32,35,36,40,41,51,52,53,57,58,59)]
model.mid = lm(mid$Value ~., data=mid2)
stargazer(model.mid, type = "text") # Adjusted R2  0.432
