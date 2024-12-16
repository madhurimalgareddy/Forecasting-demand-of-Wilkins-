library(readxl)
library(forecast)
wilkins <- read_excel("C:/Users/madhu/Downloads/wilkins.xlsx")
View(wilkins)
#apply multiple regression on pvb total
mr_pvb_1 = lm(PVBTOTAL~unemploymentrate+bankloan+singleunit+multiunit,data = wilkins)
summary(mr_pvb_1)
# Apply multiple regression to Fire Valve total
mr_Valve = lm(VALVETOTAL~unemploymentrate+bankloan+singleunit+multiunit, data = wilkins)
summary(mr_Valve)
# As p value for unemployment and bank loan are higher than 0.05 they are not significantly important. can drop them
mr_pvb_1 = lm(PVBTOTAL~singleunit+multiunit,data = wilkins)
# chane pvb total to time series data
pvb_ts_c = ts(wilkins$PVBTOTAL, start = c(2001, 1), frequency = 4)
# Holt model with additive seasonality
holt_pvb = holt(pvb_ts_c, damped = TRUE, h = 8)
# Holt winters model with additive seasonality
hw_pvb = hw(pvb_ts_c, seasonal = "additive",damped = TRUE, h = 8)
# Summaries of both models
summary(holt_pvb)
summary(hw_pvb)
# exponential smoothing for pvb
ses_model_1 <- ses(pvb_ts_c, h = 8)
summary(ses_model_1)
# Apply the above models to Valve total
# convert Fire Valve total to time series data
valve_ts_c = ts(wilkins$VALVETOTAL, start = c(2001, 1), frequency = 4)
# Holt model 
holt_valve = holt(valve_ts_c, damped = TRUE, h = 8)
summary(holt_valve)
# Holt winters additive
hw_valve = hw(valve_ts_c, seasonal = "additive",damped = TRUE, h = 8)
summary(hw_valve)
# holt winter
hw_valve_1 = hw(valve_ts_c, h = 8)
summary = (hw_valve_1)
# exponential smoothing for valves
ses_model <- ses(valve_ts_c, h = 8)
summary(ses_model)

