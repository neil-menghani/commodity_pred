# Regression Model
# Neil Menghani, nlm2138
# Steven Lee, sl3944

library(readr)

# load commodity and weather data
comm_data <- read_csv("~/Desktop/comm_data.csv")
weather_data <- read_csv("~/Desktop/weather_data.csv")
comm = merge(x=comm_data, y=weather_data, by="date", all.x=TRUE) # left join datasets by date
c = comm[order(as.Date(comm$date, format="%m/%d/%Y")),] # order by date



# Run time series regression model on four 6-month periods of data

# Period 1
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 31:126) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-1] = sum(w %*% t(x))

	# update weights based on regression of each asset on the temp of their 3 biggest producers
	fit = lm(c$'coffee_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'viet'[(row-30):(row-1)] + c$'colo'[(row-30):(row-1)])
	coffee = coefficients(fit)
	w[1] = coffee[1] + c$'braz'[row] * coffee[2] + c$'viet'[row] * coffee[3] +
		c$'colo'[row] * coffee[4]
	fit = lm(c$'corn_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'arge'[(row-30):(row-1)] + c$'braz'[(row-30):(row-1)])
	corn = coefficients(fit)
	w[2] = corn[1] + c$'iowa'[row] * corn[2] + c$'arge'[row] * corn[3] +
		c$'braz'[row] * corn[4]
	fit = lm(c$'sugar_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'thai'[(row-30):(row-1)] + c$'aust'[(row-30):(row-1)])
	sugar = coefficients(fit)
	w[3] = sugar[1] + c$'braz'[row] * sugar[2] + c$'thai'[row] * sugar[3] +
		c$'aust'[row] * sugar[4]	
	fit = lm(c$'soybean_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'braz'[(row-30):(row-1)] + c$'arge'[(row-30):(row-1)])
	soybean = coefficients(fit)
	w[4] = soybean[1] + c$'iowa'[row] * soybean[2] + c$'braz'[row] * soybean[3] +
		c$'arge'[row] * soybean[4]
	fit = lm(c$'wheat_%'[(row-30):(row-1)] ~ c$'kans'[(row-30):(row-1)] +
		c$'russ'[(row-30):(row-1)] + c$'cana'[(row-30):(row-1)])
	wheat = coefficients(fit)
	w[5] = wheat[1] + c$'kans'[row] * wheat[2] + c$'russ'[row] * wheat[3] +
		c$'cana'[row] * wheat[4]
	
	w = w / sum(abs(w)) # normalize weights

}
print("Period 1:")
period1 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe1 = (mean(returns-0.0009/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.09%
print(period1)
print(sharpe1)

# Period 2
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 186:253) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-156] = sum(w %*% t(x))

	# update weights based on regression of each asset on the temp of their 3 biggest producers
	fit = lm(c$'coffee_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'viet'[(row-30):(row-1)] + c$'colo'[(row-30):(row-1)])
	coffee = coefficients(fit)
	w[1] = coffee[1] + c$'braz'[row] * coffee[2] + c$'viet'[row] * coffee[3] +
		c$'colo'[row] * coffee[4]
	fit = lm(c$'corn_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'arge'[(row-30):(row-1)] + c$'braz'[(row-30):(row-1)])
	corn = coefficients(fit)
	w[2] = corn[1] + c$'iowa'[row] * corn[2] + c$'arge'[row] * corn[3] +
		c$'braz'[row] * corn[4]
	fit = lm(c$'sugar_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'thai'[(row-30):(row-1)] + c$'aust'[(row-30):(row-1)])
	sugar = coefficients(fit)
	w[3] = sugar[1] + c$'braz'[row] * sugar[2] + c$'thai'[row] * sugar[3] +
		c$'aust'[row] * sugar[4]	
	fit = lm(c$'soybean_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'braz'[(row-30):(row-1)] + c$'arge'[(row-30):(row-1)])
	soybean = coefficients(fit)
	w[4] = soybean[1] + c$'iowa'[row] * soybean[2] + c$'braz'[row] * soybean[3] +
		c$'arge'[row] * soybean[4]
	fit = lm(c$'wheat_%'[(row-30):(row-1)] ~ c$'kans'[(row-30):(row-1)] +
		c$'russ'[(row-30):(row-1)] + c$'cana'[(row-30):(row-1)])
	wheat = coefficients(fit)
	w[5] = wheat[1] + c$'kans'[row] * wheat[2] + c$'russ'[row] * wheat[3] +
		c$'cana'[row] * wheat[4]
	
	w = w / sum(abs(w)) # normalize weights

}
print("Period 2:")
period2 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe2 = (mean(returns-0.0009/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.09%
print(period2)
print(sharpe2)


# Period 3
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 283:379) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-253] = sum(w %*% t(x))

	# update weights based on regression of each asset on the temp of their 3 biggest producers
	fit = lm(c$'coffee_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'viet'[(row-30):(row-1)] + c$'colo'[(row-30):(row-1)])
	coffee = coefficients(fit)
	w[1] = coffee[1] + c$'braz'[row] * coffee[2] + c$'viet'[row] * coffee[3] +
		c$'colo'[row] * coffee[4]
	fit = lm(c$'corn_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'arge'[(row-30):(row-1)] + c$'braz'[(row-30):(row-1)])
	corn = coefficients(fit)
	w[2] = corn[1] + c$'iowa'[row] * corn[2] + c$'arge'[row] * corn[3] +
		c$'braz'[row] * corn[4]
	fit = lm(c$'sugar_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'thai'[(row-30):(row-1)] + c$'aust'[(row-30):(row-1)])
	sugar = coefficients(fit)
	w[3] = sugar[1] + c$'braz'[row] * sugar[2] + c$'thai'[row] * sugar[3] +
		c$'aust'[row] * sugar[4]	
	fit = lm(c$'soybean_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'braz'[(row-30):(row-1)] + c$'arge'[(row-30):(row-1)])
	soybean = coefficients(fit)
	w[4] = soybean[1] + c$'iowa'[row] * soybean[2] + c$'braz'[row] * soybean[3] +
		c$'arge'[row] * soybean[4]
	fit = lm(c$'wheat_%'[(row-30):(row-1)] ~ c$'kans'[(row-30):(row-1)] +
		c$'russ'[(row-30):(row-1)] + c$'cana'[(row-30):(row-1)])
	wheat = coefficients(fit)
	w[5] = wheat[1] + c$'kans'[row] * wheat[2] + c$'russ'[row] * wheat[3] +
		c$'cana'[row] * wheat[4]
	
	w = w / sum(abs(w)) # normalize weights

}
print("Period 3:")
period3 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe3 = (mean(returns-0.0005/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.05%
print(period3)
print(sharpe3)


# Period 4
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 409:507) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-379] = sum(w %*% t(x))

	# update weights based on regression of each asset on the temp of their 3 biggest producers
	fit = lm(c$'coffee_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'viet'[(row-30):(row-1)] + c$'colo'[(row-30):(row-1)])
	coffee = coefficients(fit)
	w[1] = coffee[1] + c$'braz'[row] * coffee[2] + c$'viet'[row] * coffee[3] +
		c$'colo'[row] * coffee[4]
	fit = lm(c$'corn_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'arge'[(row-30):(row-1)] + c$'braz'[(row-30):(row-1)])
	corn = coefficients(fit)
	w[2] = corn[1] + c$'iowa'[row] * corn[2] + c$'arge'[row] * corn[3] +
		c$'braz'[row] * corn[4]
	fit = lm(c$'sugar_%'[(row-30):(row-1)] ~ c$'braz'[(row-30):(row-1)] +
		c$'thai'[(row-30):(row-1)] + c$'aust'[(row-30):(row-1)])
	sugar = coefficients(fit)
	w[3] = sugar[1] + c$'braz'[row] * sugar[2] + c$'thai'[row] * sugar[3] +
		c$'aust'[row] * sugar[4]	
	fit = lm(c$'soybean_%'[(row-30):(row-1)] ~ c$'iowa'[(row-30):(row-1)] +
		c$'braz'[(row-30):(row-1)] + c$'arge'[(row-30):(row-1)])
	soybean = coefficients(fit)
	w[4] = soybean[1] + c$'iowa'[row] * soybean[2] + c$'braz'[row] * soybean[3] +
		c$'arge'[row] * soybean[4]
	fit = lm(c$'wheat_%'[(row-30):(row-1)] ~ c$'kans'[(row-30):(row-1)] +
		c$'russ'[(row-30):(row-1)] + c$'cana'[(row-30):(row-1)])
	wheat = coefficients(fit)
	w[5] = wheat[1] + c$'kans'[row] * wheat[2] + c$'russ'[row] * wheat[3] +
		c$'cana'[row] * wheat[4]
	
	w = w / sum(abs(w)) # normalize weights

}
print("Period 4:")
period4 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe4 = (mean(returns-0.0005/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.05%
print(period4)
print(sharpe4)
