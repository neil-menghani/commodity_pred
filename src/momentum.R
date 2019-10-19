# Momentum Model
# Neil Menghani, nlm2138
# Steven Lee, sl3944

library(readr)

# load commodity data
comm_data <- read_csv("~/Desktop/comm_data.csv")

# Run momentum model on four 6-month periods of data

# Period 1
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 31:126) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-1] = sum(w %*% t(x))
	
	# update weights based on how well the asset has been doing in the previous 30 days
	w[1] = mean(c$'coffee_%'[(row-30):(row-1)])
	w[2] = mean(c$'corn_%'[(row-30):(row-1)])
	w[3] = mean(c$'sugar_%'[(row-30):(row-1)])
	w[4] = mean(c$'soybean_%'[(row-30):(row-1)])
	w[5] = mean(c$'wheat_%'[(row-30):(row-1)])
	
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
	
	# update weights based on how well the asset has been doing in the previous 30 days
	w[1] = mean(c$'coffee_%'[(row-30):(row-1)])
	w[2] = mean(c$'corn_%'[(row-30):(row-1)])
	w[3] = mean(c$'sugar_%'[(row-30):(row-1)])
	w[4] = mean(c$'soybean_%'[(row-30):(row-1)])
	w[5] = mean(c$'wheat_%'[(row-30):(row-1)])

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
	
	# update weights based on how well the asset has been doing in the previous 30 days
	w[1] = mean(c$'coffee_%'[(row-30):(row-1)])
	w[2] = mean(c$'corn_%'[(row-30):(row-1)])
	w[3] = mean(c$'sugar_%'[(row-30):(row-1)])
	w[4] = mean(c$'soybean_%'[(row-30):(row-1)])
	w[5] = mean(c$'wheat_%'[(row-30):(row-1)])
	
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
	
	# update weights based on how well the asset has been doing in the previous 30 days
	w[1] = mean(c$'coffee_%'[(row-30):(row-1)])
	w[2] = mean(c$'corn_%'[(row-30):(row-1)])
	w[3] = mean(c$'sugar_%'[(row-30):(row-1)])
	w[4] = mean(c$'soybean_%'[(row-30):(row-1)])
	w[5] = mean(c$'wheat_%'[(row-30):(row-1)])
	
	w = w / sum(abs(w)) # normalize weights
}
print("Period 4:")
period4 = (prod(returns+1))^2-1 # calculate overall return from daily return
sharpe4 = (mean(returns-0.0005/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.05%
print(period4) # annualize from 6-month period
print(sharpe4)

