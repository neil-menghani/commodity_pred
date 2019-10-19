# K-Nearest Neighbors Model
# Neil Menghani, nlm2138
# Steven Lee, sl3944

library(readr)
library(class)

# load commodity and weather data
comm_data <- read_csv("~/Desktop/comm_data.csv")
weather_data <- read_csv("~/Desktop/weather_data.csv")
comm = merge(x=comm_data, y=weather_data, by="date", all.x=TRUE) # left join datasets by date
c = comm[order(as.Date(comm$date, format="%m/%d/%Y")),] # order by date

set.seed(1234)

k = 5 # k chosen based on cross-validation on training set

# Run time series regression model on four 6-month periods of data

# Period 1
w = rep(0, 5) # begin with weight of 0 for each asset
returns = rep(0, 29)

for (row in 31:126) {
	# update returns based on previous day's performance
	x = c(c$'coffee_%'[row-1], c$'corn_%'[row-1], c$'sugar_%'[row-1], 
		c$'soybean_%'[row-1], c$'wheat_%'[row-1])
	returns[row-1] = sum(w %*% t(x))

	# update weights based on K-Nearest Neighbors Model with normalized variables
	coffee = c$'coffee_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	viet = c$'viet'[(row-30):(row-1)]
	colo = c$'colo'[(row-30):(row-1)]
	coffee_train = t(rbind(braz, viet, colo))
	coffee_test = c(c$'braz'[row], c$'viet'[row], c$'colo'[row])
	coffee_pred = knn(train = coffee_train, test = coffee_test, cl = coffee, k=k)
	a = levels(droplevels(coffee_pred))
	w[1] = as.numeric(a)

	corn = c$'corn_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	corn_train = t(rbind(iowa, arge, braz))
	corn_test = c(c$'iowa'[row], c$'arge'[row], c$'braz'[row])
	corn_pred = knn(train = corn_train, test = corn_test, cl = corn, k=k)
	a = levels(droplevels(corn_pred))
	w[2] = as.numeric(a)

	sugar = c$'sugar_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	thai = c$'thai'[(row-30):(row-1)]
	aust = c$'aust'[(row-30):(row-1)]
	sugar_train = t(rbind(braz, thai, aust))
	sugar_test = c(c$'braz'[row], c$'thai'[row], c$'aust'[row])
	sugar_pred = knn(train = sugar_train, test = sugar_test, cl = sugar, k=k)
	a = levels(droplevels(corn_pred))
	w[3] = as.numeric(a)

	soybean = c$'soybean_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	soybean_train = t(rbind(iowa, braz, arge))
	soybean_test = c(c$'iowa'[row], c$'braz'[row], c$'arge'[row])
	soybean_pred = knn(train = soybean_train, test = soybean_test, cl = soybean, k=k)
	a = levels(droplevels(soybean_pred))
	w[4] = as.numeric(a)

	wheat = c$'wheat_%'[(row-30):(row-1)]
	kans = c$'kans'[(row-30):(row-1)]
	russ = c$'russ'[(row-30):(row-1)]
	cana = c$'cana'[(row-30):(row-1)]
	wheat_train = t(rbind(kans, russ, cana))
	wheat_test = c(c$'kans'[row], c$'russ'[row], c$'cana'[row])
	wheat_pred = knn(train = wheat_train, test = wheat_test, cl = wheat, k=k)
	a = levels(droplevels(wheat_pred))
	w[5] = as.numeric(a)

	w = w / sum(abs(w))
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

	# update weights based on K-Nearest Neighbors Model with normalized variables
	coffee = c$'coffee_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	viet = c$'viet'[(row-30):(row-1)]
	colo = c$'colo'[(row-30):(row-1)]
	coffee_train = t(rbind(braz, viet, colo))
	coffee_test = c(c$'braz'[row], c$'viet'[row], c$'colo'[row])
	coffee_pred = knn(train = coffee_train, test = coffee_test, cl = coffee, k=k)
	a = levels(droplevels(coffee_pred))
	w[1] = as.numeric(a)

	corn = c$'corn_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	corn_train = t(rbind(iowa, arge, braz))
	corn_test = c(c$'iowa'[row], c$'arge'[row], c$'braz'[row])
	corn_pred = knn(train = corn_train, test = corn_test, cl = corn, k=k)
	a = levels(droplevels(corn_pred))
	w[2] = as.numeric(a)

	sugar = c$'sugar_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	thai = c$'thai'[(row-30):(row-1)]
	aust = c$'aust'[(row-30):(row-1)]
	sugar_train = t(rbind(braz, thai, aust))
	sugar_test = c(c$'braz'[row], c$'thai'[row], c$'aust'[row])
	sugar_pred = knn(train = sugar_train, test = sugar_test, cl = sugar, k=k)
	a = levels(droplevels(corn_pred))
	w[3] = as.numeric(a)

	soybean = c$'soybean_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	soybean_train = t(rbind(iowa, braz, arge))
	soybean_test = c(c$'iowa'[row], c$'braz'[row], c$'arge'[row])
	soybean_pred = knn(train = soybean_train, test = soybean_test, cl = soybean, k=k)
	a = levels(droplevels(soybean_pred))
	w[4] = as.numeric(a)

	wheat = c$'wheat_%'[(row-30):(row-1)]
	kans = c$'kans'[(row-30):(row-1)]
	russ = c$'russ'[(row-30):(row-1)]
	cana = c$'cana'[(row-30):(row-1)]
	wheat_train = t(rbind(kans, russ, cana))
	wheat_test = c(c$'kans'[row], c$'russ'[row], c$'cana'[row])
	wheat_pred = knn(train = wheat_train, test = wheat_test, cl = wheat, k=k)
	a = levels(droplevels(wheat_pred))
	w[5] = as.numeric(a)

	w = w / sum(abs(w))
}
print("Period 1:")
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

	# update weights based on K-Nearest Neighbors Model with normalized variables
	coffee = c$'coffee_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	viet = c$'viet'[(row-30):(row-1)]
	colo = c$'colo'[(row-30):(row-1)]
	coffee_train = t(rbind(braz, viet, colo))
	coffee_test = c(c$'braz'[row], c$'viet'[row], c$'colo'[row])
	coffee_pred = knn(train = coffee_train, test = coffee_test, cl = coffee, k=k)
	a = levels(droplevels(coffee_pred))
	w[1] = as.numeric(a)

	corn = c$'corn_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	corn_train = t(rbind(iowa, arge, braz))
	corn_test = c(c$'iowa'[row], c$'arge'[row], c$'braz'[row])
	corn_pred = knn(train = corn_train, test = corn_test, cl = corn, k=k)
	a = levels(droplevels(corn_pred))
	w[2] = as.numeric(a)

	sugar = c$'sugar_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	thai = c$'thai'[(row-30):(row-1)]
	aust = c$'aust'[(row-30):(row-1)]
	sugar_train = t(rbind(braz, thai, aust))
	sugar_test = c(c$'braz'[row], c$'thai'[row], c$'aust'[row])
	sugar_pred = knn(train = sugar_train, test = sugar_test, cl = sugar, k=k)
	a = levels(droplevels(corn_pred))
	w[3] = as.numeric(a)

	soybean = c$'soybean_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	soybean_train = t(rbind(iowa, braz, arge))
	soybean_test = c(c$'iowa'[row], c$'braz'[row], c$'arge'[row])
	soybean_pred = knn(train = soybean_train, test = soybean_test, cl = soybean, k=k)
	a = levels(droplevels(soybean_pred))
	w[4] = as.numeric(a)

	wheat = c$'wheat_%'[(row-30):(row-1)]
	kans = c$'kans'[(row-30):(row-1)]
	russ = c$'russ'[(row-30):(row-1)]
	cana = c$'cana'[(row-30):(row-1)]
	wheat_train = t(rbind(kans, russ, cana))
	wheat_test = c(c$'kans'[row], c$'russ'[row], c$'cana'[row])
	wheat_pred = knn(train = wheat_train, test = wheat_test, cl = wheat, k=k)
	a = levels(droplevels(wheat_pred))
	w[5] = as.numeric(a)

	w = w / sum(abs(w))
}
print("Period 3:")
period3 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe3 = (mean(returns-0.0005/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.09%
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

	# update weights based on K-Nearest Neighbors Model with normalized variables
	coffee = c$'coffee_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	viet = c$'viet'[(row-30):(row-1)]
	colo = c$'colo'[(row-30):(row-1)]
	coffee_train = t(rbind(braz, viet, colo))
	coffee_test = c(c$'braz'[row], c$'viet'[row], c$'colo'[row])
	coffee_pred = knn(train = coffee_train, test = coffee_test, cl = coffee, k=k)
	a = levels(droplevels(coffee_pred))
	w[1] = as.numeric(a)

	corn = c$'corn_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	corn_train = t(rbind(iowa, arge, braz))
	corn_test = c(c$'iowa'[row], c$'arge'[row], c$'braz'[row])
	corn_pred = knn(train = corn_train, test = corn_test, cl = corn, k=k)
	a = levels(droplevels(corn_pred))
	w[2] = as.numeric(a)

	sugar = c$'sugar_%'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	thai = c$'thai'[(row-30):(row-1)]
	aust = c$'aust'[(row-30):(row-1)]
	sugar_train = t(rbind(braz, thai, aust))
	sugar_test = c(c$'braz'[row], c$'thai'[row], c$'aust'[row])
	sugar_pred = knn(train = sugar_train, test = sugar_test, cl = sugar, k=k)
	a = levels(droplevels(corn_pred))
	w[3] = as.numeric(a)

	soybean = c$'soybean_%'[(row-30):(row-1)]
	iowa = c$'iowa'[(row-30):(row-1)]
	braz = c$'braz'[(row-30):(row-1)]
	arge = c$'arge'[(row-30):(row-1)]
	soybean_train = t(rbind(iowa, braz, arge))
	soybean_test = c(c$'iowa'[row], c$'braz'[row], c$'arge'[row])
	soybean_pred = knn(train = soybean_train, test = soybean_test, cl = soybean, k=k)
	a = levels(droplevels(soybean_pred))
	w[4] = as.numeric(a)

	wheat = c$'wheat_%'[(row-30):(row-1)]
	kans = c$'kans'[(row-30):(row-1)]
	russ = c$'russ'[(row-30):(row-1)]
	cana = c$'cana'[(row-30):(row-1)]
	wheat_train = t(rbind(kans, russ, cana))
	wheat_test = c(c$'kans'[row], c$'russ'[row], c$'cana'[row])
	wheat_pred = knn(train = wheat_train, test = wheat_test, cl = wheat, k=k)
	a = levels(droplevels(wheat_pred))
	w[5] = as.numeric(a)

	w = w / sum(abs(w))
}
print("Period 4:")
period4 = (prod(returns+1))^2-1 # calculate overall annual return from daily return
sharpe4 = (mean(returns-0.0005/sqrt(252))/sd(returns))*sqrt(252) # calculate sharpe using risk-free rate of 0.09%
print(period4)
print(sharpe4)



	