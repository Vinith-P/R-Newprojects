data <- read.csv ("clipboard", sep = ";", header = T)


## check the missing values in the data 
sum(is.na(data))


library(janitor)
data <- clean_names(data)

rules <- apriori(data)
str(data)

min(data$age)
hist(data$age)

data$age_years_range <- cut(data$age,
                                          breaks = c(0,25, 50,Inf),
                                          labels = c('0-24-year', '25-49 years',
                                                     'more than 50 years'),
                                          include.lowest = T)


final_data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)

final_data <- data[,-c(1,6,9,10,11:16)]
final_data <- as.data.frame(unclass(final_data), stringsAsFactors = TRUE)

rules <- apriori(final_data,parameter = list(minlen=2, maxlen= 3,supp=.7, conf=.8))

rules_high <- apriori(final_data, parameter = list(supp=.005, conf = .35, maxlen = 6), 
                      appearance =list(default = "lhs", 
                                       rhs="y=yes"))

conf_rules_high <- sort(rules_high, by="lift", decreasing = T)
redundant_high <- is.redundant(rules_high, measure="confidence")
which(redundant_high)
inspect(rules)
inspect(rules_high)
str(final_data)

