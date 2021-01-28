library("arules")
library("kutils")

#Load the data
df <- read.csv(file="bakery.csv", header=TRUE, sep=",")
df

#Replace numerical values to factors 
df <- lapply(df, function(x){as.factor(x)})
df

df[is.na(df)] <- 0
df

str(df)

#Convert to a dataframe
df2 <- as.data.frame(lapply(df, unlist))


#Order the dataframe by column 
df2[order(df2[1,])]

#Make the certain columns null to avoid redundant / insignificant processing,
#since we are checking if the shot was made or not 
#df2$TeamName = NULL
#df2$FinalsWin = NULL


#Convert the data to be a set of transactions
transactional_data <- as(df2, "transactions")
transactional_data


#Make sure transactional_data is a set of transactions and view the first two entries
class(transactional_data)
inspect(head(transactional_data, 2))


#Generate rules with a min support and confidence 
generalRules <- apriori(transactional_data, parameter = list(support = 0.01, confidence = 0.7, maxlen=3))

inspect(head(sort(generalRules, by = "lift"), 500))

summary(generalRules)


#Generate specific rules

specificRules <- apriori (transactional_data, parameter = list(supp=0.001, confidence = 0.70, maxlen=3), appearance = list(default="lhs",rhs="Chocolate.Croissant=yes"))

inspect(head(sort(specificRules, by = "count"), 200))

summary(specificRules)

