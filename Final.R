library(dplyr)
library(arules)
library(cluster)

df<-read.csv("grc.csv")
#print type of rows
str(df)
#print type of data
class(df)
#print first 6 rows
head(df)
#print last 6 rows
tail(df)
# print dimantion
dim(df)
#print summery
summary(df)


duplicated(df)
#number of duplicated row in data with out clean
sum(duplicated(df))

#print dublicate data
duplicated_rows <- df[duplicated(df), ]
print('These data are:')
print(duplicated_rows)

#dff data without duplicated
dff<-distinct(df)
#number of duplicated row in data with clean
sum(duplicated(dff))

# is data have NA
is.na(dff)
#sum of NA in data
sum(is.na(dff))


# boxplot to show outliers
outliers<-boxplot(dff$count)$out
outliers
# print outliers
dff[which(dff$count %in% outliers),]
 
# pie between cash or cridet
x=table(dff$paymentType)
print(x)
pie(x)

#make dashtabel with size 2*2
par(mfrow=c(2,2))
persentage=paste0(round(100*x/sum(x),1),"%")
pie(x,labels =persentage,col = c('red','blue') )
legend("bottomright",legend = c('cash','cridiet'),fill = c('red','blue'))

# bar plot Total spending by age
#y <- tapply(dff$total,dff$age,sum)
#barplot(y,col = "blue",border = "red",
   # xlab = "Age",ylab = "Total Spending",main = "Total Spending by Age",las = 2)

age_spinding <- dff%>%
  group_by(age)%>%
  summarise(total_spinding =sum(total))
plot(x=age_spinding$age,y=age_spinding$total_spinding,
     xlab = "Age",ylab = "Total Spending",main = "Total Spending by Age",col = "blue")#,las = 2 )


z <- data.frame(city = dff$city, total = dff$total)

sum_cities <- aggregate(total ~ city, z, sum)
sum_cities_ordered <- sum_cities[order(-sum_cities$total), ]

#bar plot between city and total
barplot(height = sum_cities_ordered$total, names.arg = sum_cities_ordered$city,
        col = "pink", las = 2, main = "Total Spending by City (Descending)",
        xlab = "City", ylab = "Total Spending")


#box plot to distribution of total spending
boxplot(dff$total, col = "blue", main = "distribution of total spending")



# while loop to handle if the user entering wrong number
# clustering block

flag=T
while(flag) {
  num_clusters <- as.integer(readline(prompt = "Enter number of clusters:(from 2 to 4) "))
  if(num_clusters<2||num_clusters>4){
    print("invalid number of cluster please enter a number between 2 and 4")
}else{flag=F}
}
clustering <- kmeans(dff[, c("age", "total")], centers = num_clusters)
dff$cluster <- clustering$cluster
print(dff[, c("customer", "age", "total", "cluster")])

# transactions = converting chr class items to transactions class to be apropriate to extract apriori algorithm
transactions <- as(strsplit(dff$items, ","), "transactions")
print(transactions)
# enter min_supp and min_conf by user 
min_support <- as.numeric(readline(prompt = "Enter minimum support: "))
min_confidence <- as.numeric(readline(prompt = "Enter minimum confidence: "))
# extracting apriori algorithm
rules <- apriori(transactions, parameter = list(supp = min_support, conf = min_confidence,minlen=2))
print(rules)
# top 4 rules by confidence
print("top 4 association rules are ")
inspect(head(sort(rules, by = "confidence"), n=4))
