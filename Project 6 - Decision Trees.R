library(rpart)
library(rpart.plot)
mydata = read.csv("Auctions.csv")

#1
mydata$Category = as.factor(mydata$Category)
mydata$Weekend = as.factor(mydata$Weekend)
str(mydata)

# 2 & 3 &  4
set.seed(100)
train.indx = sample(1:nrow(mydata), 1000)
train.df = mydata[train.indx,]
test.df = mydata[-train.indx,]
tree.default = rpart(Competitive~., data=train.df, method = "class")
rpart.plot(tree.default, extra = 1)

#5 & 6
newval = data.frame(Category="Books", SellerRating=3000, Duration=5, Weekend="1", OpenPrice=2.0)
predict(tree.default, newval, type="class")

#7
set.seed(1234)
tree.large = rpart(Competitive~., data = train.df, method = "class", cp=0, minsplit=20, xval=10)
rpart.plot(tree.large, extra = 1)

#8
printcp(tree.large)

#9
tree.pruned = prune(tree.large, cp=0.006)
rpart.plot(tree.pruned, extra = 1)

#10
pred.large = predict(tree.large, test.df, type="class")
confusion.large = table(pred.large, test.df$Competitive)
confusion.large
sum(diag(confusion.large))/sum(confusion.large)

pred.pruned = predict(tree.pruned, test.df, type="class")
confusion.pruned = table(pred.pruned, test.df$Competitive)
confusion.pruned
sum(diag(confusion.pruned))/sum(confusion.pruned)







