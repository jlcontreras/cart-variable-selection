setwd("/home/jose/Universidad/DataScience/CART/cart-variable-selection/")
library(rpart)

# Spam dataset because 6 variables is an appropriate number: can be handled by our computers
# and at the same time there are enough to do some variable selection
spam = read.table('data/spam.txt')
colnames(spam) = c('crl.tot','dollar','bank','money','nooo','make','yesno')

# Create the max tree
treemax = rpart(yesno~., data=spam, control=rpart.control(minsplit=1, cp=10^(-15)))

# VI ranking
treemax$variable.importance



# Will return the vector of posterior probability for each modality, 
# as there is a qualitative variable.
predict(treemax)

# As we want the predicted class, and not the probab for each modality,
# we jsut have to set the type of prediction
pred = predict(treemax, type="class")

# How many times is the prediction not equal to the actual value?
sum(spam$yesno != pred)

# Divide in training and test sets
# Optionally, we can set a seed in order to always get the same 'random' sampling
# set.seed(1274)
train.sample = sample(nrow(spam), floor(nrow(spam)*(2/3)))

spam.train = spam[train.sample,]
spam.test  = spam[-train.sample,]

# Create the max tree using the training set
treemax = rpart(yesno~., data=spam.train, control=rpart.control(minsplit=1, cp=10^(-15)))

# Make predictions on the test set
pred = predict(treemax, newdata=spam.test, type="class")

# Confusion matrix
cm = table(spam.test$yesno, pred)

err.test = 1 - (cm[1,1] + cm[2,2])/sum(cm)
# Could be done like this
# err.test = (cm[1,2] + cm[2,1])/sum(cm)
# but it's easier to do it this way in case the confusion matrix is bigger than 2x2
# Or, alternatively
# err.test = sum(spam.test$yesno != pred)/length(spam.test$yesno)


# Cross validation

n = nrow(spam)
V = 10

# Equivalent to Rank = sample(n)
alea = runif(n)
Rank = rank(alea)

tail = n%/%V
block = (Rank-1)%/%tail + 1
# There is one observation whose block is 11, we dont want that
block[block==11]=10

block = as.factor(block)
err.cv = numeric(0)

# Perform the cross validation
for(k in 1:V){
  tree = rpart(yesno~., data=spam[block != k,], control=rpart.control(minsplit=1, cp=10^(-15)))
  pred = predict(tree, newdata = spam[block == k,], type="class")
  err = sum(spam$yesno[block == k] != pred)/length(spam$yesno[block == k])
  err.cv = rbind(err.cv, err)
}

# Understanding the complexity table

# Function to find the cp param
selection <- function(tree){
  pcp = printcp(tree)
  # Threshold = minimum(xerr) + it's std 
  min.xerr = which(pcp[,4] == min(pcp[,4]))
  t = pcp[min.xerr,4] + pcp[min.xerr,5]
  # There could be more than one value, so we choose the minimum
  threshold = t[which(t == min(t))[1]]
  
  # Multiply by 1 to turn booleans into ints
  a = (pcp[,4] <= threshold)*1
  cp = pcp[which(a == 1)[1],1]
}

# Now, prune the tree with the appropriate cp param
cp = selection(treemax)
tree.selected = prune(treemax, cp)

# Print all subtrees
P = printcp(treemax)
k = nrow(P)

# 2 because the root tree cannot be printed
for (i in 2:k){
  cp = P[k -i +2, 1]
  T = prune(treemax,cp)
  X11()
  plot(T)
}

# Me quiero ir a casa ya
T = rpart(yesno~., data=spam)
plot(T)
text(T)
summary(T)

