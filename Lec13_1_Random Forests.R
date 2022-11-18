  # source code from James et al. (2013)
  
  # 1. Fitting Classification Trees
  library(tree)
  library(ISLR)
  # data set (sales of child car seats at 400 different stores)
  attach(Carseats)
  Carseats$High = ifelse(Sales <= 8, "No", "Yes")  # high vs low sales
  Carseats$High = as.factor(Carseats$High)

  # use tree() to fit a calssification tree
  tree.carseats =tree(High~.-Sales, Carseats) # use all variables but Sales
  summary(tree.carseats)
  #plot tree
  windows(width = 100, height = 50)
  par(mfrow=c(1,1))
  plot(tree.carseats)
  text(tree.carseats, pretty =0)
  tree.carseats
  
  # Partition training and testing data 
  set.seed(2)
  train=sample (1: nrow(Carseats ), 200)
  Carseats.test=Carseats [-train ,]
  High.test=Carseats$High[-train]
  # train and test
  tree.carseats =tree(High~.-Sales, Carseats, subset=train)
  tree.pred=predict(tree.carseats, Carseats.test, type="class")
  # evaluate the performance of DT
  table(tree.pred, High.test)
  accuracy <- (104+50)/200
  accuracy
  mean(tree.pred==High.test)
  
  # pruning the tree by using cross-validation function cv.tree()
  # determine the optimal level of tree complexity based on cost complexity pruning (alpha)
  set.seed(3)
  cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
  names(cv.carseats)  # k denote alpha in cost complexity pruning
  cv.carseats
  par(mfrow=c(1,2))
  plot(cv.carseats$size, cv.carseats$dev, type="b")  #size = 8
  plot(cv.carseats$k, cv.carseats$dev, type="b")   # alpha = 1.4
  # use prune.misclass() to prune the tree to obtain nine-node tree
  prune.carseats = prune.misclass (tree.carseats, best=9)
  par(mfrow=c(1,1))
  plot(prune.carseats)
  text(prune.carseats, pretty =0)
  
  #evaluate the performance of pruned tree
  tree.pred=predict(prune.carseats, Carseats.test, type="class")
  table(tree.pred, High.test)
  mean(tree.pred==High.test)
  accuracy <- (97+59)/200
  accuracy
  
  # 2. Fitting Regression Trees
  
  # use Boston data set
  library(MASS)
  set.seed(1)
  train = sample (1:nrow(Boston), nrow(Boston)/2)
  tree.boston=tree(medv~.,Boston, subset=train)
  summary(tree.boston)
  plot(tree.boston)
  text(tree.boston, pretty =0)
  
  # pruning the tree by using cross-validation function cv.tree()
  cv.boston=cv.tree(tree.boston)
  plot(cv.boston$size, cv.boston$dev, type='b')
  prune.boston=prune.tree(tree.boston, best=5)
  plot(prune.boston)
  text(prune.boston, pretty = 0)
  
  yhat=predict(tree.boston, newdata=Boston[-train,])
  boston.test=Boston[-train ,"medv"]
  plot(boston.test,yhat)
  abline (0,1)
  mean((yhat -boston.test)^2)
  
  # 3. Bagging and Random Forests
  # Bagging (is just m = p for RF)
  library(randomForest)
  set.seed(1)
  bag.boston= randomForest(medv~.,data=Boston, subset=train,
                            mtry=13,importance =TRUE)
  bag.boston
  
  yhat.bag = predict (bag.boston, newdata=Boston[-train,])
  plot(boston.test, yhat.bag); abline (0,1)
  mean((yhat.bag -boston.test)^2)
  
  # Random Forests (change mtry=6)
  set.seed(6)
  rf.boston= randomForest(medv~.,data=Boston, subset=train,
                           mtry=6,importance =TRUE)
  yhat.rf = predict (rf.boston, newdata=Boston[-train,])
  plot(boston.test, yhat.rf); abline (0,1)
  mean((yhat.rf -boston.test)^2)
  
  # Use importance() function to view the importance of each variable
  importance (rf.boston)
  varImpPlot (rf.boston)
  
  # 4. Boosting using gbm()
  library(gbm)
  set.seed(1)
  boost.boston=gbm(medv~.,data=Boston[train,], distribution=
                     "gaussian", n.trees=5000, interaction.depth=4)
  summary(boost.boston)
  
  # partial dependence plots
  par(mfrow=c(1,2))
  plot(boost.boston, i="rm")
  plot(boost.boston, i="lstat")
  
  yhat.boost=predict (boost.boston, newdata =Boston[-train ,],
                      n.trees=5000)
  par(mfrow=c(1,1))
  plot(boston.test, yhat.boost); abline (0,1)
  mean((yhat.boost - boston.test)^2)
  
  # use different lambda (default = 0.001)
  boost.boston=gbm(medv~.,data=Boston[train,], distribution=
                     "gaussian", n.trees=5000, interaction.depth=4,
                   shrinkage=0.2, verbose=F)
  mean((yhat.boost - boston.test)^2)
