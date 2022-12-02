
CVmaster = function(classifier, train_feat, train_labels, folds=5, loss,
                    fit_type="response") {
  
  # input: 
    # classifier : generic fit object 
    # train_feat : data frame object 
    # train : data frame object with y labels, has column named "label"
    # folds : number of k folds for cross validation 
    # loss : generic loss function: 
    # currently supports classification loss 
    # fit_type : type of fit, valid values : {"response","class"}
  
  # returns: 
    # output : data frame with columns c("k","loss"), i.e. loss at each 
    # fold k and not averaged over the k folds. To complete k-fold 
    # cross validation, take mean of output$loss 
  
  train_labels = train_labels$label
  
  # data frame to output 
  output = data.frame(k = NA, loss = NA)
  
  # creating k folds 
  val = createFolds(train_labels, k = folds)
  
  for (i in 1:folds){
    
    # predict on validation set 
    predictions = predict(classifier,
                          train_feat[val[[i]],],
                          type=fit_type)
    
    # from probability calc label 
    # need pred_labels to be -1 or 1 
    if (fit_type == 'response'){
      pred_labels = rep(-1, length(predictions))
      pred_labels[predictions > 0.5] = 1
    } else if (fit_type == 'class') {
      pred_labels = predictions$class
    } else {
      print("fit not supported")
    }
    
    # calculate an arbitrary loss function 
    L = loss(train_labels[val[[i]]], pred_labels)
    
    # then rbind 
    output = rbind(output, c(i,L))
    
  }
  
  # remove null row 
  return(output[-1,])
}