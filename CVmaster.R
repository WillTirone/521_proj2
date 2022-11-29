

#' Title
#'
#' @param classifier some text about classifier
#' @param train_feat 
#' @param train_labels 
#' @param folds 
#' @param loss 
#'
#' @return
#' @export
#'
#' @examples
CVmaster = function(classifier, train_feat, train_labels, folds=5, loss) {
  
  
  # setting fit type for use in predictions later
  if (fit$family$family == "binomial"){
    fit_type = 'response'
  } else {
    fit_type = ''
  }
  
  # data frame to output 
  output = data.frame(k = NA, loss = NA)
  
  # creating k folds 
  val = createFolds(train_labels, k = folds)
  
  for (i in 1:folds){
    
    # predict on validation set 
    predictions = predict(classifier,
                          data.frame(train_feat[val[[i]],]),
                          type=fit_type)
    
    # calculate an arbitrary loss function 
    L = loss(train_labels[val[[i]]], predictions)
    
    # then rbind 
    output = rbind(output, c(i,L))
  }
  
  return(output)
}