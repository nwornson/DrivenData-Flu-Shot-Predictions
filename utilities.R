## useful functions

load_data = function(){
  train = read.csv('train_features_scaled_imputed.csv',stringsAsFactors = TRUE)
  train_respondent = train$respondent_id
  train = train[-1]

  labels = read.csv('training_set_labels.csv')

  test = read.csv('test_features_scaled_imputed.csv',stringsAsFactors = TRUE)
  test_respondent = test$respondent_id
  test = test[-1]


  train_w_lab = cbind(train,labels[,-1])

  return(list(data=train_w_lab,
              test=test,
              data_id=train_respondent,
              test_id=test_respondent))

}

train_val_split = function(data,prop){

  size = floor(prop * nrow(data))
  idx = sample(nrow(data),size)

  split_train = data[idx,]
  val = data[-idx,]

  return(list(train = split_train,
         val = val))
}


cv.lasso.best = function(X,y,X_val,y_val,alpha){
  
  
  cv.lasso = cv.glmnet(X,y,
                         alpha = alpha,
                         family = 'binomial') 
  
  opto.model = glmnet(X,y,
                        alpha = alpha, 
                        family = "binomial",
                        lambda = cv.lasso$lambda.min)
  
  cv.lasso.probs = opto.model %>% 
    predict(newx = X_val,type = 'response')
  
  score = auc(as.numeric(y_val),as.numeric(cv.lasso.probs))
  
  
  return(list(score=score,lambda=cv.lasso$lambda.min))
  
}


bplot_fill = function(data,x,y,fill,xlab = x,ylab=y,...){
  ggplot(data,aes(!!sym(x),!!sym(y),fill= !!sym(fill))) +
    geom_bar(stat='identity',position='dodge') +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    ylab(ylab) + xlab(xlab) +
    #guides(fill=guide_legend(title = 'Treatment')) +
    theme(panel.grid = element_blank()) +
    theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
          panel.background = element_rect(colour = "black", fill = "white"), 
          strip.text = element_text(size=10)) 
  
}


