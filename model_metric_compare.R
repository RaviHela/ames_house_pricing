get_model_metrics <- function(model, df){
  
  #model_type <- str_replace(class(model)[[1]] , "_", "")
  model_type <- model[[2]]
  
  #print(model)
  # class(model)
  # print(model[[1]])
  # print(model[[2]])
  
  #print(deparse(substitute(model)))
  
  #print(names({{model}}))
  df %>%
    bind_cols(predict(model[[1]], df)) %>%
    select(log_sale_price, predicted = .pred) %>%
    
    #add squared error
    mutate(
      sq_error_log = (predicted - log_sale_price) ^ 2,
      sale_price = exp(log_sale_price),
      predicted_sp = exp(predicted),
      sq_error = predicted_sp - sale_price
    ) %>%
    
    #plot erro
    #ggplot(aes(x = sq_error, y = predicted_lm)) + geom_point() + theme_light()
    mutate(truth = log(sale_price),
           estimate = log(predicted_sp)) %>%
    
    
    #get model metrics
    metrics(truth, estimate) %>%
    select(-.estimator) %>%
    mutate(m_type  = model_type) %>%
    filter(.metric == "rmse")
}