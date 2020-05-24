#function to help filter corelated variables
split_arrange_names <- function(x){
  
  return(paste0(sort(unlist(str_split(x, "_"))), collapse = ""))
}

#get simplifeid correlation dataframe
get_corr_df_simple <- function(df, cut_off = NA) {
  
  simpl_df <-
    df %>% select_if(is.numeric) %>% correlate(use = "pairwise.complete.obs") %>%
    pivot_longer(
      -rowname,
      names_to = "features",
      values_to = "corr",
      values_drop_na = T
    ) %>%
    mutate(t1 = paste(rowname, features, sep = "_")) %>%
    mutate(t2 = sapply(t1, split_arrange_names)) %>%
    arrange(t2) %>%
    group_by(t2) %>%
    mutate(rank = row_number(t2)) %>%
    ungroup %>% filter(rank == 1) %>%
    select(rowname, features, corr) %>%
    arrange(desc(abs(corr))) 
  
  if(is.na(cut_off)){
    return(simpl_df)
  }else{
    
    return(simpl_df %>% filter(abs(corr) >= cut_off))
  }
  
}