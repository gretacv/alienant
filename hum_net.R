#'Human network
#'
#' @param PartiesByACBRs_df A dataframe [n, m] n i is the number of ACBRs and m the number of parties. rownames should refer

hum_net=function(PartiesByACBRs_df=NULL){
  df1.mt <- as.matrix(t(PartiesByACBRs_df))
  Human_mat <- t(df1.mt) %*% df1.mt
  diag(Human_mat)=0
  human_mat_TF=ifelse(Human_mat>0,1,0)
  netHuman=graph.adjacency(Human_mat,mode="undirected",weighted=TRUE,diag=FALSE)
  return(netHuman)
}
