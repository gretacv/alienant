#'Climatic network
#'
#' @param clim_class_df A dataframe with at least two columns with the ACBR numbers and column with the classified score


clim_net=function(clim_class_df=NULL, ACBR_A="ACBR_A", ACBR_B="ACBR_B", class_score="score"){

  sel_col=c(grep(ACBR_A, names(clim_class_df)), grep(ACBR_B, names(clim_class_df)), grep(class_score, names(clim_class_df)))
  df=subset(clim_class_df, select=c(sel_col))
  names(df)=c("ACBR_A", "ACBR_B", "score")


  hum_score_matrix=dcast(df,ACBR_A~ACBR_B,value.var = "score")



  hum_score_matrix=hum_score_matrix[order(as.numeric(as.character(hum_score_matrix$ACBR_A))),order(as.numeric(names(hum_score_matrix)),na.last = FALSE)]
  row.names(hum_score_matrix)=hum_score_matrix$ACBR_A
  hum_score_matrix=as.matrix(hum_score_matrix[,-1])


  hum_score_matrix_clean=matrix(nrow = 16,ncol=16, dimnames = list(1:16, 1:16))

  cols <- colnames(hum_score_matrix)[colnames(hum_score_matrix) %in% colnames(hum_score_matrix_clean)]
  rows <- rownames(hum_score_matrix)[rownames(hum_score_matrix) %in% rownames(hum_score_matrix_clean)]
  hum_score_matrix_clean[rows, cols] <- hum_score_matrix[rows, cols]

  hum_score_matrix_clean[lower.tri(hum_score_matrix_clean)] = t(hum_score_matrix_clean)[lower.tri(hum_score_matrix_clean)]

  diag(hum_score_matrix_clean)=0
  hum_score_matrix_clean[which(is.na(hum_score_matrix_clean))]=0

  net_humToHum=graph.adjacency(hum_score_matrix_clean,mode="undirected",weighted=TRUE,diag=FALSE)
  return(net_humToHum)

}
