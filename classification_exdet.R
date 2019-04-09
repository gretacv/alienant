#'Classification of the climatic similarity using the proportion of climate analogues an ACBR has of another and viceversa
#'
#' @param exdet_list A list of data frames. Each element of the list contains the coordinate of each point in the projection ACBR and a field with the result from exdet.



classification_exdet=function(exdet_list=NULL, field_exdet=NULL){

  dfCount_hum= ldply(exdet_list, function(x) length(which(x[,grep(field_exdet,names(x))]>=0 & x[,grep(field_exdet,names(x))]<=1)))
  names(dfCount_hum)=c("ACBRpair", "exdet")


  dfProp_hum=ldply(exdet_list, function(x)
    length(which(x[,grep(field_exdet,names(x))]>=0 & x[,grep(field_exdet,names(x))]<=1))/length(x[,grep(field_exdet,names(x))])) #divided by the area with human impact
  names(dfProp_hum)=c("ACBRpair","exdet")

  #first i need two new columns, one with ACBRx another with ACBRy and fill them up using the split string function
  split_pairs_hum=strsplit(dfProp_hum$ACBRpair, "_")
  ACBRx_hum=as.character(unlist(lapply(split_pairs_hum, function(x) x[3])))
  ACBRy_hum=as.character(unlist(lapply(split_pairs_hum, function(x) x[5])))
  dflongProp_hum=data.frame(ACBRx_hum, ACBRy_hum,dfProp_hum$exdet)



  dfCopy_hum=dflongProp_hum
  names(dfCopy_hum)=c("ACBR_A","ACBR_B","CA_B")
  toDelete_hum=as.numeric(vector())
  dfRes_hum=data.frame()
  loops=1:dim(dfCopy_hum)[1]

  for (i in 1:dim(dfCopy_hum)[1])
  {
    if(i%in%loops)
    {
      x=dfCopy_hum[i,]
      toDelete_hum = which(
        as.character(dfCopy_hum$ACBR_B) == as.character(x$ACBR_A) &
          as.character(dfCopy_hum$ACBR_A) == as.character(x$ACBR_B)
      )
      if(length(toDelete_hum)>0)
      {
        loops = loops[-which(loops == toDelete_hum)]
        dfRes_hum = rbind(dfRes_hum, cbind(dfCopy_hum[i, ], dfCopy_hum$CA_B[toDelete_hum]))
      }

      #print(i)
    }

  }

  names(dfRes_hum)[4]="CA_A"
  row.names(dfRes_hum)=paste(dfRes_hum$ACBR_A,dfRes_hum$ACBR_B,sep="_")
  dfRes_hum$id=row.names(dfRes_hum)
  dfRes_hum$CA_B[which(is.na(dfRes_hum$CA_B))]=0
  dfRes_hum$CA_A[which(is.na(dfRes_hum$CA_A))]=0
  res_score_hum=as.numeric(vector())

  for(i in 1:dim(dfRes_hum)[1])
  {
    x1=dfRes_hum$CA_A[i]
    x2=dfRes_hum$CA_B[i]

    #low, F
    if (x1<=1/3){x1.score=1}
    if (x2<=1/3){x2.score=1}

    #null
    if (x1==0){x1.score=0}
    if (x2==0){x2.score=0}

    #medium, D
    if (x1>1/3&x1<=2/3){x1.score=3}
    if (x2>1/3&x2<=2/3){x2.score=3}

    #high, A
    if (x1>2/3){x1.score=6}
    if (x2>2/3){x2.score=6}

    if(x1.score==x2.score){res_score_hum[i]=x1.score}
    else{
      if((x1.score ==0&x2.score!=0)|(x2.score==0&x1.score!=0)) {
        if((x1.score ==0&x2.score==1)|(x2.score==0&x1.score==1)) {res_score_hum[i]=1} #low, F
        if((x1.score ==0&x2.score==3)|(x2.score==0&x1.score==3)) {res_score_hum[i]=2} #medium-low, E
        if((x1.score ==0&x2.score==6)|(x2.score==0&x1.score==6)) {res_score_hum[i]=4} #high unilateral, C
      }

      if((x1.score ==1&x2.score==3)|(x2.score==1&x1.score==3)) {res_score_hum[i]=2} #medium-low, E
      if((x1.score ==1&x2.score==6)|(x2.score==1&x1.score==6)) {res_score_hum[i]=4} #high unilateral, C
      if((x1.score ==3&x2.score==6)|(x2.score==3&x1.score==6)) {res_score_hum[i]=5} #medium-high, B

    }
  }

  dfRes_hum$score=res_score_hum

  return(dfRes_hum)
}









