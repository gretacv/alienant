#'Calculate Exdet between ACBRs
#'
#' @param d A data frame with the climatic and human information
#' @param HFP_field A string identifying the column with the HFP classification. 1=low, 2=high. Defaults to hfp_ex_resample.
#' @param ACBR_field A string identifying the column that contains the ACBR id which are integers from 1 to 16
#' @param near_thr
#' @param near_field
#' @param bios
exdet_acbrs=function(d=NULL, HFP_field="hfp_ex_resample", ACBR_field="acbr_id", near_thr=1000, near_field="NEAR_DIST", bios=NULL){
  names(d)[grep(ACBR_field,names(d))]="acbr_id"
  names(d)[grep(HFP_field,names(d))]="hfp_ex_resample"
  names(d)[grep(near_field,names(d))]="NEAR_DIST"


  Bios4ExDet=list()
  for (i in 1:length(unique(d$acbr_id)))
  {
    ii=unique(d$acbr_id)[i]
    Bios4ExDet[[ii]]=d[d$acbr_id==ii,]
    names(Bios4ExDet)[ii]=paste("ACBR",ii,sep="_")

  }

  mBiosHUMAN=lapply(Bios4ExDet, function(x)x[x$hfp_ex_resample==2&x$NEAR_DIST<=near_thr,]) #2 is over 30 included

  res_exdet_Hum=list()
  l_flag=1
  for (i in 1:length(mBiosHUMAN))
  {tryCatch(

    {
      acbrRef=mBiosHUMAN[[i]]

      for (j in 1:length(mBiosHUMAN))
      {
        if(i!=j)
        {
          acbrProj=mBiosHUMAN[[j]]


          exdet_btw_i_j_4_10=ecospat.exdet(ref=acbrRef[,grep("bio", names(acbrRef))], p=acbrProj[,grep("bio", names(acbrProj))])

          res_exdet_btw_i_j=cbind(acbrProj[,grep("x",names(acbrProj))],
                                  acbrProj[,grep("y",names(acbrProj))],
                                  exdet_btw_i_j_4_10)

          names(res_exdet_btw_i_j)[3]=paste(grep(pattern="bio", x=names(acbrRef), value=TRUE), sep="_", collapse = "_")

          res_exdet_Hum[[l_flag]]=res_exdet_btw_i_j
          names(res_exdet_Hum)[l_flag]=paste("exdet", names(mBiosHUMAN)[i],names(mBiosHUMAN)[j], sep="_")
          l_flag=l_flag+1}
        #print(paste(i,j))
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e),i, "\n")})

  }

  return(res_exdet_Hum)
}
