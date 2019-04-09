#'Prepare the climatic data
#'
#' @param wd A character vector of full path names where the bioclimatic variables are located; the default corresponds to the working directory
#' @param bioVar A character vector with the names of bioclimatic variables, e.g. bio4
#' @param dis_fact An integer specifying the disaggregation factor. Defaults to 50.

prep_clim<-function(wd=".", bioVar=NULL, dis_fact=50){

  clim_stack=stack()

  for (f in 1:length(bioVar)){
    bioFile=grep(pattern = bioVar[f],x = list.files(path=wd))
    clim_stack= stack(clim_stack, paste(wd, list.files(wd)[bioFile],sep = "/"))
  }


  names(clim_stack)=paste(bioVar)
  #crop using extent(acbr_poly)
  merraClim_ACBR=crop(clim_stack, extent(acbr_poly))



  acbr_poly=data(acbr_poly)
  acbrID=unique(acbr_poly@data$ ACBR_ID)

  mBios_raster=list()

  for(i in 1: length(acbrID))
  {
    acbr=acbrID[i]
    s_poly=acbr_poly[acbr_poly@data$ ACBR_ID==acbr,]
    acbrCrop=crop(merraClim_ACBR, extent(s_poly))
    acbrDis=disaggregate(acbrCrop, dis_fact)
    m=mask(acbrDis, s_poly)
    mACBR=raster(m,1)
    names(mACBR)="acbr_id"
    values(mACBR)[which(!is.na(values(mACBR)))]=acbr
    stackm=stack(m,mACBR)
    mBios_raster[[i]]=stackm
    names(mBios_raster)[i]=paste("ACBR_", acbr, sep="")
    print(paste("loop", i, "acbrId",acbr, "done"))
  }

  mBios_points=lapply(mBios_raster, rasterToPoints)
  allPoints=plyr::ldply(mBios_points)
  return(allPoints)

}
