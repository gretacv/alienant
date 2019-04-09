#'Prepare the human foot print data
#'
#' @param HFP_thr An integer specifying the threshold to differenciate high from low human impact. Defaults to 30.
#' @param points_ex data frame with the coordinates of the points after the disaggregation and the ACBR information.Coordinates should be identified by x and y
#' @param saveFile logical, indicating if the reclassified raster should be saved. The default is FALSE.
#' @param wd A character vector of full path names where the reclassified Human Footprint raster is saved if saveFile=TRUE; the default corresponds to the working directory
#' @param resample logical, indicating if the reclassified information should be resampled to be at the same resolution than the climatic data. Default is TRUE

prep_hfp<-function(HFP_thr=30, points_ex=NULL,  saveFile=FALSE, wd=".", resample=TRUE, arcpy_wd= "C:/Python27/ArcGIS10.4"){
  m=c(0, HFP_thr, 1, HFP_thr, 100, 2)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  hfp30=reclassify(x=hfp, rcl= rclmat)

  if(saveFile==TRUE){
    writeRaster(hfp30, filename =  paste(wd,"hfp30.tif", sep="/"), format="GTiff")
  }

  hfp_ex=extract(hfp30,points_ex[,c("x","y")])
  points_ex=cbind(points_ex, hfp_ex)

  if (resample==TRUE){
    na_points=points_ex[which(is.na(hfp_ex)),]
    val_points=points_ex[which(!is.na(hfp_ex)),]

    write.csv(na_points, paste(wd,"na_points.csv", sep="/"))
    write.csv(val_points,paste(wd,"val_points.csv", sep="/"))

    resample_HFP(wd = wd, arcpy_wd = arcpy_wd)
    na_points_near=read.dbf("na_points_Layer_Layer.dbf")
    names(na_points_near)=c("na_FID", names(na_points), "NEAR_FID", "NEAR_DIST", "val_FID", "val_id", names(val_points))
    na_points_near_sel=subset(na_points_near, select=c(1:7,9:11,19))
    hfp_ex_resample=vector()
    hfp_ex_resample[which(is.na(hfp_ex))]=na_points_near_sel$hfp_ex
    hfp_ex_resample[which(!is.na(hfp_ex))]=val_points$hfp_ex

    points_ex$hfp_ex_resample=hfp_ex_resample
    points_ex$NEAR_DIST=NA
    points_ex$NEAR_DIST[which(is.na(hfp_ex))]=na_points_near_sel$NEAR_DIST
    points_ex$NEAR_DIST[which(is.na(points_ex$NEAR_DIST))]=0
  }

  return(points_ex)

}
