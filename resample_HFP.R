#'Resample the human foot print data
#'
#' @param wd A character vector of full path names where the .csv files with the hfp data has been saved; the default corresponds to the working directory

resample_HFP=function(wd=".", arcpy_wd="C:/Python27/ArcGIS10.4"){
  use_python(arcpy_wd)
  arcpy=import("arcpy")

  path=r_to_py(wd)

  na_pointsCSV=r_to_py(paste(wd,"na_points.csv", sep="/"))

  step1=arcpy$MakeXYEventLayer_management(table=na_pointsCSV, in_x_field="x", in_y_field="y", out_layer="na_points_Layer", spatial_reference="PROJCS['South_Pole_Stereographic',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Stereographic'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',0.0],PARAMETER['Scale_Factor',1.0],PARAMETER['Latitude_Of_Origin',-90.0],UNIT['Meter',1.0]];-30548400 -30548400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision", in_z_field="")

  val_pointsCSV=r_to_py(paste(wd,"val_points.csv", sep="/"))
  step2=arcpy$MakeXYEventLayer_management(table=val_pointsCSV, in_x_field="x", in_y_field="y", out_layer="val_points_Layer", spatial_reference="PROJCS['South_Pole_Stereographic',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Stereographic'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',0.0],PARAMETER['Scale_Factor',1.0],PARAMETER['Latitude_Of_Origin',-90.0],UNIT['Meter',1.0]];-30548400 -30548400 10000;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision", in_z_field="")


  step3=arcpy$FeatureClassToShapefile_conversion(Input_Features="val_points_Layer;na_points_Layer", Output_Folder=path)


  arcpy$env.workspace=path
  naPointsSHP=r_to_py(paste(wd,"na_points_Layer.shp", sep="/"))

  step4=arcpy$MakeFeatureLayer_management(in_features=naPointsSHP, out_layer="na_points_Layer_Layer", where_clause="", workspace="", field_info="FID FID VISIBLE NONE;Shape Shape VISIBLE NONE;Field1 Field1 VISIBLE NONE;F_id F_id VISIBLE NONE;x x VISIBLE NONE;y y VISIBLE NONE;bio10 bio10 VISIBLE NONE;bio4 bio4 VISIBLE NONE;acbr_id acbr_id VISIBLE NONE;hfp_ex hfp_ex VISIBLE NONE")

  valPointsSHP=r_to_py(paste(wd,"val_points_Layer.shp", sep="/"))
  step5=arcpy$MakeFeatureLayer_management(in_features=valPointsSHP, out_layer="val_points_Layer_Layer", where_clause="", workspace="", field_info="FID FID VISIBLE NONE;Shape Shape VISIBLE NONE;Field1 Field1 VISIBLE NONE;F_id F_id VISIBLE NONE;x x VISIBLE NONE;y y VISIBLE NONE;bio10 bio10 VISIBLE NONE;bio4 bio4 VISIBLE NONE;acbr_id acbr_id VISIBLE NONE;hfp_ex hfp_ex VISIBLE NONE")

  step6=arcpy$Near_analysis(in_features="na_points_Layer_Layer", near_features="'val_points_Layer_Layer'", search_radius="", location="NO_LOCATION", angle="NO_ANGLE", method="PLANAR")
  step7=arcpy$AddJoin_management(in_layer_or_view="na_points_Layer_Layer", in_field="NEAR_FID", join_table="val_points_Layer_Layer", join_field="FID", join_type="KEEP_ALL")

  step8= arcpy$FeatureClassToShapefile_conversion(Input_Features="na_points_Layer_Layer", Output_Folder=path)


}
