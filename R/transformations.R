# Coordinate Reference System (CRS class in sp package)
# The CRS is stored in the 'proj4string' in the spatial object
# Elements of the proj4string:
# +init     e.g. "+init=EPSG:32633"
# +proj     e.g. "+proj=longlat" or "+proj=utm"
# +ellps    e.g. "+ellps=WGS84"
# +datum    e.g. "+datum=WGS84"
# +zone     e.g. "+zone=33"
# +units    e.g. "+units=m"
# +no_defs  e.g. "no_defs" (no "=<somethig>")
# +towgs84  e.g. "+towgs84=0,0,0 "

# Some important epsg codes
getCodeEPSG_WGS84 <- function(){return(4326)}
getCodeEPSG_ETRS89 <- function()(return(4258))
getCodeEPSG_ED50 <- function(){return(4230)}

# Find valid CRS
getCRSclass <- function(epsg_code){
  string <- paste0("+init=epsg:",epsg_code)
  CRS_class <- sp::CRS(string)
  return(CRS_class)
}

# Provide epsg OR crs_class
getSpatialCoordinates <- function(xy, epsg_code = NULL, crs_class = NULL){
  spatial_xy <- sp::SpatialPoints(xy)
  if(is.null(crs_class)){
    crs_class <- getCRSclass(epsg_code)
  }
  sp::proj4string(spatial_xy) <- crs_class
  return(spatial_xy)
}

# TODO: also allow spatial object, check before.

#' Convert coordinate reference system
#'
#' @param xy lat lon coordinates
#' @param epsg_code_from code for coordinates given (make optional if already spatial object)
#' @param epsg_code_to the epsg_code that should be transformed into
#' @export
transform_latlon_to_different_CRS <- function(xy, epsg_code_from, epsg_code_to){
  spatial_coordinates <- getSpatialCoordinates(xy, epsg_code = epsg_code_from)
  new_coords <- sp::spTransform(spatial_coordinates, getCRSclass(epsg_code_to))
  return(new_coords)
}
