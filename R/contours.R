# TODO: documentation
# TODO: include CRS?
# TODO: does not work with both negative AND positive values
# Usefull for bathymetry
#' @param xyz A dataframe with column x, y, z data (long, lat, alt)
#' @param contour_values A numeric vector of z values to contour. If this argument is provided, the stacking order of the contours will be preserved in the order of first occurence within the supplied vector.
#' @return Returns a SpatialLinesDataFrame object (sp package) that can be used to draw all contour lines. All contour lines with a certain z-value can be found with 'contour_value' in the df.
get_spatial_contours <- function(xyz,
                                 contour_values = seq(from = -10, to = -200, by = -20)){
  # Calculate contours from potentially irregular xyz data.
  # returns dataframe with x, y, z, and Group.
  # z is only the values mentioned in contour_values
  # x, y are the coordinates of each point on the line forming the contour
  # group is a unique identifier for each independent contour path
  cont <- contoureR::getContourLines(x = xyz, levels = contour_values)

  # Create list: each element one contour level (z value)
  # There can be multiple contour groups with same z value
  # contour_10{
  #     group1{
  #       df(x, y)
  #      },
  #     group2{
  #       df(x, y)
  #      }
  # } etc.
  my_list <- list()
  for(i in 1:length(contour_values)){
    my_list[[i]] <- cont %>%
      dplyr::filter(z == contour_values[i]) %>%
      dplyr::group_by(Group) %>%
      dplyr::select(Group, x, y) %>%
      dplyr::group_split(keep = F)
    my_list[[i]] <- lapply(my_list[[i]], Line)
    my_list[[i]] <- sp::Lines(my_list[[i]], paste0("contour_",abs(contour_values[i])))
  }

  # Create 'SpatialLines' file
  my_contours <- sp::SpatialLines(my_list)

  # Create 'SpatialLinesDataFrame' object
  df <- data.frame(
    contour_value = abs(contour_values),
    # the unique identifiers for each 'Lines' object
    row.names = paste0("contour_",abs(contour_values))
  )
  my_contours_df <- sp::SpatialLinesDataFrame(my_contours, df)

  return(my_contours_df)
}
