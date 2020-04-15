context("Coordinate transformations")

martini_tower_coord_in_WGS84 <- data.frame(lon = 6.568215, lat = 53.21938)
martini_tower_coord_in_ED50 <-  data.frame(lon = 6.569523, lat = 53.22012)

test_that("Coordinates are transformed correctly", {
  expect_equal(
    coordinates(transform_latlon_to_different_CRS(
      martini_tower_coord_in_ED50,
      epsg_code_from = getCodeEPSG_ED50(),
      epsg_code_to = getCodeEPSG_WGS84()
    ))[1],
    as.double(martini_tower_coord_in_WGS84[1]),
    tolerance = 4e-06)
  expect_equal(
    coordinates(transform_latlon_to_different_CRS(
      martini_tower_coord_in_ED50,
      epsg_code_from = getCodeEPSG_ED50(),
      epsg_code_to = getCodeEPSG_WGS84()
    ))[2],
    as.double(martini_tower_coord_in_WGS84[2]),
    tolerance = 4e-06)
})
