## Get WGS84 Coordinates

#' @title Get the Coordinates \code{WGS84}
#' @description Get the real WGS84 coordinates. That is, the coordinates from amap, tencent, baidu are not the real coordinates, which should convert to WGS84 for geocomputation.
#' @param location Location name in China characters, can be a character or a vector.
#' @param key Application key of amap.com.
#' @importFrom dplyr mutate select
#' @importFrom magrittr `%>%`
#' @importFrom tidyr separate
#' @importFrom purrr map2_chr
#' @return A tibble with 4 variables, \code{location}, location parsed, longitude, latitude.
#' @export
#'
#' @examples
#' geocode(location = sample_data)
#'
#'
geocode <- function(location, key){
  tmp <- geocode_amap(location = location, key = key)
  df <- tmp %>%
    dplyr::mutate(lonlat = purrr::map2_chr(.x = lon, .y = lat, ~gcj2wgs(lon = .x, lat = .y))) %>%
    dplyr::select(-c("lon", "lat")) %>%
    tidyr::separate(col = lonlat, into = c("lon", "lat"), sep = ",", convert = TRUE)
  return(df)
}
