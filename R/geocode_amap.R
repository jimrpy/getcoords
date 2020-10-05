# Get amap lat lon coordinates
#' @title Get the GCJ02 Coordinates Parsed by Amap
#' @param location Location name, vector, maximum length is 10.
#' @param key Application key of amap.com.
#' @importFrom magrittr `%>%`
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr separate
#' @importFrom tibble as_tibble
#' @return A tibble with 4 variables, \code{location}, location parsed, longitude, latitude.
#'
geocode_amap_base <- function(location, key = key){
  api = "https://restapi.amap.com/v3/geocode/geo?address="
  key = Sys.getenv("key_amap_tmp") ## Your key in amap app
  location2char <- paste(location, collapse = "|")
  url <- paste0(api, location2char, "&batch=true&key=", key)
  try(tmp <- jsonlite::fromJSON(txt = url))
  df <- data.frame(loc.input = location,
                   loc.parse = tmp$geocodes$formatted_address,
                   lonlat = tmp$geocodes$location) %>%
    tidyr::separate(col = lonlat,
                    into = c("lon", "lat"),
                    sep = ",",
                    convert = TRUE) %>%
    tibble::as_tibble()
  return(df)
}


#' @title Get GCJ02 Coordinates of Large Vector of Locations
#' @description Give a large number of \code{location} vector, the GCJ02 coordinates retrieved from amap.
#'
#' @param location Location name, vector, for more than 10 elements.
#' @param key Application key of amap.com.
#' @importFrom purrr map_dfr
#' @return A combined tibble with 4 variables, \code{location}, location parsed, longitude, latitude.
#'

geocode_amap <- function(location, key = key){
  locationlst <- chunk(x = location, n = 10)
  df <- purrr::map_dfr(.x = locationlst,
                       .f = ~geocode_amap_base(location = .x))
  return(df)
}

