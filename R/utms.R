#' @name Assign UTM Zone
#' @title Assign UTM Zones
#' @description Assigns UTM Zone to given Lat Lon pair
#' @param lat, lon and utm config dataframe. UTM config for Northern Hemisphere is provided.
#' @details For assigning UTM Zones it does [lat.min, lat.max] & [lon.min, lon.max)
assign.utm.zone <- function( lat, lon, utm.config ){ 
  d.utm <- lapply( split( utm.config, utm.config$zone.number ), function( x){
    rn <-  which( lat >= x$lat.min & lat <= x$lat.max & lon >= x$lon.min & lon < x$lon.max )
    ifelse( length( rn ) > 0, return( cbind( rn = rn, utm = x$zone.number ) ), return( NULL ) )
  })
  d.utm <- d.utm[ sapply( d.utm, function(x) !is.null( x ) ) ]
  d.utm.df <- Reduce( function(...) merge(..., all = T ), d.utm )
  d.utm.df <- d.utm.df[ order( d.utm.df[ , 'rn' ] ), ]
  return( d.utm.df[ , 'utm' ] )
}


#' @name Transform GPS Coords to UTM Coords
#' @title Transform GPS Coordinates to UTM Coordinates
#' @description Transforms GPS Coordinates to UTM Coordinates (East, North)
#' @param lat, lon and utm config dataframe. UTM config for Northern Hemisphere is provided.
transform.gps.to.utm.coords <- function( d , utm.config ){ 
  suppressPackageStartupMessages( require( rgdal ) )
  utm.coords.df <- Reduce( function(...) merge( ..., all = T ), lapply( split( d , d$utm ), function(x){
      return( cbind( rn = as.numeric( row.names( x ) ),
                     as.data.frame( coordinates( spTransform( x = SpatialPoints( coords = x[ , 1:2 ], proj4string = CRS( '+proj=longlat +datum=WGS84' ) ),
                                                              CRSobj = CRS( as.character( utm.config$proj4string[ x$utm[[1]] ] ) ) ) ) ), row.names = NULL ) )
  } ) )
  utm.coords.df <- utm.coords.df[ order( utm.coords.df$rn ), ]
  names( utm.coords.df )[ 2:3 ] <- c( 'east', 'north' )
  return( utm.coords.df[ , c( 'east', 'north' ) ] )
}

#' @name Get UTM Coords
#' @title Get UTM Coordinates
#' @description Run Assign UTM Zone and Transform GPS
#' @param Parse lat and lon
get.utm.coords <- function( lat, lon ){
  utm.config <- data.frame( 'zone.number' = c( 1:60 ),
                            'lat.min' = c( rep( 0 , times = 60 ) ),
                            'lat.max' = c( rep( 84 , times = 60 ) ),
                            'lon.min' = c( seq( from = - 180, to = -6, by = 6 ), seq( from = 0, to = 174, by = 6 ) ),
                            'lon.max' = c( seq( from = - 174, to = 0, by = 6 ), seq( from = 6, to = 180, by = 6 ) ) )
  utm.config$proj4string <- do.call( what = paste0, args = list( '+proj=utm +zone=', 
                                                                 utm.config$zone.number,
                                                                 ' +ellps=WGS84 +datum=WGS84 +units=m +no_defs') )
  d <- data.frame( lat = lat, lon = lon, utm = 0, east = 0, north = 0 )
  d$utm <- assign.utm.zone( lat, lon, utm.config )
  d[ , c( 'east', 'north' ) ] <- transform.gps.to.utm.coords( d = d[ , c( 'lon', 'lat', 'utm' ) ], utm.config )
  return( d[ , c( 'utm', 'east', 'north' ) ] )
}