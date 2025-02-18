require(dplyr)
require(sf)
require(terra)

#' Rasterise vector data and convert to x,y,z 
#'
#' @description
#' uses a raster mask to rasterise vector (e.g. point or polygon) data.
#' before converting to a dataframe containing x,y,value. The dataframe can be 
#' returned by the function  and/or saved as csv.
#'
#' @details
#' Typically the vector data has been read from a shape file or geodatabase
#' from a shape file, using sf::st_read().
#' 
#' The vector data is transformed to the same coordinate reference system as the
#' raster mask before being rasterised. The resulting raster will contain 
#' values taken from the first variable (column) in the shape (spatial data)
#' same. The user can specify the name of another column to be used: 
#' `variable`(see below)
#' 
#' By default the resulting raster data will be returned as a dataframe
#' containing x, y, value columns. If the user specified `return_df = FALSE``
#' when calling the function, then the function will not return a dataframe.
#' Instead the raster will be returned.
#' 
#' The user can also specify the name of a csv file `filecsv`. If this is 
#' specified then the results will be saved to the file.
#'
#' @param rmask     a raster containing 1 layer. This raster shows which 
#'                  cells should contain values when the data is rasterised
#'                  if the raster contains more than than one layer, only
#'                  the first layer will be used. The function assumes that
#'                  raster cells containing `NA`values are outside the area
#'                  of interest or are land cells. Cells containing values 
#'                  other than `NA` will contain a value in the output raster. 
#' 
#' @param shp       the vector data (spatial data frame) which contains
#'                  data to be rasterised to the raster mask `rmask`   
#' 
#' @param variable  character name of the column (variable) in the 
#'                  vector input `shp`. If no variable is specified, then
#'                  the function uses the first variable (column) to find 
#'                  the values returned
#' 
#' @param filecsv   character - specifying filename/path of for a csv file
#'                  if no file is specified (default), then the function
#'                  will not save the xyz data to file
#'                    
#' @param return_df boolean, default = TRUE. If TRUE, then the function
#'                  returns a dataframe of x,y,value data (see below). 
#'                  If FALSE, then the function returns NULL
#'
#' @return a dataframe with 3 columns
#'  * `x` :  x-coordinate of point
#'  * `y` : y-coordinate of point
#'  * `value` - value of raster at point x,y
#'  
#'  x,y are given in the same coordinate reference system as 
#'  the input raster mask `rmask` 
#'  
#'  If `return_df = FALSE`then the function returns the number of non-NA
#'  cells found after rasterising.
#'
#' @examples
#'
#' # example 1 - get dataframe of xyz values
#' df <-rasterise_mm(r_study_area, shp)
#'
#' # example 2 - rasterise, saving xyz values to csv file
#' rasterise_mm(r_study_area, shp, filecsv="xyz_data.csv", return_df=FALSE)

#' @export

rasterise_mm <- function(rmask, 
                         shp, 
                         variable=NA_character_, 
                         filecsv=NA_character_, 
                         return_df=TRUE){

  # ---- dataframe of x,y values from the area raster ----
  
  df_mask <- rmask %>%
    terra::xyFromCell(1:(terra::ncell(rmask))) %>%
    as.data.frame()
  
  vals <- rmask %>% 
    terra::values(na.rm=F, dataframe=T)
  
  names(vals) <- "area"
  
  df_mask <- df_mask %>%
    bind_cols(vals)
  
  # df_mask now has all coordinates for the square area containing
  # the study area - the marine cells we are interested in have a 
  # value of 1 and other cells have NA values
  # (land or outside the study area) 
  
  if(!is.na(variable)){
    # if a variable name is specified, select this variable:
    shp <- shp %>%
      select(all_of(c(variable)))
  }else{
    # otherwise select the first variable
    shp <- shp %>%
      select(1)
    variable <- ""
  }
  
  shp_vals <- shp %>% 
    sf::st_drop_geometry()
  
  if(!is.numeric(shp_vals[,1])){
    # a non-numeric column was selected replace the values with 1
    shp[,1] <- 1
  }
  
  # transform shp to same CRS as the area raster, r (UTM33)
  shp <- shp %>%
    sf::st_transform(crs=sf::st_crs(rmask))
  
  # rasterise the the shp file
  r <- terra::rasterize(shp, rmask, field=variable)
  
  # count the number of non-NA values
  n <- r %>% 
    terra::values(na.rm=T, dataframe=T) %>%
    nrow()
  
  if(n>0){
    # get a dataframe of x,y,values
    df <- r %>% 
      terra::values(na.rm=F, dataframe=T)
    
    names(df) <- "value"
    
    df <- df_mask %>%
      bind_cols(df)
    
    # remove the rows from df which are not in the area
    df <- df %>%
      filter(!is.na(area)) %>%
      select(-area)
    
    # now we have grid cells only for water in the study area
    
    # replace any NA values with 0
    df <- df %>%
      mutate(value=ifelse(is.na(value),0,value))
    
    if(!is.na(filecsv)){
      foldercsv <- dirname(filecsv)
      if(!dir.exists(foldercsv)){
        dir.create(foldercsv)
      }
      write.table(df, file=filecsv, sep=",", quote = F,
                  fileEncoding = "UTF-8", row.names=F)
    }

  }else{
    # the rasterized data contains only NA values
    # there were no overlapping data
    cat("no data in this area\n")
    
    # set df to be empty dataframe
    df <- data.frame()
    n <- 0
  }
  
  if(return_df==TRUE & nrow(df)>0){
      return(df)
  }else{
    return(n)
  }
    
}
  