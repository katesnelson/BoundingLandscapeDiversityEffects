## Process Item #2b
## Description: Script to calculate configuration metrics for landscape rasters. 
  # Primary use is to calculate metrics for county landscape raster using reclassified CDL data.
## Authors: Kate Nelson
## Date: Last updated 01/03/2023




library(pacman)
p_load(tidyverse, sf, raster, terra, landscapemetrics, foreach, parallel, doParallel, tigris, cdlTools)



# Get Data ----

data(fips_codes)

fc <- as.data.frame(fips_codes) %>%
  dplyr::filter(!state_name %in% c("Alaska", "Hawaii","Puerto Rico", "American Samoa",
                                   "Guam", "Northern Mariana Islands", "U.S. Minor Outlying Islands",
                                   "U.S. Virgin Islands" )) #get all state fips codes except for these places

sc <- unique(fc$state_code)



yrs <- c(2008:2020)



counties <- tigris::counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL)



mask <- c(0, 81, -128) #mask only background, cloud, and missing data

ag_mask <- c(0, 61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 190, 195)



# Extraction for landscape metrics package for county bbox ----

beocat_cores<- 5


detectCores()
cl <- makeCluster(beocat_cores, cores=5) #this makes clusters 
registerDoParallel(cl)


 foreach(i=1:length(sc)) %do% { 
   lm_dat <- foreach(j=1:length(yrs), .combine = 'rbind',.errorhandling = "remove") %dopar% {
      
      library(pacman)
      p_load(tidyverse, sf, raster, terra, landscapemetrics, foreach, parallel, doParallel, cdlTools)
            
            state_fips <- sc[i]
            year <- yrs[j]
            ras_yr <- getCDL(state_fips, year, location = "CDLdata")[[1]] # get the land use data
            
            cty <- counties %>% dplyr::filter(STATEFP == state_fips)
            cty_list <- unique(cty$GEOID)
            
            
             x <- foreach(k = 1:length(cty_list), 
                      .combine = 'rbind') %do% {
                 
                        c <- cty[cty$GEOID == cty_list[k],] # this subsets the county boundaries to the bounding box of one county
                        c <- st_transform(c, st_crs(ras_yr))
                        r <- raster::crop(ras_yr, c) # now crop the raster to the county bounding box
                        r[r %in% mask] <- NA # set mask values to null so they don't influence landscape metric calculations
                        
                        ed <- lsm_l_ed(r)$value 
                        contag <- lsm_l_contag(r)$value 
                        mean_area <- lsm_l_area_mn(r)$value
                        lpi <- lsm_l_lpi(r)$value
                        shdi <- lsm_l_shdi(r)$value
                        sidi <- lsm_l_sidi(r)$value
                        rich <- lsm_l_pr(r)$value
                        rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
                        prd <- lsm_l_prd(r)$value
                
              
                final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty_list[k], year) #combines it with the name of the polygon
                 return(final)
                      }
            saveRDS(x, paste0("Metrics/CDL_",state_fips,"_bbox_all_", year,".rds"))
            return(x)
           }
           
         
           # gc()
          }

parallel::stopCluster(cl) 


## Now with ag mask ----

beocat_cores<- 5


files<-list.files("CDLdata/") #get list of CDL files
remove_files <- list.files("CDLdata/", pattern = ".tif.vat.dbf")
files <- files[!(files %in% remove_files)]
remove_files <- list.files("CDLdata/", pattern = "reclass")
files <- files[!(files %in% remove_files)]

n<-length(files) 
length(files) <- prod(dim(matrix(unlist(files), ncol = beocat_cores))) #pad the end of the list so we have an even matrix without recycling of values
files<- matrix(unlist(files), ncol=beocat_cores, byrow=TRUE) #break into a X x 20 matrix for beocat parallel runs

detectCores()
cl <- makeCluster(beocat_cores, cores=10) #this makes clusters 
registerDoParallel(cl)


foreach(i=1:nrow(files)) %do% { 
  lm_dat <- foreach(j=1:ncol(files), .combine = 'rbind',.errorhandling = "remove") %dopar% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel, cdlTools)
    
    state_fips <- substr(files[i,j],10,11)
    year <- substr(files[i,j],5,8)
    ras_yr <- raster(paste0("CDLdata/", files[i,j]))
    
    cty <- counties %>% dplyr::filter(STATEFP == state_fips)
    cty_list <- unique(cty$GEOID)
    
   x <- foreach(k = 1:length(cty_list), 
            .combine = 'rbind') %do% {
              
              c <- cty[cty$GEOID == cty_list[k],] # this subsets the county boundaries to the bounding box of one county
              c <- st_transform(c, st_crs(ras_yr))
              r <- raster::crop(ras_yr, c) # now crop the raster to the county bounding box
              r[r %in% ag_mask] <- NA # set mask values to null so they don't influence landscape metric calculations
              
              ed <- lsm_l_ed(r)$value 
              contag <- lsm_l_contag(r)$value 
              mean_area <- lsm_l_area_mn(r)$value
              lpi <- lsm_l_lpi(r)$value
              shdi <- lsm_l_shdi(r)$value
              sidi <- lsm_l_sidi(r)$value
              rich <- lsm_l_pr(r)$value
              rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
              prd <- lsm_l_prd(r)$value
              
              
              final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty_list[k], year) #combines it with the name of the polygon
              return(final)
            }
    saveRDS(x, paste0("Metrics/CDL_",state_fips,"_bbox_ag_", year,".rds"))
    return(x)
  }

}


parallel::stopCluster(cl) 





# Extraction for landscape metrics package for county boundaries ----

beocat_cores<- 5



files<-list.files("CDLdata/") #get list of CDL files
remove_files <- list.files("CDLdata/", pattern = ".tif.vat.dbf")
files <- files[!(files %in% remove_files)]
remove_files <- list.files("CDLdata/", pattern = "reclass")
files <- files[!(files %in% remove_files)]

n<-length(files) #more than 1,000
length(files) <- prod(dim(matrix(unlist(files), ncol = beocat_cores))) #pad the end of the list so we have an even matrix without recycling of values
files<- matrix(unlist(files), ncol=beocat_cores, byrow=TRUE) #break into a X x 20 matrix for beocat parallel runs

detectCores()
cl <- makeCluster(beocat_cores, cores=10) #this makes clusters --> looks like number of clusters needs to equal number of cores specified in session start-up
registerDoParallel(cl)


foreach(i=111:nrow(files)) %do% { 
  lm_dat <- foreach(j=1:ncol(files), .combine = 'rbind',.errorhandling = "remove") %dopar% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel, cdlTools)
    
    state_fips <- substr(files[i,j],10,11)
    year <- substr(files[i,j],5,8)
    ras_yr <- raster(paste0("CDLdata/", files[i,j]))
    
    cty <- counties %>% dplyr::filter(STATEFP == state_fips)
    cty_list <- unique(cty$GEOID)
    
    x <- foreach(k = 1:length(cty_list), 
            .combine = 'rbind') %do% {
              
              c <- cty[cty$GEOID == cty_list[k],] # this subsets the county boundaries to the bounding box of one county
              c <- st_transform(c, st_crs(ras_yr))
              r <- raster::crop(ras_yr, c) # now crop the raster to the county bounding box
              r <- raster::mask(r, c) # now crop the raster to the county bounding box
              r[r %in% mask] <- NA # set mask values to null so they don't influence landscape metric calculations
              
              ed <- lsm_l_ed(r)$value 
              contag <- lsm_l_contag(r)$value 
              mean_area <- lsm_l_area_mn(r)$value
              lpi <- lsm_l_lpi(r)$value
              shdi <- lsm_l_shdi(r)$value
              sidi <- lsm_l_sidi(r)$value
              rich <- lsm_l_pr(r)$value
              rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
              prd <- lsm_l_prd(r)$value
              
              
              final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty_list[k], year) #combines it with the name of the polygon
              return(final)
            }
    saveRDS(x, paste0("Metrics/CDL_",state_fips,"_bdry_all_", year,".rds"))
    return(x)
  }

}


parallel::stopCluster(cl) 


## Now with ag mask ----


beocat_cores<- 5


files<-list.files("CDLdata/") #get list of CDL files
remove_files <- list.files("CDLdata/", pattern = ".tif.vat.dbf")
files <- files[!(files %in% remove_files)]
remove_files <- list.files("CDLdata/", pattern = "reclass")
files <- files[!(files %in% remove_files)]

n<-length(files) #more than 1,000
length(files) <- prod(dim(matrix(unlist(files), ncol = beocat_cores))) #pad the end of the list so we have an even matrix without recycling of values
files<- matrix(unlist(files), ncol=beocat_cores, byrow=TRUE) #break into a X x 20 matrix for beocat parallel runs

detectCores()
cl <- makeCluster(beocat_cores, cores=10) #this makes clusters --> looks like number of clusters needs to equal number of cores specified in session start-up
registerDoParallel(cl)


foreach(i=35:nrow(files)) %do% { #011923 stopped after row 6, restart at i = 7, 021023 stopped at row 46, restart i= 47, 021023 stopped at row 53, restart i= 54
  lm_dat <- foreach(j=1:ncol(files), .combine = 'rbind',.errorhandling = "remove") %dopar% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel, cdlTools)
    
    state_fips <- substr(files[i,j],10,11)
    year <- substr(files[i,j],5,8)
    ras_yr <- raster(paste0("CDLdata/", files[i,j]))
    
    cty <- counties %>% dplyr::filter(STATEFP == state_fips)
    cty_list <- unique(cty$GEOID)
    
   x <- foreach(k = 1:length(cty_list), 
            .combine = 'rbind') %do% {
              
              c <- cty[cty$GEOID == cty_list[k],] # this subsets the county boundaries to the bounding box of one county
              c <- st_transform(c, st_crs(ras_yr))
              r <- raster::crop(ras_yr, c) # now crop the raster to the county bounding box
              r <- raster::mask(r, c) # now crop the raster to the county bounding box
              r[r %in% ag_mask] <- NA # set mask values to null so they don't influence landscape metric calculations
              
              ed <- lsm_l_ed(r)$value 
              contag <- lsm_l_contag(r)$value 
              mean_area <- lsm_l_area_mn(r)$value
              lpi <- lsm_l_lpi(r)$value
              shdi <- lsm_l_shdi(r)$value
              sidi <- lsm_l_sidi(r)$value
              rich <- lsm_l_pr(r)$value
              rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
              prd <- lsm_l_prd(r)$value
              
              
              final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty_list[k], year) #combines it with the name of the polygon
              return(final)
            }
    saveRDS(x, paste0("Metrics/CDL_",state_fips,"_bdry_ag_", year,".rds"))
    return(x)
  }

}



parallel::stopCluster(cl) 
