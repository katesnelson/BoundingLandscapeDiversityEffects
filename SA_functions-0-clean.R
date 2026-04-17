## Supplementary Item - Process Item #0
# Author: Kate Nelson


# Key Functions


###function to spatial join by largest spatial overlap 


#requires a county boundary object and agroecosystem object

area.overlay<-function(CountyData, AgroEcosystemData, projection, savename){

  d1 <- CountyData
  
  if (st_crs(d1)[[2]]!= projection){
    d1 <- st_transform(d1, projection)
  }
  
  d1 <- d1[,c("GEOID","geometry")]
  
  
  d2 <- AgroEcosystemData
  d2 <- d2 %>% st_as_sf(.) %>% 
    st_transform(.,projection) #use an equal-area projection for U.S.
  
  #ONLY for Ag-diversity project --> build a unique AERCODE number for each AER polygon (so discontinuous areas with same AER class are considered to be different)
  d2$AERCODE <- seq(1:length(d2$US_L3CODE)) 
  d2 <- d2[,c("US_L3CODE","geometry")]
  
  #join Counties to Agro-Eco-Regions by assigning Counties to the AER with which they have the largest spatial overlap
  d1_d2 <- st_intersection(d1,d2) %>% 
    mutate(int_area = as.numeric(st_area(.))) %>% 
    arrange(., desc(int_area)) #sort from large to small intersection area
  
  cnty_AER <- distinct(d1_d2, GEOID, .keep_all=TRUE) #keep only the first record for each GEOID
  
  saveRDS(cnty_AER, paste0(wd,"/StartingData/",savename,".RDS"))

  return(cnty_AER)
}




###function to prep standardized data for modeling 


my.inla.group <-
function (x, n = 25, method = c("cut", "quantile"), idx.only = FALSE) 
{
  inla.group.core <- function(x, n = 25, method = c("cut", 
                                                    "quantile"), idx.only) {
    if (n < 1) {
      stop("Number of groups must be > 0")
    }
    if (n == 1) {
      return(rep(median(x), length(x)))
    }
    method <- match.arg(method)
    if (method == "cut") {
      a <- cut(x, n)
    }
    else {
      aq <- unique(quantile(x, probs = seq(0,1, by = 100/n/100), na.rm = T))
      a <- cut(x, breaks = as.numeric(aq), include.lowest = TRUE)
    }
    nlev <- nlevels(a)
    xx <- list()
    for (i in 1:nlev) {
      xx[[i]] <- list()
    }
    for (i in 1:length(x)) {
      xx[[as.numeric(a[i])]] <- c(unlist(xx[[as.numeric(a[i])]]), 
                                  x[i])
    }
    values <- numeric(nlev)
    ff.local <- function(xx) {
      if (length(xx) > 0) {
        return(median(xx))
      }
      else {
        return(NA)
      }
    }
    if (!idx.only) {
      values <- unlist(sapply(xx, ff.local))
      return(as.numeric(values[as.numeric(a)]))
    }
    else {
      return(as.numeric(a))
    }
  }
  if (missing(x)) {
    return(NULL)
  }
  if (any(is.na(x))) {
    idx.ok <- !is.na(x)
    x[idx.ok] <- inla.group.core(x[idx.ok], n, method, idx.only)
    return(x)
  }
  else {
    return(inla.group.core(x, n, method, idx.only))
  }
}


prep.data.std.new<-function(data, savename, projection, metrics){
  
  
  dat <- data %>% 
    mutate(., Yr= YEAR - min(YEAR))
  
  dat$GEOID <- as.character(dat$GEOID)
  

  #prep for building adjacency matrix
  d_sub <- dat[!is.na(dat$YIELD),]
  names <- unique(d_sub$GEOID)
  
  #clip county file to crop producing areas only and setup indexing by CNTY
  cnty <- counties[counties$GEOID %in% names,] %>% 
    arrange(., GEOID) %>% 
    mutate (.,CNTY=seq(1,nrow(.),1)) #order dataset by county, build group index that corresponds to adjacency matrix
  
  ##Build the relational matrix for areas of interest
  neighbors <- as(cnty,"Spatial") %>% 
    poly2nb(., queen=F)#convert county sf to spatial polygons for inla relation matrix & create neighbors list from polygon object (neighbors share one or points at boundary)
  H.adj <- nb2mat(neighbors, style ="B", zero.policy=TRUE ) #convert to a sparse matrix to reduce memory (neighbor list to binary coded neighbor weights matrix)
  H.adj <-as(as(as(H.adj, "dMatrix"), "generalMatrix"), "TsparseMatrix") #convert to a sparse style matrix
  saveRDS(H.adj, paste0(wd,"/PreppedData/H.adj.",savename,".rds"))
  
  
  ###add indexes and set up vars for log-linear model with rw (by binning) and quadratic (by squaring) terms
  d_sub <- left_join(d_sub,st_set_geometry(cnty[,c("GEOID","CNTY")], NULL),by="GEOID")#join new index to crop dataset
  d_sub <- d_sub %>% 
    arrange(.,CNTY,YEAR) #Ordering dataset by county then time 
  
  d_sub$AERCODE.id <- d_sub$US_L3CODE
  d_sub$YEAR.id <- d_sub$Yr  #so index starts at 1
  d_sub$YEAR.id2 <- (d_sub$Yr )^2
  d_sub$AERCODE.id2 <- d_sub$US_L3CODE
  
  #build percent area controls
  
  d_sub <- d_sub %>% 
    mutate(AREA = AG_SQKM/TOTAL_SQKM*100,
           CROPAREA = CROPAREA/TOTAL_SQKM*100)
  
  #Scale controls and bin for rw1 evaluation
 
  d_sub <- d_sub %>% 
      mutate_at(c("TP","GDD", "PERC_IRR",  "AREA", "CROPAREA"), ~as.numeric(scale(.)))  %>%
    mutate_at(c("TP","GDD"), ~inla.group(., n = 20, method = "quantile")) %>%
    mutate_at(metrics, ~my.inla.group(., n = 10, method = "quantile")) 
  
  
    d_sub$PERC_IRR[is.na(d_sub$PERC_IRR)] <- 0
  
  #Yield transformed to log(Yield)
  
  d_sub$YIELD <- log(d_sub$YIELD)
  
  
  saveRDS(d_sub,paste0(wd,"/PreppedData/",savename,".rds"))
  
}


dup_missing_evals <- function(metric, crop, n){
  
  vals <- df_new_met %>%
    select({{metric}}) %>%
    as.vector()
  
  q <- vals[[1]] %>%
    quantile(., probs = seq(0.1,1, by = 100/n/100), na.rm =T)
  
  uq <- unique(q)
  
  
  ###Need to reconcile q with uq
  
  q_eval <- q[1:length(q)] 
  uq_eval <- uq[1:length(uq)]
  
  q_eval <- q_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(q_eval)))
  
  uq_eval <- uq_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(uq_eval)))
  
  
  counts_X <- q_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  counts_Y <- uq_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  merged_counts <- full_join(counts_Y, counts_X, by = "Value", suffix = c("_Y", "_X"))
  
  # Identify elements in Y that need to be repeated to reproduce X
  repeated_elements <- merged_counts %>%
    filter(n_Y < n_X)
  
  rep_these <- repeated_elements %>%
    # filter(ID_X != 1) %>%
    mutate(times = n_X - n_Y) %>%
    distinct(Value, ID_Y, .keep_all = TRUE) %>%
    ungroup()
  
  rep_IDs <- rep(rep_these$ID_Y, rep_these$times)
  
  # rep_IDs <- unique(rep_these$ID_Y) #The IDS that need to be repeated
  
  uq_eval <- uq[1:length(uq)]
  
  dup_evals <- sort(append(uq, uq[rep_IDs], after = max(rep_IDs))) #check to make sure it works before changing model output
  
  # Now duplicate elements in model output based on index of duplicate quantile values
  
  r <- readRDS(paste0("Output/", metric, crop, "_ntiles_02142024.rds"))
  rw.a <- r$marginals.random[[6]]
  
  rw.a.dup <-append(rw.a, rw.a[rep_IDs], after = max(rep_IDs))
  rw.a.dup <- rw.a.dup[order(names(rw.a.dup))]
  
  r$marginals.random[[6]] <- rw.a.dup
  
  saveRDS(r,paste0("Output/", metric, crop, "_ntiles_02142024.rds")) 
} 

stackit <- function(x = model_results){
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    scope <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    boundary <- str_extract(model_results[i],"BBOX")
    boundary <- ifelse(is.na(boundary), "CNTY", boundary)
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
     
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
   
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundary = boundary,
             scope = scope)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want get the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  

    ## Extract summary of EQUALLY combined posteriors ----

  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 10000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
 
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  ##Calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  ##Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  metrics <- str_flatten(unique(rw.post.list[[1]]$metric), collapse = ", ")
  crops <- str_flatten(unique(rw.post.list[[1]]$crop), collapse = ", ")
  classifications <- str_flatten(unique(rw.post.list[[1]]$classification), collapse = ", ")
  boundaries <- str_flatten(unique(rw.post.list[[1]]$boundary), collapse = ", ")
  scopes <- str_flatten(unique(rw.post.list[[1]]$scope), collapse = ", ")
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with landscape metrics \n", metrics, 
                          "; crops ", crops, "; classifications ", classifications, "; \nscope ", scopes,
                          " and boundaries ", boundaries, ". \nArea within the 95% credibility interval is ", round(area,2), ".")) +
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1", size = 0.5) +
    geom_line(data=rw.quant.plot, aes(x=Percentile, y=`quant0.25`), color = "darkgray", linetype = 2, linewidth = 0.5) +
    geom_line(data=rw.quant.plot, aes(x=Percentile, y=`quant0.75`), color = "darkgray", linetype = 2, linewidth = 0.5) +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0("Images/",metrics, 
                crops, 
                classifications, 
                scopes, 
                boundaries, 
                Sys.Date(), ".jpg"), width = 6, height = 4, units = "in")
  
  
  saveRDS(rw.quant.plot, paste0("Images/",metrics, 
                                crops, 
                                classifications, 
                                scopes, 
                                boundaries, 
                                Sys.Date(), ".rds")) #
  
  ##Summary table ----
  
  sig_dif <- rw.quant.plot %>%
       mutate(Nonzero = ifelse(quant0.025 < 0 & quant0.975 > 0, "No", "Yes")) %>%
    summarize(Sig = any(Nonzero == "Yes")) %>% as.character()
  
  med_delta <- rw.quant.plot$quant0.5[rw.quant.plot$Percentile == 100] - 
    rw.quant.plot$quant0.5[rw.quant.plot$Percentile == 10]
  
  min_delta <- ifelse(med_delta > 0, min(rw.quant.plot[rw.quant.plot$Percentile == 100, 1:39]) - 
    max(rw.quant.plot[rw.quant.plot$Percentile == 10, 1:39]), max(rw.quant.plot[rw.quant.plot$Percentile == 100, 1:39]) - 
      min(rw.quant.plot[rw.quant.plot$Percentile == 10, 1:39]))
  
  first_sig <- ifelse(med_delta > 0, rw.quant.plot %>%
                        mutate(Nonzero = ifelse(quant0.025 < 0 & quant0.975 > 0, "No", "Yes")) %>%
                        filter(Nonzero == "Yes") %>%
                        summarize(first_sig = min(Percentile, na.rm = T)) %>% as.character(),
                        "Negative Change")
  
  cc=check_curve(rw.quant.plot$Percentile,rw.quant.plot$quant0.5)
  ipbese=bese(rw.quant.plot$Percentile,rw.quant.plot$quant0.5,cc$index)
  
  data.frame(area = area, nmodels = len,
             metrics = metrics, crops = crops, 
             classifications = classifications,
             boundaries = boundaries,
             scopes = scopes, 
             sig_difference = sig_dif,
             median_effect_change = med_delta,
             min_effect_change = min_delta,
             perc_higher = first_sig,
             inflection_pt = ipbese$iters$ESE) %>% write.table("SummaryTable.csv", append = TRUE, col.names = FALSE, sep = ",")
  
} # end of stackit function




###function to build basic model formulas
build.formula<-function(independent, data, priors = c("pc", "default")){

  if (priors == "pc" ){       
    formula <- paste0('YIELD ~  1 + PERC_IRR + AREA +
      f(TP, model="rw1",scale.model=T, hyper = list (theta = list(prior="pc.prec",param=c(v,0.01)))) + 
      f(GDD,model="rw1", scale.model=T, hyper = list (theta = list(prior="pc.prec",param=c(v,0.01)))) + 
      f(CNTY, model="bym2", graph=h, scale.model=TRUE, constr = TRUE,  
           hyper=list( phi =list(prior = "pc", param = c(phi.u, phi.alpha), inital=-3), 
                       prec =list(prior = "pc.prec", param = c(u,alpha), inital = 5))) +
      f(AERCODE.id, YEAR.id)+ f(AERCODE.id2, YEAR.id2)+ 
       f(',independent, ', model="rw1", scale.model=T, hyper = list (theta = list(prior="pc.prec",param=c(v,0.01))))')
    formula <- as.formula(noquote(gsub("[\n]","", formula)))
    }else{ 
     
  if (priors == "default" ){
    formula <- paste0('YIELD ~ 1 + PERC_IRR + AREA +
      f(TP, model="rw1",scale.model=T) + 
      f(GDD,model="rw1", scale.model=T) +
      f(CNTY, model="bym", graph=h, scale.model=TRUE) +
      f(AERCODE.id, YEAR.id)+ f(AERCODE.id2, YEAR.id2) +
      f(', independent, ', model="rw1", scale.model=T)')
    formula <- as.formula(noquote(gsub("[\n]","",formula)))
    }
    }
}



###function to run basic gaussian model 

run.model<-function(formula, data, priors ="pc", savename){
  
  df <- readRDS(paste0(wd,"/PreppedData/",data)) #read in the data 

   v<-NA
   n<-NA
   Q<-NA
   u<-NA
   alpha<-NA
   phi.u<-NA
   phi.alpha<-NA
   phi.prior<-NA
   
  if (priors == "pc"){ #setup pc priors
    v=sd(df$YIELD,na.rm=TRUE)/0.31
    n=dim(df)[1]
    Q = INLA:::inla.pc.bym.Q(h)
    Q = INLA:::inla.scale.model(Q, constr=list(A=matrix(1, 1, n), e=0))
    u = 0.2/0.31
    alpha = 0.01
    phi.u = 0.5
    phi.alpha = 2/3 ## prob(phi < phi.u) = phi.alpha
    phi.prior = INLA:::inla.pc.bym.phi(Q=Q, u= phi.u, alpha = phi.alpha)
  }
   
  OUT<-inla(formula, data=df, family = "gaussian",     #run the inla model
            control.predictor = list(compute=TRUE),
            control.compute=list(dic=TRUE, cpo=TRUE, return.marginals.predictor=TRUE),
            control.fixed = list(prec = 0.0000001))
  saveRDS(OUT, file=paste0(wd,"/Output/",savename)) #save the model output
 
} 








