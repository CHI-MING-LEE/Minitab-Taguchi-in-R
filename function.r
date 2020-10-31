# add signal factor
addsig <- function(DA = NULL, sig_level = NULL){
  sig <- rep(sig_level, each=nrow(DA))
  DA <- coredata(DA)[rep(seq(nrow(DA)),length(sig_level)),]
  DA <- as.data.frame(DA, row.names = 1:nrow(DA))
  data <- cbind.data.frame(DA, signal = sig)
  return(data)
}

# combine separate mixed level table as the input of map_table
mapping <- function(plan_table = NULL, map_list = NULL){
  plan_matrix <- matrix(0,nrow = dim(plan_table)[1], ncol =  dim(plan_table)[2])
  # Mapping with factor
  n = 1
  for(j in 1:length(map_list)){
    k = n
    map_table = map_list[[j]]
    for(i in n:(nrow(map_table)+n-1)){
      plan_matrix[,i] <- factor(plan_table[,i], labels = map_table[i-k+1,-1] %>% as.character()) %>% as.character()
    }
    n = n + nrow(map_table)
  }
  plan_matrix <- as.data.frame(plan_matrix)
  colnames(plan_matrix) <- do.call(c,lapply(map_list, function(x) c(as.character(x[,1]))))
  return(plan_matrix)
}

# extract the table
table_extract <- function(level_table = NULL, cate = NULL, lv = NULL, fct = NULL, fct_sum = NULL){
  if(cate == "single"){
    # extract table
    tmp <- subset(level_table, level_table$levels == lv &  level_table$factors == fct)
    # extract names
    tmp.names <- lapply(1:nrow(tmp), function(x) paste(tmp[x,"name"],tmp[x,"runs"],tmp[x,"levels"],tmp[x,"factors"], sep = stri_dup(intToUtf8(160), 10)))
    choice.names <- as.list(1:length(tmp.names))
    names(choice.names) <- unlist(tmp.names)
    return(choice.names)
  }
  if(cate == "mixed"){
    # extract table
    tmp_mx <- subset(level_table, level_table$factors_sum == fct_sum)
    # extract names
    tmp_mx.names <- lapply(1:nrow(tmp_mx), function(x) paste(tmp_mx[x,"name"],tmp_mx[x,"runs"],
                                                             tmp_mx[x,"level_a"],tmp_mx[x,"factors_a"],tmp_mx[x,"level_b"], tmp_mx[x,"factors_b"],sep = stri_dup(intToUtf8(160),12)))
    choice_mx.names <- as.list(1:length(tmp_mx.names))
    names(choice_mx.names) <- unlist(tmp_mx.names)
    return(choice_mx.names)
  }
}


# get mixed param
get_param <- function(tb_csv = NULL, fct_sum = NULL, usr_chs = NULL, lv = NULL, fct = NULL, cate = NULL){
  if(cate == "single"){
    row_index <- as.numeric(usr_chs)
    tmp <- subset(tb_csv, tb_csv$levels == lv &  tb_csv$factors == fct)
    runs <- tmp[row_index,'runs'] %>% as.numeric
    factors <- tmp[row_index,'factors'] %>% as.numeric
    return(list(runs=runs,level=lv,factors=factors))
  }
  if(cate == "mixed"){
    row_index <- as.numeric(usr_chs)
    tmp_mx <- subset(tb_csv, tb_csv$factors_sum == fct_sum)
    runs <- tmp_mx[row_index,'runs'] %>% as.numeric
    level_a = tmp_mx[row_index,'level_a'] %>% as.numeric
    level_b = tmp_mx[row_index,'level_b'] %>% as.numeric
    factors_a <- tmp_mx[row_index,'factors_a'] %>% as.numeric
    factors_b <- tmp_mx[row_index,'factors_b'] %>% as.numeric
    return(list(runs=runs,level_a=level_a,level_b=level_b,factors_a=factors_a,factors_b=factors_b))
  }
}

# generate the table
generate_taguchi <- function(lv = NULL, runs = NULL, fct = NULL, sig_boolean = NULL, sig_lv = NULL, cate = NULL){
  if(cate == "single"){
      # if nruns is larger than (factors^levels), it must be replicated
      if(log(runs,base=lv) > fct){
          plan <- oa.design(nruns = runs, nlevels = rep(lv,fct))
          plan <- coredata(plan)[rep(seq(nrow(plan)),lv**(log(runs,base=lv)-fct))]
            # if signal is cheched
            if(sig_boolean & !is.null(sig_lv)){
              plan <- addsig(DA = plan, sig_level = 1:sig_lv)
            }
          plan <- as.data.frame(plan)
          return(plan)
      }else{
          plan <- oa.design(nruns = runs, nlevels = rep(lv,fct))
            # if signal is cheched
            if(sig_boolean & !is.null(sig_lv)){
              plan <- addsig(DA = plan, sig_level = 1:sig_lv)
            }
          plan <- as.data.frame(plan)
          return(plan)
      }
  }
  if(cate == "mixed"){
    return(NULL) # no time to maintain
  }
  
}

# generate initial handsontable for single level design
generate_init_single <- function(init_table = NULL, fct = NULL, lv = NULL){
    a <- init_table
    a <- a[, !colnames(a) %in% 'signal']
    a <- as.matrix(a[,1:fct])
    # change letter as a2 because of some unpredictible bug...
    a2 <- t(apply(a, 2, function(x) levels(as.factor(x))))
    a2 <- cbind(c(LETTERS,letters)[!c(LETTERS,letters)%in%c("I","i")][1:fct],a2)
    colnames(a2) <- c("Factors" ,paste0("Level", 1:lv))
    row.names(a2) <- 1:fct
    return(a2)
}

# show design names for summary
design_name <- function(level_table = NULL, lv = NULL, fct = NULL, fct_sum = NULL, cate = NULL){
  if(cate == "single"){
    tmp <- subset(level_table, level_table$levels == lv &  level_table$factors == fct)
    # extract names
    tmp.names <- lapply(1:nrow(tmp), function(x) paste(tmp[x,"name"]))
    return(unlist(tmp.names))
  }
  if(cate == "mixed"){
    # extract table
    tmp_mx <- subset(level_table, level_table$factors_sum == fct_sum)
    # extract names
    tmp_mx.names <- lapply(1:nrow(tmp_mx), function(x) paste(tmp_mx[x,"name"]))
    return(unlist(tmp_mx.names))
  }
  
}

