setwd('/Users/zchen/Desktop/workspace')

zz <- file("output.Rout", open = "wt")
sink(zz)
sink(zz, type = "message")

library(dplyr)
library(RPostgreSQL)
library(reshape)
library(forecast)
library(TTR)
library(knitr)
library(reshape2)
library(ggplot2)
library(markdown)
library(xtable)
library(sendmailR)


############################# PART I: READ DATA #############################
myRedshift <- src_postgres("db-name",
                           host = "host",
                           port = port,
                           user = "user-name",
                           password ="password")

print('Reading query...')
query = "
removed for security reason
"
  
run_query <- tbl(myRedshift,sql(query))
dt <- collect(run_query)
dt <- data.frame(dt)

region_query <- tbl(myRedshift, sql('select * from l_region')) 
region <- data.frame(collect(region_query))
  
############################# PART II: DEFINE FUNCTIONS #############################
### data prep 1: read data and remove partial week
### won't use this function (use redshift to fetch data)
read_data <- function(path = '/Users/zchen/Desktop/workspace', file_name = '3.csv', colname = c('region_id','l_cohort_period','l_activity_period','rank', 'num_bookings','cohort_size'), date_col =c('l_activity_period','l_cohort_period')){
  data_readin <- read.csv(paste(path,file_name,sep =''),stringsAsFactors = FALSE)
  for(i in date_col){
    data_readin[[i]]<-as.Date(data_readin[[i]])
  }
  #   data_readin$l_activity_period <- as.Date(data_readin$l_activity_period)
  #   data_readin$l_cohort_period <- as.Date(data_readin$l_cohort_period)
  data_readin[colname]
}                                                           ##### output: dt

### data prep 2: rollup regions like London and SF. *** WON'T USE ***
rollup_multiple_region_id <- function(data = dt, region_list = list(c(6,33,34), c(36,45,46,47))){
  rolled_up <- data
  for(i in region_list){
    to_rollup <- data[data$region_id %in% i,]
    rolled_up <- rolled_up[!rolled_up$region_id %in% i,]
    rollup_output<-aggregate(.~ l_cohort_period + l_activity_period + rank,data=to_rollup, FUN=sum)
    rollup_output$region_id <- paste(i, collapse = ',')
    rollup_output<- rollup_output[colnames(rolled_up)]
    rolled_up<- rbind(rolled_up, rollup_output)
  }
  rolled_up
}                                                           ##### output: dt                

### data prep 3: calculate ratio
calc_cohort_ratio <- function(data = dt, numerator_col = 'num_bookings', denominator_col = 'cohort_size', output_ratio_name = 'bookings_per_initial_user'){
  ratio_added <- data
  ratio_added[output_ratio_name] <- 1.0* ratio_added[numerator_col] / ratio_added[denominator_col]
  ratio_added
}                                                           ##### output: dt                



### order curve 1: calculate ratio
### input partial bookings/cohort ratio and partial cohort size (training set)
### calculate weighted average of bookings/cohort by cohort age
calc_per_cohort_ratio<- function(ratio_data = regional_ratio_sub,cohort_size = regional_cohort_sub, train_len = train_len){
  avg_ratio <- data.frame("rank"=numeric(ncol(ratio_data)-1),"bookings_per_cohort"=numeric(ncol(ratio_data)-1))
  for (i in 1:(ncol(ratio_data)-1)){
    avg_ratio$rank[i] = i
    avg_ratio$bookings_per_cohort[i] = sum(ratio_data[max(0,nrow(ratio_data)-train_len + 1):nrow(ratio_data),(i+1)]*cohort_size[max(0,nrow(cohort_size)-train_len + 1):nrow(cohort_size),(i+1)]/sum(cohort_size[max(0,nrow(cohort_size)-train_len + 1):nrow(cohort_size),(i+1)], na.rm = TRUE), na.rm=TRUE)
  }
  avg_ratio
}                                                           ##### output: w_cohort_ratio                


### cohort size 2: predict cohort size
### input pivoted cohort subset, use EMA(n=2,wilder=TRUE) to fill cohort size in test period
### return a vertical list of cohort size

calc_cohort_size<- function(last_week_train = last_week_train, original_data = nyc, test_len = 1, skip_k = 0){
  fetch_cohort <- original_data[original_data$l_activity_period == last_week_train,c('rank','cohort_size')]
  # fill out gaps (in extreme cases, there's no cohort size info for a certain week)
  max_rank <- max(fetch_cohort$rank, 100)
  full_rank <- data.frame(rank = seq(1,max_rank))
  fetch_cohort <- merge(full_rank, fetch_cohort, all.x = TRUE)
  fetch_cohort[is.na(fetch_cohort)] <- 0
  
  # use EMA to predict newest cohort size
  cohort_ts <- fetch_cohort[order(fetch_cohort$rank, decreasing =TRUE),'cohort_size']
  pred_new_cohort <- round(EMA(cohort_ts, n = 2, wilder = TRUE)[length(cohort_ts)],0)
  
  num_new <- test_len+skip_k
  new_cohorts <- data.frame(rank = seq(1,num_new), cohort_size = pred_new_cohort)
  update_cohort <- fetch_cohort
  update_cohort$rank <- update_cohort$rank + num_new 
  update_cohort <- rbind(update_cohort, new_cohorts)
}                                                           ##### output: new_cohort_size


### prediction 1: inner join w_cohort_ratio and new_cohort_size
### output week_ending, pred_tot, pred_new, pred_old
### V1: count all future cohorts as "new"
predict_booking_num <- function(cohort_ratio = w_cohort_ratio, cohort_size = new_cohort_size, test_len = 1, skip_k = 0, last_week_train = last_week_train){
  joined <- merge(cohort_ratio, cohort_size)
  joined$num_bookings <- round(joined$bookings_per_cohort *joined$cohort_size,0)
  
  num_new <- (test_len + skip_k)
  pred_week <- last_week_train + num_new * 7
  pred_from_new <- sum(joined[joined$rank <= num_new, 'num_bookings'])
  pred_from_old <- sum(joined[joined$rank > num_new, 'num_bookings'])
  pred_total <- sum(joined$num_bookings)
  pred_output <- data.frame(week_ending = pred_week, pred_total = pred_total, pred_from_new = pred_from_new, pred_from_old = pred_from_old)
}                                                           ##### output: step_prediction

### V2: only count people finish 1st booking in a certain week as "new"
predict_booking_num1 <- function(cohort_ratio = w_cohort_ratio, cohort_size = new_cohort_size, test_len = 1, skip_k = 0, last_week_train = last_week_train){
  joined <- merge(cohort_ratio, cohort_size)
  joined$num_bookings <- round(joined$bookings_per_cohort *joined$cohort_size,0)
  
  num_new <- (test_len + skip_k)
  pred_week <- last_week_train + num_new * 7
  pred_from_new <- sum(joined[joined$rank <= 1, 'num_bookings'])    ##CHANGE @0425
  pred_from_old <- sum(joined[joined$rank > 1, 'num_bookings'])     ##CHANGE @0425
  pred_total <- sum(joined$num_bookings)
  pred_output <- data.frame(week_ending = pred_week, pred_total = pred_total, pred_from_new = pred_from_new, pred_from_old = pred_from_old)
}                                                           ##### output: step_prediction

### actual 1: input week_ending and regional_data
### output week_ending, actual_tot, actual_new, actual_old
actual_booking_num <- function(week_ending = pred_week_ending, data = nyc, new_len = 1){
  actual_week_data <- data[data$l_activity_period == week_ending, c('rank','num_bookings')]
  
  actual_week <- week_ending
  actual_from_new <- sum(actual_week_data[actual_week_data$rank <= new_len,'num_bookings'])
  actual_from_old <- sum(actual_week_data[actual_week_data$rank > new_len,'num_bookings'])
  actual_total <- sum(actual_week_data$num_bookings)
  actual_output <- data.frame(week_ending = actual_week, actual_total = actual_total, actual_from_new = actual_from_new, actual_from_old = actual_from_old)
}                                                           ##### output: step_actual

### merge actual and prediction
merge_actual_pred <- function(actual = step_actual, pred = step_prediction, region_id = 5){
  merged <- merge(pred, actual)
  cols <- colnames(merged)
  merged$region_id <-region_id
  merged <- merged[c('week_ending','region_id',cols[2:length(cols)])]
#   #add quality metrics
#   merged$delta <- merged$pred_total - merged$actual_total
#   merged$abs_delta <- abs(merged$delta)
#   merged$abs_deltaxactual <- merged$abs_delta * merged$actual_total
#   merged$sq_deltaxactual <- merged$delta^2 * merged$actual_total
#   merged
}

### add region to future prediction
add_region_pred_only <- function(pred = future_prediction, region_id = 5){
  cols <- colnames(pred)
  region_added <- pred
  region_added$region_id <- region_id
  region_added <- region_added[c('week_ending','region_id',cols[2:length(cols)])]
  region_added
}

### create quality metrics output
### read in output table (all regions all weeks), calculate delta%, MAE, MSE by region
calc_quality_metrics <- function(data = output, latest_n = NULL){
  quality_metrics <- data.frame()
  week_endings <-  unique(data$week_ending)
  # when only a couple of recent qulaity metrics is needed, use parameter latest_n
  if(is.numeric(latest_n)){
    week_endings <- week_endings[order(week_endings, decreasing = TRUE)]
    week_endings <- week_endings[1:latest_n]
  }
  for(i in 1:length(week_endings)){
    week <- week_endings[i]
    week_subset <- data[data$week_ending == week,]
    
    ## calculate quality metrics - total
    week_subset$delta_total <- week_subset$pred_total - week_subset$actual_total
    week_subset$abs_delta_total <- abs(week_subset$delta_total)
    week_subset$abs_deltaxactual_total <- week_subset$abs_delta_total * week_subset$actual_total
    week_subset$sq_deltaxactual_total <- week_subset$delta_total^2 * week_subset$actual_total
    
    ## calculate quality metrics - new
    week_subset$delta_from_new <- week_subset$pred_from_new - week_subset$actual_from_new
    week_subset$abs_delta_from_new <- abs(week_subset$delta_from_new)
    week_subset$abs_deltaxactual_from_new <- week_subset$abs_delta_from_new * week_subset$actual_from_new
    week_subset$sq_deltaxactual_from_new <- week_subset$delta_from_new^2 * week_subset$actual_from_new
    
    ## calculate quality metrics - old
    week_subset$delta_from_old <- week_subset$pred_from_old - week_subset$actual_from_old
    week_subset$abs_delta_from_old <- abs(week_subset$delta_from_old)
    week_subset$abs_deltaxactual_from_old <- week_subset$abs_delta_from_old * week_subset$actual_from_old
    week_subset$sq_deltaxactual_from_old <- week_subset$delta_from_old^2 * week_subset$actual_from_old    
    
    step_quality <- data.frame(
      week = week, 
      ## part 1: total
      pred_total = format(sum(week_subset$pred_total, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      actual_total = format(sum(week_subset$actual_total, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_total = format(sum(week_subset$pred_total, na.rm =TRUE) - sum(week_subset$actual_total, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_pct_total = paste(round(100.0 * sum(week_subset$delta_total, na.rm =TRUE) / sum(week_subset$actual_total, na.rm =TRUE), 2), '%', sep =''), 
      mae_total = format(round(sum(week_subset$abs_deltaxactual_total, na.rm =TRUE) / sum(week_subset$actual_total, na.rm =TRUE), 0), big.mark = ',', scientific = FALSE),
      
      ## part 2: from new
      pred_from_new = format(sum(week_subset$pred_from_new, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      actual_from_new = format(sum(week_subset$actual_from_new, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_from_new = format(sum(week_subset$pred_from_new, na.rm =TRUE) - sum(week_subset$actual_from_new, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_pct_from_new = paste(round(100.0 * sum(week_subset$delta_from_new, na.rm =TRUE) / sum(week_subset$actual_from_new, na.rm =TRUE), 2), '%', sep =''), 
      mae_from_new = format(round(sum(week_subset$abs_deltaxactual_from_new, na.rm =TRUE) / sum(week_subset$actual_from_new, na.rm =TRUE), 0), big.mark = ',', scientific = FALSE),
      
      ## part 3: from old
      pred_from_old = format(sum(week_subset$pred_from_old, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      actual_from_old = format(sum(week_subset$actual_from_old, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_from_old = format(sum(week_subset$pred_from_old, na.rm =TRUE) - sum(week_subset$actual_from_old, na.rm =TRUE), big.mark = ',', scientific = FALSE), 
      delta_pct_from_old = paste(round(100.0 * sum(week_subset$delta_from_old, na.rm =TRUE) / sum(week_subset$actual_from_old, na.rm =TRUE), 2), '%', sep =''), 
      mae_from_old = format(round(sum(week_subset$abs_deltaxactual_from_old, na.rm =TRUE) / sum(week_subset$actual_from_old, na.rm =TRUE), 0), big.mark = ',', scientific = FALSE)
      )
    quality_metrics <- rbind(quality_metrics, step_quality)
  }
  quality_metrics[order(quality_metrics$week),]
}


#### MASTER FUNCTION ####
##### in a loop over regions...
master_func <- function(data = data, skip_k = 0, train_len = 10, test_len = 1 ){
  output <- data.frame()
  prediction <- data.frame()
  for(region_id in sort(unique(data$region_id))){ 
    print(paste('region ',region_id,' begin...'))
    
    # nyc<-data[data$region_id ==5,]    ## subsetting
    nyc <- data[data$region_id ==region_id,]
    ### pivot
    nyc<- nyc[order(nyc$l_activity_period),]
    ratio_pivot<-cast(nyc,l_activity_period ~ rank,value= 'bookings_per_initial_user')
    size_pivot<-cast(nyc,l_activity_period ~ rank,value= 'cohort_size')
    
    ratio_pivot[is.na(ratio_pivot)]<-0
    size_pivot[is.na(size_pivot)]<-0
    
    ### Historic Regional Output ###
    
    ## this is an quick example
    # regional_ratio_sub<-ratio_pivot[100:110,]
    # regional_cohort_sub<-size_pivot[100:110,]
    
    
    region_output<- data.frame()
    if(nrow(ratio_pivot) - skip_k - train_len - test_len < 0){next}
    for(i in 0:(nrow(ratio_pivot) - skip_k - train_len - test_len)){
      ## subset ratio, cohort pivot table
      regional_ratio_sub <- ratio_pivot[(1 + i):(train_len + i),]
      regional_cohort_sub <- size_pivot[(1 + i):(train_len + i),]
      
      ## fetch last week in training set, calculate pred week ending
      last_week_train <- max(regional_ratio_sub$l_activity_period)
      pred_week_ending <- last_week_train + (skip_k + test_len) * 7
      
      w_cohort_ratio <- calc_per_cohort_ratio(ratio_data = regional_ratio_sub,cohort_size = regional_cohort_sub, train_len = train_len)
      new_cohort_size <- calc_cohort_size(last_week_train = last_week_train, original_data = nyc, test_len = test_len, skip_k = skip_k)
      step_prediction <- predict_booking_num1(cohort_ratio = w_cohort_ratio, cohort_size = new_cohort_size, test_len = test_len, skip_k = skip_k, last_week_train = last_week_train)
      step_actual <- actual_booking_num(week_ending = pred_week_ending, data = nyc, new_len = 1)
      step_output <- merge_actual_pred(actual = step_actual, pred = step_prediction, region_id = region_id)
      region_output <- rbind(region_output, step_output)
      print(paste('step',i))
    }
    
    ### Predict Future Week ###
    last_week <- max(ratio_pivot$l_activity_period)
    first_week_not_incl <- last_week - (train_len) * 7
    pred_week <- last_week + 7 * (skip_k + test_len)
    ## subset ratio, cohort pivot table
    ragional_ratio_topred <- ratio_pivot[ratio_pivot$l_activity_period<=last_week & ratio_pivot$l_activity_period > first_week_not_incl,]
    ragional_cohort_topred <- size_pivot[size_pivot$l_activity_period<=last_week & size_pivot$l_activity_period > first_week_not_incl,]
    
    w_cohort_ratio_pred <- calc_per_cohort_ratio(ratio_data = ragional_ratio_topred,cohort_size = ragional_cohort_topred, train_len = train_len)
    new_cohort_size_pred <- calc_cohort_size(last_week_train = last_week, original_data = nyc, test_len = test_len, skip_k = skip_k)
    region_prediction <- predict_booking_num1(cohort_ratio = w_cohort_ratio_pred, cohort_size = new_cohort_size_pred, test_len = test_len, skip_k = skip_k, last_week_train = last_week)
    region_prediction <- add_region_pred_only(pred = region_prediction, region_id = region_id)
    
    ### JOIN regional output and regional prediction to output and prediction
    output <- rbind(output, region_output)
    prediction <- rbind(prediction, region_prediction)
    
    print(paste('region ',region_id,' is done.'))
  }
  
  quality_metrics <- calc_quality_metrics(data = output, latest_n = NULL)
  return(list(output = output, prediction = prediction, quality_metrics = quality_metrics))
}
############################# PART III: RUN FUNCTIONS, GET OUTPUT, PREDICTION, QUALITY METRICS  #############################  
dt<-calc_cohort_ratio(dt)

one_week <- master_func(data = dt, skip_k = 0, train_len = 10, test_len = 1)
two_week <- master_func(data = dt, skip_k = 1, train_len = 10, test_len = 1)
three_week <- master_func(data = dt, skip_k = 2, train_len = 10, test_len = 1)

############################# PART IV: BEAUTIFY TABLES TO PRESENT  #############################  
### Page 1: Predictions for Future 3 weeks
one_week_pred <- one_week$prediction
two_week_pred <- two_week$prediction
three_week_pred <- three_week$prediction

# weekly actual total
one_week_output <- one_week$output
one_week_output_tot_actual <- aggregate(cbind(actual_total, actual_from_new, actual_from_old) ~ week_ending, data = one_week_output, FUN = sum)

# weekly pred total
one_week_output_tot_pred <- aggregate(cbind(pred_total, pred_from_new, pred_from_old) ~ week_ending, data = one_week_output, FUN = sum)


# pred total following three weeks
pred_tot1 <- aggregate(cbind(pred_total, pred_from_new, pred_from_old) ~ week_ending, data = one_week_pred, FUN=sum)
pred_tot2 <- aggregate(cbind(pred_total, pred_from_new, pred_from_old) ~ week_ending, data = two_week_pred, FUN=sum)
pred_tot3 <- aggregate(cbind(pred_total, pred_from_new, pred_from_old) ~ week_ending, data = three_week_pred, FUN=sum)
pred_tot <- rbind(pred_tot1, pred_tot2, pred_tot3)

# fetch partial dates to present
one_week_output_tot_actual_sub <- one_week_output_tot_actual[(one_week_output_tot_actual$week_ending >= max(one_week_output_tot_actual$week_ending) - 7 * 10),]
one_week_output_tot_pred_sub <- one_week_output_tot_pred[(one_week_output_tot_pred$week_ending >= max(one_week_output_tot_pred$week_ending) - 7 * 10),]

# merge future prediction, get data ready to plot
part1_actual <- one_week_output_tot_actual_sub
part1_pred <- rbind(one_week_output_tot_pred_sub, pred_tot)


### Page 2: Predictions for Future 3 weeks by region
one_week_pred_sub <- one_week_pred[c('region_id','pred_total')]
colnames(one_week_pred_sub)[2] <- as.character(max(one_week_pred$week_ending))

two_week_pred_sub <- two_week_pred[c('region_id','pred_total')]
colnames(two_week_pred_sub)[2] <- as.character(max(two_week_pred$week_ending))

three_week_pred_sub <- three_week_pred[c('region_id','pred_total')]
colnames(three_week_pred_sub)[2] <- as.character(max(three_week_pred$week_ending))

region_pred <- merge(one_week_pred_sub, two_week_pred_sub)
region_pred <- merge(region_pred, three_week_pred_sub)

### Page 3: Quality Metrics
quality_metrics_one <- calc_quality_metrics(data = one_week_output)

############################# PART V: SAVE ENVIRONMENT  #############################  

save.image("~/Desktop/workspace/env.RData")

knit("~/Desktop/workspace/output.Rmd")
# pandoc('output.md',format='latex')

system(paste("pandoc -V geometry:margin=0.5in -fmarkdown-implicit_figures -o", "/Users/zchen/Desktop/workspace/output/", "output",format(Sys.time(), '%y-%m-%d'), ".pdf ", "output", ".md" ,sep=""))



############################# PART VI: SEND EMAIL  #############################  

from <- '<zcdata@example.com>'
to <- c('<zcdata@example.com>')
subject <-paste('Completed Bookings Prediction:',format(Sys.time(), '%Y-%m-%d'))
txt <- 
'Hi,

Please find attached completed bookings model prediction and historic performance. Let me know if you have any questions.
Thanks.

Zhijiao'
body <-list(txt,
            mime_part(paste('~/Desktop/workspace/output/','output',format(Sys.time(), '%y-%m-%d'),'.pdf',sep = ''), 
                      name = paste(subject,'.pdf',sep = '')))
mailControl = list(smtpServer = 'ASPMX.L.GOOGLE.COM')

sapply( to , function(x) sendmail( from = from , 
                                   to = x, 
                                   subject = subject,
                                   msg = body, 
                                   control = mailControl) )  


