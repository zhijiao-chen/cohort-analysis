---
title: "Completed Bookings Model Weekly Report"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: pdf_document
fontsize: 9pt
geometry: margin=0.5in 
---
# Completed Bookings Prediction: Future Three Weeks

```{r, echo= FALSE}
load("~/Desktop/workspace/Marketplace/Demand/modeling_production/env.RData")
colnames(pred_tot) <- c('Week Ending', 'Pred - Total', 'Pred - New Cust', 'Pred - Returning Cust')
rownames(pred_tot) <- pred_tot[['Week Ending']]
pred_tot <- pred_tot[-which(colnames(pred_tot)=='Week Ending')]
pred_tot <- t(pred_tot)
pred_tot <- format(pred_tot, big.mark = ',', scientific = FALSE)
knitr::kable(pred_tot, align = c('l','r','r','r'))
```  

  
## Completed Bookings from New vs. Returning Customer (Including 3-week Projection)

```{r, echo=FALSE, fig.height= 10, fig.width= 15}
plot(part1_actual$week_ending, part1_actual$actual_total, type = 'o', pch = 20, cex = 0.6, col='#0080FF', lwd = 3, xlim = c(min(part1_pred$week_ending),max(part1_pred$week_ending)),ylim = c(0, max(part1_actual$actual_total)+3000), xlab = 'Week Ending', ylab = '# Completed Bookings')
points(part1_pred$week_ending, part1_pred$pred_total, type = 'b',lty = 2, pch = 20, cex = 0.6, col='#00CDED', lwd = 1.5)
text(part1_actual$week_ending, part1_actual$actual_total-800, format(round(part1_actual$actual_total,0), big.mark = ',', scientific = FALSE),cex=1.2, col = '#0080FF')
text(part1_pred$week_ending, part1_pred$pred_total+600, format(round(part1_pred$pred_total,0), big.mark = ',', scientific = FALSE),cex=1, col = '#00CDED')

lines(part1_actual$week_ending, part1_actual$actual_from_new, type = 'o', pch = 20, cex = 0.6, col='#DF3A01', lwd = 3)
points(part1_pred$week_ending, part1_pred$pred_from_new, type = 'b',lty = 2, pch = 20, cex = 0.6, col='#F79F81', lwd = 1.5)
text(part1_actual$week_ending, part1_actual$actual_from_new-800, format(round(part1_actual$actual_from_new,0), big.mark = ',', scientific = FALSE),cex=1.2, col = '#DF3A01')
text(part1_pred$week_ending, part1_pred$pred_from_new+600, format(round(part1_pred$pred_from_new,0), big.mark = ',', scientific = FALSE),cex=1, col = '#F79F81')

lines(part1_actual$week_ending, part1_actual$actual_from_old, type = 'o', pch = 20, cex = 0.6, col='#298A08', lwd = 3)
points(part1_pred$week_ending, part1_pred$pred_from_old, type = 'b',lty = 2, pch = 20, cex = 0.6, col='#64C839', lwd = 1.5)
text(part1_actual$week_ending, part1_actual$actual_from_old-800, format(round(part1_actual$actual_from_old,0), big.mark = ',', scientific = FALSE),cex=1.2, col = '#298A08')
text(part1_pred$week_ending, part1_pred$pred_from_old+600, format(round(part1_pred$pred_from_old,0), big.mark = ',', scientific = FALSE),cex=1, col = '#64C839')

legend('topright',c('Actual Total','Actual From New','Actual From Returning','Pred Total','Pred From New','Pred From Returning'), lty = c(rep(1,3), rep(2,3)), lwd = c(rep(3,3), rep(2,3)), seg.len = 3.5, bty = 'n', cex = 0.8, ncol = 2, col = c('#0080FF','#DF3A01','#6AA164','#00CDED','#F79F81','#64C839'))

```

\newpage

# Completed Bookings Prediction: Future Three Weeks (by Region)  
```{r, echo=FALSE}
region_pred <- merge(region_pred, region[c('region_id','region')])
region_pred<- format(region_pred, big.mark = ',', scientific = FALSE)
region_pred <- region_pred[order(region_pred$region_id),c(1,5,2,3,4)]
colnames(region_pred)[1:2] <- c('Region ID', 'Region')
knitr::kable(region_pred, row.name = FALSE, align = c('r','l','r','r','r'))

```

\newpage
# Quality Metrics
### - Total
```{r, echo=FALSE}
quality_metrics_one <- quality_metrics_one[quality_metrics_one$week>=max('2016-04-02',(max(quality_metrics_one$week) - 30 * 7)),]
quality_metrics_one <- quality_metrics_one[order(quality_metrics_one$week),]
quality_metrics_one_total <- quality_metrics_one[c('week','pred_total','actual_total','delta_total','delta_pct_total','mae_total')]
colnames(quality_metrics_one_total) <- c('Week Ending', 'Pred', 'Actual', 'Delta', 'Weighted Delta%', 'Weighted Mean Absolute Error')
knitr::kable(quality_metrics_one_total, row.name = FALSE, align = c('c','r','r','r','r','r'))
```
  
### - From New Cohort
```{r, echo=FALSE}
quality_metrics_one_from_new <- quality_metrics_one[c('week','pred_from_new','actual_from_new','delta_from_new','delta_pct_from_new','mae_from_new')]
colnames(quality_metrics_one_from_new) <- c('Week Ending', 'Pred', 'Actual', 'Delta', 'Weighted Delta%', 'Weighted Mean Absolute Error')
knitr::kable(quality_metrics_one_from_new, row.name = FALSE, align = c('c','r','r','r','r','r'))
```
  
### - From Returning Cohort
```{r, echo=FALSE}
quality_metrics_one_from_old <- quality_metrics_one[c('week','pred_from_old','actual_from_old','delta_from_old','delta_pct_from_old','mae_from_old')]
colnames(quality_metrics_one_from_old) <- c('Week Ending', 'Pred', 'Actual', 'Delta', 'Weighted Delta%', 'Weighted Mean Absolute Error')
knitr::kable(quality_metrics_one_from_old, row.name = FALSE, align = c('c','r','r','r','r','r'))
```
          
- Weighted Delta%: On average (weighted by regional bookings number), how much % of bookings was not captured by model;
- Weighted Mean Absolute Error: On average (weighted by regional bookings number), how many bookings prediction was off compared with actual;

\newpage
# Historic Model Performance (by Region)
### `r paste('Week', max(one_week_output$week_ending))`
```{r, echo=FALSE}
one_week_output <- one_week_output[one_week_output$week_ending==max(one_week_output$week_ending),]
one_week_output <- merge(one_week_output, region[c('region_id','region')])
one_week_output$delta <- one_week_output$pred_total - one_week_output$actual_total
one_week_output <- one_week_output[order(one_week_output$region_id),c('region_id','region','pred_total','actual_total','delta')]
one_week_output$delta_pct <- round(1.0 * one_week_output$delta / one_week_output$actual_total,3)
colnames(one_week_output) <- c('RegionID', 'Region', 'Pred Total', 'Actual Total', 'Delta', 'Delta %')
one_week_output[c('Pred Total', 'Actual Total', 'Delta')] <- format(one_week_output[c('Pred Total', 'Actual Total', 'Delta')], big.mark = ',', scientific = FALSE)
one_week_output['Delta %'] <- paste(100 * one_week_output[['Delta %']], '%', sep = '')
knitr::kable(one_week_output, row.name = FALSE, align = c('r','l','r','r','r','r'))
```