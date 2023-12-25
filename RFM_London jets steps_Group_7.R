### read the data

lj_data <- London_Jets_Spreadsheet_Supplement

#### select relevant columns using dplyr
lj_data_customer <- lj_data[,-c(2:8,10,14:20)]

### convert and create the date column (using lubridate, and make_date function)

library(lubridate)
?make_date

lj_data_customer$trans_date <- make_date(year = lj_data_customer$LastTransYear,
                                         month = lj_data_customer$LastTransMonth,
                                         day = "01")

## fix the data analysis date
max(lj_data_customer$trans_date)
analysis_date<-as_date("2001-12-02")
?rfm_table_customer_2
## create the rfm score using rfm_table_customer_2
str(lj_data_customer)
library(rfm)
rfm_analysis <- rfm_table_customer_2(lj_data_customer, customer_id = CustID,
                                     n_transactions = Num_Games,
                                     latest_visit_date = trans_date,
                                     total_revenue = Tot_Sales,
                                     analysis_date = analysis_date
                                     )

rfm_analysis
rfmdata <- data.frame (rfm_analysis$rfm)
### create some menaingful groups based on your (group logic)
segment_titles <- c("Die hard fans", "Loyals", "Potential Loyalists",
                    "Need attention", "At Risk", "Can't lose them",
                    "Not interested","One timers", "New fans")


r_low <-  c(4, 3, 2, 1, 1, 1, 1, 1, 3)# minimum value of recency
r_high <- c(5, 5, 4, 3, 1, 2, 1, 2, 5)#maximum value of recency
f_low <-  c(4, 3, 2, 1, 2, 3, 1, 1, 1)
f_high <- c(5, 5, 4, 4, 4, 5, 1, 1, 2)
m_low <-  c(4, 3, 2, 2, 3, 4, 1, 1, 1)
m_high<-  c(5, 5, 4, 4, 5, 5, 1, 2, 5)

### create the summary of the segments using dplyr package and provide useful interpretation
##of the groups

rfm_segments<-rfm_segment(rfm_analysis, segment_titles, r_low, r_high, 
                          f_low, f_high, m_low, m_high)

rfm_segments

library(plotly)
library(treemap)
library(rfm)
library(dplyr)


rfm_segments_overall <- rfm_segments %>% count(segment) %>% arrange(desc(n)) %>% 
  rename(Count = n) %>% mutate(Percentage = (Count/ sum(Count))*100)
rfm_segments_overall

myscore <- rfm_segments%>%group_by(segment)%>% summarise (AF = median(transaction_count),
                                                       ARD = median(recency_days),
                                                       AMT = median (amount))%>%
  arrange(desc(AMT))


rfm_plot_median_recency(rfm_segments,print_plot = TRUE)
rfm_plot_median_frequency(rfm_segments,print_plot = TRUE)
rfm_plot_median_monetary(rfm_segments,print_plot = TRUE)

#write.csv(rfm_segments, "London_Jet_RFMAnalysis_test.csv")

library(writexl)
write_xlsx(rfm_segments, "London_Jet_RFMAnalysis_test.xlsx")
