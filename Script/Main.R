library('openxlsx')
library('data.table')
library('tidyverse')
library('ggplot2')
library(lubridate)

project_path = "https://github.com/Hjacer/Govhack2021_Ken_Behrens_Collective/"
script_path <- paste0(project_path,'Script/')
output = paste0(project_path,'Output/')
out_plot_ts <- paste0(output,'Time series Plots/')
out_plot_pit <- paste0(output,'Point in time plots/')
input = paste0(project_path,'Input/')

file_name1 = "All_Wellbeing_Measures.xlsx"

wellbeing_data <- read.xlsx(paste0(input,file_name1))
setDT(wellbeing_data)
wellbeing_data[,PeriodEndDate2:=as.Date(PeriodEndDate,format =  "%m/%d/%Y %H:%M:%S")]
wellbeing_data <- wellbeing_data[!is.na(PeriodEndDate2)]
domain_list <- unique(wellbeing_data$Domain)

# domain_list <- c('Living standards','Housing and home','Health','Social connection')
# domain_list <- c('Living standards','Housing and home','Access and connectivity')
# domain_name = "Living standards"
domain_ts_data_all <- data.table()
domain_pit_data_all <- data.table()
for (domain_name in domain_list) {
  print(domain_name)
  domain_name = gsub("/",'',domain_name)
  domain_data <- wellbeing_data[Domain==domain_name,]
  group_cols <- c('Indicator','Measure','CategoryName','CategoryOption','Type','Unit','PeriodEndDate2')
  
  domain_data_sum <- domain_data[,.(Value_Tot = sum(Value)),
                                 by=group_cols]
  # domain_data_all_sum <- domain_data_sum[CategoryName=='All']
  
  # indicator_level <- c("Income levels","Net worth","Cost of living","Financial position")
  
  # for (ind in indicator_level) {
  #   subdata <- domain_data_sum[Indicator==ind,]
  # }
  setDT(domain_data_sum)
  domain_data_sum[is.na(CategoryOption) & Type=='ACT',CategoryOption:='ACT']
  for (measure in unique(domain_data_sum$Measure)) {
    print(measure)
    measure=gsub("/",'',measure)
    subdata <- domain_data_sum[Measure==measure,]
    
    if (length(unique(subdata$PeriodEndDate2))==1) {
      print('Point in time data')
      subdata[,Domain:=domain_name]
      domain_pit_data_all <- rbind(domain_pit_data_all,subdata)
    }
    
    else {
      print('Time series data')
      subdata[,Domain:=domain_name]
      domain_ts_data_all <- rbind(domain_ts_data_all,subdata)
    
      g <-  ggplot(subdata, aes(x=PeriodEndDate2, y=Value_Tot, group=CategoryOption, color=CategoryOption)) +
        geom_line() + ggtitle(label=domain_name,subtitle = measure)
      print(g)
      ggsave(filename=paste0(out_plot_ts,'Plot ',domain_name,'-',measure,'.png'),plot =g)
    }
  }
  
  # domain_data_sum[,Domain:=domain_name]
  # domain_data_sum_all <- rbind(domain_data_sum_all,domain_data_sum)
}
