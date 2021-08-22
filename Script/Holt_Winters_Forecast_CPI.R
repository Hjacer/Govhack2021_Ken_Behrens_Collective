library(TTR)
library(forecast)

cpi_data <- domain_data_sum_all[year(PeriodEndDate2)>=2000 & Measure=='Consumer Price Index']
st_dt = min(cpi_data$PeriodEndDate2)
ed_dt = max(cpi_data$PeriodEndDate2)
cpi_ts = ts(data=cpi_data[,c('Value_Tot')],start=c(year(st_dt),month(st_dt)),
            end=c(year(ed_dt),month(ed_dt)), frequency = 4)
cpi_ts

cpi_ml <- HoltWinters(cpi_ts,gamma=TRUE)
cpi_fore <- forecast:::forecast.HoltWinters(cpi_ml, h=8)
g <- plot(cpi_fore)
