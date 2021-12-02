library(taskscheduleR)
taskscheduler_create(taskname = "stock-scrape",
                     rscript = 'path-to-file.R', 
                     schedule = "MINUTE", 
                     starttime = "09:25", 
                     startdate = format(Sys.Date(), "%d/%m/%Y"), 
                     modifier=5)

