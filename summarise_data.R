data[(device_id == "TAS1E31150003" & epoch_id == ymd_hms("2016-05-04 18:44:00")), 
     .(avg.vec = mean(vec.mag),
       sd.vec = sd(vec.mag),
       get.peak.summary(vec.mag, k = 25, freq = 100), 
       steps = max(steps)), 
     .(device_id, epoch_id) ]

p <- proc.time()
vector.summary <- data[(epoch_id >= ymd_hms("2016-05-04 18:30:00") & 
                          epoch_id < ymd_hms("2016-05-04 18:55:00")), 
     get.peak.summary(vec.mag, k = 25, freq = 100), 
     .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)

peak.summary <- data[(epoch_id >= ymd_hms("2016-05-04 18:30:00") & 
                        epoch_id < ymd_hms("2016-05-04 18:55:00")), 
     .(avg.vec = mean(vec.mag),
       sd.vec = sd(vec.mag),
       steps = max(steps)), 
     .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)
(proc.time() - p)[3]

summary <- vector.summary[peak.summary]
tables()
