
# join epoch steps to raw
data <- data[epoch] 

# plot raw data for single epoch
data[datetime >= ymd_hms("2016-05-04 18:45:00") & datetime <= ymd_hms("2016-05-04 18:45:05")] %>%
  ggplot(aes(datetime, vec.mag, group = device_id)) + 
  geom_line(aes(col = as.factor(steps))) +
  facet_wrap(~device_id, ncol = 2) + 
  labs(title = "Example of raw data for a single 5 second epoch starting at 2016-05-04 18:45:00",
       subtitle = "2 devices wore worn by the same subject as a control, a third was worn by a second subject,\nthe final device was attached to the collar of a spaniel who was off lead.
The lines are coloured by the number of steps identified by the Actilife software in the epoch.",
       caption = "NOTE: devices times are not 100% synchronous +/- 1s",
       y = "Accel Vector Magnitude (m/s)",
       x = "Time (s)") + 
  theme_bw() +
  scale_colour_discrete(name = "Steps in epoch") + 
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = "white"),
        panel.grid.minor = element_blank())


# plot raw data for single epoch
data[datetime >= ymd_hms("2016-05-04 18:33:30") & datetime < ymd_hms("2016-05-04 18:33:35")] %>%
  ggplot(aes(datetime, vec.mag, group = device_id)) + 
  geom_line(aes(col = as.factor(steps))) +
  facet_wrap(~device_id, ncol = 2) + 
  labs(title = "Example of raw data for a single 5 second epoch starting at 2016-05-04 18:33:30",
       subtitle = "In this plot the algorithm found an equal number of steps for each device.
However it is apparent that there are significant differences in the raw data profiles.",
       caption = "NOTE: devices times are not 100% synchronous +/- 1s",
       y = "Accel Vector Magnitude (m/s)",
       x = "Time (s)") + 
  theme_bw() +
  scale_colour_discrete(name = "Steps in epoch") + 
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = "white"),
        panel.grid.minor = element_blank())


data[epoch_id == ymd_hms("2016-05-04 18:33:30"), sd(steps), device_id]
