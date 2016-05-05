epoch[timestamp >= ymd_hms("20160504 183000") & timestamp <= ymd_hms("20160504 185500") ,
      .(n_steps = sum(steps),
        mean_steps = round(mean(steps),2),
        sd_steps = round(sd(steps), 2),
        max_steps = max(steps),
        min_steps = min(steps)), serialnumber]

ggplot(epoch[timestamp >= ymd_hms("20160504 183000") & timestamp <= ymd_hms("20160504 185500")], 
       aes(timestamp, steps, group = serialnumber)) + 
  geom_line() +
  facet_wrap(~serialnumber, ncol = 2) + 
  labs(title = "Summary of step activity from 5s epochs",
       subtitle = "Steps per epoch data for adult 2 subjects (1 wearing 2 devices) and a dog.\nContinuous walking except for a short break when one of the subjects went for swim in river.",
       caption = "Data filtered to show walking period between 18:30 - 18:55",
       y = "Steps / epoch",
       x = "Time (hh:mm)") + 
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

# find epochs with all devices similar steps
dcast(epoch, timestamp ~ serialnumber, fill = NA, value.var = "steps") %>% 
  group_by(timestamp) %>%
  mutate(sd = sd(c(TAS1E31150003,TAS1E31150005,TAS1E31150028,TAS1E31150059))) %>%
  ungroup %>%
  filter(sd != 0) %>%
  arrange(sd)

tables()
