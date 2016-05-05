epoch[timestamp >= ymd_hms("20160504 182500") & timestamp <= ymd_hms("20160504 190500") ,.(n_rows = .N,
         n_steps = sum(steps),
         mean_steps = mean(steps),
         max_steps = max(steps),
         min_steps = min(steps)), serialnumber]

ggplot(epoch[timestamp >= ymd_hms("20160504 182500") & timestamp <= ymd_hms("20160504 190500")], 
       aes(timestamp, steps, group = serialnumber)) + 
  geom_line(aes(col = serialnumber)) +
  facet_wrap(~serialnumber, ncol = 2) + 
  labs(title = "Summary of step activity from 5s epoch",
       subtitle = "NOTE: Walk occurred 18:30 to 18:55",
       caption = "Data filtered to show period between 18:25 - 19:05",
       y = "Steps / epoch",
       x = "") + 
  theme_bw() +
  theme(legend.position = "none")

# find epochs with all devices similar steps
dcast(epoch, timestamp ~ serialnumber, fill = NA, value.var = "steps") %>% 
  group_by(timestamp) %>%
  mutate(sd = sd(c(TAS1E31150003,TAS1E31150005,TAS1E31150028,TAS1E31150059))) %>%
  ungroup %>%
  filter(sd != 0) %>%
  arrange(sd)

tables()
