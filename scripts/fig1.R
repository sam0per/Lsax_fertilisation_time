dm <- read.csv("../Littorina_mating/data/CZ_all_mating_clean.csv", sep = ";")
dm <- read.csv("Documents/research/projects/2.mating/data/CZ_all_mating_clean.csv", sep = ";")
# CZ_data <- read.csv("Documents/research/projects/2.mating/data/CZ_all_mating_clean.csv", sep = ";")
head(dm)

library(ggplot2)
dm$dur_mean <- round(dm$dur_mean)
mean(dm$dur_mean, na.rm = TRUE)
min(dm$dur_mean, na.rm = TRUE)
max(dm$dur_mean, na.rm = TRUE)
median(dm$dur_mean, na.rm = TRUE)

dm <- dm[!is.na(dm$dur_mean) & dm$dur_mean!=0 & dm$dur_mean!=240, ]
mp <- ggplot(data = dm) +
  geom_histogram(aes(x = dur_mean), col = "black", binwidth = 1) +
  geom_vline(xintercept = mean(dm$dur_mean, na.rm = TRUE), linetype = "dashed", col = "blue", size = 1) +
  geom_vline(xintercept = median(dm$dur_mean, na.rm = TRUE), linetype = "dashed", col = "orange", size = 1) +
  scale_x_continuous(breaks = c(1, seq(20, to = max(dm$dur_mean, na.rm = TRUE), by = 20)),
                     limits = c(0, max(dm$dur_mean, na.rm = TRUE)+1)) +
  xlab("\ncopulation duration (min)") +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2))
mp
ggsave(filename = "Lsax_fertilisation_time/figures/mating_trials_mean_duration.pdf", plot = mp, scale = 0.7,
       dpi = "screen")
