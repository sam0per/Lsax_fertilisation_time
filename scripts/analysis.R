rm(list = ls())

dp <- read.csv(file = "Lsax_fertilisation_time/data/mating_time_pilot.csv", stringsAsFactors = FALSE)
head(dp)

dd <- read.csv(file = "Lsax_fertilisation_time/data/FLs_size_mm_sex.csv", stringsAsFactors = FALSE)
head(dd)
dd <- dd[complete.cases(dd), ]

dd <- rbind(dp, dd)
head(dd)
dd$dev <- rowSums(x = dd[, c("veliger", "shelled", "crawlaway")])
dd <- dd[, c("snail_ID", "termination", "size_mm", "sex", "ecotype", "time_min", "egg", "misdev", "dev", "notes")]

table(dd$time_min)

library(ggplot2)
library(tidyr)
# dl <- gather(dd, stage, count, egg:crawlaway, factor_key=TRUE)
dl <- gather(dd, stage, count, egg:dev, factor_key=TRUE)
head(dl)
table(dl$sex)

dl <- dl[dl$termination!="NATURAL", ]
dl <- dl[dl$stage!="misdev", ]
table(dl$time_min)
dl$time_min <- factor(x = dl$time_min, levels = c("Control", "1", "5", "10", "30"))
# dl$time_min <- factor(x = dl$time_min, levels = c("1", "3", "5", "11", "30"))
# dl <- dl[order(dl$time_min), ]
# dl$grp <- paste(dl$snail_ID, dl$stage, sep = "_")

# dl <- dl[dl$time_min!="3", ]
# dl <- dl[dl$time_min!="3" & dl$time_min!="11", ]

dl$snail_ID <- as.character(dl$snail_ID)
table(dl$snail_ID)

# TEST DIFFERENCE BETWEEN DATASETS
dl$proj <- ifelse(test = dl$notes=="pilot", yes = "pilot", no = "follow-up")
tt <- intersect(unique(dl$time_min[dl$proj=="pilot"]), unique(dl$time_min[dl$proj=="follow-up"]))
for (i in tt) {
  # i <- "Control"
  dtest <- dl[dl$time_min == i, ]
  # dgrid <- expand.grid(proj=unique(dl$proj), time_min=i, stage=levels(dl$stage))
  dsum <- aggregate(x = dtest$count, by = list(proj=dtest$proj, time_min=dtest$time_min, stage=dtest$stage), sum)
  dsum <- dsum[order(dsum$proj), ]
  dsum <- data.frame(p1=dsum$x[1:2], p2=dsum$x[3:4])
  dcq <- chisq.test(dsum)
  # dcq$expected
  print(dcq)
}


# library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)
dpal <- data.frame(time_min=levels(dl$time_min), pal=c("grey", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac"))
dpal$time_min <- as.character(dpal$time_min)
dpal$pal <- as.character(dpal$pal)

# dlm <- aggregate(x = dl$count, by = list(stage = dl$stage, time_min = dl$time_min), mean)
# colnames(dlm)[3] <- "count"
# 
# ggplot(data = dl, aes(x = stage, y = count, col = time_min)) +
#   geom_point(aes(group = snail_ID)) +
#   geom_line(aes(group = snail_ID)) +
#   geom_point(data = dlm, aes(x = stage, y = count), size = 5, col = "black") +
#   geom_point(data = dlm, aes(x = stage, y = count, col = time_min), size = 3) +
#   scale_color_manual(values = as.character(dpal$pal)) +
#   labs(x="", col="") +
#   theme(legend.position = "top",
#         legend.title = element_text(size = 12), legend.text = element_text(size = 11),
#         axis.text = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         panel.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#         axis.line = element_line(size = 0.2, linetype = "solid",
#                                  colour = "black"),
#         panel.grid = element_line(colour = "gray70", size = 0.2))

rm(list = setdiff(ls(), c("dl", "dpal")))
dp <- dl[dl$proj=="pilot", ]
# dp$time_min <- factor(x = dp$time_min, levels = c("Control", "1", "5", "10", "30"))
dpm <- aggregate(x = dp$count, by = list(stage = dp$stage, time_min = dp$time_min), mean)
colnames(dpm)[3] <- "count"
pp <- ggplot(data = dp, aes(x = stage, y = count, col = time_min)) +
  geom_point(aes(group = snail_ID)) +
  geom_line(aes(group = snail_ID)) +
  geom_point(data = dpm, aes(x = stage, y = count), size = 5, col = "black") +
  geom_point(data = dpm, aes(x = stage, y = count, col = time_min), size = 3) +
  scale_color_manual(values = dpal[dpal$time_min %in% unique(as.character(dp$time_min)), "pal"]) +
  labs(x="", col="") +
  theme(legend.position = "top",
        legend.title = element_text(size = 12), legend.text = element_text(size = 11),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2))
pp
ggsave(filename = paste0("Lsax_fertilisation_time/figures/count_dev_stages_", unique(dp$proj), ".pdf"), plot = pp,
       scale = 0.7, dpi = "screen")

# TEST DIFFERENCE BETWEEN TREATMENTS IN PILOT
dcom <- combn(x = unique(as.character(dp$time_min)), m = 2)
apply(X = dcom, MARGIN = 2, FUN = function(x) {
  # x <- dcom[, 1]
  dtest <- dp[dp$time_min %in% x, ]
  dsum <- aggregate(x = dtest$count, by = list(time_min=dtest$time_min, stage=dtest$stage), sum)
  dsum <- dsum[order(dsum$time_min), ]
  dsum <- data.frame(Control=dsum$x[1:2], Treat=dsum$x[3:4])
  dcq <- chisq.test(dsum)
  outd <- data.frame(proj=unique(dp$proj), time_min=paste(x, collapse = "-"), stage=unique(dtest$stage), Xsquared=dcq$statistic,
                     df=dcq$parameter, pval=round(dcq$p.value, 5), dcq$observed[,1], round(dcq$expected[,1]),
                     dcq$observed[,2], round(dcq$expected[,2]))
  colnames(outd) <- c("proj", "time_min", "stage", "Xsquared", "df", "pval", paste0("Obs_", x[1]), paste0("Exp_", x[1]),
                      paste0("Obs_", x[2]), paste0("Exp_", x[2]))
  return(outd)
})
# 
# 
# 
rm(list = setdiff(ls(), c("dl", "dpal")))
dc <- dl[dl$proj=="follow-up" & dl$ecotype=="crab", ]
dcm <- aggregate(x = dc$count, by = list(stage = dc$stage, time_min = dc$time_min), mean)
colnames(dcm)[3] <- "count"
pp <- ggplot(data = dc, aes(x = stage, y = count, col = time_min)) +
  geom_point(aes(group = snail_ID)) +
  geom_line(aes(group = snail_ID)) +
  geom_point(data = dcm, aes(x = stage, y = count), size = 5, col = "black") +
  geom_point(data = dcm, aes(x = stage, y = count, col = time_min), size = 3) +
  scale_color_manual(values = dpal[dpal$time_min %in% unique(as.character(dc$time_min)), "pal"]) +
  labs(x="", col="") +
  theme(legend.position = "top",
        legend.title = element_text(size = 12), legend.text = element_text(size = 11),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2))
pp
ggsave(filename = paste0("Lsax_fertilisation_time/figures/count_dev_stages_", unique(dc$proj), ".pdf"), plot = pp,
       scale = 0.7, dpi = "screen")

# TEST DIFFERENCE BETWEEN TREATMENTS IN FOLLOW-UP
dc <- dc[order(dc$time_min), ]
dcom <- combn(x = unique(as.character(dc$time_min)), m = 2)
apply(X = dcom, MARGIN = 2, FUN = function(x) {
  # x <- dcom[, 1]
  dtest <- dc[dc$time_min %in% x, ]
  dsum <- aggregate(x = dtest$count, by = list(time_min=dtest$time_min, stage=dtest$stage), sum)
  dsum <- dsum[order(dsum$time_min), ]
  dsum <- data.frame(Control=dsum$x[1:2], Treat=dsum$x[3:4])
  dcq <- chisq.test(dsum)
  outd <- data.frame(proj=unique(dc$proj), time_min=paste(x, collapse = "-"), stage=unique(dtest$stage), Xsquared=dcq$statistic,
                     df=dcq$parameter, pval=round(dcq$p.value, 5), dcq$observed[,1], round(dcq$expected[,1]),
                     dcq$observed[,2], round(dcq$expected[,2]))
  colnames(outd) <- c("proj", "time_min", "stage", "Xsquared", "df", "pval", paste0("Obs_", x[1]), paste0("Exp_", x[1]),
                      paste0("Obs_", x[2]), paste0("Exp_", x[2]))
  return(outd)
})
