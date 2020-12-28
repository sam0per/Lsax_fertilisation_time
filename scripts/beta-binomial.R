library("glmmTMB")
library("bbmle")
library("ggplot2")
library("patchwork")
library("emdbook")
library("VGAM")
library("tibble")
library("dplyr")

rm(list = ls())
dd <- read.csv(file = "Lsax_fertilisation_time/data/Fert_Lsax_clean.csv", stringsAsFactors = FALSE)

table(dd$stage)
dd[dd$stage=="SAME",]

dd <- dd[dd$vip=="S", ]
dd$misdev <- NULL

nrow(dd[dd$termination=="NATURAL", ])
table(dd$termination)
table(dd$ecotype)

dd$proj <- ifelse(test = dd$notes=="pilot", yes = "pilot", no = "followup")
data.frame(table(dd$ecotype, dd$proj))
tb1 <- data.frame(table(dd$termination, dd$ecotype, dd$time_min, dd$proj))
tb1 <- tb1[tb1$Freq!=0, ]

dd <- dd[dd$termination!="NATURAL", ]

dd$tot <- dd$egg+dd$dev
dd$p_egg <- dd$egg/dd$tot
table(dd$tot)
d0 <- nrow(dd[dd$tot==0,])
dd <- dd[dd$tot!=0,]

dd$time_min <- ifelse(test = dd$time_min=="10" | dd$time_min=="30", yes = "10+", no = dd$time_min)
dd$time_min <- factor(x = dd$time_min, levels = c("Control", "1", "5", "10+"))
dd$termination <- factor(x = dd$termination, levels = c("NONE", "ARTIFICIAL"))

tb1 <- data.frame(table(dd$termination, dd$ecotype, dd$time_min, dd$proj))
tb1 <- tb1[tb1$Freq!=0, ]
sum(tb1$Freq)
data.frame(table(dd$time_min))

CTs <- ggplot(data = dd, aes(x = time_min, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "", size = "") +
  theme(legend.position = "none")
CT <- ggplot(data = dd, aes(x = termination, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "n. developed/(n. dev. + n. eggs)", size = "n dev + n eggs") +
  theme(legend.position = "top", legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))
CT + CTs

fit <- Coef(vglm(cbind(dev, tot-dev) ~ 1, betabinomial, data = dd, trace = FALSE))
fit

detach("package:VGAM", unload=TRUE)
table(dd$proj)

ggplot(data = dd, aes(x = proj, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.4) +
  labs(y = "n. developed/(n. dev. + n. eggs)", size = "n dev + n eggs") +
  theme(legend.position = "top", legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))

# fit_betabin <- glm(formula = p_dev ~ proj, weights = tot, family = "betabinomial", data = dd)
# summary(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ proj*termination, weights = tot, family = "betabinomial", data = dd)
summary(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ termination, weights = tot, family = "betabinomial", data = dd)
summary(fit_betabin)

fit_betabin <- glm(formula = p_dev ~ time_min, weights = tot, family = "betabinomial", data = dd)
summary(fit_betabin)

sfit <- round(summary(fit_betabin)$coefficients, 3)
pr_fit <- profile(fit_betabin)
confint(pr_fit)

fam <- family(fit_betabin)
fam
str(fam)
ilink <- fam$linkinv
ilink
ndata <- bind_cols(dd, setNames(as_tibble(predict(fit_betabin, dd, se.fit = TRUE)[1:2]),
                                c('fit_link','se_link')))
ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
ndata <- add_column(ndata, fit = predict(fit_betabin, newdata = ndata, type = 'response'))
head(ndata)
ndata <- unique(ndata[, c("time_min", "fit_resp", "right_upr", "right_lwr")])

pCTs <- ggplot(data = dd, aes(x = time_min, y = p_dev)) +
  geom_point(aes(size=tot), alpha=0.5) +
  labs(y = "Proportion of offspring", size = "", x = "Time (min)") +
  theme(legend.position = "none",
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.line = element_line(size = 0.2, linetype = "solid",
                                 colour = "black"),
        panel.grid = element_line(colour = "gray70", size = 0.2)) +
  geom_errorbar(data = ndata, aes(y = fit_resp, ymin = right_lwr, ymax = right_upr),
                width = 0.1, size = 1) +
  geom_point(data = ndata, aes(x = time_min, y = fit_resp), col = "blue", size = 3)
pCTs
ggsave(filename = "Lsax_fertilisation_time/figures/prop_dev_betabinomial.pdf", plot = pCTs,
       scale = 0.7, dpi = "screen")
# dd[dd$p_dev==0,]  

ddt <- dd[dd$termination=="ARTIFICIAL", ]
fit_betabin <- glm(formula = p_dev ~ time_min, weights = tot, family = "betabinomial", data = ddt)
summary(fit_betabin)
