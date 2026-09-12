
library(implicitMeasures)
data("raw_data")
head(raw_data)

levels(raw_data$blockcode)
levels(raw_data$response)

iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
                           block_id = "blockcode",
                           mapA_practice = "practice.iat.Milkbad",
                           mapA_test = "test.iat.Milkbad",
                           mapB_practice = "practice.iat.Milkgood",
                           mapB_test = "test.iat.Milkgood",
                           latency_id = "latency",
                           accuracy_id = "correct",
                           trial_id = "trialcode",
                           trial_eliminate = c("reminder", "reminder1"),
                           demo_id = "blockcode",
                           trial_demo = "demo")
str(iat_cleandata)

iat1 = compute_iat(iat_cleandata, Dscore = "d1")
summary(iat1)
plot(iat1)
plot(iat1, graph = "points")
plot(iat1, graph = "points", order_sbj = "D-increasing")

t.test(iat1[iat1$cond_ord %in% "MappingA_First", "dscore_d1"],
       iat1[iat1$cond_ord %in% "MappingB_First", "dscore_d1"])$statistic

multi = multi_dscore(iat_cleandata$data_keep)
head(multi)
summary(multi)
plot(multi, graph = "individual")


iat1mapping = iat1[, c("participant", "cond_ord")]
iat1multiverse = merge(iat1mapping, multi$scores)

labels = colnames(iat1multiverse)[grep("dscore", colnames(iat1multiverse))]

results = data.frame(dscore = labels, 
                     t_stat = 0, 
                     p_value = 0)

for (i in 1:length(labels)) {
  temp = t.test(iat1multiverse[iat1multiverse$cond_ord %in% "MappingA_First", labels[i]],
                iat1multiverse[iat1multiverse$cond_ord %in% "MappingB_First", labels[i]])
results[i, "t_stat"] = temp$statistic
results[i, "p_value"] = temp$p.value
}
results$sig = factor(results$p_value < .05)
library(tidyverse)

ggplot(results, 
       aes(x = dscore, y = t_stat, size = p_value, col = sig)) + 
  geom_point(size = 3)

