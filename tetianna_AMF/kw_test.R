


w1 = wilcox.test(percent_colonization ~ as.factor(water), data = AMF_results)

summary(w1)

names(AMF_results)


kruskal.test(percent_colonization ~ as.factor(water), data = AMF_results)

kruskal.test(percent_colonization ~ as.factor(ACAM_density), data = AMF_results)
