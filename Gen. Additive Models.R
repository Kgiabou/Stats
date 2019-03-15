range<-na.exclude(read.delim("gam_range.txt", h=T, sep="\t"))

breadth<-na.exclude(read.delim("Niche Breadth Eurasia until 11kyr.txt", h=T, sep="\t"))

env<-range[,3:10]
pc<-dudi.pca(env, center=TRUE, scannf=TRUE, nf=2)
culture<-range[,11]
data<-data.frame(pc$li,culture)
data<-data.frame(range[,2],data)
colnames(data)<-paste(c("Range.size","PC1","PC2","culture"))
write.table(data, "PCA_range.txt", quote=FALSE)
gam1<-gam(Range.size~ PC1 + PC2 + factor(culture), family=poisson, link="log", data=data)

steps<-step.gam(gam1, scope=list("PC1"= ~ 1 + PC1 + s(PC1,4), "PC2"= ~ 1 + PC2 + s(PC2,4), "culture"= ~ 1 + factor(culture)), direction="both", trace=TRUE)
