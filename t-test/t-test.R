setwd("D:/MultiOmics/Classical")
library(coefplot)
library(glmnet) 
library(dplyr) 
library(ggplot2)  
library(reshape2)
library(ggpubr) 
library(rstatix)
library(EnvStats)


x<-read.csv("D:/MultiOmics/Data/FeatVal.csv") 
rownames(x) <- x[,2]; x <- x[,-1:-2]  

x <- x[, c(names(x)[-which(names(x) == "Response")], "Response")]
x[x=="False"] <- 0 
x[x=="True"] <- 1 

x$Response <- ifelse(x$Response == 0, "RD", "pCR")
x[x=="NaN"] <- 0 

x[,1:93] <- lapply(x[,1:93], as.numeric)
x.m <- melt(x) 


colnames(x.m) <- c("Response", "Feature", "value")

stat.test <- x.m %>%
  group_by(Feature) %>%
  t_test(value ~ Response) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

stat.test <- stat.test %>%
  add_xy_position(fun = "mean_se", x ="Response")  

# stat.test <- stat.test[order(stat.test$p),]
# stat.test <- stat.test[stat.test$p.adj<0.05,]

stat.test <- subset(stat.test,stat.test$p.adj<0.05)

stat.test2 <- stat.test

stat.test2$statistic2<-stat.test2$statistic/ abs(stat.test2$statistic)
stat.test2$`-Log(p.adj)*Direction` <- - log10(stat.test2$p.adj ) * stat.test2$statistic2
stat.test2 <- stat.test2[order(-stat.test2$`-Log(p.adj)*Direction`),]

stat.test2$Feature<- as.character(stat.test2$Feature)
stat.test2$Feature <- factor(stat.test2$Feature, levels=stat.test2$Feature)

stat.test2 <- stat.test2[, c(1,16,17)]

pdf("D:/MultiOmics/Classical/Fig1b4.pdf",width=20,height=10)
ggplot(stat.test2, aes(x=Feature, y=`-Log(p.adj)*Direction`, fill= as.factor(statistic2)))+
 # geom_boxplot(width=0.65)+
  geom_bar(stat="identity", width=0.65)+
  scale_color_discrete(name="")+theme(axis.title.y=element_blank())+
  theme_bw()+theme(text=element_text(size =30))+theme(axis.text.x=element_text(angle=90))+
  theme(legend.position="none")+
  ylim(-4, 4) 
dev.off() 

 
x.m <-x.m[x.m$Feature %in% stat.test$Feature,]

stat.test <- x.m %>%
  group_by(Feature) %>%
  t_test(value ~ Response) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

stat.test <- stat.test %>%
  add_xy_position(fun = "mean_se", x ="Response")  



dev.off()
pdf("D:/MultiOmics/Classical/ResponseFig1b1.pdf",width=24,height=4)
ggplot(x.m , aes(x=Response, y=value, color= Response))+
  geom_violin( ) +
  geom_boxplot(width=0.1, alpha=0.2) + 
  facet_wrap(~ Feature,ncol=9) +  
  stat_pvalue_manual(stat.test, label = "p.adj",size = 7, hide.ns = TRUE)+
   stat_n_text(size = 8)+ 
  theme_bw()+theme(text = element_text(size =25))+scale_y_continuous(expand=expansion(mult=c(0.02,-0.3))) +
  theme(legend.position="none")
dev.off() 

 