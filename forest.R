rm(list = ls())
setwd("F:/env biomass diversity/4 Predicting the structure of soil communities/forest/before")
library(tidyverse)
library(psych)
load(file = "gradientforest.RData")


setwd("F:/env biomass diversity/4 Predicting the structure of soil communities/forest")
### bacteria maize
skim(sorted_models2) 
sorted_models2$fac <- NA
sorted_models2$fac[which(sorted_models2$var %in% c("bio1","bio12"))] <- "cli"
sorted_models2$fac[which(sorted_models2$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
sorted_models2$fac[which(sorted_models2$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP","TK", "AK", "TS", "AS"))] <- "np"
sorted_models2$fac[which(sorted_models2$var %in% c(  "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"

bac_ma2 <- NULL
for (i in 1:1882) {
  a <- filter(sorted_models2,spec %in% levels(sorted_models2$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(sorted_models2$spec)[i]
  b$sum <- sum(b$x)
  bac_ma2 <- rbind(bac_ma2,b)
}
colnames(bac_ma2) <- c("fac","rsq.var","asv","rsq")
bac_ma2$fac <- factor(bac_ma2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(bac_ma2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.5))+
  theme_classic()
  #theme(axis.text.y = element_blank(),
   #     axis.ticks = element_blank())
plot
ggsave(plot, filename = "maize_bacteria_extendforest.pdf",width = 10, height = 10) 


### bacteria rice
bac_ri$fac <- NA
bac_ri$fac[which(bac_ri$var %in% c("bio1","bio12"))] <- "cli"
bac_ri$fac[which(bac_ri$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
bac_ri$fac[which(bac_ri$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP","TK", "AK", "TS", "AS" ))] <- "np"
bac_ri$fac[which(bac_ri$var %in% c( "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"
skim(bac_ri) 

bac_ri2 <- NULL
for (i in 1:2333) {
  a <- filter(bac_ri,spec %in% levels(bac_ri$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(bac_ri$spec)[i]
  b$sum <- sum(b$x)
  bac_ri2 <- rbind(bac_ri2,b)
}
colnames(bac_ri2) <- c("fac","rsq.var","asv","rsq")
bac_ri2$fac <- factor(bac_ri2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(bac_ri2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.55))+
  theme_classic()
plot
ggsave(plot, filename = "rice_bacteria_extendforest.pdf",width = 10, height = 10) 



### fungi maize
fun_ma$fac <- NA
fun_ma$fac[which(fun_ma$var %in% c("bio1","bio12"))] <- "cli"
fun_ma$fac[which(fun_ma$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
fun_ma$fac[which(fun_ma$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP","TK", "AK", "TS", "AS"))] <- "np"
fun_ma$fac[which(fun_ma$var %in% c(  "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"
skim(fun_ma) 

fun_ma2 <- NULL
for (i in 1:230) {
  a <- filter(fun_ma,spec %in% levels(fun_ma$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(fun_ma$spec)[i]
  b$sum <- sum(b$x)
  fun_ma2 <- rbind(fun_ma2,b)
}
colnames(fun_ma2) <- c("fac","rsq.var","asv","rsq")
fun_ma2$fac <- factor(fun_ma2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(fun_ma2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.5))+
  theme_classic()
plot
ggsave(plot, filename = "maize_fungi_extendforest.pdf",width = 10, height = 10) 



### fungi rice
fun_ri$fac <- NA
fun_ri$fac[which(fun_ri$var %in% c("bio1","bio12"))] <- "cli"
fun_ri$fac[which(fun_ri$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
fun_ri$fac[which(fun_ri$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP","TK", "AK", "TS", "AS"))] <- "np"
fun_ri$fac[which(fun_ri$var %in% c(  "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"
skim(fun_ri) 

fun_ri2 <- NULL
for (i in 1:293) {
  a <- filter(fun_ri,spec %in% levels(fun_ri$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(fun_ri$spec)[i]
  b$sum <- sum(b$x)
  fun_ri2 <- rbind(fun_ri2,b)
}
colnames(fun_ri2) <- c("fac","rsq.var","asv","rsq")
fun_ri2$fac <- factor(fun_ri2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(fun_ri2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.5))+
  theme_classic()
plot
ggsave(plot, filename = "rice_fungi_extendforest.pdf",width = 10, height = 10) 



### protist maize
pro_ma$fac <- NA
pro_ma$fac[which(pro_ma$var %in% c("bio1","bio12"))] <- "cli"
pro_ma$fac[which(pro_ma$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
pro_ma$fac[which(pro_ma$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP","TK", "AK", "TS", "AS"))] <- "np"
pro_ma$fac[which(pro_ma$var %in% c( "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"
skim(pro_ma) 

pro_ma2 <- NULL
for (i in 1:527) {
  a <- filter(pro_ma,spec %in% levels(pro_ma$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(pro_ma$spec)[i]
  b$sum <- sum(b$x)
  pro_ma2 <- rbind(pro_ma2,b)
}
colnames(pro_ma2) <- c("fac","rsq.var","asv","rsq")
pro_ma2$fac <- factor(pro_ma2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(pro_ma2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.5))+
  theme_classic()
plot
ggsave(plot, filename = "maize_protist_extendforest.pdf",width = 10, height = 10) 


### protist rice
pro_ri$fac <- NA
pro_ri$fac[which(pro_ri$var %in% c("bio1","bio12"))] <- "cli"
pro_ri$fac[which(pro_ri$var %in% c("DOC", "OM","CEC","pH","Clay","Silt","Sand"))] <- "phy"
pro_ri$fac[which(pro_ri$var %in% c("TN", "AN", "NO3", "NH4", "TP", "AP", "TK", "AK", "TS", "AS"))] <- "np"
pro_ri$fac[which(pro_ri$var %in% c( "TFe", "AFe", "TCu", "ACu", "TZn", "AZn", "TMn", "AMn"))] <- "mic"
skim(pro_ri) 

pro_ri2 <- NULL
for (i in 1:471) {
  a <- filter(pro_ri,spec %in% levels(pro_ri$spec)[i])
  b <- aggregate(a$rsq.var, by = list(a$fac), sum)
  b$asv <- levels(pro_ri$spec)[i]
  b$sum <- sum(b$x)
  pro_ri2 <- rbind(pro_ri2,b)
}
colnames(pro_ri2) <- c("fac","rsq.var","asv","rsq")
pro_ri2$fac <- factor(pro_ri2$fac, levels = c("cli","phy","np","mic"))

cols <- c("cli" = "#00b4d8", 
          "phy" = "#bc6c25",
          "np" = "#9b5de5",
          "mic" = "#81b29a")
plot <- ggplot(pro_ri2, aes(x = rsq.var, y = reorder(asv,rsq), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,0.5))+
  theme_classic()
plot
ggsave(plot, filename = "rice_protist_extendforest.pdf",width = 10, height = 10) 



#### complete random forest
bac_ma2$com <- bac_ma2$rsq.var/bac_ma2$rsq
bac_ri2$com <- bac_ri2$rsq.var/bac_ri2$rsq

fun_ma2$com <- fun_ma2$rsq.var/fun_ma2$rsq
fun_ri2$com <- fun_ri2$rsq.var/fun_ri2$rsq

pro_ma2$com <- pro_ma2$rsq.var/pro_ma2$rsq
pro_ri2$com <- pro_ri2$rsq.var/pro_ri2$rsq

mic_bac_ma2 <- filter(bac_ma2, fac %in% "mic")
colnames(mic_bac_ma2)[5] <- "mic"
mic_bac_ma2 <- mic_bac_ma2[,c(3,5)]
bac_ma3 <- merge(bac_ma2, mic_bac_ma2, by = "asv")
plot_bac_ma <- ggplot(bac_ma3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_bac_ma



mic_bac_ri2 <- filter(bac_ri2, fac %in% "mic")
colnames(mic_bac_ri2)[5] <- "mic"
mic_bac_ri2 <- mic_bac_ri2[,c(3,5)]
bac_ri3 <- merge(bac_ri2, mic_bac_ri2, by = "asv")
plot_bac_ri <- ggplot(bac_ri3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_bac_ri


mic_fun_ma2 <- filter(fun_ma2, fac %in% "mic")
colnames(mic_fun_ma2)[5] <- "mic"
mic_fun_ma2 <- mic_fun_ma2[,c(3,5)]
fun_ma3 <- merge(fun_ma2, mic_fun_ma2, by = "asv")
plot_fun_ma <- ggplot(fun_ma3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_fun_ma


mic_fun_ri2 <- filter(fun_ri2, fac %in% "mic")
colnames(mic_fun_ri2)[5] <- "mic"
mic_fun_ri2 <- mic_fun_ri2[,c(3,5)]
fun_ri3 <- merge(fun_ri2, mic_fun_ri2, by = "asv")
plot_fun_ri <- ggplot(fun_ri3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_fun_ri


mic_pro_ma2 <- filter(pro_ma2, fac %in% "mic")
colnames(mic_pro_ma2)[5] <- "mic"
mic_pro_ma2 <- mic_pro_ma2[,c(3,5)]
pro_ma3 <- merge(pro_ma2, mic_pro_ma2, by = "asv")
plot_pro_ma <- ggplot(pro_ma3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_pro_ma


mic_pro_ri2 <- filter(pro_ri2, fac %in% "mic")
colnames(mic_pro_ri2)[5] <- "mic"
mic_pro_ri2 <- mic_pro_ri2[,c(3,5)]
pro_ri3 <- merge(pro_ri2, mic_pro_ri2, by = "asv")
plot_pro_ri <- ggplot(pro_ri3, aes(x = com, y = reorder(asv,mic), fill = fac))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = cols)+
  labs(x = "", y = "")+
  scale_y_discrete(breaks = NULL)+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,1))+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14))
plot_pro_ri


plot <- (plot_bac_ma + plot_bac_ri)/(plot_fun_ma + plot_fun_ri)/(plot_pro_ma + plot_pro_ri)
plot
ggsave(plot, filename = "gradientforest_complete.pdf",width = 20,height = 30)


