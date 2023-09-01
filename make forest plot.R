library(forcats)
library(dplyr)
library(dplyr)
library(ggplot2)

Book2 <- read.csv("C:/Users/cwyse/OneDrive - Maynooth University/Desktop/Book2.csv")
Book2 <- Book2[c(1:45),] 

Book2$subgroup <-factor(Book2$subgroup)
Book2$subgroup_level <-factor(Book2$subgroup_level, levels=Book2$subgroup_level)

Book2[4,"col"]<-"Lower"
Book2$col <- factor(Book2$col)
                    #, labels = c("Ref", "Lower", "NS", "Higher"))

 
p <- ggplot(data = Book2, aes(x = adj_or, y = subgroup_level)) +
  geom_vline(xintercept = 1, linetype = 2, linewidth=0.5, color = "black") +
  geom_point(aes(color = col), size = 3) +
  xlab("Adjusted Effect Size") +
  ylab("") +
  geom_errorbar(aes(xmax = ci_high, xmin = ci_low, color = col), size = 0.8, width = 0.5) + 
  theme(plot.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust = 0, vjust = 1, angle = 180, face = "bold"),
        legend.title = element_blank()) +
  theme_bw() +
   scale_y_discrete(breaks=c(1:45),
                              labels = labels) +
  scale_color_manual(values = c("black","grey", "blue", "red"),limits = c("Ref", "NS", "Lower", "Higher"),
                       name = "")

#fig3
p.grid <- p + facet_grid(subgroup ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(width = 2, multi_line = TRUE))

png("fig3.png", units="cm", width=20, height=30, res=300)
p.grid 
dev.off()










#make figure 4


Book2 <- read.csv("C:/Users/cwyse/OneDrive - Maynooth University/Desktop/Book2.csv")

labels <- c( "No", "Yes","Female", "Male","am/am" , "pm/am" , "am/pm" , "pm/pm" , "No", "Yes","<60",">60","<30",">=30", "No","Yes", "No", "Yes")

Book2$subgroup <-factor(Book2$subgroup)
Book2$subgroup_level <-factor(c(1:18))

Book2$col <- factor(Book2$col)

p <- ggplot(data = Book2, aes(x = adj_or, y = subgroup_level)) +
  geom_vline(xintercept = 1, linetype = 2, linewidth=0.5, color = "black") +
  geom_point(aes(color = col), size = 3) +
  xlab("Hazard Ratio, 95% CI") +
  ylab("") +
  geom_errorbar(aes(xmax = ci_high, xmin = ci_low, color = col), size = 0.8, width = 0.5) + 
  theme(plot.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust = 0, vjust = 1, angle = 180, face = "bold"),
        legend.title = element_blank()) +
  theme_bw() +
  scale_y_discrete(breaks=c(1:18),
                   labels = labels) +
  scale_color_manual(values = c("black","grey", "blue", "red"),limits = c("Ref", "NS", "Lower", "Higher"),
                     name = "")

#fig4
p.grid <- p + facet_grid(subgroup ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(width = 2, multi_line = TRUE))

png("fig4.png", units="cm", width=20, height=15, res=300)
p.grid 
dev.off()
