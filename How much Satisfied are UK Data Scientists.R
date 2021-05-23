# label parameters of the box-plot
xlab <- "Gender of Participants"
ylab <- "Annual Income in Pound sterling "
title <- "Annual income and Differnce In level of Satisfaction between Male and Female Data Scientists"

p5 <- ggplot(data=df,mapping = aes(x=Gender,y=Income))

p5+geom_boxplot(alpha=0.7)+ stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  scale_y_continuous(name = ylab,
                     limits=c(0,1000000))+
  scale_x_discrete(name=xlab)+
  ggtitle(title)+
  theme(legend.position = "bottom", axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 18, family="Times", face = "italic"),
        text=element_text(size = 10, family="Times"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))
