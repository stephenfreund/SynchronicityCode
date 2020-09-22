library(ggplot2)


times <- read.csv(file = 'data.txt')

baseLine <- times[ which(times$Threads==1),]
total <- merge(times, baseLine, by=c("Language","ADT"))
total$speedup <- 1 / (total$Time.x / total$Time.y)

ggplot(total, aes(x=Threads.x, y=speedup, group=ADT, color="red")) +
  geom_line() +
  facet_grid(ADT ~ Language, scales="free")

ggsave("plots.pdf", width=10, height=14, units="in")