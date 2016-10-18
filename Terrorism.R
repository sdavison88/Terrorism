library(ggplot2)
library(ggthemes)

globalterrorismdb_0616dist <- read.csv("~/Desktop/Career/globalterrorismdb_0616dist.csv", comment.char="#")

t_agg <- aggregate(eventid ~ country_txt + iyear, data=globalterrorismdb_0616dist, FUN=length)
t_agg_sort <- t_agg[with(t_agg,order(iyear,eventid)),]


t_plot <- ggplot(t_agg,aes(iyear,eventid))+
  geom_line(aes(color=country_txt))  +
  theme_fivethirtyeight() +
  theme(legend.position="none") +
  ggtitle("Terrorist Attacks by Country")+
  scale_fill_wsj()

t_agg_sort$colors <- "World"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Pakistan")] <- "Pakistan"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Afghanistan")] <- "Afghanistan"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Iraq")] <- "Iraq"
t_agg_sort$colors[which(t_agg_sort$country_txt=="India")] <- "India"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Philippines")] <- "Philippines"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Somalia")]<-"Somalia"
t_agg_sort$colors[which(t_agg_sort$country_txt=="Ukraine")]<-"Ukraine"

t_agg_sort_A <- t_agg_sort[with(t_agg_sort,order(iyear,colors,decreasing = T)),]


t_bplot <- ggplot(t_agg_sort_A,aes(iyear,eventid,fill=colors))+
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("#FDDE9B","#D592E3","#4A90E2","#FC6B57","#7ED321","#417505","#50E3C2","#9B9B9B"))+
  ggtitle("Terrorist Attacks by Year")

t_agg_sort$colors <- "All"
t_agg_sort$colors[which(t_agg_sort$country_txt=="United States")] <- "USA"

pareto_agg <- aggregate(eventid ~ country_txt, data=t_agg, FUN=sum)

ggplot(t_agg_sort[which(t_agg_sort$country_txt=='United States' & t_agg_sort$iyear>'1972'),],aes(iyear,eventid))+
  geom_line(aes(color=country_txt)) +
  geom_text(data = subset(t_agg_sort,iyear=='2015' & country_txt=="United States"), aes(label=country_txt)) +
  theme_fivethirtyeight() +
  theme(legend.position="none") +
  ggtitle("Terrorism in the United States")

t_agg_US_In <- aggregate(eventid ~ country_txt + iyear + attacktype1_txt, data=globalterrorismdb_0616dist[which(globalterrorismdb_0616dist$country_txt=="United States"),], FUN=length)
t_agg_US_In <- t_agg_US_In[with(t_agg_US_In,order(iyear,-eventid)),]

ggplot(t_agg_US_In[which(t_agg_US_In$iyear>"1972"),],aes(iyear,eventid,fill=attacktype1_txt))+
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  ggtitle("US Attack Types by Year")

t_agg_All_In <- aggregate(eventid ~ country_txt + iyear + attacktype1_txt, data=globalterrorismdb_0616dist, FUN=length)
t_agg_All_In <- t_agg_All_In[with(t_agg_All_In,order(iyear,eventid,country_txt)),]

ggplot(t_agg_All_In[which(t_agg_All_In!=""),],aes(iyear,eventid,fill=attacktype1_txt))+
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  theme(legend.title=element_blank()) +
  ggtitle("All Attack Types by Year")


########################
dat <- data.frame(x=runif(10),y=runif(10),
                  grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Set1")
names(myColors) <- levels(c("a","b","c"))
colScale <- scale_colour_manual(name = "grp",values = myColors)
myColors
p <- ggplot(dat,aes(x,y,colour = grp)) + geom_point()
p1 <- p + colScale
p1

#A second plot with only four of the levels
p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale

mycolors2 <- unique(t_agg_sort[,c(1,4)])

latlong <- aggregate(eventid ~ latitude + longitude, data=latlong, FUN=length)

ggpareto <- function(x) {
  
  title <- deparse(substitute(x))
  
  x <- data.frame(modality = na.omit(x))
  
  library(dplyr)
  
  Df <- x %>% group_by(modality) %>% summarise(frequency=n()) %>% 
    arrange(desc(frequency))
  
  Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
  
  Df <- Df %>% mutate(modality_int = as.integer(modality), 
                      cumfreq = cumsum(frequency), cumperc = cumfreq/nrow(x) * 100)
  nr <- nrow(Df)
  N <- sum(Df$frequency)
  
  Df_ticks <- data.frame(xtick0 = rep(nr +.55, 11), xtick1 = rep(nr +.59, 11), 
                         ytick = seq(0, N, N/10))
  
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  
  library(ggplot2)
  
  g <- ggplot(Df, aes(x=modality, y=frequency)) + 
    geom_bar(stat="identity", aes(fill = modality_int)) +
    geom_line(aes(x=modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x=modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks=seq(0, N, N/10), limits=c(-.02 * N, N * 1.02)) + 
    scale_x_discrete(breaks = Df$modality) +
    guides(fill = FALSE, color = FALSE) + 
    annotate("rect", xmin = nr + .55, xmax = nr + 1, 
             ymin = -.02 * N, ymax = N * 1.02, fill = "white") +
    annotate("text", x = nr + .8, y = seq(0, N, N/10), label = y2, size = 3.5) +
    geom_segment(x = nr + .55, xend = nr + .55, y = -.02 * N, yend = N * 1.02, color = "grey50") +
    geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
    labs(title = paste0("Pareto Chart of ", title), y = "absolute frequency") +
    theme_fivethirtyeight()+
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  
  return(list(graph = g, Df = Df[, c(3, 1, 2, 4, 5)]))
}
pareto_agg <- aggregate(eventid ~ country_txt, data=t_agg, FUN=sum)
p_expanded <- pareto_agg[rep(row.names(pareto_agg), pareto_agg$eventid), 1]

Example <- rep(c(letters[1:2], LETTERS[1:3]), c(15, 39, 6, 42, 50))


ggplot(t_agg_sort,aes(iyear,eventid))+
  geom_line(aes(color=country_txt)) +
  geom_text(data = subset(t_agg,iyear=='2015'),aes(label=country_txt)) +
  theme_fivethirtyeight() +
  theme(legend.position="none") +
  scale_fill_wsj()
