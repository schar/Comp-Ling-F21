library(ggplot2)
library(scales)
library(ggrepel)

csv <- read.csv('brown-grams.csv')


g <- ggplot(csv, aes(x=rank, y=freq, label=gram)) +
  #stat_function(fun=function(x) 1/(10*x), color="#D8D8D8") +
  geom_point(color="#ff0064", shape=".", alpha=.5) +
  geom_text_repel(data=subset(csv, rank==1 | rank==10 | rank==100 | rank==1000 | rank==10000 | rank==100000),
            aes(rank,freq,label=gram)) +
  #geom_text_repel(data=subset(csv, rank==1), label="freq=1/(10*rank)", color="#d8d8d8", nudge_y=.25, nudge_x=.6,
  #  segment.colour=NA) +
  scale_x_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +
  scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))

g + 
  scale_colour_viridis_d() + 
  theme_minimal() + 
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggsave('zipf.pdf')#, width=8, height=4.8)

h <- ggplot(csv, aes(x=rank, y=freq, label=gram)) +
  geom_point(color = "#ff0064", shape=".", alpha=.5) +
  geom_text_repel(data=subset(csv, rank==1 | rank==10 | rank==100 | rank==1000 | rank==10000 | rank==100000),
            aes(rank,freq,label=gram)) +
  scale_colour_viridis_d() + 
  theme_minimal() + 
  theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggsave('zipf2.pdf')#, width=8, height=4.8)