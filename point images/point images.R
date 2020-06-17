library(tidyverse)
library(here)
library(imager)


filename="airwhale.jpg"
image=load.image(here("images",filename))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=red+green+blue,
          rgb=rgb(red,green,blue))


teiler=5

img1=img%>%
  mutate(
    x=ceiling(x/teiler),
    y=ceiling(y/teiler)  )%>%
  group_by(x,y)%>%
  summarise(
    red=mean(red),
    green=mean(green),
    blue=mean(blue)
   )%>%
  ungroup()%>%
  mutate( dark=red+green+blue,
          rgb=rgb(red,green,blue),
          x=ifelse(x==max(img$x)/teiler,NA,x),
          y=ifelse(y==max(img$y)/teiler,NA,y)
             )%>%
  arrange(y)


ggplot(img1,aes(x=x,y=y,size=(-(dark)^1/10)*40))+
  geom_point(show.legend = FALSE,col="black",alpha=0.6,shape=15)+
  theme_void()+
  coord_fixed()+
  scale_y_reverse()+
  theme(plot.background = element_rect(fill = "white"))



grid=tibble(
  x=rep(c(1:(max(img$x)/teiler-1),NA),(max(img$y)/teiler))*100,
  y=round(sin(1:(max(img$y/teiler))/2)+rep(c(1:(max(img$y)/teiler-1),NA),each=max(img$x)/teiler),2)*100
)

ggplot(grid,aes(x=x,y=y))+
  geom_path(show.legend = FALSE,col="black",alpha=1)+
  theme_void()+
  coord_fixed()+
  scale_y_reverse()+
  theme(plot.background = element_rect(fill = "white"))





divisor=10/max(img$x)
ggsave(here("point images","results", paste0(format(Sys.time(),"%m-%d-%H.%M"),"-pointed.png")),width=max(img$x)*divisor,height=max(img$y)*divisor)
