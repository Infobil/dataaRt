library(tidyverse)
library(here)
library(imager)


image=load.image(here("sorting/test.jpg"))
img=as.data.frame(image,wide="c")%>%rename(red1=c.1,green1=c.2,blue1=c.3)%>%
  mutate(red=red1, blue=blue1, green=green1,
         dark=red+green+blue)%>% 
  mutate(rgb=rgb(red,green,blue))


img1=img
lengi=max(img$y)
newlist=list()
for ( i in min(img$x):(max(img$x))){
  
  line=img1%>%filter(x==i )
  line$y =line$y[order(line$dark)]
  newlist[[i]]=line

}

img1=newlist%>%bind_rows()

p<- ggplot(img1,aes(x,y))+
  geom_raster(data=img1,aes(x,y,fill=rgb))+

  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()+
  coord_fixed()
p
