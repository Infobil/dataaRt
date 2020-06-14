library(tidyverse)
library(here)
library(imager)


#import image with imager
filename="tree.jpg"
image=load.image(here("images",filename))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=red+green+blue)



img1=img%>%mutate(
  redgreen=(red+green)/2,
  greenblue=(green+blue)/2,
  bluered=(blue+red)/2,
  rgb=rgb(red,green,blue),
  rbg=rgb(red,blue,green),
  gbr=rgb(green,blue,red),
  grb=rgb(green,red,blue),
  brg=rgb(blue,red,green),
  bgr=rgb(blue,green,red),
  nrcolor=(floor((y-1+sample(0:20,nrow(img),replace = TRUE))/(max(img$y)/2))*3+
             floor((x-1+sample(-5:5,nrow(img),replace = TRUE))/(max(img$x)/3))+1),
  color=ifelse(nrcolor>3,
               ifelse(nrcolor==4,grb,
                    ifelse(nrcolor==5,brg,bgr)),
               
               ifelse(nrcolor<=1,rgb,
                      ifelse(nrcolor==2,rbg,gbr)))
)



p<- ggplot(img1,aes(x,y))+
  geom_raster(data=img1,aes(x,y,fill=color))+
  
  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "black"))

p

divisor=10/max(img$x)
ggsave(here("colorgames","results", paste0(format(Sys.time(),"%m-%d-%H.%M"),"-colored.png")),width=max(img$x)*divisor,height=max(img$y)*divisor)

