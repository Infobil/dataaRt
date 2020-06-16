library(tidyverse)
library(here)
library(imager)


#import image with imager
filename="banana.jpg"
image=load.image(here("images",filename))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=red+green+blue,
          rgb=rgb(red,green,blue))

img1=img

mirrorslices=function(numberofslices=40){

slicecords=sample(1:max(img$y),numberofslices*2)%>%sort()*max(img$x)

(1:(numberofslices*2))*(ceiling(max(img$y)/numberofslices/2))

max(img$y)/4


for( i in 1:(numberofslices)){
  cordA=slicecords[(i*2-1)]+1
  cordB=slicecords[(i*2)]
  img1$x[cordA:cordB]=rep(max(img$x):1,(cordB-cordA+1)/max(img$x))
}


}

p<- ggplot(img1,aes(x,y))+
  geom_raster(aes(x,y,fill=rgb))+
  
  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "black"))

p


divisor=10/max(img$x)
ggsave(here("mirror slices","results", paste0(format(Sys.time(),"%m-%d-%H.%M"),"-colored.png")),width=max(img$x)*divisor,height=max(img$y)*divisor)
