library(tidyverse)
library(here)
library(scico)
library(ambient)
library(beepr)

filename="banj.jpg"
image=imager::load.image(here("images",filename))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=round((red+green+blue)/3,1),
          rgb=rgb(red,green,blue) )


distancebetweenlines=20
img1=img%>%filter(y%%distancebetweenlines==0)%>%arrange(x,-y)

lengthY=round(max(img1$y)/distancebetweenlines)
lengthX=max(img1$x)


points=tibble(
  x=rep(1:lengthX,each=lengthY),
  y=1,
  line=rep(1:lengthY,lengthX),
)

points$y[1:lengthY]=seq(from=1,by=distancebetweenlines,length.out = lengthY)


for(i in 1:(lengthX-1)){
  
  dif=img1$dark[((i-1)*lengthY+1):(i*lengthY)]-img1$dark[((i)*lengthY+1):((i+1)*lengthY)]
  points$y[((i)*lengthY+1):((i+1)*lengthY)]=points$y[((i-1)*lengthY+1):(i*lengthY)]*(1+dif/20)

}



ggplot(points,aes(x,y,group=line,col=line))+
  geom_path(col="black",show.legend = FALSE)+
  scale_color_scico(palette="batlow")+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "white"))


divisor=10/max(points$x)
ggsave(here("randomlines","results", paste0(format(Sys.time(),"%m-%d-%H%M%S"),"-parallines.png")),width=max(points$x)*divisor,height=max(points$y)*divisor)

