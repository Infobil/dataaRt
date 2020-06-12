library(tidyverse)
library(here)
library(imager)


image=load.image(here("sorting","banana.jpg"))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=red+green+blue)%>% 
  mutate(rgb=rgb(red,green,blue))


img1=img
lengi=max(img$y)
newlist=list()
for ( i in min(img$x):(max(img$x))){
  
  line=img1%>%filter(x==i )
  line$rgb =line$rgb[order(line$dark,line$green,line$red)]
  newlist[[i]]=line

}

img1=newlist%>%bind_rows()

p<- ggplot(img,aes(x,y))+
  geom_raster(data=img1,aes(x,y,fill=rgb))+

  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "#000033"))

p



img2=img

newlist=list()
for(i in 1:max(img2$x)){
  
    line=img2%>%filter(x==i )
    
    line$contrast=c(abs(line$dark[2:nrow(line)]-line$dark[1:(nrow(line)-1)])>.1,FALSE)
    for(bla in 1:(nrow(line)-5)){
      if(line$contrast[bla]==TRUE){
        line$contrast[(bla+1):(bla+3)]=FALSE
      }
    }
    
    pairs=floor(line$contrast%>%sum()/2)
    
    cords=data.frame(
      start=(line%>%filter(contrast==TRUE)%>%pull(y))[seq(from=1,by=2,length.out = pairs)],
      end=(line%>%filter(contrast==TRUE)%>%pull(y))[seq(from=2,by=2,length.out = pairs)])  %>%
      mutate(
       bstand=end-start,
       negativ=ifelse(start-bstand<1,1,start-bstand),
       abstand=start-negativ,
       endreal=start+abstand
    )
    
    
    j=1
    while(j<=nrow(cords)){
      line[cords$start[j]:cords$endreal[j],7]=line[cords$start[j]:cords$endreal[j],]%>%arrange(-dark)%>%pull(rgb)
      
    #  line[cords$negativ[j]:cords$start[j],7]=line[cords$start[j]:cords$endreal[j],]%>%arrange(-dark)%>%pull(rgb)
      j=j+1 
       }
  
    newlist[[i]]=line
}

 img2=newlist%>%bind_rows()

 p<- ggplot(img2,aes(x,y))+
   geom_raster(data=img2,aes(x,y,fill=rgb))+
   
   scale_fill_identity()+
   scale_y_reverse()+
   theme_void()+
   #coord_fixed()+
   theme(plot.background = element_rect(fill = "black"))
 
 p

 ggsave(here("sorting","results/blm5.png"),width=10,height=10)






