library(tidyverse)
library(here)
library(imager)


#import image with imager
image=load.image(here("pixel sorting","images","banana.jpg"))
img=as.data.frame(image,wide="c")%>%rename(red=c.1,green=c.2,blue=c.3)%>%
  mutate( dark=red+green+blue,
          rgb=rgb(red,green,blue))


# sorting whole image --------
#along the x-axis. just selects the whole line and sorts it.
#sorts for dark, which is just the 3 colorchannels summed up


img1=img

newlist=list()
#each line of the image will be taken, sorted and added to a list, which will be binded together in the end 
#using the list gave some flexibility when experimenting around
for ( i in min(img$x):(max(img$x))){
  
  #one line of the image is selected
  line=img1%>%filter(x==i )
  
  #to experiment this line should be played with. sorting different things for different effects
  line$rgb =line$rgb[order(line$dark,line$green,line$red)]
  
  newlist[[i]]=line

}

#add list together to a dataframe
img1=newlist%>%bind_rows()

p<- ggplot(img,aes(x,y))+
  geom_raster(data=img1,aes(x,y,fill=rgb))+

  scale_fill_identity()+
  scale_y_reverse()+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "#000033"))

p




# select to be sorted area by contrast ------------------------------------

img2=img

#code will check the difference in darkness value between two pixels close to each other. if the value is bigger then this, it will register. change to get differnt results. (darkness 0:3)
threshold=0.2

newlist=list()
for(i in 1:max(img2$x)){
  #filters a line for x
    line=img2%>%filter(x==i )
  #subtracts the darkness-value from itself with a y-difference of two pixels. if bigger then threshold value changes
    line$contrast=c(FALSE,FALSE,abs(line$dark[3:nrow(line)]-line$dark[1:(nrow(line)-2)])>threshold)
    
  #because changes in color happen over more then one pixel, when threshold is reached, change the following five pixels to FALSE  
    for(bla in 1:(nrow(line)-5)){
      if(line$contrast[bla]==TRUE){
        line$contrast[(bla+1):(bla+4)]=FALSE
      }
    }
    
    #check how many contrast pairs there are --> start and endpoints
    pairs=floor(line$contrast%>%sum()/2)
    
    
    cords=data.frame(
      #select every second contrast points
      start=(line%>%filter(contrast==TRUE)%>%pull(y))[seq(from=1,by=2,length.out = pairs)],
      end=(line%>%filter(contrast==TRUE)%>%pull(y))[seq(from=2,by=2,length.out = pairs)])  %>%
      mutate(
       #this part is necessary to take into account the border. the sorted line is mirror at the first contrast point. if it reaches the images border that must be calculated. and if the line is shorter then expected the origin line must be shortened too. 
       distance1=end-start,
       negativ=ifelse(start-distance1<1,1,start-distance1),
       distance2=start-negativ,
       endreal=start+distance2
    )
    
    
    j=1
    while(j<=nrow(cords)){
      
      #here the sorting finally happens
      line[cords$negativ[j]:cords$start[j],7]=line[cords$start[j]:cords$endreal[j],]%>%arrange(-dark)%>%pull(rgb)
     
      #this version doesn't mirror the line but just sorts the selected part
      #line[cords$start[j]:cords$end[j],7]=line[cords$start[j]:cords$end[j],]%>%arrange(-dark)%>%pull(rgb)
      
     
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

 ggsave(here("sorting","results", paste0(format(Sys.time(),"%m-%d-%H.%M").png")),width=10,height=10)






