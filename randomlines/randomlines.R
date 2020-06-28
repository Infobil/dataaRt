library(tidyverse)
library(here)
library(scico)
library(ambient)
library(beepr)


steppp=function(linie){
  
  #take two values for new direction. one for y, other for x-axis
  
  direction=sample(-1:1,2,replace=TRUE)
  if(x>9950 | x<50 |
     y>9950 | y<50){
    var$oldline<<- .4
  }
  #if current line isn't the same as the old line, that means a new line has started --> set to origin point
  if(linie!=var$oldline){
    x<<-500
    y<<-500
    v_x<<-1
    v_y<<-1
  }
  
  #create row-dataset with the help of variables saved outside of function (x,y,v)
  row=tibble(
    x1=round(x,1),
    y1=round(y,1),
    #this is the speed of the point. it is added to the direction. so if a line was acellarting it needs time to change direction
    v_x=v_x*0.8+(grid$noise[x+y*1000]),
    v_y=v_y*0.8+(grid$noise[y+x*1000]),
    linie=linie,
    x2=NA,
    y2=NA)
    
  #new coordinates are calculated.
  row$x2=x+direction[1]+row[[3]]/var$vdivider
  row$y2=y+direction[2]+row[[4]]/var$vdivider
  
  #if the line leaves the border, old line is set to a value never used, so coordinates are resetted
  
  
  #variables of the rows are saved outside function. (this is probably pretty stupid process but yolo)  
  x<<-row$x2
  y<<-row$y2
  v_x<<-row$v_x
  v_y<<- row$v_y
  var$oldline<<-row$linie
  
  #return row which wil lbe then added to list
  return(row)
}

#create base grid with random noise. will be used for de/acceleration of lines later.
grid=long_grid(
  x=1:10000,
  y=1:10000
)%>%mutate(
  noise=gen_simplex(x, y,
    seed=52,
    #needs to be changed depending on grid size. the bigger the finer.
    frequency = 0.00039,
    )
  )
plot(grid,noise)


x<<-500
y<<-500
v_x<<-1
v_y<<-1
var=list(
  #vdivider has an influence on the importance of v, the speed, if it is low, the role of direction is more important and can change faster.
  vdivider=.5,
  #how many lines should be drawn
  nrlines=10,
  #how many points per line
  pointsperline=100,
  #oldline which is needed in the function.
  oldline=.5
 )

#with the var list create a vector which basically just repeats the line number pointsperlinetimes
linegroup=rep(1:var$nrlines,each=var$pointsperline)
#so the only thing fed into the function is the number of the line. this could be done a lot more elegant, but I still have troubles using apply function probarly
points=lapply(linegroup,steppp)%>%bind_rows()


ggplot(points,aes(group=linie, col=linie))+
  #geom_point(size=7,alpha=0.2,show.legend = FALSE)+
  geom_path(aes(x1,y1),size=2,lineend = "round",alpha=1,show.legend = FALSE)+
  #geom_raster(aes(fill=linie),show.legend = FALSE)+
  scale_color_scico(palette="roma")+
  theme_void()+
  coord_fixed()+
  theme(plot.background = element_rect(fill = "black"))

beep(sound=2)

ggplot(points,aes(group=linie))+
  geom_curve(aes(x1,y1,xend=x2,yend=y2),
             curvature = .8,angle=110,
             size=1,lineend = "round",col="white",alpha=0.6,
             show.legend = FALSE)+
  scale_color_scico(palette="roma")+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"))


divisor=10/max(points$x)
ggsave(here("randomlines","results", paste0(format(Sys.time(),"%m-%d-%H%M%S"),"-rlines.png")),width=max(points$x)*divisor,height=max(points$y)*divisor)

