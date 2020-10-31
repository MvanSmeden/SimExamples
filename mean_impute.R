require(gganimate)
require(tidyverse)

set.seed(736)

sd <- .3
X <- rnorm(15)
Y <- X+rnorm(length(X),sd=sd)

scale <- seq(1,30,by=1)
OUT <- data.frame(scale=scale[1],X=X,Y=Y,imp=0)
for(i in 2:length(scale)){
  XTEMP<- rnorm(1)
  YTEMP<- XTEMP+rnorm(1,sd=sd)
  OUT <- rbind(OUT,rbind(cbind(scale=i,OUT[OUT$scale==i-1,-1]),
                         data.frame(scale=i,X=XTEMP,Y=mean(Y),imp=1)))
}

p1 <- OUT %>% ggplot(aes(x=X,y=Y,group=scale,color=as.factor(imp)))+geom_point(size=3)+ transition_states(scale, transition_length = 2, state_length = 3) + 
  ease_aes('cubic-in-out')+ theme_minimal()+theme(axis.text.y=element_blank(),axis.text.x=element_blank()) +
  ylab("Y") + geom_smooth(method = "lm", se = F,col="black",size=1.5) + geom_abline(lty=2) + 
  ggtitle("Replace missing by mean") + theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5),legend.position = "none")

anim <- animate(p1,width=700)
anim_save("meanreplace.gif")




