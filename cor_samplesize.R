# dependencies
require(gganimate)
require(tidyverse)

# code for 1 figure
set.seed(2020)

n <-10
r <- 10
D <- data.frame(MASS::mvrnorm(r*n,mu=c(0,0,0),Sigma=diag(3)))
D$id <- rep(1:r,n)
Cor <- data.frame(cbind(id=unique(D$id),
             corX1X2=by(D,D$id,function(x) cor(x[,1],x[,2])),
             corX1X3=by(D,D$id,function(x) cor(x[,1],x[,3]))))

pl <-Cor %>% ggplot(aes(x=corX1X2,y=corX1X3,group=id))+geom_point(size=3)+ transition_time(id) + 
  ease_aes("linear")+ theme_classic() + geom_point(aes(x=0,y=0),color="red",size=8) +
  geom_point(aes(x=0,y=0),color="white",size=5) + geom_point(aes(x=0,y=0),color="red",size=2)+
  geom_vline(xintercept = 0,lty=2) +  geom_hline(yintercept=0,lty=2) +
  ylab("correlation X1 and X3") + xlab("correlation X1 and X2") +ylim(c(-1,1)) + xlim(c(-1,1))+
  ggtitle("N = 10") + theme(plot.title = element_text(size = 15, face = "bold",hjust = 0.5))

# code for 4 figures
set.seed(80808)

n<-c(10,25,100,500)
r <- 10
D <- data.frame(MASS::mvrnorm(sum(r*n),mu=c(0,0,0),Sigma=diag(3)))
D$ID <- as.factor(c(rep(1:r,n[1]),rep(1:r,n[2]),rep(1:r,n[3]),rep(1:r,n[4])))
D$N <-  as.factor(c(rep(n[1],r*n[1]),rep(n[2],r*n[2]),rep(n[3],r*n[3]),rep(n[4],r*n[4])))
  
Cor <- D %>% group_by(ID,N) %>% summarise(corX1X2 = cor(X1,X2), corX1X3 = cor(X1,X3))

pl2 <-Cor %>% ggplot(aes(x=corX1X2,y=corX1X3,group=ID)) + 
  facet_wrap(.~N,labeller="label_both") + transition_time(as.numeric(ID)) + ease_aes("linear")+
  theme_classic() + geom_point(aes(x=0,y=0),color="red",size=8) +
  geom_point(aes(x=0,y=0),color="white",size=5) + geom_point(aes(x=0,y=0),color="red",size=2)+
  geom_point(size=3) + geom_vline(xintercept = 0,lty=2) +  geom_hline(yintercept=0,lty=2) +
  ylab("correlation X1 and X3") + xlab("correlation X1 and X2") +ylim(c(-1,1)) + xlim(c(-1,1))+
  theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))

anim <- animate(pl2,width=700)
anim_save("cor2.gif")

