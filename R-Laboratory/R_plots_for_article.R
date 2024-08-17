ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 14.5, shape2 = 8),colour='#006600',size=1)+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+scale_y_continuous(limits = c(0,4))+theme_classic()
ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 2),colour='#006600',size=1)+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+scale_y_continuous(limits = c(0,4))+theme_classic()

ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 0.31, shape2 = 1.81),colour='#006600',size=1)+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+scale_y_continuous(limits = c(0,4))+theme_classic()

ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 8.11, shape2 = 9.35),colour='#006600',size=1)+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+scale_y_continuous(limits = c(0,4))+theme_classic()

ggplot(data.frame(x=c(0,1.5,3),y=c(0,1.5,3)),aes(x=x,y=y))+stat_function(fun = dbeta, args = list(shape1 = 34.35, shape2 = 39.64),colour='#006600',size=1)+scale_x_continuous(limits = c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+scale_y_continuous(limits = c(0,8))+theme_classic()
 