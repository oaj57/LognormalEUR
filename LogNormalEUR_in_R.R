library(ggplot2)
library(scales)
#First generate some sample lognormal data. Note: the R package/system 
#tidyr is great for loading in your own EUR [Expected Ultimate Recovery] 
#from your favorite oil and gas reserves package. Remember the middle term x here is e^x Let's see what 15 vs. 100 samples from the same mathematical pot look like!

x1 <- rlnorm(15, 4.9, 1)
x2 <-rlnorm(200,4.9,1)

#HOMEWORK: Max(rank(x)) is a proxy for which count function? ANSWER: length(x) type help(length) for details
y1 <- (rank(x1)-0.5)/max(rank(x1))
y2 <-(rank(x2)-0.5)/max(rank(x2))

#ggplot2 which I really must learn better feeds on data frames which is also something new for me
jcpData <- data.frame(x1,y1)
jcpBigData <- data.frame(x2,y2)
#this creates the graphs and used to take me four times as many lines in python:

jcp_test_graph2 <- ggplot(data = jcpBigData, aes(x = x2, y = y2)) +
  geom_point(data = jcpBigData, aes(x = x2, y= y2), color = "green") +
  geom_point(data = jcpData, aes( x = x1, y = y1)) +
  scale_y_continuous(trans = scales::probability_trans("norm"),  
  breaks = c( 0.01,0.02, 0.05, 0.10, 0.20, 0.3, 0.4, 0.5, 0.6, 0.70, 0.8, 0.9 , 0.95,0.98, 0.99), 
                     limits = c(0.01, 0.99), labels = scales::percent) +

  scale_x_continuous(trans = scales::log10_trans(), minor_breaks = c(2,3,4,5,6,7,8,9,20,30,40,50,60,70,80,90,200,300,
  400,500,600,700,800,900, 2000)) +
  labs(x = "EUR sample data set, [ANY UNITS]", y = "Percentile", title = "JCP_LOGNORMAL_R_TEST; compare small to large n") +
  theme_bw()+
  theme(panel.grid.major = element_line(size = 0.2, colour = "black"), 
        panel.grid.minor = element_line(size = 0.05, colour = "black")) +
  stat_smooth(method='lm', color = 'black', level = 0.95)+ 
  stat_smooth(data = jcpData, aes(x=x1, y=y1), method = 'lm', color = 'red', level = 0.95) 


#This shows the graph, fun eh? : there may be some truncation errors I have not had a chance 
#to verify the "limits" statement above does what I think it is doing.
jcp_test_graph2 