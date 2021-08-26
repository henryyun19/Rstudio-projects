#Henry Yun CDS301-DL2
data("USArrests")
attach(USArrests)

library(ggplot2)
library(plotly)
library(lattice)
library(dplyr)

#only states that have had more than 150 assault incidents to focus on 
#serious states
So_arr <- USArrests %>% filter(Assault > 150)

#first to get a better understadning of the different crimes, a visual represtion was made
chart.Boxplot(So_arr)

#Next was to see if there were any correlation in how Assault led to more murder or rape
p <- plot_ly(So_arr,x = ~Assault, y = ~Rape, z = ~Murder,
             marker = list(color = ~Rape, colorscale = c('#32521C', '#316468'), showscale = TRUE))
p <- p%>% add_markers()
p <- p%>% layout(scene = list(xaxis = list(title = 'Assault'),
                              yaxis = list(title = 'Rape'),
                              zaxis = list(title = 'Murder')),
                 annotations = list(
                   x = 1.13,
                   y = 1.05,
                   text = '',
                   xref = '',
                   yref = '',
                   showarrow = FALSE
                 ))
p

#now for better correlation, I combined the two to compare with Assault
So_arr_t <-So_arr %>% mutate(Tot = Rape + Murder)
plot_ly(data = So_arr_t, x = ~Assault, y = ~Tot, type = 'scatter', mode = 'markers')

#then to finish up the visualization a wirefram was made 
Rseq <- seq(8.3, 46, by = 0.25)
Mseq <- seq(3.4, 17.4, by = 0.10)
d.grid <- expand.grid(Rape = Rseq, Murder = Mseq)

g <- loess(Assault ~ Murder * Rape, span = 0.25,
           degree = 2, data = So_arr)
d.fit <- predict(g, d.grid)
asp <- diff(range(d.grid$M)) / diff(range(d.grid$R))
w <- wireframe(d.fit ~ d.grid$Murder * d.grid$Rape,
               aspect = asp, shade = TRUE,
               xlab = "Murder",
               ylab = "Rape")
w
#This eventually shows that when one crime is commited the other is not as much
