data(mpg)
library(ggplot2)

##############################################
#ggplot2
##############################################
#scatterplot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
#add class color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
#add class size: can also be shape, size ...
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) 

#facets
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#smooth line
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
#add color
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, linetype=drv, color = drv),
    show.legend = FALSE
  )
#add points plus smooth,do not shown se
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(se=FALSE)

###################################################
#bar plot
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
#same height bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
#add position=
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

