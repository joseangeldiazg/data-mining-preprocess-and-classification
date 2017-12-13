# Apéndice glm

library(rgl)
#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

# x, y, z : numeric vectors corresponding to
#  the coordinates of points
# axis.col : axis colors
# xlab, ylab, zlab: axis labels
# show.plane : add axis planes
# show.bbox : add the bounding box decoration
# bbox.col: the bounding box colors. The first color is the
# the background color; the second color is the color of tick marks
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
  
  lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
  # Add axes
  xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
  rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
  rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
  
  # Add a point at the end of each axes to specify the direction
  axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                c(0, 0, zlim[2]))
  rgl.points(axes, color = axis.col, size = 3)
  
  # Add axis labels
  rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
            adj = c(0.5, -0.8), size = 2)
  
  # Add plane
  if(show.plane) 
    xlim <- xlim/1.1; zlim <- zlim /1.1
  rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
             z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
  
  # Add bounding box decoration
  if(show.bbox){
    rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
             emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
             xlen = 3, ylen = 3, zlen = 3) 
  }
}




# Ejemplo que ilustra el modelo de clasificación usando una variable
datos <-data.frame(y=as.numeric(as.numeric(iris$Species)),
                   x4=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)
library(splines)
model3 <- glm(y~x2, data =datos)
# model3 <- glm(y~poly(x2,3), data =datos)
# model3 <- glm(y~ns(x2,16), data =datos)



b <- predict(model3, datos)
rb <- round(b)
x <- seq(1,7,0.01)
x <- rbind(x2=x,y=0)
x <-t(x)
x <-as.data.frame(x)
xy <- predict(model3, newdata = x)
rxy <-round(xy)
plot(datos$x2, datos$y, col = rb, xlab = "Longitud del Sépalo", ylab = "Tipo de Iris", title("Specie ~ Petal.Length"))
lines(x[,1],xy,col=ifelse(xy<=1.5, "black", ifelse(xy<=2.5, "red", "green")))
segments(datos$x2,datos$y,datos$x2,b, col=datos$y, lty=4)
abline(2.5,0,col="green")
abline(1.5,0,col="red")


# Ejemplo que ilustra el modelo de clasificación en base a 2 variables
model3 <- glm(y~ns(x2,16)+ns(x1,16), data =datos)
c <- predict(model3, newdata = datos)
plot(datos$x2,datos$x1,col=round(c),pch=0, 
     xlab = "Longitud del Sépalo", ylab = "Longitud del Pétalo", title("Specie ~ Petal.Length y Sepal.Length,16"))
points(datos$x2,datos$x1,col=datos$y,pch="x")

grid.lines = 52
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
y.pred <- matrix(predict(model3, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)

rgl_init()
rgl.bbox()
rgl.spheres(datos$x2, datos$y , datos$x1, r = 0.1, color = round(c))

rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
            alpha = 0.5, lit = FALSE)  
# Add grid lines
rgl.surface(x.pred, z.pred, y.pred, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")


#### Ilustrar como se distribuye los espacios de clasificación

plantilla <-matrix(c(0,0,0),ncol=3)
for (p1 in seq(1,8,0.05))
  for (p2 in seq(1,7,0.05))
    plantilla <-rbind(plantilla, c(x1=p1,x2=p2,y=0))

plantilla <- as.data.frame(plantilla)
plantilla[,3] <- predict(model3, newdata = plantilla)

plot(datos$x2,datos$x1,col=round(c),pch=0, 
     xlab = "Longitud del Sépalo", ylab = "Longitud del Pétalo", title("Specie ~ Petal.Length y Sepal.Length"))
points(plantilla$x2,plantilla$x1,
       col= ifelse(round(plantilla$y)<1, 0, ifelse(round(plantilla$y)>3, 0, round(plantilla$y) )),pch=20)
points(datos$x2,datos$x1,col=round(c),pch=0)
points(datos$x2,datos$x1,col=datos$y,pch="x")


# Ejemplo que ilustra el modelo de clasificación en base a 3 variables
model3 <- glm(y~ns(x2,16)+ns(x1,16)+ns(x3,16), data =datos)
c <- predict(model3, newdata = datos)

rgl_init()
rgl.bbox()
rgl.spheres(x=datos$x2, y=datos$x1, z=datos$x3, r=0.1, col=ifelse(datos$y == round(c), datos$y, "yellow"))
#aspect3d(1,1,1)

