#2-D Norms
penorm2 <- Vectorize(function(x,y) x^2 + y^2,vectorize.args=c('x','y'))

penorm2infi <- Vectorize(function(x,y) max(abs(x),abs(y)),vectorize.args=c('x','y'))

penorm21 <- Vectorize(function(x,y) abs(x) + abs(y),vectorize.args=c('x','y'))

penorms2 <- Vectorize(function(x,y) sqrt(x^2 + y^2),vectorize.args=c('x','y'))

#3-D Norms
penorm3 <- Vectorize(function(x,y,z) x^2 + y^2 + z^2,vectorize.args=c('x','y','z'))

penorm3infi <- Vectorize(function(x,y,z) max(abs(x),abs(y),abs(z)),vectorize.args=c('x','y','z'))

penorm31 <- Vectorize(function(x,y,z) abs(x) + abs(y) + abs(z),vectorize.args=c('x','y','z'))

penorms3 <- Vectorize(function(x,y,z) sqrt(x^2 + y^2 + z^2),vectorize.args=c('x','y','z'))
