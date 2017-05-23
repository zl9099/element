# 交互3D
install.packages("rgl")
install.packages("httpuv")
library(rgl)
with(mtcars,{
    plot3d(wt, disp, mpg, col="red", size=3)
})

# 静态3D
library(scatterplot3d)
with(mtcars,{
    scatterplot3d(wt,   # x-axis
    disp, # y-axis
    mpg,  # z-axis
    main="3D Scatterplot")
})

