
  # Create graphs using R base
  
  # Histogram
  
  df <- read.csv("paldang.csv")   # read in paldang.csv file
  x <-df$Cond        # assign conductivity as a dataframe x
  h<-hist(x, breaks = 24, xlab="Conductivity", main = "Paldang Reservoir")  # create a hist object
  
  hist(x, breaks = 24, col="grey", border = "white", 
       probability = T, xlab="Conductivity", ylim = c(0,0.03))
  lines(density(x))
  
  # df1 <- read.csv("Jooam.csv")
  # h<-hist(df1$Chla, breaks = 24, xlab="Chla", main = "Jooam Reservoir")  # create a hist object
  # 
  # hist(df1$COD, breaks = 24, col="grey", border = "white",
  #      probability = T, xlab="COD", ylim = c(0,1.00))
  # lines(density(df1$COD))


  # Scattered plot
  
  attach(df)
  plot(BOD, COD, main="Scatterplot", xlab="BOD", 
       ylab= "COD", pch = 1, cex=3) 
   
  # install.packages("scatterplot3d")
  library(scatterplot3d)
  with(df, {
    scatterplot3d(x = BOD,
                  y = COD, 
                  z = Chla,
                  main="3-D Scatterplot")
  })  
  

  # Box plot
  mar.orig <- par()$mar # save the original values
  par(mar = c(6,6,4,4)) # set your new values
    boxplot(Sepal.Length~Species, data=iris, main="IRIS Data",
            xlab = "Species", ylab = "Sepal Length")
  par(mar = mar.orig) # put the original values back
  
  attach(df)  
  boxplot(BOD, COD,
          main = "Multiple boxplots for comparision",
          names = c("BOD", "COD")
  )
  
  # Multiple graphs
  
  attach(df)
  par(mfrow = c(2,2))  # define multiple frame with row and column
  mar.orig <- par()$mar # save the original values
  par(mar = c(5,5,3,2))  # bottom, left, top, right
  hist(Cond, breaks = 24, xlab="Conductivity", main = "Histogram")
  plot(BOD, COD, main="Scatterplot", xlab="BOD", 
       ylab= "COD", pch = 20)
  boxplot(BOD, ylab= "BOD", main = "Boxplot")
  boxplot(Chla, ylab= "Chl-a",main = "Boxplot")
  par(mar = mar.orig)
  