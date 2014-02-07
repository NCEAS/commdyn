#NCEAS code
#Expand.grid

#Matrix of Plot x Plot (within a Site).  Plot=Station; Site=Area
plots<-list("a", "b", "c", "d", "e")
Plot<-as.vector(plots)
Plot

Matrix<-expand.grid(Plot, Plot)
Matrix

#For loop for calculating Bray Curtis
out<-matrix(0,25,1)
for (s in 1:25){
     out[s]<-(bioenergetics(par=model.parms3[16,], W=2.5, TempC=Range[s,2], P=P, Eprey=Range[s,1], Efish=Efish_2010)$G)
}
out

#Monte Carlo for subsetting Plots within a Site
out.Plots<-matrix(0,1000,1)
for (s in 1:1000){
     out.Plots[s]<-(bioenergetics(par=model.parms3[16,], W=mc.2005[s,1], TempC=mc.2005[s,2], P=mc.2005[s,3], Eprey=mc.2005[s,4], Efish=mc.2005[s,5])$G)
}
out.Plots

boxplot(out.Plots, horizontal=TRUE)

mean(out.Plots) 
sd(out.Plots)
range(out.Plots) 