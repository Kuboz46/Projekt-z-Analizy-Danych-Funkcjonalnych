library(fds)
# fds - functional data sets
library(fda.usc)

data(labp)
data(labc)
data(nirp)
data(nirc)

nirp
# Functional data set
# y: Spectrum
# x: Wavelength 

class(labp) # "matrix" "array"
class(labc) # "matrix" "array"

class(nirp) # "fds"
class(nirc) # "fds"
class(nirp$y) # "matrix" "array" 
class(nirc$y) # "matrix" "array" 
class(nirp$x) # "numeric"
class(nirc$x) # "numeric"
class(nirp$xname) # "character"
class(nirc$xname) # "character"
class(nirp$yname) # "character"
class(nirc$yname) # "character"

nirp$xname # "Wavelength"
nirc$xname # "Wavelength"

nirp$yname # "Spectrum"
nirc$yname # "Spectrum"


dim(nirp$x) # NULL
dim(nirc$x) # NULL

dim(nirp$y) # 700  32
dim(nirc$y) # 700  40

length(nirp$x) # 700
length(nirc$x) # 700


# 1) Wstêp 

X11()
par(mfrow = c(1, 2))
plot(nirp)
plot(nirc)


X11()
par(mfrow = c(1, 2))
plot(labp[1, ])
plot(labc[1, ])

X11()
par(mfrow = c(1, 2))
plot(labp[2, ])
plot(labc[2, ])

X11()
par(mfrow = c(1, 2))
plot(labp[3, ])
plot(labc[3, ])

X11()
par(mfrow = c(1, 2))
plot(labp[4, ])
plot(labc[4, ])



# 2) Analiza danych funkcjonalnych metodami statystyki opisowej

# Œrednia próbkowa, mediana i wariancja

# func.mean(nirp)
# B³¹d w poleceniu 'func.mean(nirp)':nie znaleziono obiektu 'xnew'
func.mean(fdata(nirp))
# Ale to ju¿ dzia³a.

# 3) Modele regresji i ich porównanie

# Funkcjonalna zmienna objasniajaca i skalarna zmienna objasniana.

# Mo¿e przewidzieæ zawartoœæ t³uszczu w zale¿noœci od m¹ki, wody i danej funkcjonalnej?

tluszcz <- labp[1, ]
  
data(nirp)
aemet_y_scalar<-as.vector(tluszcz)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)
# 32 700


#sacharoza <- labp[2, ]
#aemet_y2_scalar<-as.vector(sacharoza)
#y2<-as.data.frame(aemet_y2_scalar)

#maka <- labp[3, ]
#aemet_y3_scalar<-as.vector(maka)
#y3<-as.data.frame(aemet_y3_scalar)

#woda <- labp[4, ]
#aemet_y4_scalar<-as.vector(woda)
#y4<-as.data.frame(aemet_y4_scalar)


f=aemet_y_scalar~aemet_x1 
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_basis_scalar<-fregre.lm(f, ldata) # allows more covariates

summary(reg_basis_scalar)

X11()
par(mfrow=c(2,2))
plot(reg_basis_scalar)

a<-(reg_basis_scalar$y-reg_basis_scalar$fitted.values)
# Estymator i reszty 

X11()
par(mfrow=c(1,2))
plot(reg_basis_scalar$beta.l$aemet_x1)
plot(reg_basis_scalar$residuals,
     ylab=expression(epsilon),main="residuals")



# model z najlepszymi splinsami wedlug funkcji fregre.basis
# reg_basis_scalar <- fregre.basis(aemet_x1, aemet_y_scalar)
# B³¹d w poleceniu 'chol.default(S)':
# wiod¹cy minor rzêdu 35 nie jest dodatnio okreœlony

# Gdyby to zadzialalo, to bym dalej tak robil
# summary(reg_basis_scalar)
# reg_basis_scalar$basis.x.opt$nbasis 
# reg_basis_scalar$basis.b.opt$nbasis 








#### ---------- wybor bazy dla X(t) ------------ #####
### B-splinsy
# W projekcie nr 1 wysz³o, ¿e funkcje b-sklejane ³adnie reprezentuj¹ nasze dane funkcjonalne

# 20
X11()
for(i in 4:20){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.bspline.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("bspline", i)) # druga reprezentacja bazowa
}
# slabo

# 40
X11()
for(i in 4:40){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.bspline.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("bspline", i)) # druga reprezentacja bazowa
}
# lepiej, ale mozna to na pewno ulepszyc

# 60
X11()
for(i in 4:60){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.bspline.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("bspline", i)) # druga reprezentacja bazowa
}
# ladnie, jeszcze troche sprobuje polepszyc

# 80
X11()
for(i in 4:80){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.bspline.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("bspline", i)) # druga reprezentacja bazowa
}
# Jest dobrze, dalej juz nie ma potrzeby zwiekszac.

###--------------  baza Fouriera 


# 20
X11()
for(i in 4:20){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.fourier.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("Fourier", i)) # pierwsza reprezentacja bazowa
}
# Marnie


# 40
X11()
for(i in 4:40){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.fourier.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("Fourier", i)) # pierwsza reprezentacja bazowa
}
# Lepiej, ale az tak super nie jest.

# 60
X11()
for(i in 4:60){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.fourier.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("Fourier", i)) # pierwsza reprezentacja bazowa
}
# Nadal wyraznie odstepstwa

# 80
X11()
for(i in 4:80){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.fourier.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("Fourier", i)) # pierwsza reprezentacja bazowa
}
# Jeszcze zwiekszylbym.


# 150
X11()
for(i in 4:150){
  par(mfrow = c(1,2))
  plot(aemet_x1, main = "original data") # oryginalne krzywe
  basis1 = create.fourier.basis(aemet_x1$rangeval, nbasis = i) # norder = 4 (by default) 
  x1 <- Data2fd(aemet_x1$argvals, t(aemet_x1$data), basis1)
  plot(fdata(x1), main = paste("Fourier", i)) # pierwsza reprezentacja bazowa
}
# Do zaakceptowania, dalsze zwiekszanie juz tego nie polepszy.




tluszcz <- labp[1, ]

data(nirp)
aemet_y_scalar<-as.vector(tluszcz)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)
# 32 700


f=aemet_y_scalar~aemet_x1 
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_basis_scalar<-fregre.lm(f, ldata) # allows more covariates

summary(reg_basis_scalar)

X11()
par(mfrow=c(2,2))
plot(reg_basis_scalar)

a<-(reg_basis_scalar$y-reg_basis_scalar$fitted.values)
# Estymator i reszty 

X11()
par(mfrow=c(1,2))
plot(reg_basis_scalar$beta.l$aemet_x1)
plot(reg_basis_scalar$residuals,
     ylab=expression(epsilon),main="residuals")




# Nieparametryczna regresja

data(nirp)

tluszcz <- labp[1, ]

data(nirp)
aemet_y_scalar<-as.vector(tluszcz)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)


reg_np<-fregre.np(aemet_x1, aemet_y_scalar,Ker=AKer.tri,metric = metric.lp)

reg_np

summary(reg_np)

X11()
par(mfrow=c(1,2))

plot(aemet_y_scalar,lwd=2)
points(reg_np$fitted.values,col="blue",pch=3,lwd=2)

plot(reg_np$residuals,lwd=2)

reg_np_cv<-fregre.np.cv(aemet_x1, aemet_y_scalar,Ker=AKer.tri)

reg_np_cv

X11()
par(mfrow=c(1,2))

plot(aemet_y_scalar,lwd=2)
points(reg_np_cv$fitted.values,col="red",pch=3,lwd=2)
plot(reg_np_cv$residuals,lwd=2)



# Sacharoza

sacharoza <- labp[2, ]

data(nirp)
aemet_y_scalar<-as.vector(sacharoza)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)
# 32 700


f=aemet_y_scalar~aemet_x1 
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_basis_scalar<-fregre.lm(f, ldata) # allows more covariates

summary(reg_basis_scalar)

# Nieparametryczna regresja

sacharoza <- labp[2, ]

data(nirp)
aemet_y_scalar<-as.vector(sacharoza)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)


reg_np<-fregre.np(aemet_x1, aemet_y_scalar,Ker=AKer.tri,metric = metric.lp)

reg_np

summary(reg_np)

# Maka


maka <- labp[3, ]

data(nirp)
aemet_y_scalar<-as.vector(maka)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)
# 32 700


f=aemet_y_scalar~aemet_x1 
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_basis_scalar<-fregre.lm(f, ldata) # allows more covariates

summary(reg_basis_scalar)

# Nieparametryczna regresja

maka <- labp[3, ]

data(nirp)
aemet_y_scalar<-as.vector(maka)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)


reg_np<-fregre.np(aemet_x1, aemet_y_scalar,Ker=AKer.tri,metric = metric.lp)

reg_np

summary(reg_np)




# Woda

woda <- labp[4, ]

data(nirp)
aemet_y_scalar<-as.vector(woda)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)
# 32 700


f=aemet_y_scalar~aemet_x1 
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_basis_scalar<-fregre.lm(f, ldata) # allows more covariates

summary(reg_basis_scalar)


X11()
par(mfrow=c(2,2))
plot(reg_basis_scalar)

a<-(reg_basis_scalar$y-reg_basis_scalar$fitted.values)

# Nieparametryczna regresja

woda <- labp[4, ]

data(nirp)
aemet_y_scalar<-as.vector(woda)
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(t(nirp$y), nirp$x)
dim(aemet_x1$data)


reg_np<-fregre.np(aemet_x1, aemet_y_scalar,Ker=AKer.tri,metric = metric.lp)

reg_np

summary(reg_np)

X11()
par(mfrow=c(1,2))

plot(aemet_y_scalar,lwd=2)
points(reg_np$fitted.values,col="blue",pch=3,lwd=2)

plot(reg_np$residuals,lwd=2)





# 4) Model klasyfikacji na podstawie danych funkcjonalnych dla grup $\{fat < 18\}$ i $\{fat \geq 18\}$



## nieparametryczna klasyfikacja

fd_labp <- fdata(t(labp))

tluszcz_less_18 <- fd_labp[which(fd_labp$data[,1] < 18)]
tluszcz_geq_18 <- fd_labp[which(fd_labp$data[,1] >= 18)]
n1 <- length(tluszcz_less_18)
n2 <- length(tluszcz_geq_18)

data(nirp)

dane<-fd_labp
grupa<-factor(c(rep(1,n1),rep(2,n2)))

n1+n2
# 32 

k<-round((n1+n2)/2)
l<-sample(1:(n1+n2),k)

dane_learn<-dane[l]
grupa_learn<-grupa[l]

dane_test<-dane[-l]
grupa_test<-grupa[-l]

np_klas<-classif.kernel(grupa_learn, dane_learn)

np_klas

#nowe_dane<-list("x"=dane_test)

predykcja<-predict(np_klas,dane_test)

table(grupa_test,predykcja)
#           predykcja
# grupa_test 1 2
#          1 2 4
#          2 3 7

mean(predykcja==grupa_test)
# 0.5625