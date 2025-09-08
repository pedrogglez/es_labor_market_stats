library(readr)
library(tidyverse)
library(cowplot)
library(nortest)
library(car)

EES_2018 <- read_delim("EES_2018.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
EES_2010 <- read_delim("EES_2010.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

for (i in 1:length(EES_2010$ESTU)){
  if (EES_2010$ESTU[i] == 5) {
    EES_2010$ESTU[i] <- 4
  }
}
for (i in 1:length(EES_2010$ESTU)){
  if (EES_2010$ESTU[i] == 6) {
    EES_2010$ESTU[i] <- 5
  }
}
for (i in 1:length(EES_2010$ESTU)){
  if (EES_2010$ESTU[i] == 7) {
    EES_2010$ESTU[i] <- 6
  }
}
for (i in 1:length(EES_2010$ESTU)){
  if (EES_2010$ESTU[i] == 8) {
    EES_2010$ESTU[i] <- 7
  }
}

# Creemos un data.frame
mi_df<-data.frame(
  'Salario' = c(EES_2010$SALBASE,EES_2018$SALBASE),
  'Año' = c(rep('2010',216769),rep('2018',216726)),
  'Sector' = c(EES_2010$CONTROL,EES_2018$CONTROL),
  'Mercado' = c(EES_2010$MERCADO,EES_2018$MERCADO),
  'Región' = c(EES_2010$NUTS1,EES_2018$NUTS1),
  'Jornada' = c(EES_2010$TIPOJOR,EES_2018$TIPOJOR),
  'Contrato' = c(EES_2010$TIPOCON,EES_2018$TIPOCON),
  'Edad' = c(EES_2010$ANOS2,EES_2018$ANOS2),
  'Horas_semana' = c(EES_2010$JSP1,EES_2018$JSP1),
  'Sexo' = c(EES_2010$SEXO,EES_2018$SEXO),
  'Horas_extra' = c(EES_2010$HEXTRA,EES_2018$HEXTRA),
  'Salario_IPC' = c((EES_2010$SALBASE*1.105),EES_2018$SALBASE),
  'Cotización_SS' = c(EES_2010$COTIZA,EES_2018$COTIZA),
  'Cotización_SS_IPC' = c((EES_2010$COTIZA)*1.105,EES_2018$COTIZA),
  'Responsabilidad' = c(EES_2010$RESPONSA,EES_2018$RESPONSA),
  'Estudios' = c(EES_2010$ESTU,EES_2018$ESTU)
)

mi_df$Año <- as.factor(mi_df$Año)


# t-test para medias
t.test(data = mi_df,Salario_IPC ~ Año)

# test de la mediana

alpha = 0.01
n=length(mi_df$Salario)
c1<-qbinom (alpha/2, n, 1/2)
c2<-qbinom(1- alpha/2, n ,1/2)



t.test(x=EES_2010$SALBASE, y=EES_2018$SALBASE, paired=FALSE, 
       var.equal = FALSE, conf.level=0.95)$conf.int


var.test(x=EES_2010$SALBASE*1.105, y=EES_2018$SALBASE, conf.level = 0.95)$conf.int




hombres2010<-filter(EES_2010, SEXO == '1')
hombres2018<-filter(EES_2018, SEXO == '1')
mujeres2010<-filter(EES_2010, SEXO == '6')
mujeres2018<-filter(EES_2018, SEXO == '6')

prop.test(x=c(112308,109359),n=c(length(hombres2010$SALBASE),
                                 length(hombres2018$SALBASE)),conf.level=0.95)$conf.int

prop.test(x=c(68312 ,68415),n=c(length(mujeres2010$SALBASE),
                                 length(mujeres2018$SALBASE)),conf.level=0.95)$conf.int

hora40_2010<-filter(EES_2010, JSP1 == 40)
hora40_2018<-filter(EES_2018, JSP1 == 40)

hora40_hombres_2010<-filter(hora40_2010,SEXO=='1')
hora40_mujeres_2010<-filter(hora40_2010,SEXO=='6')
hora40_hombres_2018<-filter(hora40_2018,SEXO=='1')
hora40_mujeres_2018<-filter(hora40_2018,SEXO=='6')

t.test(x=hora40_hombres_2010$SALBASE, y=hora40_mujeres_2010$SALBASE,
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)$conf.int

t.test(x=hora40_hombres_2018$SALBASE, y=hora40_mujeres_2018$SALBASE,
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)$conf.int


prop.test(x=c(180620,177774), n=c(216769,216726),conf.level = 0.99)$conf.int

row1<-c(180620,36149)

row2<-c(177774,38952)
joraño<-rbind(row1,row2)
chisq.test(joraño)


t.test(x=hora40_2018$SALBASE, y=hora40_2010$SALBASE, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)$conf.int

hora20_2010<-filter(EES_2010,JSP1 == 20)
hora20_2018<-filter(EES_2018, JSP1 == 20)

t.test(x=hora20_2018$SALBASE, y=hora20_2010$SALBASE, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)$conf.int


ancianos<-filter(mi_df, Edad == '06')
summarise(group_by(ancianos,Año), media = mean(Salario), media_IPC = mean(Salario_IPC))

ancianos_2010<-filter(ancianos, Año == '2010')
ancianos_2018<-filter(ancianos, Año == '2018')
t.test(x=ancianos_2018$Salario, y=ancianos_2010$Salario_IPC, paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)$conf.int


prop.test(x=c(dato_2018,dato_2010),n=c(length(EES_2018$ESTU),length(EES_2010$ESTU)))


M <- median(mi_df$Salario_IPC)
t <- sum(EES_2010$SALBASE*1.105<M) + sum(EES_2018$SALBASE<M)
U <- sum(EES_2010$SALBASE*1.105<M)
n1<-length(EES_2010$SALBASE)
n2<-length(EES_2018$SALBASE)
c1 <- qhyper(0.025,n1,n2,t)
c2 <- qhyper(0.975,n1,n2,t)
U 
c(c1,c2)
U<=c1 | U>=c2



















lillie.test(EES_2018$SALBASE)
ad.test(EES_2018$SALBASE)

T1<-EES_2010$SALBASE
T2<-EES_2018$SALBASE
q1 <- qqnorm(T1, plot.it=FALSE)
q2 <- qqnorm(T2, plot.it=FALSE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type="n", las=1,
     xlab='Theoretical Quantiles', ylab='Sample Quantiles')
points(q1, pch=19)
points(q2, col="red", pch=19)
qqline(T1, lty='dashed')
qqline(T2, col="red", lty="dashed")
legend('topleft', legend=c('T1', 'T2'), bty='n',
       col=c('black', 'red'), pch=19)

qqPlot(EES_2018$SALBASE,xlab='Distribución normal',ylab = 'Salario mensual en 2018')
qqPlot(EES_2010$SALBASE,xlab='Distribución normal',ylab = 'Salario mensual en 2010')

qqPlot(EES_2010$COTIZA,xlab='Distribución normal',ylab = 'Salario mensual en 2010')




bartlett.test(Salario ~ Año, data = mi_df)
bartlett.test(Salario_IPC ~ Año, data = mi_df)


fligner.test(Salario ~ Año, data = mi_df)
fligner.test(Salario_IPC ~ Año, data = mi_df)

leveneTest(Salario ~ Año, data = mi_df)
leveneTest(Salario_IPC ~ Año, data = mi_df)

t.test(Salario ~ Año, data = mi_df)
t.test(Salario_IPC ~ Año, data = mi_df)

summary(aov(Salario ~ Año, data = mi_df))