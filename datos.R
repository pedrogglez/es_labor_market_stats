library(readr)
library(tidyverse)
EES_2018 <- read_delim("EES_2018.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
EES_2010 <- read_delim("EES_2010.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

EES_2018$SALBASE


ggplot(data=EES_2018,aes(x=SALBASE)) + 
  geom_histogram(aes(y=..density..),binwidth=30, color='black') + 
  xlim(0,5000) + xlab('Salario mensual') + labs(title='Año 2018') + 
  ylab('Densidad')
  
ggplot(data=EES_2010,aes(x=SALBASE)) + 
  geom_histogram(aes(y=..density..),binwidth=30, color='black') + 
  xlim(0,5000) + xlab('Salario mensual') + labs(title='Año 2010') + 
  ylab('Densidad')

SAL2018acot<-filter(EES_2018,SALBASE<5000)
SAL2010acot<-filter(EES_2010,SALBASE<5000)
hist(SAL2018acot$SALBASE,col='skyblue',border=F,xlab='Salario mensual', main = 'Comparativa salarial 2010 - 2018', ylab = 'Nº trabajadores')
hist(SAL2010acot$SALBASE,add=T,col=scales::alpha('red',.3),border=F)


# Corrección IPC
EES_2010_IPC<-read_delim("EES_2010.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
EES_2010_IPC$SALBASE<-EES_2010_IPC$SALBASE*1.105
SAL2010IPCacot<-filter(EES_2010_IPC,SALBASE<5000)
hist(SAL2018acot$SALBASE,col='skyblue',border=F,xlab='Salario mensual', main = 'Comparativa salarial 2010 - 2018 con reajuste IPC', ylab = 'Nº trabajadores')
hist(SAL2010IPCacot$SALBASE,add=T,col=scales::alpha('red',.3),border=F)
boxplot(SAL2018acot$SALBASE, SAL2010acot$SALBASE, at = c(1,2), main = 'Comparación salarial 2010 - 2018', names = c(2018,2010))
boxplot(SAL2018acot$SALBASE, SAL2010IPCacot$SALBASE, at = c(1,2), main = 'Comparación salarial 2010 - 2018 con reajuste IPC', names = c(2018,2010))



EES_2010$NUTS1<-as.factor(EES_2010$NUTS1)
ggplot(data = EES_2010, aes(x = NUTS1, y=SALBASE)) + geom_boxplot(notch = TRUE) + 
  labs(x = 'Región', y = 'Salario mensual')


EES_2010_acot<-filter(EES_2010,SALBASE<5000)
ggplot(data = EES_2010_acot, aes(x = NUTS1, y=SALBASE)) + geom_boxplot(notch = TRUE) + 
  labs(x = 'Región', y = 'Salario mensual',title = 'Salario mensual en 2010 según la región')

ggplot(data = EES_2010, aes(x = CONTROL)) + 
  geom_bar(aes( y =(..count..)/sum(..count..)),fill = "#FF6666") + theme_minimal()  +
  scale_x_discrete(limit = c('1','2'),labels=c('Público','Privado')) +
  xlab("Sector") + 
  ylab("Porcentaje") + labs(title = 'Sector público vs privado en 2010')

ggplot(data = EES_2018, aes(x = CONTROL)) + 
  geom_bar(aes( y =(..count..)/sum(..count..)),fill = "#FF6666") + theme_minimal()  +
  scale_x_discrete(limit = c('1','2'),labels=c('Público','Privado')) +
  xlab("Sector") + 
  ylab("Porcentaje") + labs(title = 'Sector público vs privado en 2018')


ggplot(data = EES_2010, aes(x = MERCADO)) + 
  geom_bar(aes( y =(..count..)/sum(..count..)),fill = "#FF6666") + theme_minimal()  +
  scale_x_discrete(limit = c('1','2','3','4'),labels=c('Local','Nacional','Europa','Mundial')) +
  xlab("Mercado") + 
  ylab("Porcentaje") + labs(title = 'Mercados de las empresas en 2010')

ggplot(data = EES_2018, aes(x = MERCADO)) + 
  geom_bar(aes( y =(..count..)/sum(..count..)),fill = "#FF6666") + theme_minimal()  +
  scale_x_discrete(limit = c('1','2','3','4'),labels=c('Local','Nacional','Europa','Mundial')) +
  xlab("Mercado") + 
  ylab("Porcentaje") + labs(title = 'Mercados de las empresas en 2018')







# Creemos un data.frame
mi_df<-data.frame(
  'Salario'<- c(EES_2010$SALBASE,EES_2018$SALBASE),
    'Año' <- c(rep('2010',216769),rep('2018',216726))
)

ggplot(data=mi_df, aes(x=Salario)) +
  geom_histogram(aes(y=..density.., fill=Año), color="black",
                 alpha=0.4, position="identity")







# Extra
EES_2014<-read_delim("EES_2014.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
EES_2006<-read_delim("EES_cuatrienal_2006.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
SAL2014acot<-filter(EES_2014,SALBASE<5000)
SAL2006acot<-filter(EES_2006,SALBASE<5000)

hist(SAL2018acot$SALBASE,col='skyblue',border=F,xlab='Salario mensual', main = 'Comparativa salarial 2014 - 2018', ylab = 'Densidad')
hist(SAL2014acot$SALBASE,add=T,col=scales::alpha('red',.3),border=F)
