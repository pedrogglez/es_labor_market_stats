library(readr)
library(tidyverse)
library(cowplot)
EES_2018 <- read_delim("EES_2018.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
EES_2010 <- read_delim("EES_2010.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=EES_2018,aes(x=SALBASE)) + 
  geom_histogram(aes(y=..density.., fill = SEXO, position = 'ide'),binwidth=30, color='black') + 
  xlim(0,5000) + xlab('Salario mensual') + labs(title='Año 2018') + 
  ylab('Densidad')

ggplot(data=EES_2010,aes(x=SALBASE)) + 
  geom_histogram(aes(y=..density..),binwidth=30, color='black') + 
  xlim(0,5000) + xlab('Salario mensual') + labs(title='Año 2010') + 
  ylab('Densidad')


# Para trabajar con la variable estudios

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
  'Estudios' = c(EES_2010$ESTU,EES_2018$ESTU),
  'Nacionalidad' = c(EES_2010$TIPOPAIS,EES_2018$TIPOPAIS)
)


ggplot(data=mi_df, aes(x=Salario)) +
  geom_histogram(aes(fill=Año), color="black",
                 alpha=0.4, position="identity") + xlim(0,5000) + 
  ylab('Nº trabajadores') + labs(title = 'Comparativa salario mensual 2010 - 2018')

ggplot(data=mi_df, aes(x=Salario_IPC)) +
  geom_histogram(aes(fill=Año), color="black",
                 alpha=0.4, position="identity") + xlim(0,5000) + 
  ylab('Nº trabajadores') + xlab('Salario') +
  labs(title = 'Comparativa salario mensual 2010 - 2018 con ajuste IPC')

ggplot(mi_df,aes(x=Año,y=Salario,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = 'Comparativa salario mensual 2010 - 2018')

ggplot(mi_df,aes(x=Año,y=Salario_IPC,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = 'Comparativa salario mensual 2010 - 2018 con ajuste IPC') +
  ylab('Salario')

new_labels <- c("1" = "Noroeste", "2" = "Noreste", "3" = "Madrid", "4" = "Centro", '5' = 'Este', '6' = 'Sur', '7' = 'Canarias')

ggplot(mi_df,aes(x=Año,y=Salario,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('')+ylab('') +
  labs(title = 'Comparativa salarial 2010-2018') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
    facet_grid(~ Región, labeller = labeller(Región = new_labels))

ggplot(mi_df,aes(x=Año,y=Salario_IPC,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('')+ylab('') +
  labs(title = 'Comparativa salarial 2010-2018 con ajuste IPC') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ Región, labeller = labeller(Región = new_labels))

ggplot(mi_df, aes(x=Mercado)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Año),position='dodge') +
  scale_x_discrete(limit = c('1','2','3','4'),labels=c('Local','Nacional','Europa','Mundial')) +
  xlab("Mercado") + 
  ylab("Porcentaje") + labs(title = 'Mercados de las empresas')

ggplot(mi_df, aes(x=Sector)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Año),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Público','Privado')) +
  xlab("Sector") + 
  ylab("Porcentaje") + labs(title = 'Fuente de financiación de las empresas')

mi_df_privado<-filter(mi_df, Sector == '2')
ggplot(data=EES_2010, aes(x=SALBASE)) +
  geom_histogram(aes(y = ..density.., fill=factor(CONTROL)), color="black",
                 alpha=0.4, position="identity") + xlim(0,5000) + 
  ylab('Nº trabajadores') + labs(title = 'Comparativa salario mensual 2010 - 2018')


ggplot(mi_df, aes(x=Horas_semana, y=Salario)) + 
  geom_point(aes(colour = Año)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  labs(title = 'Salario mensual por horas semanales trabajadas y año')

ggplot(mi_df, aes(x=Horas_semana, y=Salario_IPC)) + 
  geom_point(aes(colour = Año)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual con ajuste IPC por horas trabajadas y año')

ggplot(EES_2010, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = SEXO)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2010 por horas trabajadas y sexo') +
  scale_color_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

ggplot(EES_2018, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = SEXO)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2018 por horas trabajadas y sexo') +
  scale_color_discrete(name = "Sexo", labels = c('Masculino','Femenino'))


ggplot(EES_2010, aes(x=TIPOCON)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Indefinido','Duración definida')) +
  xlab("Tipo de contrato") + 
  ylab("Porcentaje") + labs(title = 'Tipo de contrato por sexo en 2010') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

ggplot(EES_2018, aes(x=TIPOCON)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Indefinido','Duración definida')) +
  xlab("Tipo de contrato") + 
  ylab("Porcentaje") + labs(title = 'Tipo de contrato por sexo en 2018') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

a2<-ggplot(EES_2010, aes(x=TIPOCON)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Indefinido','Duración definida')) +
  xlab("Tipo de contrato") + 
  ylab("Porcentaje") + labs(title = '2010') +
  theme(legend.position = 'none')

b2<-ggplot(EES_2018, aes(x=TIPOCON)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Indefinido','Duración definida')) +
  xlab("Tipo de contrato") + 
  ylab("") + labs(title = '2018') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

c2<-plot_grid(a2,b2,rel_widths = c(1.5,2))
title2 <- ggdraw() +
  draw_label(
    "Tipo de contrato por sexo y año",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title2,c2,ncol=1,rel_heights = c(0.1,1))


EES_2010$SEXO <- as.factor(EES_2010$SEXO)

ggplot(data=EES_2010, aes(x=JSP1)) +
  geom_histogram(aes(y = ..density.., fill=SEXO), color="black",
                 alpha=0.4, position="identity", bins = 12) +
  ylab('Porcentaje') + 
  labs(title = 'Número de horas trabajadas por sexo en 2010') +
  xlab('Horas semanales trabajadas') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

EES_2018$SEXO <- as.factor(EES_2018$SEXO)

ggplot(data=EES_2018, aes(x=JSP1)) +
  geom_histogram(aes(y = ..density.., fill=SEXO), color="black",
                 alpha=0.4, position="identity", bins = 12) +
  ylab('Porcentaje') + 
  labs(title = 'Número de horas trabajadas por sexo en 2018') +
  xlab('Horas semanales trabajadas') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

EES_2010$SEXO <- as.factor(EES_2010$SEXO)

a<-ggplot(data=EES_2010, aes(x=JSP1)) +
  geom_bar(aes(y = ..count.., fill=SEXO),
                 position="dodge") +
  ylab('Nº trabajadores') + 
  labs(title = '2010') +
  xlab('Horas semanales trabajadas') +
  theme(legend.position ='none') +
  xlim(c(0,42))

EES_2018$SEXO <- as.factor(EES_2018$SEXO)

b<-ggplot(data=EES_2018, aes(x=JSP1)) +
  geom_histogram(aes(y = ..count.., fill=SEXO),position="dodge") +
  ylab('') + xlim(c(0,42)) +
  labs(title = '2018') + xlab('Horas semanales trabajadas') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

bb<-ggplot(data=EES_2018, aes(x=JSP1)) +
  geom_histogram(aes(y = ..count.., fill=SEXO),position="dodge") +
  ylab('') + xlim(c(0,42)) +
  labs(title = '2018') + xlab('Horas semanales trabajadas') +
  theme(legend.position = 'none')

plot_grid(a,bb,rel_widths = c(1.1,1))

labels_edad <- c("01" = "< 19 años", "02" = "20 - 29 años", "03" = "30 - 39 años", "04" = "40 - 49 años", '05' = '50 - 59 años', '06' = '+ 59 años')

ggplot(mi_df,aes(x=Año,y=Salario,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('')+ylab('') +
  labs(title = 'Comparativa salarial por edades 2010-2018') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ Edad, labeller = labeller(Edad = labels_edad))

ggplot(mi_df,aes(x=Año,y=Salario_IPC,fill=Año)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('')+ylab('') +
  labs(title = 'Comparativa salarial por edades 2010-2018 con ajuste IPC') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ Edad, labeller = labeller(Edad = labels_edad))

ggplot(EES_2018, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = TIPOPAIS)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2018 por horas trabajadas y nacionalidad')

ggplot(data=mi_df, aes(x=Cotización_SS)) +
  geom_histogram(aes(fill=Año), color="black",
                 alpha=0.4, position="identity") + xlim(0,300) + 
  xlab('Euros')+
  ylab('Nº trabajadores') + labs(title = 'Cotización a la Seguridad Social por trabajador')

ggplot(data=mi_df, aes(x=Cotización_SS_IPC)) +
  geom_histogram(aes(fill=Año), color="black",
                 alpha=0.4, position="identity") + xlim(0,300) + 
  xlab('Euros') +
  ylab('Nº trabajadores') + labs(title = 'Cotización a la Seguridad Social por trabajador con ajuste IPC')

a3<-ggplot(EES_2010, aes(x=TIPOJOR)) + 
  geom_bar(aes(y = (..count..)/(sum(..count..)), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Tiempo completo','Tiempo parcial')) +
  xlab("Tipo de jornada") + 
  ylab("Porcentaje") + labs(title = '2010') +
  theme(legend.position = 'none')

b3<-ggplot(EES_2018, aes(x=TIPOJOR)) + 
  geom_bar(aes(y = (..count..)/(sum(..count..)), fill=SEXO),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Tiempo completo','Tiempo parcial')) +
  xlab("Tipo de jornada") + 
  ylab("") + labs(title = '2018') +
  scale_fill_discrete(name = "Sexo", labels = c('Masculino','Femenino'))

c3<-plot_grid(a3,b3,rel_widths = c(1.5,2))
title3 <- ggdraw() +
  draw_label(
    "Tipo de jornada por sexo y año",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title3,c3,ncol=1,rel_heights = c(0.1,1))

EES_2010$REGULACION <- as.factor(EES_2010$REGULACION)

ggplot(EES_2010, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = REGULACION)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2010 por horas trabajadas y regulación') + 
  scale_color_discrete(name = "Regulación", labels = c('Estatal','Autonómico/Provincial','Empresa','Centro de trabajo','Otro'))

EES_2018$REGULACION <- as.factor(EES_2018$REGULACION)

ggplot(EES_2018, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = REGULACION)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2018 por horas trabajadas y regulación') + 
  scale_color_discrete(name = "Regulación", labels = c('Estatal','Autonómico/Provincial','Empresa','Centro de trabajo','Otro'))

EES_2010$RESPONSA <- as.factor(EES_2010$RESPONSA)

ggplot(EES_2010, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = RESPONSA)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2010 por horas trabajadas y jerarquía') + 
  scale_color_discrete(name = "Supervisión/Organización", labels = c('Sí','No'))

EES_2018$RESPONSA <- as.factor(EES_2018$RESPONSA)

ggplot(EES_2018, aes(x = JSP1, y = SALBASE)) + 
  geom_point(aes(colour = RESPONSA)) +
  ylim(0,15000) + xlab('Horas semanales trabajadas') +
  ylab('Salario') +
  labs(title = 'Salario mensual en 2018 por horas trabajadas y jerarquía') + 
  scale_color_discrete(name = "Supervisión/Organización", labels = c('Sí','No'))






ggplot(mi_df, aes(x=Estudios)) + 
  geom_bar(aes(y = (..count..)/(sum(..count..)/2), fill=Año),position='dodge') +
  scale_x_discrete(limit = c('1','2','3','4','5','6','7'),labels=c('Sin estudios','Primaria','ESO','Bachiller','FP','Grado','Máster/Doctorado')) +
  xlab("Titulación") + 
  ylab("Porcentaje") + labs(title = 'Nivel de estudio de los trabajadores')



EES_2010$CONTROL<-as.factor(EES_2010$CONTROL)
ggplot(data=EES_2010, aes(x=SALBASE)) +
  geom_histogram(aes(fill=CONTROL, y=..density..), color="black",
                 alpha=0.4, position="identity") + xlim(0,5000) + 
  ylab('Densidad') + xlab('Salario') + 
  labs(title = 'Salario según sector en 2010')

summarise(group_by(EES_2010,CONTROL),Media = mean(SALBASE))
summarise(group_by(EES_2018,CONTROL),Media = mean(SALBASE))




jovenes<-filter(mi_df,Edad <= '02')

ggplot(data = jovenes, aes(x=Salario)) + 
  geom_histogram(aes(y=..density.., fill = Año)) + 
  xlim(0,5000)

ggplot(data = jovenes, aes(x=Salario_IPC)) + 
  geom_histogram(aes(y=..density.., fill = Año)) + 
  xlim(0,5000)

ggplot(data = jovenes, aes(x=Horas_semana)) + 
  geom_bar(aes(y=..count.., fill = Año),position='dodge') + 
  xlim(0,45) + ylab('Nº trabajadores') + xlab('Horas semanales') + 
  labs(title='Horas semanales trabajadas por año por menores de 30 años')

jovenes_2010<-filter(EES_2010,ANOS2 <= '02')

a4<-ggplot(data = jovenes_2010, aes(x=JSP1)) + 
  geom_bar(aes(y=..count../sum(..count..))) + 
  xlim(0,45) + ylab('Porcentaje') + xlab('Horas semanales') + 
  labs(title='2010')

jovenes_2018<-filter(EES_2018,ANOS2 <= '02')

b4<-ggplot(data = jovenes_2018, aes(x=JSP1)) + 
  geom_bar(aes(y=..count../sum(..count..))) + 
  xlim(0,45) + ylab('') + xlab('Horas semanales') + 
  labs(title='2018')

c4<-plot_grid(a4,b4,rel_widths = c(1,1.1))
title4 <- ggdraw() +
  draw_label(
    "Horas semanales trabajadas por año por menores de 30 años",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title4,c4,ncol=1,rel_heights = c(0.1,1))


a5<-ggplot(data = EES_2010, aes(x=ANOS2)) + 
  geom_bar(aes(y=..count../sum(..count..))) + ylab('Porcentaje') +
  scale_x_discrete(limit = c('01','02','03','04','05','06'),labels=c('<19','20-29','30-39','40-49','50-59','+59')) +
  labs(title='2010') + xlab('Edad')

b5<-ggplot(data = EES_2018, aes(x=ANOS2)) + 
  geom_bar(aes(y=..count../sum(..count..))) + ylab('') +
  scale_x_discrete(limit = c('01','02','03','04','05','06'),labels=c('<19','20-29','30-39','40-49','50-59','+59')) +
  labs(title='2018') + xlab('Edad')

c5<-plot_grid(a5,b5,rel_widths = c(1,1.1))
title5 <- ggdraw() +
  draw_label(
    "Porcentaje de trabajadores por grupo de edad y año",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title5,c5,ncol=1,rel_heights = c(0.1,1))




EES_2010$TIPOJOR<-as.factor(EES_2010$TIPOJOR)

a6<-ggplot(EES_2010,aes(x=TIPOJOR,y=(SALBASE)*1.105,fill=TIPOJOR)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = '2010') +
  theme(legend.position ='none') +
  scale_x_discrete(name = "Tipo de jornada", labels = c('Tiempo completo','Tiempo parcial')) +
  ylab('Salario mensual')

EES_2018$TIPOJOR<-as.factor(EES_2018$TIPOJOR)

b6<-ggplot(EES_2018,aes(x=TIPOJOR,y=SALBASE,fill=TIPOJOR)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = '2018')  +
  theme(legend.position ='none') +
  scale_x_discrete(name = "Tipo de jornada", labels = c('Tiempo completo','Tiempo parcial')) +
  ylab('')

c6<-plot_grid(a6,b6,rel_widths = c(1,1))
title6 <- ggdraw() +
  draw_label(
    "Comparativa salarial 2010-2018 por tipo de jornada (con IPC)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title6,c6,ncol=1,rel_heights = c(0.1,1))


EES_2010$TIPOCON<-as.factor(EES_2010$TIPOCON)

a7<-ggplot(EES_2010,aes(x=TIPOCON,y=(SALBASE)*1.105,fill=TIPOCON)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = '2010') +
  theme(legend.position ='none') +
  scale_x_discrete(name = "Tipo de contrato", labels = c('Indefinido','Temporal')) +
  ylab('Salario mensual')

EES_2018$TIPOCON<-as.factor(EES_2018$TIPOCON)

b7<-ggplot(EES_2018,aes(x=TIPOCON,y=SALBASE,fill=TIPOCON)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + 
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white")  + 
  labs(title = '2018')  +
  theme(legend.position ='none') +
  scale_x_discrete(name = "Tipo de contrato", labels = c('Indefinido','Temporal')) +
  ylab('')

c7<-plot_grid(a7,b7,rel_widths = c(1,1))
title7 <- ggdraw() +
  draw_label(
    "Comparativa salarial 2010-2018 por tipo de contrato (con IPC)",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(title7,c7,ncol=1,rel_heights = c(0.1,1))


horasconcretas<-filter(mi_df,Horas_semana==40 | Horas_semana==20 | Horas_semana==35)
horasconcretas$Horas_semana<-as.factor(horasconcretas$Horas_semana)

ggplot(horasconcretas,aes(x=Horas_semana,y=Salario,fill=Horas_semana)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Comparativa salarial 2010-2018 por nº de horas trabajadas') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
    facet_grid(~ Año) + scale_fill_discrete(name = 'Horas')

ggplot(horasconcretas,aes(x=Horas_semana,y=Salario_IPC,fill=Horas_semana)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Comparativa salarial 2010-2018 por nº de horas trabajadas (IPC)') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ Año) + scale_fill_discrete(name = 'Horas')



horasconcretas_2010<-filter(EES_2010,JSP1==40 | JSP1==20 | JSP1==35)
horasconcretas_2010$JSP1<-as.factor(horasconcretas_2010$JSP1)
labels_sexo<-c('1'='Masculino','6'='Femenino')

ggplot(horasconcretas_2010,aes(x=JSP1,y=SALBASE,fill=JSP1)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Nº de horas trabajadas por sexo en 2010') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ SEXO, labeller = labeller(SEXO = labels_sexo)) + scale_fill_discrete(name = 'Horas')

horasconcretas_2018<-filter(EES_2018,JSP1==40 | JSP1==20 | JSP1==35)
horasconcretas_2018$JSP1<-as.factor(horasconcretas_2018$JSP1)
labels_sexo<-c('1'='Masculino','6'='Femenino')

ggplot(horasconcretas_2018,aes(x=JSP1,y=SALBASE,fill=JSP1)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Nº de horas trabajadas por sexo en 2018') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ SEXO, labeller = labeller(SEXO = labels_sexo)) + scale_fill_discrete(name = 'Horas')


labels_pais<-c('1'='Español','2'='Extranjero')

ggplot(horasconcretas_2010,aes(x=JSP1,y=SALBASE,fill=JSP1)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Nº de horas trabajadas por nacionalidad en 2010') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ TIPOPAIS, labeller = labeller(TIPOPAIS = labels_pais)) + scale_fill_discrete(name = 'Horas')

ggplot(horasconcretas_2018,aes(x=JSP1,y=SALBASE,fill=JSP1)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.shape = NA) + 
  ylim(0,3000) + xlab('Nº horas semanales')+ylab('Salario mensual') +
  labs(title = 'Nº de horas trabajadas por nacionalidad en 2018') +
  stat_summary(fun=mean, geom="point", size=2, shape=22, fill="white") + 
  facet_grid(~ TIPOPAIS, labeller = labeller(TIPOPAIS = labels_pais)) + scale_fill_discrete(name = 'Horas')

ggplot(mi_df, aes(x=Nacionalidad)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Año),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Española','Extranjera')) +
  xlab("Nacionalidad") + 
  ylab("Porcentaje") + labs(title = 'Comparativa de nacionalidad 2010-2018')



ggplot(mi_df, aes(x=Jornada)) + 
  geom_bar(aes(y = (..count..)/(sum(..count..)/2), fill=Año),position='dodge') +
  scale_x_discrete(limit = c('1','2'),labels=c('Tiempo completo','Tiempo parcial')) +
  xlab("Tipo de jornada") + 
  ylab("Porcentaje") + labs(title = 'Tipo de jornada por año')


ggplot(mi_df,aes(x=Horas_semana)) + 
  geom_bar(aes(y=..count.., fill = Año), position = 'dodge') + xlim(c(0,42)) + 
  ylab('Nº trabajadores') + xlab('Horas semanales trabajadas')


Madrid<-filter(mi_df, Región == '3')
Centro<-filter(mi_df, Región == '4')

Madrid2010<-filter(Madrid, Año == '2010')
Centro2010<-filter(Centro, Año == '2010')

Madrid2018<-filter(Madrid, Año == '2018')
Centro2018<-filter(Centro, Año == '2018')

ggplot(data = Madrid, aes(x = Horas_semana)) + 
  geom_bar(aes(y=..count.., fill = Año), position = 'dodge') + xlim(c(35.5,41.5))
ggplot(data = Centro, aes(x = Horas_semana)) + 
  geom_bar(aes(y=..count.., fill = Año), position = 'dodge') + xlim(c(35.5,41.5))








# HACER EL BOX DE LOS ESTUDIOS
estudiosconcretos<-filter(mi_df,)
horasconcretas$Horas_semana<-as.factor(horasconcretas$Horas_semana)

