horasconcretas_2010<-filter(EES_2010,JSP1==40 | JSP1==20 | JSP1==35)
horasconcretas_2010$JSP1<-as.factor(horasconcretas_2010$JSP1)

summarise(group_by(horasconcretas_2010,JSP1),Media = mean(SALBASE*1.105), 
          sd = sd(SALBASE*1.105), mediana = median(SALBASE*1.105), primer = quantile(SALBASE*1.105,0.25), 
          tercer = quantile(SALBASE*1.105,0.75))

horasconcretas_2018<-filter(EES_2018,JSP1==40 | JSP1==20 | JSP1==35)
horasconcretas_2018$JSP1<-as.factor(horasconcretas_2018$JSP1)

summarise(group_by(horasconcretas_2018,JSP1),Media = mean(SALBASE), 
          sd = sd(SALBASE), mediana = median(SALBASE), primer = quantile(SALBASE,0.25), 
          tercer = quantile(SALBASE,0.75))


table(EES_2010$TIPOJOR)/length(EES_2010$TIPOJOR)
table(EES_2018$TIPOJOR)/length(EES_2018$TIPOJOR)

table(EES_2010$JSP1)
table(EES_2018$JSP1)


hombres2010<-filter(EES_2010, SEXO=='1')
table(hombres2010$TIPOJOR)/length(hombres2010$JSP1)

hombres2018<-filter(EES_2018, SEXO=='1')
table(hombres2018$TIPOJOR)/length(hombres2018$JSP1)

mujeres2010<-filter(EES_2010, SEXO=='6')
table(mujeres2010$TIPOJOR)/length(mujeres2010$JSP1)

mujeres2018<-filter(EES_2018, SEXO=='6')
table(mujeres2018$TIPOJOR)/length(mujeres2018$JSP1)


hora40_2010<-filter(EES_2010, JSP1 == '40')
summarise(group_by(hora40_2010,SEXO), media = mean(SALBASE), mediana = median(SALBASE))

hora40_2018<-filter(EES_2018, JSP1 == '40')
summarise(group_by(hora40_2018,SEXO), media = mean(SALBASE), mediana = median(SALBASE))

summarise(group_by(hora40_2010,SEXO), media = mean(SALBASE*1.105), mediana = median(SALBASE*1.105))



hora20_2010<-filter(EES_2010, JSP1 == '20')
summarise(group_by(hora20_2010,SEXO), media = mean(SALBASE))
summarise(group_by(hora20_2010,SEXO), media = mean(SALBASE*1.105))

hora20_2018<-filter(EES_2018, JSP1 == '20')
summarise(group_by(hora20_2018,SEXO), media = mean(SALBASE))

summarise(group_by(EES_2010,TIPOJOR),media = mean(SALBASE))
summarise(group_by(EES_2018,TIPOJOR),media = mean(SALBASE))
