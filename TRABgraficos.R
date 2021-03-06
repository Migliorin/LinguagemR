#instalação e leitura dos pacotes
install.packages("tidyverse")
install.packages("descr")
install.packages("readxl")
install.packages("xlsx")
library(descr)
library(tidyverse)
library(readxl)
library(xlsx)

#leitura dos dados do excell
BD_Alunos <- read_excel("C:/Users/Lucas/Desktop/BD_Alunos.xlsx")

#declarando os vetores da amostra estratificada
eng <- BD_Alunos[BD_Alunos$Curso=="Engenharia",]
engLic <- BD_Alunos[BD_Alunos$Curso=="Licenciatura em Computação",]
mete <- BD_Alunos[BD_Alunos$Curso=="Meteorologia",]

#tamanho dos vetores estratificados
lenEng <- round((80/294)*(length(eng$Registros)),digits = 0) - 1
lenLic <- round((80/294)*(length(engLic$Registros)),digits = 0)
lenMete <- round((80/294)*(length(mete$Registros)),digits = 0)

#amostra estratificada
EEng <- eng[sample(nrow(eng),lenEng),]
ELic <- engLic[sample(nrow(engLic),lenLic),]
EMete <- mete[sample(nrow(mete),lenMete),]

#Unir os dataFrames estratificados
vet <- bind_rows(EEng,ELic,EMete)
write.xlsx(vet,"amostra.xlsx")

#grafico do curso e tabela----
quant <- c(lenEng,lenLic,lenMete)
nome <- c("Engenharia","Lic. em Computação","Meteorologia","Total")
frequencias <- c(lenEng,lenLic,lenMete,(80))
porcentagem <- c(((lenEng*100)/80),((lenLic*100)/80),((lenMete*100)/80),(100))

tabelaCurso <- data.frame("Curso"=nome,"Frequências"=frequencias,"Percentagem"=porcentagem )
barplot(quant,names.arg = c("Engenharia","Lic. em Computação","Meteorologia"),main = "Gráfico - Quantidade de alunos por curso 2017",xlab = "Curso",ylab='fi')
write.xlsx(tabelaCurso, "tabelaCurso.xlsx")


#grafico ensinoMedio e tabela----
militar <-length(vet[vet$Cursou_EnsinoMedio == "Escola Militar",]$Registros)
particular <-length(vet[vet$Cursou_EnsinoMedio == "Escola Particular",]$Registros)
publica <- length(vet[vet$Cursou_EnsinoMedio == "Escola Pública Normal",]$Registros)
tecnica <- length(vet[vet$Cursou_EnsinoMedio == "Escola Técnica",]$Registros)
nomeM <- c("Militar","Particular","Pública","Técnica","Total")
frequenciasM <- c(militar,particular,publica,tecnica,(80))
porcentagemM <- c(((militar*100)/80),((particular*100)/80),((publica*100)/80),((tecnica*100)/80),(100))

tabelaMedio <- data.frame("Escola"=nomeM,"Frequências"=frequenciasM,"Percentagem"=porcentagemM )
barplot(c(militar,particular,publica,tecnica),names.arg = c("Militar","Particular","Pública Normal","Técnica"),main = "Gráfico - Local Ensino Médio por alunos 2017",xlab = "Escola",ylab='fi')
write.xlsx(tabelaMedio, "tabelaMedio.xlsx")

#grafico genero e tabela----
homem <-length(vet[vet$Genero == "Masculino",]$Registros)
mulher <-length(vet[vet$Genero == "Feminino",]$Registros)
nomeG <- c("Homem","Mulher","Total")
frequenciasG <- c(homem,mulher,(80))
porcentagemG <- c(((homem*100)/80),((mulher*100)/80),(100))

tabelaGenero <- data.frame("Gênero"=nomeG,"Frequências"=frequenciasG,"Percentagem"=porcentagemG )
barplot(c(homem,mulher),names.arg = c("Masculino","Feminino"),main = "Gráfico - Gênero dos estudantes 2017",xlab = "Gênero",ylab='fi')
write.xlsx(tabelaGenero, "tabelaGenero.xlsx")
#----boxplot(vet$Idade~vet$Genero)-----Opção 2 de gráfico
#---->pie(table(vet$Genero),main = "Gênero dos estudantes 2017")<----Opção 3 de grafico


#grafico trabalhaSimOuNao e tabela----
sim <-length(vet[vet$Trabalha == "Sim",]$Registros)
nao <-length(vet[vet$Trabalha == "Não",]$Registros)
nomeSN <- c("Trabalha","Não trabalham","Total")
frequenciasSN <- c(sim,nao,(80))
porcentagemSN <- c(((sim*100)/80),((nao*100)/80),(100))

tabelaSimNao <- data.frame("Resposta"=nomeSN,"Frequências"=frequenciasSN,"Percentagem"=porcentagemSN )
barplot(c(sim,nao),names.arg = c("Trabalham","Não Trabalham"),main = "Grafico - Estudantes que trabalham 2017",xlab = "Respostas",ylab='fi')
write.xlsx(tabelaSimNao, "tabelaSimNao.xlsx")


#grafico HoraDeEstudo e tabela----
amp = max(as.numeric(vet$Horas_EstudoSemana)) - min(as.numeric(vet$Horas_EstudoSemana))
k = round(sqrt(80))
h = round(amp/k)
intervalos <-seq(from=min(as.numeric(vet$Horas_EstudoSemana)),to=64,by=h)

tabelaHoras <- data.frame(freq(cut(as.numeric(vet$Horas_EstudoSemana),breaks = intervalos,right = T)))
hist(as.numeric(vet$Horas_EstudoSemana),main = "Gráfico  - Histrograma de horas de estudo", freq=T, breaks = k ,xlab = "Horas", col = "grey",ylab="fi")
write.xlsx(tabelaHoras, "tabelaHoras.xlsx")


#grafico conhecimento matematico e tabela----
ampM = max(as.numeric(vet$Conhecimento_Matematica)) - min(as.numeric(vet$Conhecimento_Matematica))
kM = round(sqrt(80))
hM = round(amp/kM)
intervalosM <- seq(from=min(as.numeric(vet$Conhecimento_Matematica)),to=max(as.numeric(vet$Conhecimento_Matematica)),by=hM)

tabelaConhecimento <- data.frame(freq(cut(vet$Conhecimento_Matematica,breaks = 2,right = F)))
hist(as.numeric(vet$Conhecimento_Matematica),main = "Gráfico  - Histrograma de conhecimento matemático", freq=T, breaks = 5 ,xlab = "Nível de conhecimento", col = "grey",ylab="fi")
write.xlsx(tabelaConhecimento, "tabelaConhecimento.xlsx")


#-Cálculo de média, Moda, Variancia, DP, CV, CS. Para IDADE----
mediana_Idade<- summary(vet$Idade)[3]

media_Idade <- summary(vet$Idade)[4]

Moda_Idade <- names(table(vet$Idade))[table(vet$Idade)==max(table(vet$Idade))]

Variancia_Idade <- var(vet$Idade)

DesvioPadrao_Idade <- sd(vet$Idade)

Coeficientevar_Idade <- (sd(vet$Idade)/ (mean(vet$Idade))*100)

coeficiente_Simetria_Idade <- ((quantile(vet$Idade)[1]+quantile(vet$Idade)[3]-2*quantile(vet$Idade)[2]) / (quantile(vet$Idade)[3]-quantile(vet$Idade)[1]))
#relação de taylor

#-Cálculo de média, Moda, Variancia, DP, CV, CS. Para horas de estudo----
Horas_EstudoSemana <- (as.numeric(vet$Horas_EstudoSemana))

mediana_Horas_EstudoSemana <- summary(Horas_EstudoSemana)[3]

media_Horas_EstudoSemana <- summary(Horas_EstudoSemana)[4]

moda_horasEstudo_semana <- mean(as.numeric(  names(table(Horas_EstudoSemana))[table(Horas_EstudoSemana)==max(table(Horas_EstudoSemana))]))

Variancia_Horas_EstudoSemana <- var(Horas_EstudoSemana)

DesvioPadrao_Horas_EstudoSemana <- sd(Horas_EstudoSemana)

Coeficientevar_Horas_EstudoSemana <- (sd(Horas_EstudoSemana)/(media_Horas_EstudoSemana))*100

Coeficiente_Simetria_horas_Estudo_semana <- ((quantile(Horas_EstudoSemana)[1]+quantile(Horas_EstudoSemana)[3]-2*quantile(Horas_EstudoSemana)[2]) / (quantile(Horas_EstudoSemana)[3] - quantile(Horas_EstudoSemana)[1]))
#pela relação de taylor


#-Cálculo de média, Moda, Variancia, DP, CV, CS. Para conhecimento matematico----
conhecimentoMatematico <- (as.numeric(vet$Conhecimento_Matematica))

medianaCM <- summary(conhecimentoMatematico)[3]

mediaCM <- summary(conhecimentoMatematico)[4]

modaCM <- mean(as.numeric(  names(table(conhecimentoMatematico))[table(conhecimentoMatematico)==max(table(conhecimentoMatematico))]))

varianciaCM <- var(conhecimentoMatematico)

desvioPadraoCM<- sd(conhecimentoMatematico)

coeficientevarCM<- (sd(conhecimentoMatematico)/(conhecimentoMatematico))*100

coeficienteSimetriaCM<- ((quantile(conhecimentoMatematico)[1]+quantile(conhecimentoMatematico)[3]-2*quantile(conhecimentoMatematico)[2]) / (quantile(conhecimentoMatematico)[3] - quantile(conhecimentoMatematico)[1]))


#tabela de idade e horas estudos
variaveisIdade <- as.numeric(c(mediana_Idade,media_Idade,Moda_Idade,Variancia_Idade,DesvioPadrao_Idade,Coeficientevar_Idade,coeficiente_Simetria_Idade))
variaveisHoras <- c(mediana_Horas_EstudoSemana,media_Horas_EstudoSemana,moda_horasEstudo_semana,Variancia_Horas_EstudoSemana,DesvioPadrao_Horas_EstudoSemana,Coeficientevar_Horas_EstudoSemana,Coeficiente_Simetria_horas_Estudo_semana)
variaveiCM <-c(medianaCM,mediaCM,modaCM,varianciaCM,desvioPadraoCM,coeficientevarCM,coeficienteSimetriaCM)

tabelaIdadeVar <- data.frame("Idade"="-","Mediana"=variaveisIdade[1],"Media"=variaveisIdade[2],"Moda"= variaveisIdade[3],"Variância"=variaveisIdade[4],"Desvio padrão"=variaveisIdade[5],"Coeficiente de variância"=variaveisIdade[6],"Coeficiente de simetria"=variaveisIdade[7])
tabelaHoraVar <- data.frame("Horas"="-","Mediana"=variaveisHoras[1],"Media"=variaveisHoras[2],"Moda"= variaveisHoras[3],"Variância"=variaveisHoras[4],"Desvio padrão"=variaveisHoras[5],"Coeficiente de variância"=variaveisHoras[6],"Coeficiente de simetria"=variaveisHoras[7])
tabelaCMVar <- data.frame("Horas"="-","Mediana"=variaveiCM[1],"Media"=variaveiCM[2],"Moda"= variaveiCM[3],"Variância"=variaveiCM[4],"Desvio padrão"=variaveiCM[5],"Coeficiente de variância"=variaveiCM[6],"Coeficiente de simetria"=variaveiCM[7])
write.xlsx(tabelaIdadeVar,'tabelaIdadeVar.xlsx')
write.xlsx(tabelaHoraVar,'tabelaHoraVar.xlsx')
write.xlsx(tabelaCMVar,'tabelaCMVar.xlsx')

#Media dos que estudam e s/n trabalham
trabalhasim1 <- BD_Alunos[BD_Alunos$Trabalha=="Sim",]
trabalhanao1 <- BD_Alunos[BD_Alunos$Trabalha=="Não",]
summary(as.numeric(trabalhasim1$Horas_EstudoSemana))[4]
summary(as.numeric(trabalhanao1$Horas_EstudoSemana))[4]
summary(as.numeric(BD_Alunos$Horas_EstudoSemana))[4]


#Coeficiente de Variacao da idade, horas,
(sd(BD_Alunos$Idade)/mean(BD_Alunos$Idade))*100 #CV da idade
(sd(BD_Alunos$Horas_EstudoSemana)/summary(as.numeric(BD_Alunos$Horas_EstudoSemana))[4])*100 #CV da horas de estudo
(sd(BD_Alunos$Conhecimento_Matematica)/summary(as.numeric(BD_Alunos$Conhecimento_Matematica))[4])*100 #CV de matematica

