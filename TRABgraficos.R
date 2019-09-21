#instalação e leitura dos pacotes
install.packages("tidyverse")
install.packages("descr")
install.packages("readxl")
library(descr)
library(tidyverse)
library(readxl)

#leitura dos dados do excell
BD_Alunos <- read_excel("BD_Alunos.xlsx")

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

#grafico do curso e tabela----
quant <- c(lenEng,lenLic,lenMete)
nome <- c("Engenharia","Lic. em Computação","Meteorologia","Total")
frequencias <- c(lenEng,lenLic,lenMete,(80))
porcentagem <- c(((lenEng*100)/80),((lenLic*100)/80),((lenMete*100)/80),(100))

tabelaCurso <- data.frame("Curso"=nome,"Frequências"=frequencias,"Percentagem"=porcentagem )
barplot(quant,names.arg = c("Engenharia","Lic. em Computação","Meteorologia"),main = "Gráfico - Quantidade de alunos por curso 2017",xlab = "Curso",ylab='fi')
write.csv(tabelaCurso, "tabelaCurso.csv")


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
write.csv(tabelaMedio, "tabelaMedio.csv")

#grafico genero e tabela----
homem <-length(vet[vet$Genero == "Masculino",]$Registros)
mulher <-length(vet[vet$Genero == "Feminino",]$Registros)
nomeG <- c("Homem","Mulher","Total")
frequenciasG <- c(homem,mulher,(80))
porcentagemG <- c(((homem*100)/80),((mulher*100)/80),(100))

tabelaGenero <- data.frame("Gênero"=nomeG,"Frequências"=frequenciasG,"Percentagem"=porcentagemG )
barplot(c(homem,mulher),names.arg = c("Masculino","Feminino"),main = "Gráfico - Gênero dos estudantes 2017",xlab = "Gênero",ylab='fi')
write.csv(tabelaGenero, "tabelaGenero.csv")
#----boxplot(vet$Idade~vet$Genero)-----Opção 2 de gráfico
#---->pie(table(vet$Genero),main = "Gênero dos estudantes 2017")<----Opção 3 de grafico


#grafico trabalhaSimOuNao e tabela----
sim <-length(vet[vet$Trabalha == "Sim",]$Registros)
nao <-length(vet[vet$Trabalha == "Não",]$Registros)
nomeSN <- c("Homem","Mulher","Total")
frequenciasSN <- c(sim,nao,(80))
porcentagemSN <- c(((sim*100)/80),((nao*100)/80),(100))

tabelaSimNao <- data.frame("Resposta"=nomeSN,"Frequências"=frequenciasSN,"Percentagem"=porcentagemSN )
barplot(c(sim,nao),names.arg = c("Trabalham","Não Trabalham"),main = "Grafico - Estudantes que trabalham 2017",xlab = "Respostas",ylab='fi')
write.csv(tabelaSimNao, "tabelaSimNao.csv")


#grafico HoraDeEstudo e tabela----
amp = max(as.numeric(vet$Horas_EstudoSemana)) - min(as.numeric(vet$Horas_EstudoSemana))
k = round(sqrt(80))
h = round(amp/k)
intervalos <-seq(from=min(as.numeric(vet$Horas_EstudoSemana)),to=64,by=h)

tabelaHoras <- data.frame(freq(cut(as.numeric(vet$Horas_EstudoSemana),breaks = intervalos,right = T)))
hist(as.numeric(vet$Horas_EstudoSemana),main = "Gráfico  - Histrograma de horas de estudo", freq=T, breaks = k ,xlab = "Horas", col = "grey",ylab="fi")
write.csv(tabelaHoras, "tabelaHoras.csv")


#grafico conhecimento matematico e tabela----
ampM = max(as.numeric(vet$Conhecimento_Matematica)) - min(as.numeric(vet$Conhecimento_Matematica))
kM = round(sqrt(80))
hM = round(amp/kM)
intervalosM <- seq(from=min(as.numeric(vet$Conhecimento_Matematica)),to=max(as.numeric(vet$Conhecimento_Matematica)),by=hM)

tabelaConhecimento <- data.frame(freq(cut(vet$Conhecimento_Matematica,breaks = 2,right = F)))
hist(as.numeric(vet$Conhecimento_Matematica),main = "Gráfico  - Histrograma de conhecimento matemático", freq=T, breaks = 5 ,xlab = "Nível de conhecimento", col = "grey",ylab="fi")
write.csv(tabelaConhecimento, "tabelaConhecimento.csv")


#-Cálculo de média, Moda, Variancia, DP, CV, CS. Para horas de estudo.----
mediana_Idade<- summary(vet$Idade)[3]

media_Idade <- summary(vet$Idade)[4]

Moda_Idade <- names(table(vet$Idade))[table(vet$Idade)==max(table(vet$Idade))]

Variancia_Idade <- var(vet$Idade)

DesvioPadrao_Idade <- sd(vet$Idade)

Coeficientevar_Idade <- (sd(vet$Idade)/ (mean(vet$Idade))*100)

percentis = seq(.01,.99,.01) #para achar os percentis

Coeficiente_Simetria_Idade <- ((summary(vet$Idade)["3rd Qu."]) - summary(vet$Idade)["1st Qu."])/((2*((quantile(vet$Idade, percentis)["90%"])-(quantile(vet$Idade, percentis)["10%"]))))


#-Cálculo de média, Moda, Variancia, DP, CV, CS. Para horas de estudo----
Horas_EstudoSemana <- (as.numeric(vet$Horas_EstudoSemana))

mediana_Horas_EstudoSemana <- summary(Horas_EstudoSemana)[3]

media_Horas_EstudoSemana <- summary(Horas_EstudoSemana)[4]

moda_horasEstudo_semana <- mean(as.numeric( names(table(Horas_EstudoSemana))[table(Horas_EstudoSemana)==max(table(Horas_EstudoSemana))]))

Variancia_Horas_EstudoSemana <- var(Horas_EstudoSemana)

DesvioPadrao_Horas_EstudoSemana <- sd(Horas_EstudoSemana)

Coeficientevar_Horas_EstudoSemana <- (sd(Horas_EstudoSemana)/(media_Horas_EstudoSemana))*100

Coeficiente_Simetria_horas_Estudo_semana <- ((summary(Horas_EstudoSemana)["3rd Qu."]) - summary(Horas_EstudoSemana)["1st Qu."])/((2*((quantile(Horas_EstudoSemana, percentis)["90%"])-(quantile(Horas_EstudoSemana, percentis)["10%"]))))
  

#tabela de idade e horas estudos
variaveisIdade <- as.numeric(c(mediana_Idade,media_Idade,Moda_Idade,Variancia_Idade,DesvioPadrao_Idade,Coeficientevar_Idade,Coeficiente_Simetria_Idade))
variaveisHoras <- c(mediana_Horas_EstudoSemana,media_Horas_EstudoSemana,moda_horasEstudo_semana,Variancia_Horas_EstudoSemana,DesvioPadrao_Horas_EstudoSemana,Coeficientevar_Horas_EstudoSemana,Coeficiente_Simetria_horas_Estudo_semana)

tabelaIdadeVar <- data.frame("Idade"="-","Mediana"=variaveisIdade[1],"Media"=variaveisIdade[2],"Moda"= variaveisIdade[3],"Variância"=variaveisIdade[4],"Desvio padrão"=variaveisIdade[5],"Coeficiente de variância"=variaveisIdade[6],"Coeficiente de simetria"=variaveisIdade[7])
tabelaHoraVar <- data.frame("Horas"="-","Mediana"=variaveisHoras[1],"Media"=variaveisHoras[2],"Moda"= variaveisHoras[3],"Variância"=variaveisHoras[4],"Desvio padrão"=variaveisHoras[5],"Coeficiente de variância"=variaveisHoras[6],"Coeficiente de simetria"=variaveisHoras[7])
write.csv(tabelaIdadeVar,'tabelaIdadeVar.csv')
write.csv(tabelaHoraVar,'tabelaHoraVar.csv')
