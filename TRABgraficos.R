#instalação e leitura dos pacotes
install.packages("tidyverse")
install.packages("descr")
library(descr)
library(tidyverse)

#leitura dos dados do excell
BD_Alunos <- read_excel("~/Área de Trabalho/BD_Alunos.xlsx")

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

#grafico do curso e tabela
quant <- c(lenEng,lenLic,lenMete)
nome <- c("Engenharia","Lic. em Computação","Meteorologia","Total")
frequencias <- c(lenEng,lenLic,lenMete,(80))
porcentagem <- c(((lenEng*100)/80),((lenLic*100)/80),((lenMete*100)/80),(100))
barplot(quant,names.arg = c("Engenharia","Lic. em Computação","Meteorologia"),main = "Quantidade de alunos por curso 2017",xlab = "Curso")
tabelaCurso <- data.frame("Nome"=nome,"Frequências"=frequencias,"Percentagem"=porcentagem )


#grafico ensinoMedio e tabela
militar <-length(vet[vet$Cursou_EnsinoMedio == "Escola Militar",]$Registros)
particular <-length(vet[vet$Cursou_EnsinoMedio == "Escola Particular",]$Registros)
publica <- length(vet[vet$Cursou_EnsinoMedio == "Escola Pública Normal",]$Registros)
tecnica <- length(vet[vet$Cursou_EnsinoMedio == "Escola Técnica",]$Registros)
nomeM <- c("Militar","Particular","Pública","Técnica","Total")
frequenciasM <- c(militar,particular,publica,tecnica,(80))
porcentagemM <- c(((militar*100)/80),((particular*100)/80),((publica*100)/80),((tecnica*100)/80),(100))
barplot(c(militar,particular,publica,tecnica),names.arg = c("Militar","Particular","Pública Normal","Técnica"),main = "Local onde foi feito o Ensino Médio 2017",xlab = "Escola")
tabelaMedio <- data.frame("Nome"=nomeM,"Frequências"=frequenciasM,"Percentagem"=porcentagemM )
view(tabelaMedio)

#grafico genero e tabela
homem <-length(vet[vet$Genero == "Masculino",]$Registros)
mulher <-length(vet[vet$Genero == "Feminino",]$Registros)
nomeG <- c("Homem","Mulher","Total")
frequenciasG <- c(homem,mulher,(80))
porcentagemG <- c(((homem*100)/80),((mulher*100)/80),(100))
tabelaGenero <- data.frame("Nome"=nomeG,"Frequências"=frequenciasG,"Percentagem"=porcentagemG )
barplot(c(homem,mulher),names.arg = c("Masculino","Feminino"),main = "Gênero dos estudantes 2017",xlab = "Gênero")
#----boxplot(vet$Idade~vet$Genero)-----Opção 2 de gráfico
#---->pie(table(vet$Genero),main = "Gênero dos estudantes 2017")<----Opção 3 de grafico

#grafico trabalhaSimOuNao
sim <-length(vet[vet$Trabalha == "Sim",]$Registros)
nao <-length(vet[vet$Trabalha == "Não",]$Registros)
barplot(c(sim,nao),names.arg = c("Trabalham","Não Trabalham"),main = "Estudantes que trabalham 2017",xlab = "Respostas")

#grafico HoraDeEstudo

