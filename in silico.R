#biblioteca de rede neural
library(neuralnet)

#função de normalização
norm <- function(x){
  return((x - min(x))/(max(x)- min(x)))
}

#criando 10 vezes o dado e extraindo a média deles
#criar um data frame chamado fornec.razao0 (atraso) com 10 "rodadas" de função uniforme, indo de 3 até 10
fornec.razao0 <- as.data.frame(replicate(n=10, runif(500,3,10)))

#criar uma coluna chamada media que vai conter a aplicação (apply) nas linhas (MARGIN 1) da função (FUN) mean
fornec.razao0$media <- apply(X=fornec.razao0,MARGIN=1,FUN=mean)

#criar um data frame chamado fornec.razao1 (entrega normal) com 10 "rodadas" de função uniforme, indo de 0,5 até 3
fornec.razao1 <- as.data.frame(replicate(n=10, runif(500,0.5,3)))

#criar uma coluna chamada media que vai conter a aplicação (apply) nas linhas (MARGIN 1) da função (FUN) mean
fornec.razao1$media <- apply(X=fornec.razao1,MARGIN=1,FUN=mean)

#unindo os dataframes em um único data frame pelas colunas
fornec.razao <- do.call("rbind", list(fornec.razao0,fornec.razao1))

#ficando apenas com a coluna de media
fornec.razao <- as.data.frame(fornec.razao$media)

#renomeando
names(fornec.razao)[1] <- "Razão"

#criando uma coluna com as classes para os valores
fornec.razao$Classe <- c(rep(0,500),rep(1,500))

#apagando os dataframe intermediários
remove(fornec.razao0,fornec.razao1)

#fazer o mesmo para as outras duas variáveis (Percentual: de 0,3% até 5% para 1 e 5% até 27% para 0; Distância: de 1k até 500km para 1 e de 500km até 1800km para 0)
fornec.percent0 <- as.data.frame(replicate(n=10, runif(500,5,27)))
fornec.percent0$media <- apply(X=fornec.percent0,MARGIN=1,FUN=mean)
fornec.percent1 <- as.data.frame(replicate(n=10, runif(500,0.3,5)))
fornec.percent1$media <- apply(X=fornec.percent1,MARGIN=1,FUN=mean)
fornec.percent <- do.call("rbind", list(fornec.percent0,fornec.percent1))
fornec.percent <- as.data.frame(fornec.percent$media)
names(fornec.percent)[1] <- "Percentual"
fornec.percent$Classe <- c(rep(0,500),rep(1,500))
remove(fornec.percent0,fornec.percent1)
#------------------------------
fornec.dist0 <- as.data.frame(replicate(n=10, runif(500,500,1800)))
fornec.dist0$media <- apply(X=fornec.dist0,MARGIN=1,FUN=mean)
fornec.dist1 <- as.data.frame(replicate(n=10, runif(500,1,500)))
fornec.dist1$media <- apply(X=fornec.dist1,MARGIN=1,FUN=mean)
fornec.dist <- do.call("rbind", list(fornec.dist0,fornec.dist1))
fornec.dist <- as.data.frame(fornec.dist$media)
names(fornec.dist)[1] <- "Distância"
fornec.dist$Classe <- c(rep(0,500),rep(1,500))
remove(fornec.dist0,fornec.dist1)

#normalizando os valores
fornec.razao$Razão <- norm(fornec.razao$Razão)
fornec.percent$Percentual <- norm(fornec.percent$Percentual)
fornec.dist$Distância <- norm(fornec.dist$Distância)

#depois de visualizar e entender, hora de agrupar tudo em um único dataframe
#separando a classe dos data frames existentes
classe <- fornec.razao$Classe

#ficando apenas com os valores normalizados e apagando as classes dos data frame (vetorizar)
fornec.razao <- fornec.razao$Razão
fornec.percent <- fornec.percent$Percentual
fornec.dist <- fornec.dist$Distância

#unindo em um único dataframe
df <- data.frame(fornec.razao,fornec.percent,fornec.dist,classe)

#renomeando
names(df)[1] <- "Razão"
names(df)[2] <- "Percentual"
names(df)[3] <- "Distância"
names(df)[4] <- "Classe"

#retirando uma amostra aleatória
#criando um vetor de tamanho 1000 com probabilidades de 70% e 30% de apresentar valores 1 e 2, respectivamente
sampling <- sample(2,1000,replace=T,prob=c(.7,.3))

#dataframe de treino
treino <- df[sampling==1,]

#dataframe de teste
teste <- df[sampling==2,]

#criando o modelo de rede neural
model.ann <- neuralnet(Classe ~
                         Razão + Percentual + Distância, treino,hidden=c(5,4))

#testando
resultado.teste <- neuralnet::compute(model.ann,teste[,1:3])

#arredondando
resultado <- as.data.frame(round(resultado.teste$net.result),0)

#comparando
comparacao <- as.data.frame(teste$Classe)
comparacao$resultado <- resultado 

#modelo visual
plot(model.ann)
