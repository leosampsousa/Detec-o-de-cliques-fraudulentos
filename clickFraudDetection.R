library(data.table)

#Como o dataset original é muito grande, depois de o carregar, selecionei
#apenas uma amostra e salvei em um csv.

#train <- fread('train.csv')
#set.seed(0)
#train <- train[sample(nrow(train), 5e6),]
#fwrite(train, 'train_bigger_sample.csv')

data <- fread('train_bigger_sample.csv')

###Analise exploratória dos dados

str(data)

#Criando uma coluna contendo só a hora do clique. Fazer isso para analisar o número de
#cliques em relação a hora.

library(tidyr)

#Formatando a data para POSIXct
data$click_time <- as.POSIXct(data$click_time, format = '%Y-%m-%d %H:%M:%S')
data$attributed_time <- as.POSIXct(data$attributed_time, format = '%Y-%m-%d %H:%M:%S')

#Covertendo a variavel target para fator
data$is_attributed <- as.factor(data$is_attributed)

plot1_data <- data.frame(table(format(data$click_time, '%H')))

library(ggplot2)
library(ggthemes)
#Opção para desativar a notação cientifica
options(scipen = 999)

#criando o plot
ggplot(data = plot1_data, aes(x = Var1, y=Freq, group = 1))+
  ggtitle(label = 'Número de cliques VS Hora do dia',
          subtitle = 'Perido de 06/11/2017 a 09/11/2017')+
  xlab('Horas') +
  ylab('Numero de cliques')+
  geom_line(aes(group = 1), colour = 'steelblue')+
  geom_point(colour = 'steelblue', size = 3)+
  theme_bw()


##Nesse grafico, pode se observar uma queda consideravel no número de cliques
# no perido da noite, e uma crescente retomada a medida que se aproxima da madrugada

#Agora vamos reproduzir o mesmo plot mas sendo a soma de
#cliques por cada hora de cada dia.

#Para cada data diferente, é preciso a quantidade de cliques de cada hora
data_ocorrencia <- format(data$click_time, '%Y-%m-%d')
hora_ocorrencia <- format(data$click_time, '%H')
plot2_data <- data.frame(table(data_ocorrencia, hora_ocorrencia))
plot2_data

library(dplyr)
#Ordenando por data
plot2_data <- plot2_data %>%
  arrange(data_ocorrencia)
plot2_data

#Unindo as colunas de data e hora em uma só
plot2_data <- unite(plot2_data, 'click_time', data_ocorrencia:hora_ocorrencia, sep = ' ')
plot2_data$click_time <- as.POSIXct(plot2_data$click_time, format = '%Y-%m-%d %H')

plot2_data <- plot2_data %>%
  filter(Freq != 0)

plot2 <- ggplot(data = plot2_data, aes(x = click_time, y=Freq))+
  ggtitle(label = 'Número de cliques')+
  geom_line(colour = 'steelblue')+
  geom_point(colour = 'steelblue')+
  theme_bw() +
  xlab('Data') +
  ylab('Numero de cliques')

plot2

##Percebemos que é o mesmo padrão para os dias também.

plot3_data <- summarise_all(data[,-c(6,7,8)],n_distinct)
#Transformando o data.frame para o formato longo
library(reshape2)
plot3_data <- melt(plot3_data)
##Gráfico mostrando a contagem dos atributos
#Pegando os valores não repetidos de cada feature e armazenando 

ggplot(data = plot3_data, aes(x = reorder(variable, -value), y = value)) + 
  geom_bar(aes(fill = variable), stat = 'identity') + 
  ggtitle('Valores únicos de cada feature')+
  scale_y_log10(breaks = c(50,100,250, 500, 10000, 50000)) +
  geom_text(aes(label = value), vjust = 2, color = 'white', size = 4)+
  theme_minimal()+
  labs(x = 'features', y = 'Numero de valores únicos')+
  scale_fill_brewer('Legenda', palette = 'Dark2')

#Plot para a distribuição da variavel target
plot4_data <- melt(100*prop.table(table(data$is_attributed)))
plot4_data$Var1 <- as.factor(plot4_data$Var1)
levels(plot4_data$Var1) <- c('Not downloaded', 'Downloaded')

plot4 <- ggplot(data = plot4_data, aes(x = Var1, y = value)) + 
  geom_bar(stat = 'identity', fill = '#1D8348', width = 0.4) +
  ggtitle('Cliques em anuncios vs Downloads')+
  geom_text(aes(label = paste(round(value, 2), '%')), vjust = -0.3, color = 'black', size = 4)+
  labs(x = '', y='%')
plot4



#Plotando os valores mais recorrentes de cada variavel por total de cliques e por total de cliques
#que geraram download

app_plot <- data[,.N, by = app][order(-N)][1:8] %>%
  ggplot(aes(reorder(app, -N), N)) + 
  geom_bar(stat = 'identity', fill = '#cc3333') + 
  ggtitle('Frequencia de cliques por aplicativo') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'app', y='frequência')


device_plot <- data[,.N, by = device][order(-N)][1:8] %>%
  ggplot(aes(reorder(device, -N), N)) + 
  geom_bar(stat = 'identity', fill = '#cc3333') + 
  ggtitle('Frequencia de cliques por dispositivo') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'Device', y='frequência')


os_plot <- data[,.N, by = os][order(-N)][1:8] %>%
  ggplot(aes(reorder(os, -N), N)) + 
  geom_bar(stat = 'identity', fill = '#cc3333') + 
  ggtitle('Frequencia de cliques por sistema operacional') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'OS', y='frequência')


ip_plot <- data[,.N, by = ip][order(-N)][1:8] %>%
  ggplot(aes(reorder(ip, -N), N)) + 
  geom_bar(stat = 'identity', fill = '#cc3333') + 
  ggtitle('Frequencia de cliques por IPs') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'IP', y='frequência')


is_attributed_plot <- data[,.N, by = is_attributed][order(-N)] %>%
  ggplot(aes(reorder(is_attributed, -N), N)) + 
  geom_bar(stat = 'identity', fill = '#cc3333') + 
  ggtitle('Frequencia de cliques') +
  geom_text(aes(label = round(N/sum(N), 4)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'Download', y='frequência')


#Selecionando os registros que fizeram o download do aplicativo

plot5_data <- data %>% 
  select(ip,app,device,os) %>%
  filter(data$is_attributed == 1) %>% as.data.table()

app_plot2 <- plot5_data[,.N, by = app][order(-N)][1:8] %>%
  ggplot(aes(reorder(app, -N), N)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  ggtitle('Frequencia de cliques que geraram download por aplicativo') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'app', y='frequência')


device_plot2 <- plot5_data[,.N, by = device][order(-N)][1:8] %>%
  ggplot(aes(reorder(device, -N), N)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  ggtitle('Frequencia de cliques que geraram download por dispositivo') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'Device', y='frequência')


os_plot2 <- plot5_data[,.N, by = os][order(-N)][1:8] %>%
  ggplot(aes(reorder(os, -N), N)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  ggtitle('Frequencia de cliques que geraram download por sistema operacional') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'OS', y='frequência')


ip_plot2 <- plot5_data[,.N, by = ip][order(-N)][1:8] %>%
  ggplot(aes(reorder(ip, -N), N)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  ggtitle('Frequencia de cliques que geraram download por IPs') +
  geom_text(aes(label = round(N/sum(N), 2)), vjust = 2, color = 'white', size = 4.5) +
  theme_minimal()+
  labs(x = 'IP', y='frequência')

source('multiplot.R')
multiplot(app_plot, ip_plot, device_plot, os_plot, app_plot2, ip_plot2, device_plot2, os_plot2, layout = matrix(1:8, 4, 2))


#Diferenca click time com attributed time
plot6_data <- data %>% 
  select(click_time, attributed_time) %>%
  filter(data$is_attributed == 1)

diferenca = 0
for(indice in 1:nrow(plot6_data)){
  diferenca[indice] <- difftime(plot6_data[indice,2],
                        plot6_data[indice,1],
                        units = 'secs')
}


summary(diferenca)


#Selecionando aleatoriamente instancias da classe majoritaria e mantendo todas
#da classe minoritaria

notDownloaded_index <- which(data$is_attributed == 0)
downloaded_index <- which(data$is_attributed == 1)


n_samp_notDownloaded <- 200000
n_samp_downloaded <- min(length(notDownloaded_index), 
                         length(downloaded_index))

pick_notDownloaded <- sample(notDownloaded_index, n_samp_notDownloaded)
pick_downloaded <- sample(downloaded_index, n_samp_downloaded)

new_data <- data[c(pick_notDownloaded, pick_downloaded),]

#split treino e teste

library(caTools)
amostra <- sample.split(new_data$is_attributed, SplitRatio = 0.70)

train <- subset(new_data, amostra == TRUE)
test <- subset(new_data, amostra == FALSE)

train$click_time <- as.POSIXct(train$click_time)
test$click_time <- as.POSIXct(test$click_time)

#criando o modelo com parametros default

library(randomForest)

rf_default <- randomForest(is_attributed ~ .,
                           data = train[,-7])


#Testando performace nos dados de teste
rf_default.predict <- predict(rf_default, test[,-7])
library(caret)
confusionMatrix(table(rf_default.predict,test$is_attributed), positive = '0')

#Curva ROC e AUC
library('pROC')
par(pty = 's')
roc <- plot(roc(train$is_attributed, rf_default$votes[,1]), plot=T,legacy.axes=TRUE,
            xlab = 'False Positive Rate', ylab='True Positive Rate', col='#377eb8',
            lwd = 4, print.auc=TRUE, main = 'Curva ROC')
legend('bottomright', legend='Random Forest', 
       col = '#377eb8', lwd = 2, cex = 0.8)

