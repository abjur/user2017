### Esse script realiza a análise preditiva. Etapas a serem seguidas:

library(ggplot2)
library(LOGIT)
library(caret)
library(doMC)
registerDoMC(cores=2)
lirary(lime)

## Carregar a base criada com o script_extracao_e_manipulacao_custodia.R



## Extrair somente o data.frame e as variáveis de interesse.

base<- dplyr::select(custodia$documents,-c(texts,materia,`_language`,`_encoding`))
base<-as.data.frame(unclass(base))
base<-base[c(3,1,2,4:8)]

## Divisão da base de dados em training e testing para análise e validação posterior

btrain<-createDataPartition(base$decisao,p=.85,list=F)
train<-base[btrain,]
test<-base[-btrain,]

## Estabele os parámetros para o trainamento do modelo.

ctrl <- trainControl(method = "repeatedcv", # Para resampling usa validação cruzada repetica
                     number = 10, ## Número de iterações
                     repeats = 5, ## Número de folds a serem computados
                     summaryFunction = twoClassSummary, ## Função para computar métricas de desempenho na validação cruzada
                    classProbs = TRUE, ## Computa as probabilidades das classes/etiquetas
                     savePredictions = TRUE, ## salva as predições no resampling
                     allowParallel = TRUE, ## autoriza paralelização.
                     sampling="up" ## Equilibra as classes para cima, já que a maioria é "denegado"
)


## Regressão logística

mod_GLM <- train(decisao ~ .,data=train, method="glm", family="binomial",
trControl = ctrl, tuneLength = 5,
metric = "ROC")

#### Predição

pglm<-predict(mod_GLM,test)
ppglm<-confusionMatrix(pglm,test$decisao)
ppglm


#### Diagnóstico

p<-predict(mod_GLM,test,"prob")[[1]]
t<-ifelse(test$decisao=="concedido",1,0)
a<-classifierplots(t,p)

## Gradient boosting

# Uma vez que a base não é muito grande, preferimos utilizar um encolhimento de 0.001 e 0.01.
# A base final optou por 0.01. Então este foi usado.

grid_gbm <- expand.grid(interaction.depth=5, n.trees = 250,
                         shrinkage=0.01,
                         n.minobsinnode=10)

mod_GBM <- train(decisao ~ .,  data=train, method="gbm",
                  trControl = ctrl,tuneGrid=grid_gbm,tuneLength = 5,metric = "ROC")

#### Predição

pgbm<-predict(mod_GBM,test)
ppgbm<-confusionMatrix(pgbm,test$decisao)
ppgbm




# O modelo a seguir utiliza C5.0 com boosting

grid_c50 <- expand.grid( .winnow = TRUE, .trials=40, .model="rules" )

mod_C50 <- train(decisao~.,data=train, method = "C5.0",
trControl = ctrl,tuneGrid=grid_c50,tuneLength = 5,metric = "ROC")

pC50<-predict(mod_C50,test)
ppC50<-confusionMatrix(pC50,test$decisao)
ppC50


## Floresta aleatória
grid_rf <- expand.grid( .mtry=2)

mod_RF <- train(y=train[,1],x=train[2:8], method = "rf",
                  trControl = ctrl, tuneGrid=grid6,tuneLength = 5,metric = "ROC")
pRF<-predict(mod_RF,test)
ppRF<-confusionMatrix(pRF,test$decisao)
ppRF


## XGBoost

grid_XGB <- expand.grid(nrounds = 100,
                       max_depth = 3,
                       eta = .3,
                       gamma = 0,
                       colsample_bytree = .8,
                       min_child_weight = 1,
                       subsample = .5)

mod_XGB <- train(decisao~.,data=train, method = "xgbTree",
                  trControl = ctrl, tuneGrid=XGB,metric = "ROC")

pXGB<-predict(mod_XGB,test)
pXGB<-confusionMatrix(pXGB,test$decisao)
ppXGB


### Comparando os modelos

results <- resamples(list(GLM=mod_glm,
                          GBM=mod_gbm,
                          C50=mod_C50,
                          RF=mod_RF,
                          XGB=mod_XGB),decreasing=T,
                  
                     metrics="ROC")

summary(results)

## Plotando a importância das variáveis:

## Cria uma lista com todas os modelos

mod_lista<-list(mod_GLM,mod_GBM,mod_C50,mod_RF,mod_XGB)

## Aplica o ggplot a esta lista

a<-lapply(mod_lista,function(x) ggplot(varImp(x))+labs(x="Variáveis", y="Importância"))

## Coloca todo mundo num grid:

plot_grid(plotlist = a,labels=c("GLM","GBM","C5.0","RF","XGB"))


## Explicando regressão logística

explain <- lime(train, mod_GLM)


pred <- data.frame(sample_id = 1:nrow(test),
                   predict(mod_GLM, test, type = "prob"),
                   observado = test$decisao)
pred$predito <- colnames(pred)[2:4][apply(pred[, 2:4], 1, which.max)]

pred$correto <- ifelse(pred$observado == pred$predito, "correto", "errado")


pred_correto <- filter(pred, correto == "correto")
pred_errado <- filter(pred, correto == "errado")

test_data_correto <- test %>%
  mutate(sample_id = 1:nrow(test)) %>%
  filter(sample_id %in% pred_correto$sample_id) %>%
  sample_n(size = 3) %>%
  remove_rownames() %>%
  tibble::column_to_rownames(var = "sample_id") %>%
  select(-decisao)

explanation_cor <- explain(test_data_correto, n_labels = 2, n_features = 8)

plot_features(explanation_cor, ncol = 3)+
  labs(x="Variável",y="Peso")+
  scale_fill_manual(values=c("darkgreen","darkred"),
    labels=c("apoia","contraria"))

g1<-ggplot_build(gg)

ggsave(filename = "gg_certo.pdf",width=15,height=7,device = cairo_pdf)


test_data_errado <- test %>%
  mutate(sample_id = 1:nrow(test)) %>%
  filter(sample_id %in% pred_errado$sample_id) %>%
  sample_n(size = 2) %>%
  remove_rownames() %>%
  tibble::column_to_rownames(var = "sample_id")


explanation_errado <- explain(test_data_errado, n_labels = 2, n_features = 8)


plot_features(explanation_errado, ncol = 2)
labs(x="Variável",y="Peso")+
  scale_fill_manual(values=c("darkgreen","darkred"),
                    labels=c("apoia","contraria"))

#ggsave(filename = "~/R/custodia/plots/gg_errado.pdf",width=15,height=7,device = cairo_pdf)

