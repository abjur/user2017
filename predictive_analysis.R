### Esse script realiza a análise preditiva. Etapas a serem seguidas:

library(ggplot2)
library(LOGIT)
library(caret)
library(doMC)
registerDoMC(cores=2)


## Carregar a base criada com o script_extracao_e_manipulacao_custodia.R

custodia <- readRDS('data-raw/custodia.rds')

## Extrair somente o data.frame e as variáveis de interesse.

base<- dplyr::select(custodia$documents,-c(texts,materia,`_language`,`_encoding`))
base<-as.data.frame(unclass(base))
base<-base[c(3,1,2,4:8)]

## Divisão da base de dados em training e testing para análise e validação posterior
set.seed(19910401)

btrain<-createDataPartition(base$decisao,p=.85,list=F)
train<-base[btrain,]
test<-base[-btrain,]


## script RB -------------------------------------------------------------------
## rede ------------------------------------------------------------------------
library(bnlearn)
library(dplyr)
train_fct <- train %>%
  mutate_all(as.factor) %>%
  as.data.frame()
wl <- data.frame(
  from = c('camara', 'impetrante'),
  to = c('decisao', 'decisao')
)
bl1 <- data.frame(
  from = names(train_fct)[names(train_fct) != 'camara'],
  to = rep('camara', ncol(train_fct) - 1L)
)
bl2 <- data.frame(
  from = rep('decisao', ncol(train_fct) - 1L),
  to = names(train_fct)[names(train_fct) != 'decisao']
)
bl <- rbind(bl1, bl2)
rede <- bnlearn::hc(train, whitelist = wl, blacklist = bl)

# rede <- bnlearn::bn.cv(
#   data = train, bn = 'hc',
#   algorithm.args = list(whitelist = wl, blacklist = bl)
# )


# aprendendo parametros --------------------------------------------------------
model <- bnlearn::bn.fit(rede, train_fct)
model_naive <- bnlearn::naive.bayes(
  x = train_fct, training = 'decisao',
  explanatory = names(train_fct)[names(train_fct) != 'decisao']
)
# bd teste ---------------------------------------------------------------------
test_fct <- test %>%
  mutate_all(as.factor) %>%
  as.data.frame()

# faz predição no modelo com estrutura aprendida
# desnecessário se usar naive bayes
predizer <- function(modelo, bd, i, N = 20000) {
  nm <- deparse(substitute(modelo))
  q0 <- "bnlearn::cpquery(fitted = %s, event = (decisao == 'concedido'), %s, n = %d)"
  q1 <- t(bd[i,-1]) %>%
  {setNames(as.character(.), row.names(.))} %>%
    paste(names(.), ., sep = " == '") %>%
    paste(collapse = "') & (") %>%
    sprintf("evidence = (%s')", .)
  eval(parse(text = sprintf(q0, nm, q1, N)))
}
# calcula a probabilidade de concedido dadas as informações de cada observação
# faz isso para cada combinação diferente observada na base de teste (distinct)
# desnecessário se usar naive bayes
all_subsets <- test %>% select(-decisao) %>% distinct()
p <- progress::progress_bar$new(total = nrow(all_subsets))
all_subsets <- all_subsets %>%
  mutate(res = purrr::map_dbl(1:nrow(all_subsets), ~{
    p$tick()
    predizer(model, all_subsets, .x, N = 1e6)
  }))

# adiciona os resultados
test_y <- test %>%
  inner_join(all_subsets, names(all_subsets)[names(all_subsets) != 'res']) %>%
  mutate(res_naive = predict(model_naive, test_fct, prob = TRUE) %>%
           attributes() %>%
           with(prob) %>%
           magrittr::extract(1, TRUE)) %>%
  # mudar aqui se for usar o outro modelo
  mutate(res = res_naive)


# brincadeira com curva ROC: desnecessário
seq_range <- range(test_y$res) %>%
{seq(from = .[1], to = .[2], by = 0.001)}
valores_roc_curve <- purrr::map_df(seq_range, function(x) {
  tabela <- table(
    test_y$decisao == 'concedido',
    factor(test_y$res > x, levels = c("FALSE", "TRUE"))
  )
  tibble::tibble(
    corte = x,
    FPR = tabela[1,2] / sum(tabela[1,]),
    TPR = tabela[2,2] / sum(tabela[2,]),
    TNR = tabela[1,1] / sum(tabela[1,]),
    FNR = tabela[2,1] / sum(tabela[2,])
  )
})
valores_roc_curve %>%
  ggplot(aes(x = FPR, y = TPR)) +
  geom_step() +
  geom_abline(color = 'blue', linetype = 'dashed') +
  theme_minimal(16) +
  coord_equal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
valores_roc_curve %>%
  mutate(custo = FPR + FNR) %>%
  ggplot(aes(x = corte, y = custo)) +
  geom_line() +
  theme_minimal(16) +
  coord_equal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

## avaliação -------------------------------------------------------------------
prb <- with(test_y, res)
observado <- with(test_y, decisao == 'concedido')
pprb <- confusionMatrix(pglm,test$decisao)
pprb
classifierplots::roc_plot(observado, predito)
## /script RB ------------------------------------------------------------------

## trivial ---------------------------------------------------------------------
ptrivial <- rep(factor(c('concedido', 'denegado'))[2], nrow(test))
pptrivial <- confusionMatrix(ptrivial, test$decisao)
pptrivial
## /trivial --------------------------------------------------------------------

## RB com bnclassify -----------------------------------------------------------


grid <- expand.grid(
  modelo = c('nb', 'tan_cl', 'bsej', 'tan_hc', 'tan_hcsp'),
  boot = 1:5 / 5, 
  smooth = 0:10 / 5,
  stringsAsFactors = FALSE
)
acc <- 0
mod <- NULL
melhor_grid <- NULL
p <- progress::progress_bar$new(total = nrow(grid))
for(i in 1:nrow(grid)) {
  dag_args <- if(grid[i, 1] %in% c("nb", "tan_cl")) NULL else list(k = 8)
  candidato <- bnclassify::bnc(
    dag_learner = grid[i, 1], class = 'decisao', 
    dag_args = dag_args,
    dataset = train, 
    smooth = grid[i, 3], 
    awnb_bootstrap = grid[i, 2]
  )
  pnb <- predict(candidato, test)
  ppnb <- confusionMatrix(pnb, test$decisao)
  acerto <- ppnb$overall[1]
  p$tick()
  if (acerto > acc) {
    print(grid[i,])
    acc <- acerto
    mod <- candidato
    melhor_grid <- grid[i,]
  }
}

pnb <- predict(mod, test)
ppnb <- confusionMatrix(pnb, test$decisao)
ppnb
p <- predict(mod, test, TRUE)[, 1]
t <- ifelse(test$decisao=="concedido",1,0)
classifierplots::roc_plot(t,p)

# /RB com bnclassify -----------------------------------------------------------
# com keras --------------------------------------------------------------------
library(keras)
X <- model.matrix(decisao ~ ., data = base)
attributes(X) <- list(dim = dim(X))
X_train <- X[btrain, ]
X_test <- X[-btrain, ]
y_train <- as.integer(train$decisao == 'concedido')
y_test <- as.integer(test$decisao == 'concedido')

model <- keras_model_sequential() %>% 
  layer_dense(input_shape = ncol(X), units = 30) %>% 
  layer_activation('relu') %>% 
  layer_dropout(0.2) %>%
  layer_dense(units = 10) %>% 
  layer_dense(units = 1) %>% 
  layer_activation('softmax')
  
model %>% 
  compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics = 'accuracy'
  )  

model %>% 
  fit(
    x = X_train, y = y_train, batch_size = 16, epochs = 10, 
    validation_data = list(X_test, y_test)
  )

# /com keras --------------------------------------------------------------------

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
a<-classifierplots::roc_plot(t,p)

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

p<-predict(mod_GBM,test,"prob")[[1]]
t<-ifelse(test$decisao=="concedido",1,0)
classifierplots::roc_plot(t, p)



# O modelo a seguir utiliza C5.0 com boosting

grid_c50 <- expand.grid( .winnow = TRUE, .trials=40, .model="rules" )

mod_C50 <- train(decisao~.,data=train, method = "C5.0",
                 trControl = ctrl,tuneGrid=grid_c50,tuneLength = 5,metric = "ROC")

pC50<-predict(mod_C50,test)
ppC50<-confusionMatrix(pC50,test$decisao)
ppC50

p <- predict(mod_C50,test,"prob")[[1]]
t <- ifelse(test$decisao=="concedido",1, 0)
classifierplots::roc_plot(t, p)

## Floresta aleatória
grid_rf <- expand.grid( .mtry=2)

mod_RF <- train(y=train[,1],x=train[2:8], method = "rf",
                trControl = ctrl, tuneGrid=grid_rf, tuneLength = 5,
                metric = "ROC")
pRF<-predict(mod_RF,test)
ppRF<-confusionMatrix(pRF,test$decisao)
ppRF

p <- predict(mod_RF,test,"prob")[[1]]
t <- ifelse(test$decisao=="concedido",1, 0)
classifierplots::roc_plot(t, p)

## XGBoost

grid_XGB <- expand.grid(nrounds = 100,
                        max_depth = 3,
                        eta = .3,
                        gamma = 0,
                        colsample_bytree = .8,
                        min_child_weight = 1,
                        subsample = .5)

mod_XGB <- train(decisao~.,data=train, method = "xgbTree",
                 trControl = ctrl, tuneGrid=grid_XGB,metric = "ROC")

pXGB <- predict(mod_XGB,test)
pXGB <- confusionMatrix(pXGB,test$decisao)
ppXGB

p <- predict(mod_RF,test,"prob")[[1]]
t <- ifelse(test$decisao=="concedido",1, 0)
classifierplots::roc_plot(t, p)

### Comparando os modelos

results <- list(GLM = mod_glm, GBM = mod_gbm, C50 = mod_C50,
                RF = mod_RF, XGB = mod_XGB) %>%
  resamples(decreasing = TRUE, metrics = "ROC")
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

