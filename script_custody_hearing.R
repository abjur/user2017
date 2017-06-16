devtools::install_github("jjesusfilho/tjsp")
library(tjsp)
library(quanteda)
library(dplyr)
library(textreadr)

## Extrai os metadados
df<-tjsg_meta("audiencia de custodia")

## mantêm somente habeas corpus e mandado de segurança
df<-df[str_detect(df$assunto.processo,"(Mandado|Habeas)"),]
df$assunto.processo<-as.character(df$assunto.processo)

## Baixa o inteiro teor
inteiroTJSP(df$cdacordao)

## Lê o inteiro teor
docs<-read_dir(getwd(),combine=T,doc.col="cdacordao")
names(docs)[2]<-"inteiro.teor"

## Merge
df<-merge(df,docs,by="cdacordao")
rownames(df)<-1:nrow(df)
## alguns ajustes

names(df)[c(4,7)]<-c("materia","camara")
df$camara<-stri_replace_all_regex(df$camara,"(\\d+)(.*)","$1cam")

## Partes
## Quando hc, o impetrante é a defesa, quando MS, o impetrante é o MP

df$impetrante<-ifelse(str_detect(df$assunto.processo,"Habeas"),"Defesa","MP")

## Começar a trabalhar com o pacote quanteda

custodia<-corpus(df[c(4,7,14,15)],
                 text_field = "inteiroTeorLimpo",
                 notes="Acórdãos do Tribunal de Justiça de São Paulo sobre audiências de custódia",
                 enc="UTF-8")


### Limpa os inteiros teores

df$inteiroTeorLimpo<-clearPt(df$inteiro.teor)

## Remover stopwords?

#df$inteiroTeorLimpo<-removeFeatures(df$inteiroTeorLimpo,stopwords("portuguese"))
  

# Decisão

## ordem

ordem<-kwic(custodia,"(denega.*|denaga.*|.*deferi.*|conced.*|\\bconhec.*|prejudic.*)",window=1,valuetype = "regex")
 
ordem<-as.data.frame(ordem,stringsAsFactor=F)
ordem<-ordem[duplicated(ordem$docname)==FALSE,]

row.names(ordem)<-1:nrow(ordem)


o<-ifelse(str_detect(ordem$keyword,"(conced.*|deferida)"),"concedido",
          ifelse(str_detect(ordem$keyword,"den.*"),"denegado",
                 ifelse(str_detect(ordem$keyword,"conhec.*"),"conhecido","prejudicado")))

ordem$keyword<-o
ordem<-ordem[c(1,5)]
names(ordem)[2]<-"decisao"

## Classificar os recursos que foram conhecidos, mas ainda não sabemos a decisão.

conh<-kwic(custodia,"conhec.*",11,valuetype = "regex")
conh<-conh[duplicated(conh$docname)==FALSE,]

con<-subset(ordem,decisao=="conhecido")

conh<-conh[is.element(conh$docname,con$docname),]
row.names(conh)<-1:nrow(conh)

conh$decisao<-ifelse(str_detect(conh$post,"deneg.*|mante.*"),"denegado",ifelse(str_detect(conh$pre,"nao$"),"nao_conheceram","duvida"))
conh<-as.data.frame(conh,stringsAsFactor=F)




## Ajustes manuais

conh$decisao[3]<-"nao_conheceram"


conh$decisao[58]<-"prejudicado"

conh$decisao[71]<-"concedido"

conh$decisao[87]<-"prejudicado"

conh$decisao[104]<-"denegado"

conh$decisao[105]<-"denegado"
conh$decisao[163]<-"concedido"
conh$decisao[180]<-"concedido"
conh$decisao[196]<-"prejudicado"
conh$decisao[204]<-"prejudicado"

conh<-conh[c(1,7)]
names(conh)[2]<-"dec"
ord<-left_join(ordem,conh)
ord$decisao<-ifelse(is.na(ord$dec),ord$decisao,ord$dec)


docvars(custodia,"decisao")<-ord$decisao


# Circunstância do suspeito

## Primariedade

prim<-kwic(custodia,"primar.*",window=9,valuetype = "regex")
prim<-as.data.frame(prim,stringsAsFactor=F)

prim<-prim[duplicated(prim$docname)==FALSE,]
rownames(prim)<-1:nrow(prim)
## antecedentes

antec<-kwic(custodia,"anteceden*",window=3,valuetype = "glob")
antec<-as.data.frame(antec,stringsAsFactor=F)
antec<-antec[duplicated(antec$docname)==FALSE,]
rownames(antec)<-1:nrow(antec)

### Comparando com primários
##### em que não constam
n_prim<-antec[which(is.element(antec$docname,prim$docname)==FALSE),]

n_prim<-n_prim[which(str_detect(n_prim$pre,"(nao ostenta.*|nao apresenta.*|nao tem)")),]

### Que constam
s_prim<-antec[which(is.element(antec$docname,prim$docname)==TRUE),]

### Uma revisão qualitativa dos textos aponta que há certa segurança de que
# provavelmente todos os casos apontados como primários o são de fato.

### junta n_prim a primário

prim<-rbind(prim,n_prim)

## Reincidência

reincid<-kwic(custodia,"reinciden*",window=3,valuetype = "glob")
reincid<-as.data.frame(reincid,stringsAsFactor=F)
reincid<-reincid[duplicated(reincid$docname)==FALSE,]


#### casos coincidentes
s_a_prim<-reincid[which(is.element(reincid$docname,prim$docname)==TRUE),]

s_a_prim<-s_a_prim[which(str_detect(s_a_prim$pre,"\\. periculosidade\\.")==FALSE),]

## Retirar todos os casos da custodia primários em que se verificou que ao menos um dos coautores é reincidente ou
## há forte suspeita de reincidência.

prim<-prim[which(is.element(prim$docname,s_a_prim$docname)==FALSE),]



#### casos não coincidentes
n_a_prim<-reincid[which(is.element(reincid$docname,prim$docname)==FALSE),]

prim<-prim[c(1,5)]

## Criando variável primariedade

reincidente<-setdiff(ordem$docname,prim$docname)
reincidente<-data.frame(docname=reincidente,primariedade="não")
names(prim)[2]<-"primariedade"
prim$primariedade<-"sim"
prim<-rbind(prim,reincidente)
docvars(custodia,"primariedade")<-prim$primariedade

### 


### Gravidade do crime
custodia$documents$materia<-clearPt(custodia$documents$materia)
custodia$documents$materia<-str_trim(custodia$documents$materia)

gravidade<-ifelse(str_detect(custodia$documents$materia,"droga|roubo|extorsao\\b|trafico"),"media",
                       ifelse(str_detect(custodia$documents$materia,"homicidio|latrocinio|estupro|vida|extorsao\\s*mediante\\s*sequestro"),"alta",
                              ifelse(str_detect(custodia$documents$materia,"furto|receptacao|armas|corrupcao|fe\\s+publica|constrangimento|concussao|estelionato|domestica|corrupcao|dano|transito|documento|fals.*|abuso|ameaca|quadrilha|lesao|injuria|incendio|adulteracao|arbitrario|desacato|carcere\\s*privado"),"baixa",
                                     "outros")))

docvars(custodia,"gravidade")<-gravidade


## Requisitos processuais


## ordem pública

op<-kwic(custodia,"ordem publica")
op<-op[duplicated(op$docname)==FALSE,]
op<-as.data.frame(op,strinsAsFactor=F)
publica<-data.frame(docname=paste0("text",1:3164),presenca=NA,stringsAsFactors = F)
publica<-left_join(publica,op[c(1,5)])
publica$keyword<-ifelse(is.na(publica$keyword),"não","sim")
docvars(custodia,"ordemPublica")<-publica$keyword

docvars(custodia,"ordemPublica")<-publica$keyword

## Conveniência da instrução criminal

ci<-kwic(custodia,"conveniencia da instrucao criminal")
ci<-ci[duplicated(ci$docname)==FALSE,]
instrucao<-data.frame(docname=paste0("text",1:3164),presenca=NA,strinsAsFactor=F)
instrucao<-left_join(instrucao,ci[c(1,5)])
instrucao$keyword<-ifelse(is.na(instrucao$keyword),"não","sim")
docvars(custodia,"instrucaoCriminal")<-instrucao$keyword


## Garantia da aplicacao da lei

al<-kwic(custodia,"assegurar a aplicacao da lei penal")
al<-al[duplicated(al$docname)==FALSE,]
aplicacao<-data.frame(docname=paste0("text",1:3164),presenca=NA,stringsAsFactors = F)
aplicacao<-left_join(aplicacao,al[c(1,5)])
aplicacao$keyword<-ifelse(is.na(aplicacao$keyword),"não","sim")
docvars(custodia,"aplicacaoLei")<-aplicacao$keyword

## Excluindo o desnecessario

custodia<-corpus_subset(custodia,decisao=="concedido"|decisao=="denegado",select=camara:aplicacaoLei)

custodia<-corpus_subset(custodia,gravidade!="outros")


### Incluindo metadados
metadoc(custodia,"language")<-"Portuguese"
metadoc(custodia, "encoding") <- "UTF-8"
