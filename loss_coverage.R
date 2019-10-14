# PREPARACAO DE DADOS -----------------------------------------------------
setwd('/home/matheus/Dropbox/TCC_R/dados/')
library(ggplot2); library(reshape2); library(gridExtra);library(xtable)
list.files()
prop = read.csv('proporcoes.csv', sep = ';', dec = ',', header = T)
pop_brasil_idade_simples = read.csv('pop_idade_simples.csv', sep = ';'); pop_brasil_idade_simples = pop_brasil_idade_simples[,names(pop_brasil_idade_simples) %in% c('IDADE','X2003','X2017')] 
pop_brasil_idade_simples$IDADE = as.character(pop_brasil_idade_simples$IDADE); pop_brasil_idade_simples$IDADE[nrow(pop_brasil_idade_simples)] = 90
pop_brasil_idade_simples$IDADE = as.numeric(pop_brasil_idade_simples$IDADE);pop_brasil_idade_simples$X2003 = as.numeric(pop_brasil_idade_simples$X2003);pop_brasil_idade_simples$X2017 = as.numeric(pop_brasil_idade_simples$X2017)
dados_risco = read.csv('dados.csv')
dados_risco$RISCO[is.na(dados_risco$RISCO)] = 0
names(dados_risco)
ano = function(vetor){
  retval = as.POSIXlt(as.Date(vetor))$year + 1900
  return(retval)
}
mes = function(vetor){
  retval = as.POSIXlt(as.Date(vetor))$mon + 1
  return(retval)  
}
russam = read.table('fator_rusam.txt')
names(russam)[2:8] = 2011:2017
russam$RELATIVIZADO = russam$f.bar/max(russam$f.bar)
razdep_bh_2000 = read.csv('razdep-bh.csv', sep = ';',dec = ','); razdep_bh_2000[,2] = razdep_bh_2000[,2]/100
razdep_bh_2010 = prop[10,3] / sum(prop[c(1:9),3])
var_razdep_idosos = 1 - sum((pop_brasil_idade_simples$X2017[pop_brasil_idade_simples$IDADE>=59]) / sum(pop_brasil_idade_simples$X2003[pop_brasil_idade_simples$IDADE<59 & pop_brasil_idade_simples$IDADE >= 15]))
fator_5_1 = read.csv('fator_5_1.csv', sep = ';', dec = ',')

faixas_atuais = c('0 a 18 anos', "19 a 23 anos", "24 a 28 anos","29 a 33 anos", "34 a 38 anos", "39 a 43 anos","44 a 48 anos", "49 a 53 anos", "54 a 58 anos","59 anos ou +")
f1 = 0:18; f2 = 19:23 ; f3 = 24:28; f4= 29:33 ; f5= 34:38; f6 = 39:43 ; f7 = 44:48; f8 = 49:53; f9 = 54:58; f10 = 59:160
prop[,c(6:8)] = prop[,c(6:8)]/100
prop$DSC_FAIXA_ETARIA = rep(faixas_atuais,2)

## ADICIONANDO FAIXAS ETARIAS NOS DADOS
for (contador_faixa in 1:10) {
  dados_risco$DSC_FAIXA_ETARIA[(2017 - ano(dados_risco$DAT_NASC)) %in% eval(parse(text = paste0('f',contador_faixa)))] = faixas_atuais[contador_faixa]
  russam$DSC_FAIXA_ETARIA[russam$Idade %in% eval(parse(text = paste0('f',contador_faixa)))] = faixas_atuais[contador_faixa]
  cat('faixa: ',faixas_atuais[contador_faixa],'\n')
}

## CALCULANDO RISCO MEDIO DOS DADOS POR FAIXA
for (faixa in faixas_atuais) {
  prop$PREMIO_MEDIO[prop$DSC_FAIXA_ETARIA == faixa & prop$LOCAL == 'BH'] = mean(dados_risco$premio_medio[dados_risco$DSC_FAIXA_ETARIA == faixa], na.rm = T)
  prop$RISCO_ALFA_MEDIO[prop$DSC_FAIXA_ETARIA == faixa & prop$LOCAL == 'BH'] = mean(dados_risco$RISCO[dados_risco$DSC_FAIXA_ETARIA == faixa], na.rm = T)
  prop$RISCO_RUSSAM_MEDIO[prop$DSC_FAIXA_ETARIA == faixa & prop$LOCAL == 'BH'] = mean(russam$RELATIVIZADO[russam$DSC_FAIXA_ETARIA == faixa], na.rm = T)
}


# GRAFICOS DESCRITIVOS ----------------------------------------------------
p = ggplot(prop[prop$LOCAL == 'BH',], aes(DSC_FAIXA_ETARIA, PREMIO_MEDIO, group = 1)) + geom_point() + geom_line() + theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),panel.grid.major = element_line(), panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(),axis.title.x = element_text(size = 10,color = 'black'), axis.text.x = element_text(size = 10, angle = 0, vjust = .55,color = 'black'),axis.title.y = element_text(size = 10,color = 'black'), axis.text.y = element_text(size = 10, color = 'black')) + xlab('')+  ylab('Prêmio médio')
rR = ggplot(prop[prop$LOCAL == 'BH',], aes(DSC_FAIXA_ETARIA, RISCO_RUSSAM_MEDIO, group = 1)) + geom_point() + geom_line() + theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),panel.grid.major = element_line(), panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(),axis.title.x = element_text(size = 10,color = 'black'), axis.text.x = element_text(size = 10, angle = 0, vjust = .55,color = 'black'),axis.title.y = element_text(size = 10,color = 'black'), axis.text.y = element_text(size = 10, color = 'black')) + xlab('')+  ylab('Russam médio')
rA = ggplot(prop[prop$LOCAL == 'BH',], aes(DSC_FAIXA_ETARIA, RISCO_ALFA_MEDIO, group = 1)) + geom_point() + geom_line() + theme_bw() + theme(axis.line = element_line(size=1, colour = "black"),panel.grid.major = element_line(), panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(),axis.title.x = element_text(size = 10,color = 'black'), axis.text.x = element_text(size = 10, angle = 0, vjust = .55,color = 'black'),axis.title.y = element_text(size = 10,color = 'black'), axis.text.y = element_text(size = 10, color = 'black')) + xlab('')+  ylab(expression(bar(alpha)))
grid.arrange(p, rR, rA, ncol = 1)


# FUNCAO LOSS COVERAGE PARA 10 FAIXAS ETARIAS -----------------------------
LC = function(vetor_premio,vetor_risco_bh,tau,lambda){
  vetor_risco_br = prop$TOTAL_PROP[prop$LOCAL == 'Brasil']
  p = vetor_risco_bh
  risk = numeric()
  risk[1] = length(dados_risco$COD_USR[dados_risco$RISCO <= vetor_risco_br[1]]) / length(dados_risco$COD_USR)
  for (i in 2:10) {
    risk[i] = length(dados_risco$COD_USR[dados_risco$RISCO > risk[(i-1)] & dados_risco$RISCO < vetor_risco_br[i]]) / length(dados_risco$COD_USR)
  }
  pii = risk * vetor_premio
  
  demand_function = function(lambda){
    tau = 1
    d = tau*(vetor_premio/p)**-lambda
    return(d)
  }
  
  expected_loss_coverage = sum(demand_function(lambda)*p*pii)
  
  lc = expected_loss_coverage
  
  return(lc)
}
# CRIANDO REGRAS --------------------------------------------------------
prop$REGRA_1 = NA
prop$REGRA_2 = NA
prop$REGRA_3 = NA
prop$REGRA_4 = NA
prop$REGRA_5 = NA


  #---------REGRA 1: f10 <= variacao razdep idosos * f1
  prop$REGRA_1[1:10] = seq(min(prop$PREMIO_MEDIO[1:10]), (6*var_razdep_idosos)*max(prop$PREMIO_MEDIO[1:10]), length.out = 10)

  #---------REGRA 2: variacao acumulada entre f1 e f7 < variacao acumulada entre f7 e f10
  prop$REGRA_2[1:10] = c(prop$PREMIO_MEDIO[1],rep(prop$PREMIO_MEDIO[2],8),prop$PREMIO_MEDIO[10])
  
  #---------REGRA 3:
  prop$REGRA_3[1:10] = rep(mean(prop$PREMIO_MEDIO[1:10]),10)
  
  #---------REGRA 4:
  prop$REGRA_4[1:10] = 1000*prop$RISCO_RUSSAM_MEDIO[1:10]
  
  #---------REGRA 5:
  prop$REGRA_5[1:10] = fator_5_1$Fator_5_1 * prop$PREMIO_MEDIO[1:10]
  
  

# RESULTADOS PARA DIFERENTES REGRAS -------------------------------------

tabela_latex = function(l){
# RESULTADOS PARA DIFERENTES REGRAS - UTILIZANDO RISCO ALFA E LAMBDA = l-------------------------------------
LC_REGRA_ANS = LC(vetor_premio = prop$PREMIO_MEDIO[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l)
LC_REGRA_1 = LC(vetor_premio = prop$REGRA_1[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l)
LC_REGRA_2 = LC(vetor_premio = prop$REGRA_2[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_3 = LC(vetor_premio = prop$REGRA_3[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_4 = LC(vetor_premio = prop$REGRA_4[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_5 = LC(vetor_premio = prop$REGRA_5[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = l) 
alfa = data.frame(LC_REGRA_ANS/LC_REGRA_ANS,LC_REGRA_1/LC_REGRA_ANS, LC_REGRA_2/LC_REGRA_ANS,LC_REGRA_3/LC_REGRA_ANS,LC_REGRA_4/LC_REGRA_ANS,LC_REGRA_5/LC_REGRA_ANS)
# RESULTADOS PARA DIFERENTES REGRAS - UTILIZANDO RISCO RUSSAM E LAMBDA = l-------------------------------------
LC_REGRA_ANS = LC(vetor_premio = prop$PREMIO_MEDIO[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l)
LC_REGRA_1 = LC(vetor_premio = prop$REGRA_1[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_2 = LC(vetor_premio = prop$REGRA_2[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_3 = LC(vetor_premio = prop$REGRA_3[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_4 = LC(vetor_premio = prop$REGRA_4[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l) 
LC_REGRA_5 = LC(vetor_premio = prop$REGRA_5[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = l) 
russam = data.frame(LC_REGRA_ANS/LC_REGRA_ANS,LC_REGRA_1/LC_REGRA_ANS, LC_REGRA_2/LC_REGRA_ANS,LC_REGRA_3/LC_REGRA_ANS,LC_REGRA_4/LC_REGRA_ANS, LC_REGRA_5/LC_REGRA_ANS)

return(xtable(data.frame(rbind(alfa,russam), row.names = c('risco alfa','risco russam')),
              caption = paste0('lambda = ',l), digits = 4))
}

tabela_latex(0.4)
# GRAFICOS NAO ANCORADOS PELA REGRA ANS ----------------------------------------------------------------
#   ALFA 
names(prop)[9] = 'REGRA_ANS'
d = data.frame(matrix(NA, nrow = 900,3))
names(d) = c('Regra','Lambda','LC')
d$Regra = rep(c('REGRA_ANS', paste0('REGRA_',1:5)),each = 150)
d$Lambda = rep(seq(.1,.9,length.out = 150),6)

for (regra in c('REGRA_ANS', paste0('REGRA_',1:5))) {
  for (lambda in seq(.1,.9,length.out = 150)) {
    d$LC[d$Regra == regra & d$Lambda == lambda] = 
      eval(parse(text = paste0(
        'LC(vetor_premio = prop$',regra,'[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = ',lambda,')'
      )))
  }
  cat('regra: ', regra,' \n')
  
  }


#RUSSAM 
names(prop)[9] = 'REGRA_ANS'
r = data.frame(matrix(NA, nrow = 900,3))
names(r) = c('Regra','Lambda','LC')
r$Regra = rep(c('REGRA_ANS', paste0('REGRA_',1:5)),each = 150) 
r$Lambda = rep(seq(.1,.9,length.out = 150),6)

for (regra in c('REGRA_ANS', paste0('REGRA_',1:5))) {
  for (lambda in seq(.1,.9,length.out = 150)) {
    r$LC[r$Regra == regra & r$Lambda == lambda] = 
      eval(parse(text = paste0(
        'LC(vetor_premio = prop$',regra,'[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = ',lambda,')'
      )))
    
  }
  cat('regra: ', regra,' \n')
}

d = rbind(d, r)
d$Risco = rep(c('alfa','russam'), each = nrow(r))


G_ALFA = ggplot(d[d$Risco == 'alfa',], aes(Lambda, LC, color = Regra)) + geom_line() +
  theme_bw()  +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16, color ='black'),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16, color ='black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color ='black'),
        legend.key.size = unit(1, 'cm')) +
  xlab(expression(lambda)) + ylab('LC') + 
   scale_color_manual(labels = c('Regra ANS',paste0('Regra ',1:5)), values = c('red','blue','black','darkgreen','purple','gray'))
  # scale_color_brewer(palette = 'Blues')

G_RUSSAM = ggplot(d[d$Risco == 'russam',], aes(Lambda, LC, color = Regra)) + geom_line() +
  theme_bw()  +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16, color ='black'),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16, color ='black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color ='black'),
        legend.key.size = unit(1, 'cm')) +
  xlab(expression(lambda)) + ylab('LC') + 
  scale_color_manual(labels = c('Regra ANS',paste0('Regra ',1:5)), values = c('red','blue','black','darkgreen','purple','gray'))




grid.arrange(G_ALFA, G_RUSSAM, ncol = 2)


# GRAFICOS ANCORADOS PELA REGRA ANS ----------------------------------------------------------------
  #   ALFA 
names(prop)[9] = 'REGRA_ANS'
d = data.frame(matrix(NA, nrow = 900,3))
names(d) = c('Regra','Lambda','LC')
d$Regra = rep(c('REGRA_ANS', paste0('REGRA_',1:5)),each = 150)
d$Lambda = rep(seq(.1,.9,length.out = 150),6)

for (regra in c('REGRA_ANS', paste0('REGRA_',1:5))) {
  for (lambda in seq(.1,.9,length.out = 150)) {
    d$LC[d$Regra == regra & d$Lambda == lambda] = 
      eval(parse(text = paste0(
        'LC(vetor_premio = prop$',regra,'[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = ',lambda,') /
        LC(vetor_premio = prop$REGRA_ANS[1:10],vetor_risco = prop$RISCO_ALFA_MEDIO[1:10],tau = 1,lambda = ',lambda,')'
      )))
  }
  cat('regra: ', regra,' \n')
  
}


  #RUSSAM 
names(prop)[9] = 'REGRA_ANS'
r = data.frame(matrix(NA, nrow = 900,3))
names(r) = c('Regra','Lambda','LC')
r$Regra = rep(c('REGRA_ANS', paste0('REGRA_',1:5)),each = 150) 
r$Lambda = rep(seq(.1,.9,length.out = 150),6)

for (regra in c('REGRA_ANS', paste0('REGRA_',1:5))) {
  for (lambda in seq(.1,.9,length.out = 150)) {
    r$LC[r$Regra == regra & r$Lambda == lambda] = 
      eval(parse(text = paste0(
        'LC(vetor_premio = prop$',regra,'[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = ',lambda,') /
        LC(vetor_premio = prop$REGRA_ANS[1:10],vetor_risco = prop$RISCO_RUSSAM_MEDIO[1:10],tau = 1,lambda = ',lambda,')'
      )))
    
  }
  cat('regra: ', regra,' \n')
}

d = rbind(d, r)
d$Risco = rep(c('alfa','russam'), each = nrow(r))


G_ALFA = ggplot(d[!(d$Regra == 'REGRA_ANS') & d$Risco == 'alfa',], aes(Lambda, LC, color = Regra)) + geom_line() +
  theme_bw()  +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16, color ='black'),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16, color ='black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color ='black'),
        legend.key.size = unit(1, 'cm'))+ geom_hline(yintercept = 1, col = 'red', linetype = 'dashed', size = 0.8) +
  xlab(expression(lambda)) + ylab('LC') + scale_color_manual(labels = c(paste0('Regra ',1:5)), values = c('red','blue','black','darkgreen','purple')) +
  annotate("text", x=0.8, y=2, label= "Regra ANS = 1") + 
  ggtitle('')

G_RUSSAM = ggplot(d[!(d$Regra == 'REGRA_ANS') & d$Risco == 'russam',], aes(Lambda, LC, color = Regra)) + geom_line()  +
  theme_bw()  +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16, color ='black'),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16, color ='black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color ='black'),
        legend.key.size = unit(1, 'cm'))+ geom_hline(yintercept = 1, col = 'red', linetype = 'dashed', size = 0.8) +
  xlab(expression(lambda)) + ylab('LC') + scale_color_manual(labels = c(paste0('Regra ',1:5)), values = c('red','blue','black','darkgreen','purple')) +
  annotate("text", x=0.8, y=1.05, label= "Regra ANS = 1") + 
  ggtitle(' ')





grid.arrange(G_ALFA, G_RUSSAM, ncol = 2)
