rais<-function(uf=NULL,CNAE="",CBO="",ano=NULL){
  stopifnot(length(ano)==1,is.numeric(ano))
  u<-paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",ano,"/")
  h <- curl::new_handle(dirlistonly=TRUE)
  con <- curl::curl(u, "r", h)
  tbl <- read.table(con, stringsAsFactors=FALSE, fill=TRUE)
  close(con)
  tbl<-tbl[str_which(tbl$V1,paste0(uf,collapse="|")),1]
  urls <- paste0(u, tbl)
  urls<-str_subset(urls,"[^(Estb|ESTB)]")
  fls <- basename(urls)
  fil<-str_replace(fls,"\\..*",".txt")
  
  CNAE<-paste0(CNAE,collapse = "|")
  CBO<-paste0(CBO,collapse = "|")
  
  f<-function(x,pos){
    y<-str_which(str_trim(x$X9),paste0("^",CNAE))
    z<-str_which(str_trim(x$X8),paste0("^",CBO))
    x[intersect(y,z),]
  }
  c<-data.frame()
  
  for (i in 1:length(fls)){
    file.remove(fls[i])
    file.remove(fil[i])
    download.file(urls[i],destfile = fls[i])
    system(paste0("7z X ",fls[i]))
    a<-read_delim_chunked(fil[i], col_names=FALSE, DataFrameCallback$new(f),
                          chunk_size = 1000,delim=";")
    
    if(ano<2015){
      names(a)<-paste0("X",1:45)
    }
    else{
      names(a)<-paste0("X",1:57)
    }
    
    if(nrow(a)>0) a$uf<-str_extract(fls[i],"\\D+")
    
    a<-as.data.frame(a,stringsAsFactor=F)
    c<-rbind(c,a)
    file.remove(fls[i])
    file.remove(fil[i])
  }
  
  return(c)
}

library(readr)
library(stringr)
library(data.table)

est <- c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS'
         , 'MT', 'PA', 'PB', 'PE', 'PR', 'PI', 'RJ', 'RS', 'RO', 'RN', 'RR', 'SE', 'SP', 'SC', 'TO')

c_names <- c('Bairros_SP', 'Bairros_Fortaleza', 'Bairros_RJ', 'Causa_Afastamento_1', 'Causa_Afastamento_2'
             , 'Causa_Afastamento_3', 'Motivo_Desligamento', 'CBO_2002', 'CNA_ 2.0'
             , 'CNAE_95', 'Distritos_SP', 'vinculo_ativo', 'Faixa_Etaria', 'Faixa_Hora_Contrat'
             , 'Faixa_Remun_Dez', 'Faixa_Remun_Med', 'Faixa_Tempo_Emprego', 'Escolaridade_2005'
             , 'Qtd_Hora_Contr', 'Idade', 'Ind_CEI_Vinculado', 'Ind_Simples', 'Mes_Admissao', 'Mes_Desligamento'
             , 'Mun_Trab', 'Municipio', 'Nacionalidade', 'Natureza_Juridica', 'Ind_Portador_Defic', 'Qtd_Dias_Afastamento'
             , 'Raca_Cor', 'Regiao_Adm_DF', 'Vl_Remun_Dezembro_Nom', 'Vl_Remun_Dezembro', 'Vl_Remun_Media_Nom'
             , 'Vl_Remun_Media', 'CNAE_2.0_Subclasse', 'Sexo_Trabalhador', 'Tamanho_Estabelecimento', 'Tempo_Emprego'
             , 'Tipo_Admissao', 'Tipo_Estab', 'Tipo_Estab', 'Tipo_Defic', 'Tipo_Vinculo', 'IBGE_Subsetor'
             , 'Vl_Rem_Janeiro_CC', 'Vl_Rem_Fevereiro_CC', 'Vl_Rem_Marco_CC', 'Vl_Rem_Abril_CC', 'Vl_Rem_Maio_CC'
             , 'Vl_Rem_Junho_CC', 'Vl_Rem_Julho_CC', 'Vl_Rem_Agosto_CC', 'Vl_Rem_Setembro_CC', 'Vl_Rem_Outubro_CC'
             , 'Vl_Rem_Novembro_CC', 'Ano_Chegada_Brasil', 'UF')

dt <- setNames(data.table(matrix(nrow = 0, ncol = length(c_names))), c_names)

for (i in est) {
  x <- data.table(rais(uf = i, ano = 2016))
  colnames(x) <- c_names
  dt <- rbind(dt,x[-1,])
}

fwrite(dt, paste0(pathtosave,'ESTB2016.txt'))