
# libraries

library(data.table) # no comments
library(dplyr) # data manipulation
library(shiny) # app builder
library(rgdal) # shapefiles
library(RColorBrewer) # color
library(leaflet) # maps

# reading inputs
pathtosave <- '~/Documents/rais/'

# establishments list from RAIS
estab <- fread('~/Documents/rais/ESTB2016.txt')
if (!exists('estab', inherits = F)){
  source('~/Documents/rais/rais.R')
  estab <- fread('~/Documents/rais/ESTB2016.txt')
}

#paste(shQuote(colnames(estab)), collapse=", ")
colnames(estab) <- c('bairros_sp', 'bairros_fortaleza', 'bairros_rj', 'classe_cnae_2.0', 'classe_cnae95', 'distritos_sp'
                     , 'qtd_vinculos_clt', 'qtd_vinculos_ativos', 'qtd_vinculos_estatuarios', 'exerc_atividade_ano'
                     , 'indicador_cei_vinculado', 'ind_estab_participa_pat', 'ind_rais_negativa', 'ind_simples', 'municipio'
                     , 'natureza_juridica', 'regioes_df', 'subclasse_cnae_2.0', 'tamanho_estab', 'tipo_estab'
                     , 'tipo_estab2', 'uf', 'subsetor_ibge', 'cep_estab')

# classes and subclasses labels
classes <- fread('~/Documents/rais/cnae_2.0_classes_v2.csv')

clas <- classes[Classe!="", c('Classe', 'Denominação')]
colnames(clas) <- c('Classe', 'class_desc')
clas[,classe_cnae_2.0:=as.integer(gsub("\\.|-","",Classe))]

subclas <- classes[Subclasse!="", c('Subclasse', 'Denominação')]
colnames(subclas) <- c('Subclasse', 'subclass_desc')
subclas[,subclasse_cnae_2.0:=gsub("/|-","",Subclasse)]
subclas[,classe_cnae_2.0:=as.integer(substr(subclasse_cnae_2.0,1,5))]
subclas[,subclasse_cnae_2.0:=as.integer(subclasse_cnae_2.0)]

labels <- clas[subclas, on = c('classe_cnae_2.0'), allow.cartesian = TRUE]

estab <- as.data.frame(labels[estab, on=c('classe_cnae_2.0', 'subclasse_cnae_2.0')])

# shiny app

ui <- fillPage(
  div(class="outer",
      
      tags$head(
        # Include custom CSS
        includeCSS("~/Documents/rais/styles.css"),
        includeScript("~/Documents/rais/gomap.js")
      ),
  leafletOutput('map', width = '100%', height = '100%'),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                
                h2("Número de Estabelecimentos por Estado"),
                selectInput('cnae', 'Selecione a classe CNAE: ', choices = sort(unique(estab$class_desc)), selectize = T)
                )
)
)

server <- function(input, output, session) {
  
  shp <- readOGR(dsn=path.expand("~/Documents/rais/br_unidades_da_federacao/"),"BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
  
  estab_uf <- reactive({
    estab %>% filter(class_desc==input$cnae) %>% group_by(uf) %>% summarise(n_estab=n())
  })
  
  estab_uf_shp <- reactive({
    merge(shp, estab_uf(), by.x="CD_GEOCUF", by.y="uf")
    
  })
    
  #proj4string(estab_uf_shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    
  #Encoding(estab_uf_shp()$NM_ESTADO) <- "UTF-8"
    
  #estab_uf_shp()$n_estab[is.na(estab_uf_shp$n_estab)] <- 0
  

    
    output$map <- renderLeaflet({
  
    pal <- colorNumeric("YlOrRd", domain = NULL)
    
    state_popup <- paste0("<strong>Estado: </strong>", 
                          estab_uf_shp()$NM_ESTADO, 
                          "<br><strong>Pontos: </strong>", 
                          estab_uf_shp()$n_estab)
      
    leaflet(data = estab_uf_shp()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(estab_uf_shp()$n_estab), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup) %>%
      addLegend("bottomright", pal = pal, values = ~estab_uf_shp()$n_estab,
                title = "Número de Estabelecimentos",
                opacity = 1)

  })
  
}

shinyApp(ui, server)
