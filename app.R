
library(shiny)

library(tidyverse)
library(amap)
library(cluster)
library(factoextra)
library(sf)
library(plotly)


dat = read_csv("data/worldbank.csv")
dat_sf = readRDS("data/worldbank_sf.rds")
dat = dat %>% select(-date)

z_scale = function(x) (x - mean(x)) / sd(x)
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)

normaliza_datos = function(datos, norm_type) {
  datos_num = datos %>% select_if(is.numeric)
  if (norm_type == "mean-sd") {
    datos_norm = datos_num %>% mutate_all(z_scale)
  }
  if (norm_type == "minmax") {
    datos_norm = datos_num %>% mutate_all(minmax)
  }
  if (norm_type == "median-iqr") {
    datos_norm = datos_num %>% mutate_all(rob_scale)
  }
  datos_norm = as.data.frame(datos_norm)
  rownames(datos_norm) = datos$country
  return(datos_norm)
}

kmedias = function(datos, k, distancia) {
  amap::Kmeans(datos, centers=k, iter.max=1000, nstart=50, method=distancia)
}

crea_dist_obj = function(datos, distancia) {
  amap::Dist(datos, method=distancia) 
}

plot_silhouette = function(clust_obj, dist_obj) {
  sil = silhouette(clust_obj$cluster, dist_obj)
  rownames(sil) = names(clust_obj$cluster)
  plt = 
    fviz_silhouette(sil, label=F, print.summary=F)
  ggplotly(plt)
}

plot_mapa = function(datos, datos_sf, clust_obj) {
  datos = datos %>% mutate(cluster = factor(clust_obj$cluster))
  gdat = dat_sf %>% 
    left_join(datos, by="iso3c") %>% 
    filter(iso3c != "ATA") # sin antartica
  plt = 
    ggplot(gdat, aes(fill=cluster, label=country)) +
    geom_sf() +
    theme(legend.position="bottom") +
    NULL
  ggplotly(plt)
}

# dat_norm = normaliza_datos(dat, "median-iqr")
# km = kmedias(dat_norm, 4, "euclidean")
# dist_obj = crea_dist_obj(dat_norm, distancia)
# silhouette_plot(km, dist_obj) 
# mapa(dat, dat_sf, km)
      
      
ui = fluidPage(
  h1("K-Medias")
  ,br()
  ,fluidRow(
    column(3
           ,wellPanel(
             radioButtons("normalizador", 
                          ,label = "Tipo de normalización"
                          ,choices = list("mean-sd", "minmax", "median-iqr")
                          ,selected = "mean-sd")
             ,radioButtons("distancia", 
                           ,label = "Métrica de distancia"
                           ,choices = list("euclidean", "manhattan", "pearson")
                           ,selected = "euclidean")
             ,numericInput("k", "Cantidad de clusters", value=2, min=2, max=30)
             ,actionButton("actualizar", "Actualizar")
           )
    )
    ,column(9
            ,plotlyOutput("silhouette")
            ,plotlyOutput("mapa")
    )
  )
)

server = function(input, output, session) {
  
  dat_norm = eventReactive(input$actualizar, {
    normaliza_datos(dat, isolate(input$normalizador))
  }, ignoreNULL=F)
  
  km = reactive({
    kmedias(dat_norm(), isolate(input$k), isolate(input$distancia))
  }) 
  
  dist_obj = reactive({
    crea_dist_obj(dat_norm(), isolate(input$distancia))
  })
  
  output$silhouette = renderPlotly({
    plot_silhouette(km(), dist_obj()) 
  })

  output$mapa = renderPlotly({
    plot_mapa(dat, dat_sf, km()) 
  })
  
  # observeEvent(input$k, {
  #   clusters = sort(unique(km()$cluster))
  #   outlier_clusters = clusters[km()$size == 1]
  #   print(km()$cluster[km()$cluster %in% outlier_clusters])
  # })
  
}

shinyApp(ui=ui, server=server)
