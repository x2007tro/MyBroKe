#
# handling economic indicators
#
eiAutoUpdate <- reactiveTimer(ei_refresh_time)

ei_data <- reactive({
  eiAutoUpdate()
  ei_data <- UtilGetEconIndicators(ei_fred, ei_quandl)
})

lapply(1:length(econ_indi_tab_names), function(i){
  ei_name <- econ_indi_tab_names[i]
  output[[ei_name]] <- DT::renderDataTable({
    DT::datatable(ei_data()[[ei_name]], options = list(pageLength = 50,
                                                       orderClasses = FALSE,
                                                       searching = TRUE,
                                                       paging = FALSE))
  })
})