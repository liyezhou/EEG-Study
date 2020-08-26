plotExport = function(nullObject = NULL, fileName) {
  grDevices::recordPlot() %>% ggexport(filename = fileName)
}