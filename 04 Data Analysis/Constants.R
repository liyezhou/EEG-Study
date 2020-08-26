plotExport = function(nullObject = NULL, fileName) {
  grDevices::recordPlot() %>% ggexport(filename = fileName)
}


# Helper to concatenate string & copy to clipboard
catNcopy <- function(texts, sep = "\", \"") {
  cat(texts, sep = sep)
  paste(texts, collapse = sep) %>% clipr::write_clip()
}

# Help to create Formula
pasteFormula <- function(y, x) {
  as.formula(paste(y, " ~ ", paste(x, collapse = "+")))
}

# Negation of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))
# Adding a printing pipe
'%P>%' <- function(lhs, rhs) {lhs %T>% {print(rhs(.)) } }

