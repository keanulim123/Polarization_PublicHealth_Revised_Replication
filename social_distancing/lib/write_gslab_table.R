write_gslab_table <- function(TABLE, output, header){
  write.table(TABLE, file = output, row.names = FALSE, col.names = FALSE, sep = "\t")
  table <- read_file(output) # http://stackoverflow.com/questions/9068397/import-text-file-as-single-character-string
  table <- sprintf("%s\n%s", header, gsub('"', '', table))
  write_file(table, output)
}