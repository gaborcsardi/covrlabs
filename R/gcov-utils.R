read_file_raw <- function(path) {
  .Call(c_read_file_raw, path)
}

find_last_line <- function(bytes) {
  .Call(c_find_last_line, bytes);
}
