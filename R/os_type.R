os_type = function(){
  os = Sys.info()['sysname']
  if (os == 'Darwin') os = "macos"
  tolower(os)
}
