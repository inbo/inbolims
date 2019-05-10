

sink("LOG.TXT")

args <- commandArgs()

print("Argument file")

print(args[6])

fileName = args[6]


Data <- readLines(fileName)

substr <- substring(Data[1],1,4)

if(substr != 3500 ){
  print("Modify")
  print(Data[1])
  Data[1] <- substring(Data[1],4,)
  Data <- iconv(Data, from = "UTF8", to = "ASCII")
  print(Data[1])
}

print("Nog enkel schrijven")

writeLines(Data, fileName)

print("Einde Routine")

sink()


