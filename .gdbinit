set env TCLLIBPATH .

define wme
  call dump_wme(&symtab, $arg0)
  call printf("\n")
end
