set env TCLLIBPATH .

define wme
  call debug_dump_wme(&symtab, $arg0)
  call printf("\n")
end

define pref
  call debug_dump_preference(&symtab, $arg0)
  call printf("\n")
end
