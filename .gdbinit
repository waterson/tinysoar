set env TCLLIBPATH .

define wme
  call debug_dump_wme(&symtab, $arg0)
  call printf("\n")
end

define pref
  call debug_dump_preference(&symtab, $arg0)
  call printf("\n")
end

define token
  call debug_dump_token(&symtab, $arg0)
  call printf("\n")
end

define beta
  call debug_dump_beta_node(&symtab, $arg0, 0, 0, 0)
  call printf("\n")
end
