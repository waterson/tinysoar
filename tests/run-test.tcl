#!/usr/bin/tclsh

if {$argc == 0 || $argc > 1} {
    puts stderr "usage: test.tcl \[file\]"
    exit 1
}

set output_link 4
set cycles 0
set post ""
source $argv

foreach count $cycles {
    init-soar

    while {[incr count -1] >= 0} {
        elaborate
    }
}

# Look for `^success t' on the top state.
array set attrs [lreplace [split [print $output_link]] 0 0]
set success [lindex [array get attrs ^success] 1]

if {[string compare $success "t"] == 0} {
    if {"$post" != ""} {
        exit [eval $post]
    }

    exit 0
}

exit 1
