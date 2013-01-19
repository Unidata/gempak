#!/usr/bin/wish

proc print_msg {msg {level 0}} {
    global debug

    if {$debug >= $level} {
        puts stderr "$msg"
    }
}

proc init_globals {} {
    global datafile
    global debug
    global objects
    global next_object_id

    set datafile ""
    set debug 0
    set next_object_id 0
    array set objects {}
}

proc build_canvas {} {
    global canvas
    global canvas_width
    global canvas_height
    global canvas_margin
    global canvas_x_size
    global canvas_y_size
    global x_ratio
    global y_ratio
    global coord_xmin
    global coord_xmax
    global coord_ymin
    global coord_ymax

    set coord_width [expr $coord_xmax - $coord_xmin]
    set coord_height [expr $coord_ymax - $coord_ymin]

    set canvas ""

    set canvas_size 800
    set canvas_margin [expr $canvas_size * 0.10]
    if {$coord_width > $coord_height} {
        set canvas_x_size $canvas_size
        set canvas_y_size [expr int($canvas_size * $coord_height / $coord_width)]
    } else {
        set canvas_x_size [expr int($canvas_size * $coord_width / $coord_height)]
        set canvas_y_size $canvas_size
    }

    set canvas_width [expr $canvas_x_size + $canvas_margin * 2]
    set canvas_height [expr $canvas_y_size + $canvas_margin * 2]
    set canvas_ur_x [expr $canvas_width - $canvas_margin]
    set canvas_ur_y [expr $canvas_height - $canvas_margin]

    set x_ratio [expr $canvas_x_size / $coord_width]
    set y_ratio [expr $canvas_y_size / $coord_height]

    set canvas [ canvas .can -width $canvas_width -height $canvas_height -background white]
    $canvas create rectangle $canvas_margin $canvas_margin $canvas_ur_x $canvas_ur_y -dash . -outline {#CCCCCC}
    pack $canvas
    update idletasks

    bind $canvas <ButtonRelease-1> { 
        destroy $canvas
        build_canvas
        read_input
        draw_objects
        update
    }
    bind $canvas <ButtonRelease-2> { 
        exit
    }
}

proc usage {} {
    global argv0

    puts stderr "Usage: [file tail $argv0] \[-f\] \[-D\] -i datafile"
    puts stderr "        -D           : debug output"
    puts stderr "        -f           : flip y coordinate system"
    puts stderr "        -i datafile  : Full path to demo data file"
}

proc read_args {} {
    global argv
    global debug
    global datafile
    global flip_y

    set flip_y 1
    if {[llength $argv] > 0} {
        set index 0
        while {$index < [llength $argv]} {
            set arg [string trim [lindex $argv $index]]
            if {$arg == "-h"} {
                usage
            } elseif {$arg == "-D"} {
                set debug 1
            } elseif {$arg == "-f"} {
                set flip_y 0
            } elseif {$arg == "-i"} {
                incr index
                set datafile [string trim [lindex $argv $index]]
            } else {
                print_msg "Unknown option $arg"
                usage
                exit
            }
            incr index
        }
    }

    if {$datafile == "" || ![file readable $datafile]} {
        print_msg "Cannot read datafile"
        usage
        exit
    }
}

proc compute_bbox {object} {
    global objects

    if {$objects($object,type) != "text"} {
        set xmax -9999999
        set xmin  9999999
        set ymax -9999999
        set ymin  9999999
        set verticies [expr [llength $objects($object,vertex)] - 1]
        for {set vertex 0} {$vertex < $verticies} {incr vertex} {
            set from [lindex $objects($object,vertex) $vertex]
            set next [expr $vertex + 1]
            set to [lindex $objects($object,vertex) $next]

            foreach x [list [lindex $from 0] [lindex $to 0]] {
                if {$x > $xmax} {
                    set xmax $x
                }
                if {$x < $xmin} {
                    set xmin $x
                }
            }

            foreach y [list [lindex $from 1] [lindex $to 1]] {
                if {$y > $ymax} {
                    set ymax $y
                }
                if {$y < $ymin} {
                    set ymin $y
                }
            }
        }

        set objects($object,center) [list [expr $xmin + ($xmax - $xmin) / 2] [expr $ymin + ($ymax - $ymin) / 2]]
        set objects($object,bbox) [list [list $xmin $ymin] \
                                        [list $xmin $ymax] \
                                        [list $xmax $ymax] \
                                        [list $xmax $ymin] \
                                  ]
        if {$objects($object,type) == "polygon"} {
            set objects($object,text,center) $objects($object,center)
        }
    } else {
        set objects($object,center) [lindex $objects($object,vertex) 0]
    }
}

proc parse_object {object} {
    global next_object_id
    global objects
    global coord_xmin
    global coord_xmax
    global coord_ymin
    global coord_ymax

    set isobject 0
    foreach component [lrange $object 0 end] {
        set key [lindex $component 0]
        if {$key == "area"} { 
            set coord_xmin [lindex $component 1]
            set coord_xmax [lindex $component 2]
            set coord_ymin [lindex $component 3]
            set coord_ymax [lindex $component 4]
        } elseif {$key == "incr" || $key == "dist"} { 
        } else {
            set isobject 1
            set value [lrange $component 1 end]
            lappend objects($next_object_id,$key) $value
        }
    }
    if {$isobject} {
        compute_bbox $next_object_id
        incr next_object_id
    }
}

proc parse_dataset {dataset} {
    foreach object $dataset {
        parse_object $object
    }
}

# The Tk coord system puts (0,0) at the upper left instead of the lower left
# so we need to be sure to 'flip' the Y value
proc user_to_screen {point} {
    global x_ratio
    global y_ratio
    global canvas_margin
    global canvas_height
    global coord_xmax
    global coord_xmin
    global coord_ymax
    global coord_ymin
    global flip_y

    set x [expr [lindex $point 0] - $coord_xmin]
    set y [expr [lindex $point 1] - $coord_ymin]

    set x [expr $canvas_margin + $x_ratio * $x]
    set y [expr $canvas_margin + $y_ratio * $y]
    if {$flip_y} {
        set y [expr $canvas_height - $y]
    }

    return [list $x $y]
}

# The Tk coord system puts (0,0) at the upper left instead of the lower left
# so we need to be sure to 'flip' the Y value
proc screen_to_user {point} {
    global canvas_height
    global canvas_margin
    global coord_xmax
    global coord_xmin
    global coord_ymax
    global coord_ymin

    set x [expr [lindex $point 0] - $canvas_margin]
    set y [expr [lindex $point 1] - $canvas_margin]
    set y [expr $canvas_height - $y]

    set x [expr $x/$x_ratio + $coord_xmin]
    set y [expr $y/$y_ratio + $coord_ymin]

    return [list $x $y]
}

proc draw_line {object} {
    global objects
    global canvas

    set verticies [expr [llength $objects($object,vertex)] - 1]
    for {set vertex 0} {$vertex < $verticies} {incr vertex} {
        set from [lindex $objects($object,vertex) $vertex]
        set next [expr $vertex + 1]
        set to [lindex $objects($object,vertex) $next]

        print_msg "    Plotting : $from -> $to" 1
        eval $canvas create line [user_to_screen $from] [user_to_screen $to]
    }
}

proc draw_polygon {object} {
    global objects
    global canvas

    set verticies [llength $objects($object,vertex)]
    for {set vertex 0} {$vertex < $verticies} {incr vertex} {
        set from [lindex $objects($object,vertex) $vertex]
        set next [expr $vertex + 1]
        if {$next < $verticies} {
            set to [lindex $objects($object,vertex) $next]
        } else {
            set to [lindex $objects($object,vertex) 0]
        }

        print_msg "    Plotting : $from -> $to" 1
        eval $canvas create line [user_to_screen $from] [user_to_screen $to]
    }
}

proc draw_object {object} {
    global objects
    global canvas

    if {$objects($object,type) == "polygon"} {
        draw_polygon $object
    } elseif {$objects($object,type) == "line"} {
        draw_line $object
    }

    if {[info exists objects($object,ref)]} {
        puts "$objects($object,id) -> $objects($object,ref)"
    }

    if {[info exists objects($object,arrow)]} {
        set verticies [lindex $objects($object,arrow) 0]
        set x1 [lindex $verticies 0]
        set y1 [lindex $verticies 1]
        set x2 [lindex $verticies 2]
        set y2 [lindex $verticies 3]
        set arrow [eval $canvas create line [user_to_screen [list $x1 $y1]] [user_to_screen [list $x2 $y2]] -arrow last -fill "black"]
    }
    
    if {[info exists objects($object,notplaced)]} {
        set fill "red"
    } else {
        if {[info exists objects($object,ref)]} {
            set fill "green"
        } else {
            set fill "black"
        }
    }
    
    eval $canvas create text [user_to_screen $objects($object,center)] -tags object_text_$object -anchor center -fill $fill -justify center -text $objects($object,id)

    update idletasks
}

proc clear_canvas {} {
    global canvas
    foreach id [$canvas  find all] {$canvas delete $id }
}

proc draw_objects {} {
    global next_object_id
    global objects

    for {set object 0} {$object < $next_object_id} {incr object} {
        if ([info exists objects($object,type)]) {
            draw_object $object

            print_msg "Object $object is a $objects($object,type) and contains" 1
            foreach key [array names objects "$object,*"] {
                regsub "$object," $key "" property
                if {$property != "type"} {
                    print_msg "    $property : $objects($object,$property)" 1
                }
            }
            after 100
        }
    }
}

proc read_input {} {
    global datafile
    global objects

    unset objects

    array set objects {}

    set input [open $datafile "r"]
    eval set data [list [read $input]]
    close $input

    parse_dataset $data
}

init_globals
read_args
read_input
build_canvas
draw_objects
update
puts "Processing of $datafile complete"
wm title    . $datafile
wm iconname . $datafile
