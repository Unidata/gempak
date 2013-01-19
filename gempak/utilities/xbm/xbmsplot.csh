#!/bin/csh -f
#------------------------------------------------------------------------
# xbmsplot.csh
#
# This script generates bitmaps for all of the GEMPAK symbols.
#
# The current implementation of the PLOTXBMS program can run only once
# (creating only a single bitmap) and then it must shut down and be
# restarted.  This script runs PLOTXBMS for all of the GEMPAK symbols,
# one symbol at a time, as follows:
#
#    1. Removes file 'bitmap000' from the local directory (if it exists).
#    2. Runs PLOTXBMS once for one bitmap (table-driven).
#    3. Renames the resulting file 'bitmap000' to a more descriptive name.
#	The descriptive name is a combination of a 2-letter acronym (flnm 
#	from the table) and a number that corresponds to the symbol's 
#	number as in Appendix C of the User's Guide.  For example, 
#	sc_003.xbm for sky cover number 3 (symbol for "four thenths").
#    4. Moves the bitmap to directory 'bitmaps' that is created locally 
#	to the directory in which this script runs.
##
# Log:
# M. Linda/GSC		 6/97	Original
#------------------------------------------------------------------------

$RM last.nts
$RM gemglb.nts

#------------------------------------------------------------------------
# Table that drives this script.

#   type = symbol group number as defined in PLOTXBMS
#   flnm = mnemonic equivalent to 'type' used to construct file name
#   min  = smallest symbol code number
#   max  = largest  symbol code number
#   step = increment for symbol code number

set type = "  1  2   2   2  3  3  3  4    5  6  7  8  9 10  11  11  11 "
set type = "  1  2   2   2  3  3  3  4    5  6  7  8  9 "
set flnm = ( mk wx  wx  wx ct ct ct sc   pt pw ic tu sp ln  fn  fn  fn )
set min  = (  1  0 103 201  1 11 21  0  999  3  0  0  0  0 100 105 108 )
set max  = ( 21 99 107 203  9 19 29 10 8999  9  8  8 30  9 900 605 608 )
set step = (  1  1   1   1  1  1  1  1 1000  1  1  1  1  1 100 100 100 )

#------------------------------------------------------------------------
# Prepare the destination for bitmaps.

if  ( -d bitmaps ) then
    set anyFiles = `find bitmaps -type f -print`
    if  ( "$anyFiles" != "" ) then
	$RM bitmaps/*
    endif
else
    mkdir bitmaps
endif

#------------------------------------------------------------------------
# Generate one file per symbol.  Files are named 'bitmapXXX' where XXX
# is 000, 001, 002, ...

set oldname = "bitmap000"

set cnt = 1
foreach onetype ( $type )

    set symnum = $min[$cnt]

    while ( $symnum <= $max[$cnt] )

	$RM $oldname

	plotxbms << EOF
	$onetype
	$symnum
	exit
EOF

	set zers = ''
	if  ( $symnum < 100 ) then
	    set zers = '0'
	endif
	if ( $symnum < 10 ) then
	    set zers = '00'
	endif

	set newname = "$flnm[$cnt]$zers$symnum"
	cat $oldname | sed "s,$oldname,$newname," > bitmaps/$newname.xbm

	set symnum = `expr $symnum  + $step[$cnt] `
    end

    set cnt = `expr $cnt + 1`
end

$RM $oldname

#------------------------------------------------------------------------
