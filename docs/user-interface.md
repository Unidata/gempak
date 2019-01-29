# User Interface

The GEMPAK user interface is text-based, case-insensitive, and handles many abbreviations and aliases.  GEMPAK programs run as a command prompt and receive input either directly from the user or from a shell script.

The GEMPAK interface is organized by rows, with variables (`SFFILE`) first, then a short description (`Surface data file`), then the current settings (`$GEMPAK/data/hrcbob.sfc`):

<pre>
<font color=red>vars</font>      <font color=green>description</font>                       <font color=blue>current setting</font>
----      -----------                       ---------------
<font color=red>SFFILE</font>    <font color=green>Surface data file</font>                 <font color=blue>$GEMPAK/data/hrcbob.sfc</font>
<font color=red>AREA</font>      <font color=green>Data area</font>                         <font color=blue>WV</font>
<font color=red>DATTIM</font>    <font color=green>Date/time</font>                         <font color=blue>LAST</font>
<font color=red>SFPARM</font>    <font color=green>Surface parameter list</font>            <font color=blue>SKYC;TMPF;WSYM;PMSL;;DWPF</font>
<font color=red>OUTPUT</font>    <font color=green>Output device/filename</font>            <font color=blue>T</font>
<font color=red>IDNTYP</font>    <font color=green>STNM or STID</font>                      <font color=blue>STID</font>
</pre>


GEMPAK programs are started simply by typing their name in your terminal window:

    sflist

New values are assigned by entering the definitions directly:

    GEMPAK-SFLIST>sffile = $GEMDATA/nmcbob.sfc 
    GEMPAK-SFLIST>area = den
    GEMPAK-SFLIST>sfparm = tmpf;dwpf

Variables are listed with the command `list` or `l` for short:

    GEMPAK-SFLIST>l

Programs are run with command `run` or `r` for short:

    GEMPAK-SFLIST>r

Programs are exited with the command `exit` or `e` for short:

    GEMPAK-SFLIST>e


## Exercise #1 (An Introduction)

To begin using the GEMPAK interface, start a programs such as `sflist`:

A list of all current values is followed by with a prompt on the last line.  Type `l` to show the current definitions:

    GEMPAK-SFLIST>l
    SFFILE   = $GEMPAK/data/hrcbob.sfc
    AREA     = WV
    DATTIM   = LAST
    SFPARM   = SKYC;TMPF;WSYM;PMSL;;DWPF;BRBK
    OUTPUT   = T
    IDNTYP   = STID
    GEMPAK-SFLIST>

Run the programs with `r`:

    GEMPAK-SFLIST>r
    PARM = SKYC;TMPF;WSYM;PMSL;DWPF;BRBK                                            
    
    STN    YYMMDD/HHMM      SKYC     TMPF     WSYM     PMSL     DWPF     BRBK
    BKW    910820/0600      8.00    60.00    61.00  1011.00    59.00  4190.00
    BLF    910820/0600      8.00    57.00    61.00  1010.60    56.00  4180.00
    CRW    910820/0600      8.00    63.00    10.00  1011.20    62.00  3200.00
    ...
    Parameters requested: SFFILE,AREA,DATTIM,SFPARM,OUTPUT,IDNTYP.
    GEMPAK-SFLIST>

The parameters defined by `SFPARM` are displayed for `DATTIM = LAST` (0600 UTC) and stations which fall inside the area defined by `AREA` (in this case, an area centered around West Virginia).

After checking that the definition for `SFFILE` has observations to display, exit the program with the command `e`:

    GEMPAK-SFLIST>e

## Help Pages

While in the GEMPAK prompt (which you just exited, so start `sflist` again), you can view the help file for any **program** or **parameter** using the command `phelp`:

     GEMPAK-SFLIST>phelp idntyp
     IDNTYP
    
     IDNTYP sets character or numeric station identifiers to be used for
     input or output.  The valid values are STID and STNM.  STID specifies
     station character identifiers; STNM specifies station numbers.
    
     If the value in IDNTYP is not STNM, the default of STID will be used.
    
     For example, to update station headers in SFSTNS:
    
       IDNTYP =        STID -- compares character ids in file with those
                               in the station table
       IDNTYP =        STNM -- compares numeric ids in file with those
                               in the station table
    

## Default Files

Back in the terminal window, list the files in the current working directory with `ls -la` and make note of two new files:

* `gemglb.nts` stores values for all GEMPAK program variables, updated by the last program run, available by default to any GEMPAK program run next.  A definition of `SFFILE` in `sflist` will be available in any other program which uses `SFFILE` (such as `sfmap`), as long as this file is available.</li>
* `last.nts` stores values from the last program run, used the same as `gemglb.nts` but for the last program run.

The default values for parameters such as `SFFILE` and `SFPARM` come from default files located in `$GEMPAK/defaults/*.nts` for each program.

## Environmental Variables

Environment variables are used by GEMPAK to find needed executable, table, parameter, error, help, and map files without requiring user input.

    $NAWIPS - Top of the NAWIPS distribution
    $GEMPAK - Directory for the top of the GEMPAK tree
    $GEMDATA - Location of data files
    $OS_LIB - Library directory (used for installation)
    $OS_BIN - Location of executable code
    $GEMPDF - Location of parameter definition files
    $GEMTBL - Location of tables
    $GEMERR - Location of error messages
    $GEMHLP - Location of on-line help files
    $GEMMAPS - Location of map files

Each of these is defined in a variable definition file (`~/NAWIPS/Gemenviron` for **csh/tcsh**, `~/NAWIPS/Gemenviron.profile` for **bash**).  Depending on your shell, one of these files **MUST** be sourced in the current shell environment for GEMPAK programs to work.

* For **csh/tcsh**, in a terminal, type `source ~/NAWIPS/Gemenviron`</li>
* For **bash**, type `. ~/NAWIPS/Gemenviron.profile`

If you're currently taking part in the Unidata GEMPAK Workshop, the above step has been taken care of for you by adding the `source` command to your `.tcsh` file, executed at login.

Check that the GEMPAK environmental variable are correct with the command:

    env| grep GEMPAK
