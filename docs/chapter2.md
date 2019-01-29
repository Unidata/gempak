# NAWIPS User Interface

The NAWIPS programs can be separated into two groups: **GEMPAK programs** and **GUI programs**. Access to both types of programs is outlined in the following sections.

## GEMPAK Commands

The GEMPAK user interface is a text-based interface. Programs are run and commands are input by typing in the appropriate command and value. In this section, common commands that are used in GEMPAK are presented. 

To start a GEMPAK program, type the name of the program in lower case. If the program fails to run see your system administrator and/or check your PATH environment variable.

Once a GEMPAK program is running, a list of valid parameters is presented. Next to each parameter is a short description of how the given parameter should be input, followed by the current value of the parameter. GEMPAK has a built-in system that keeps track of all the parameter definitions. This information is stored in a pair of ASCII text files, `gemglb.nts` (the global parameter file) and `last.nts` (the settings from the last program run).

Commands may then be entered following the prompt 

    GEMPAK-PROGRAM>
    
where `PROGRAM` is the name of the GEMPAK program currently executing.
After the run command is entered, the program executes and returns to the `GEMPAK-PROGRAM>` prompt allowing for additional changes and re-execution.


### Listing Commands

> In the command summary below, optional information is enclosed by `[]`


To display parameter names, definitions, and values

    display [name]

or, shortened

    d [name]

lists definitions and values of all program parameters or of the single specified parameter, `name`.

To list commands

    list [name]
or

    l [name]
    
lists values of all the program parameters or of the single specified parameter, `name`, such as `l gdattim`.

---

### Animation Commands

These commands allow manipulation of an existing animation sequence:

Starts forward animation of the loaded sequence (the loop is stopped by a single click of the left mouse button in the output graphics window).

    ;loop 

Starts backward animation the of loaded sequence (the loop is stopped by a single click of the left mouse button in the output graphics window).

    ;reverse

Steps one frame forward.

    ;step

Steps one frame backward.

    ;back

Sets the dwell rates to group # (# is a value in the range of 1-5. 1=slowest loop, 5=fastest).

    ;#

---

### Execution Commands

These commands may be abbreviated so long as the abbreviation is unique and may be either upper or lower case.

To **RUN** a GEMPAK program after all input is entered:

    run
    
or, shortened

    r
    
To **EXIT** a running GEMPAK program

    exit
    
or
   
    e

To display the current version number

    version 

---

### Help Commands
    
To display this help information on the screen

    help
    
To display help information for a program or parameter name (`name`)


    help name
    
or for paged output, use `phelp`

    phelp [name]

> If help information is sought for a parameter not used in the current program, the full parameter name must be entered. For help information on any program, the full program name must be entered.

---

### Assignment Commands
    
In a parameter assignment command, a parameter name may be abbreviated to the extent that it remains unique among all the other parameters used by that program.

The following commands assign values to the parameters:

Assigns a value to parameter PARM

    PARM = xyz

Assigns values of PARM2 to PARM

    PARM = &PARM2 

Recalls parameter values stored by SAVE [FILE].
    
    restore [FILE] 

Saves values of parameters for future RESTORE.

    save [FILE]

Assigns value to PARM based on user input from the mouse button [GAREA, GPOINT, and CXSTNS are the
only valid inputs for PARM].

    cursor PARM 

Loads a color look-up table stored in FILE:
 
    lut FILE
    
Activates, pops, and restores information about the window specified by NAME:

    window NAME

Closes the window specified by NAME:
    
    close NAME

A more detailed tutorial for the GEMPAK interface is included in Appendix D.

## GUI Programs

The NAWIPS Graphical User Interface programs may be activated most easily via a program launcher.The launcher, ntl, can be executed from the command line or set up to run at login.

NTL provides a set of buttons corresponding to the names of the GUI programs. NTL also initializes shared resources common to all of the GUIs. A single mouse click on the appropriate NTL button runs the desired GUI program.

Alternatively, each GUI program may be run in a “stand-alone” mode where the program name is entered from the UNIX command line directly.

On-line help is available for the GUI programs and most of the pop-up dialog boxes within the programs. Each of the GUI programs is described in more detail in Chapter 5.



