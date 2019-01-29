The National Centers Advanced Weather Interactive Processing System (NAWIPS) is comprised of software that ingests, analyzes, displays, and integrates various types of hydrometeorological data. These types include numerical model, surface, upper-air, satellite, radar, and text data. NAWIPS runs on UNIX workstations that support X Windows and Motif.

NAWIPS includes the GEneral Meteorological PAcKage (GEMPAK) software and a set of graphical user interface (GUI) programs. The GEMPAK software provides much of the NAWIPS core capabilities including data decoding, data analysis, navigation, and display capabilities. The NAWIPS GUI programs provide an easy-to-use interface to some of the GEMPAK capabilities.

The NAWIPS software is being developed within the National Weather Service (NWS), by the National Centers for Environmental Prediction (NCEP), formerly the National Meteorological Center (NMC). The software is currently deployed at all of the NCEP centers. Portions of NAWIPS are available to the university community via distribution from the Unidata Program Center. It is also available to the NWS Scientific Operations Officers (SOOs) via the COMET SOO/Scientific Applications Computer (SAC) program.

---

## GEMPAK Overview

GEMPAK is a set of application programs, libraries, graphic routines, and device drivers for the decoding, analysis, and display of meteorological data. GEMPAK provides a comprehensive set of meteorological parameter calculations for surface, upper-air, and gridded data. Multiple map projections are supported and data can be displayed or listed by user-defined geographic regions. Satellite and radar imagery may be subsetted and displayed with graphic overlays (e.g., geography, contours, etc.). Graphics and imagery can be animated and color enhanced.

GEMPAK programs are run from a text-based user interface, allowing interactive flexibility. UNIX scripts may also be used to simplify the user interaction with GEMPAK programs.

GEMPAK application programs fall into five classes: 

### Grid Programs (GD)

The grid (GD) programs plot contours, vertical cross-sections, time sections, vertical profiles, streamlines, and vectors. A powerful set of diagnostic functions are available. They include a suite of pre-programmed diagnostic functions for calculating many standard meteorological quantities. There are also algebraic, trigonometric, and differential operators available for constructing additional derived fields.

### Graphics Programs (GP)

### Objective Analysis (OA)

The objective analysis (OA) programs use the Barnes technique to transform irregular spaced data (i.e., surface and sounding data) into gridded form. The degree of smoothness and grid spacing may be specified.

### Surface Programs (SF)

The surface (SF) programs draw station plots and meteograms for standard and derived meteorological parameters. Data listing and editing functions are also available. Conditions may be specified for parameters to highlight features of meteorological
interest.

### Sounding Programs (SN)

The sounding (SN) programs can list and display upper-air data in pressure, height, and isentropic coordinates. Vertical profiles, station cross-sections, time-height sections, horizontal maps, and hodographs can be drawn. Conditions for upper-air parameters may be specified.


## GEMPAK Data Parameters 

All calculable meteorological parameters are given 4-character names in GEMPAK. These parameters are listed and described later in this guide.

Surface, upper-air, and gridded data are stored in GEMPAK formatted surface, sounding, and grid files, respectively. Data may be operational or from research sets. Programs exist to translate real-time surface, upper-air, and model data into GEMPAK formatted files. Currently, GEMPAK can navigate and display McIDAS Area files containing satellite or radar imagery and AWIPS satellite images in the GINI format.

## GEMLIB Library

GEMPAK programs are built in a modular fashion using an extensive set of subroutines grouped by function into the GEMPAK library GEMLIB. Each library subroutine name begins with a two letter code, indicating the library function, followed by an underscore (`_`). For example, `SF_OPNF` is a subroutine from the SF (surface) library which opens a surface file.

The GEMPAK library may be linked to programs written by the user.

## GEMPAK Graphics Software

GEMPLT is a general plotting subroutine package that provides the GEMPAK
application programs with device independent graphics capabilities. It was designed to simplify the plotting of meteorological data.

GEMPLT allows plotting in a variety of coordinate systems. The transformations from one coordinate system to another are handled internally. Lines, text, markers, weather symbols, wind barbs, and arrows can be plotted in any coordinate system. Utility functions to draw contours, streamlines, and geopolitical maps are available.

The GEMPLT subroutines communicate with two subprocesses that are maintained as separate modules. Coordinate transformations and utility functions are included in the first subprocess called gplt. The second subprocess is a device driver that draws lines, text, and symbols. The device characteristics that are needed by gplt are defined in the device driver. Since the subprocess can remain active after an application program exits, plotting characteristics defined in one program may be used in other programs that follow it in the same session. This is how graphic overlays are handled. Currently GEMPAK supports several device drivers.

## GUI Programs Overview

The NAWIPS GUI programs provide a Motif-based graphical user interface to some of the GEMPAK capabilities. These programs are:

```
NMAP/NMAP2- Product generation and display application.
NSHARP - Interactive sounding program
NSAT - Displays and animates satellite and radar imagery.
NTRANS - Displays and animates numerical model fields.
NWX - Displays text data.
NALARM - Text product viewer
```

## Document Overview

Chapter 2 contains a description of the GEMPAK text-based user interface and the NAWIPS Top Level (NTL) interface that is used to launch the GUI programs.

Chapter 3 describes the GEMPAK input variables. The variables are arranged alphabetically and are generated from the on-line GEMPAK help.

Chapter 4 contains detailed descriptions of the GEMPAK programs, arranged alphabetically. Each program entry contains a list of input variables, a description of program functions, and a few examples using the GEMPAK test data sets. These descriptions are also generated from the on-line help.

Chapter 5 describes the NAWIPS GUI programs.

Chapter 6 describes the GEMPAK real-time decoder programs.

Appendix A lists the abbreviations, definitions, and algorithms for the GEMPAK parameters. The parameters are categorized by meteorological type, e.g., temperature, stability indices, etc.

Appendix B describes the GEMPAK grid diagnostic functions. Scalar and vector diagnostics are listed alphabetically. Equations for each function are also provided.

Appendix C illustrates and describes the GEMPAK symbols, line types, and markers.

Appendix D is a tutorial for running GEMPAK programs. This appendix is useful for novice GEMPAK users.

Appendix E describes various UNIX scripts included in the NAWIPS distribution.


