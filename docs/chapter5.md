# CHAPTER 5

## Graphical User Interface Programs

## 5.1 AFOS

AFOS displays graphics and text products from the AFOS data feed. A Motif graphical
user interface controls the program functions.

AFOS products are accessed by a table-driven selection window where the products are
grouped into categorical sets. After choosing a set, a product is selected by its PIL
identifier. By default, the program displays the latest version of the product. Earlier and,
then, later versions of the product may be also viewed by selecting the “previous” or
“next” options on the control panel.

Graphic products may be combined by setting the “overlay” option. If the “overlay”
option is on, each product is automatically assigned a different color. Alternatively, if
the “overlay” option is off, each plot first clears the screen.

AFOS also provides a color editing facility that allows the graphics colors to be
modified. Edited color look-up tables can be saved and restored. Zooming and
animation are also supported.

Any valid AFOS PIL may be added to the existing tables, or new tables may be added
in order to expand the display capabilities of the program. However, the ingest program
must also be set to receive and archive the requested data if new data sets are to be
added.

The data must reside in a directory accessed through the $AFOS_DATA environment
variable. The graphics files should be in the $AFOS_DATA/graphics directory and the
text products in the $AFOS_DATA/products directory.

AFOS was originally developed at the River Forecast Center in Tulsa, OK. It was
subsequently enhanced and integrated with the N-AWIPS software.

## 5.2 NALARM

NALARM provides desktop notification when designated products arrive and provides
a text window for browsing the content of the product. Each user can have their own
alarm products which are typically filed by PQACT actions from the LDM.


## 5.3 NMAP/NMAP

NMAP displays and animates different types of meteorological data on a geographic
background. The current version supports overlay of satellite, radar, model, METAR,
ship, MOS, and upper-air data. A Motif graphical user interface controls the program
functions.

NMAP also has a product generation mode. Numerous capabilities exist to draw, and
edit graphical products and generate associated text products. Product creation and
editing can be overlayed on any data type supported by NMAP.

NMAP allows the selection of a time range and interval for data display for all data
types except VGF which is a single time. When several types of data are displayed, the
program time-matches the data in each frame.

For more information, see the online “Help” within NMAP.

## 5.4 NSAT (depricated)

NSAT displays and animates imagery in an X-Window. A Motif graphical user
interface is used to control the program functions.

NSAT loads the latest sequence of images into memory for display. The number of
images to load may be specified by the user with a maximum of 50 images per
animation sequence. The program also has the option to automatically update a
sequence as new images become available in the data base.

Images may be color-enhanced by selecting pre-defined color look-up tables.

Geopolitical boundaries and latitude/longitude lines of varying resolutions may be
overlaid on images. For latitude/longitude lines, the increments may be specified in
degrees. The attributes of these lines, including color, type, and width, may also be
specified.

Other NSAT functions include image zoom/unzoom and latitude/longitude display.
The latitude and longitude at the cursor location is displayed in the lower right hand
corner of the display window.

NSAT currently supports the display of satellite and radar images in the McIDAS Area
file format and satellite images in the AWIPS GINI format. Remapped images or
images in the native satellite projection can be displayed. The program supports a
variable number of lines and pixels in the image files. The image data are automatically
sampled to properly fill the display window.

Satellite image files must be properly located and named in the data base for NSAT to
access them. The directory structure must be organized in the following way:


```
$SAT/satellite_name/sector/chan/chan_YYMMDD_HHMM
```
An environment variable $SAT must point to the directory one level above the
satellite_name directory. The satellite_name directory specifies the name of the
satellite. The sector directory can be any name that specifies the satellite sector. The
chan string specifies the channel name, e.g., IR, VIS or WV which designates infrared,
visible, or water vapor channel, respectively. The YYMMDD_HHMM string specifies
the image date and time. An example path and file name is:

```
$SAT/GOES-8/US_4km/IR/IR_950501_
```
Currently, radar images can only be accessed using the image lists or user defined
facilities.

NSAT is integrated with the N-AWIPS software and uses the GEMPAK image display
and navigation utilities.

## 5.5 NSHARP

NSHARP provides an interactive Skew-T and Hodograph interface for upper air data
sets. Soundings can be displayed for Radiosonde, Dropsonde, Gridded and Aircraft
data.

## 5.6 NTRANS

NTRANS displays and animates N-AWIPS graphics metafiles in an X-Window. The
program is primarily used to display model fields generated by the N-AWIPS/
GEMPAK software. A Motif graphical user interface controls program functions.

NTRANS allows the selection of graphical products from a set of metafiles stored in
the data base. Metafiles are generally listed by model and contain groups of products,
usually forecast sequences that may be selected for display. For example, a metafile
group could be the 500 mb height and vorticity fields for all of the analysis and forecast
times. Selection of a metafile group loads the entire product sequence for display and
animation. Alternatively, specific frames from a group may be loaded.

The display window can be sub-divided into panels to display and animate more than
one metafile group at a time. This panel capability allows for the comparison of
different model fields, different model runs, or different models. A “valid time” option
may be set to syncronize the times for different models or model start times. The panel
structure, i.e., the number of panels and their orientation, is user configurable.

NTRANS provides a color editing facility that allows the graphics colors to be
modified. Edited color look-up tables may be saved and restored. Colors may be


toggled to the background color (thereby turning them “off”) or set to blink to highlight
particular features.

Frames or groups of frames can be printed using the NTRANS print facility.
Monochrome and color postscript printers are supported. Currently, printing of
multiple panel displays is not supported.

Metafiles must be properly located in the data base for easy NTRANS access. An
environment variable $NTRANS_META must be set to a top level directory. Each
model has a directory under $NTRANS_META and will appear as a button in the
Select Model Panel. Metafiles are stored under each model directory. Metafiles stored
in other parts of the system can be accessed less conveniently by using the NTRANS
Local Products or User Defined facilities.

NTRANS is integrated with the N-AWIPS software. It displays CGM-like metafiles
generated by GEMPAK using the NC device driver.

## 5.7 NWX

NWX displays text data from the Family of Services (FOS) data feed. A Motif
graphical user interface controls the program functions.

A graphical method is used to select the data to be displayed. Data types that can be
represented at individual locations are plotted as markers on a map. The text data are
displayed in a text window by clicking on the desired station with the cursor mouse. For
event-driven products, e.g., warnings, the program plots only the stations that have
issued those products within a user-specified time period. By default, the latest data are
displayed. Earlier versions of the data may be viewed by using the “Previous” and
“Next” options.

The displayed map region can be changed to accomodate different data types. A
number of predefined areas are listed in the MAP menu. Zooming in on any area is also
supported by drawing a bounding box with the mouse cursor.

NWX currently supports data received from the FOS data feed. NWX uses the WMO
data headers and the necessary control characters to perform its searching.

NWX is integrated with the N-AWIPS software and uses the GEMPAK image display
and navigation utilities.


