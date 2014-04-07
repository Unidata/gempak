
Converters Instructions


These are the usage instructions for 8 command line converting applications.
Bounds converter:
    bound2shp		 --Convert bounds table in *.tbl to a shapefile.   
				   "bnd2shp -h" for help
    shp2ncep		 --Add a shapefile to NCEP database bounds
    				   "shp2ncep -h" for help
    bnd2ncep		 --convert bounds table in *.tbl to shapefile.
      				   and put the converted shapefile to the NCEP database 
      				   "bnd2ncep -h" for help
Vgf converter
    vgf2xml          --Convert Pgen Vgf binary files to Xml files. 
    
Xml converter 
    xml2vgf          --Convert Pgen Xml files to Vgf binary files.
    
Color map converter
    ctbl2cmap 	     --Convert color map tbl files to cmap files.
    
Miscset converter 
    miscset2new      --Convert legacy miscset.tbl file into an equivalent resource attribute files
    
Modres converter 
    modres2new       --Convert legacy mod_res.tbl file and the restore files it references 
		       		   into an equivalent XML file and associated attribute files.


================================
Shapefile, DB related converters
================================
1: bound2shp 
    This scripts is used to convert bounds table in *.tbl to shapefile.
    and put the converted shapefile to the NCEP database
      
    Usage: bnd2ncep <boundsTable>  <shapeDir>
      			- <boundsTable>: the source file with absolute path.
      			- <shapeDir>:   the directory to store converted shapefile, and
      				  put those converted shapefile to the NCEP DB
    for more help, please type
      		    - bound2shp -h
      		    - shp2ncep -h

2: shp2ncep
    Add a shapefile to NCEP database bounds with four default geometry 
   	simplification levels '0.064,0.016,0.004,0.001', the higher value the lower resolution 
   	Note:
    	     1: postgresql is running
    	     2: NCEP database has been installed
    	     3: If there was the same table name in NCEP database, 
    	     	the original one will be deleted automatically
    	--------------------------------------------------------------------
    	Usage: shp2ncep shpDir [simplev [binDir [dbUser [dbPort [installDir [logFile]]]]]] 
     	     1: shpDir    - directory for shapefile stored
    	     2: simplev	  - geometry simplification levels 
    	     		    default: 0.08,0.04,0.02,0.01
    	     3: binDir    - directory for postgresql bin 
    		            default: /awips2/postgresql
    	     4: dbUser    - database user id
    			    default: awips 
    	     5: dbPort    - database port number
    			    default: 5432 
    	     6: installDir- directory to awips installation
    			    default: /awips2/database/sqlScripts/share/sql/ncep  
    	     7: logFile   - optional log file 
      	  		    default: ncep_sql_install.log 
      For more help, please type:
		- shp2ncep -h 

3. bnd2shp
      This scripts is used to convert bounds table in *.tbl to shapefile.
      and put the converted shapefile to the NCEP database
      
      Usage: bnd2ncep <boundsTable>  <shapeDir>
                - <boundsTable>: the source file.
                - <shapeDir>:   the directory to store converted shapefile, and
                                  put those converted shapefile to the NCEP DB
      for more help, please type
                - bound2shp -h
                - shp2ncep -h


================================
PGEN Related Converters
================================

1.  Purpose.

The PGEN VGF files are used in legacy AWIPS system.  The PGEN data XML files are used 
in the new AWIPSII system.  The converters will convert PGEN data files from one type of 
format to another, so the data can be displayed and transferred between the two systems.  
Meanwhile, the existing applications that use the VGF files remain unchanged.

The package includes all executable jar application, lib files, and necessary tables. 

2. Commands to convert *.vgf and *.xml files

2.1 The Vgf converter. 

Run:
   >vgf2xml sourcePath[or sourceFile] destinationPath [-a activity  subActivity] [-t tableName]

Note 1. If using sourcePath, all the files in the directory will be converted.  If using 
        sourceFile, only this source file on the path will be converted.
Note 2. Only *.vgf files can be converted. 
Note 3. “-a” is the Pgen configuration option followed by the activity 
		and subActivity names.  The activity and subActivity are separated 
		by a space. The subActivity can be omitted or after the activity.
Note 4. “-t” is the contour table option followed by the table name.
		It is user's responsibility to provide their tables. The table has 8 
		columns of contour color, contour parameter, level1, level2, 
		forcastHour, cint and time1, time2. See included sampleContour.tbl.
Note 5. The converted files are renamed to *.xml and stored in the 
        destination directory/path. The conversion overwrites the same 
		named files. So make backup of files.
         
2.2 The Xml converter.

Run:
    >xml2vgf sourcePath[or sourceFile] destinationPath

Note 1.  If using sourcePath, all the files in the directory will be converted.  If using 
sourceFile, only this source file on the path will be converted.
Note 2.  Only *.xml file can be converted. It is better to separate source xml files and table xml files.
Note 3.  The converted files are renamed to *.vgf and stored in the destination directory/path.


3. Other converters

3.1 The Color map converter

Run: 
    >ctbl2cmap sourcePath destinationPath

Note 1.  All the files in the directory will be converted.
Note 2.  The converted files are saved to the Satellite, Radar and Other sub-directories of 
		 the destination directory accordingly. These 3 sub-directories will be created automatically.
Note 3.  The lookup table is enhance.tbl. This table defines which sub-directory the converted file put to.

3.2 The Miscset converter
Run:
   >miscset2new  <sourcedf> <target>

Note 1.  <sourcedf>: the source file (example:  $GEMTBL/config/miscset.tbl)
Note 2.  <target>: the target directory (example:  . [current working directory])
Note 3.  The converted attribute files are placed in resource-specific subdirectories
         of the output directory; they all end in ".attr".

3.3 The Modres converter
Run:
   >modres2new <sourcedf> <target>

Note 1. <sourcedf>: the source file (example:  $GEMTBL/nmap/mod_res.tbl)
Note 2. <target>: the target directory (example:  . [current working directory])
Note 3. The converted XML file is ModelFcstGridContours.xml; the attribute files
        have derived unique names ending in ".attr".

4. Converting rules for the converters
	Due to the difference of the AWIPS and AWIPS II systems, not every element and group 
	can have correspondents in the other system.  The VGF files in the AWIPS system and 
	the XML files in the AWIPS II system are converted according to the following conditions. 

	Note that converting from VGF to  XML only converts the major color unless
	otherwise noted.

4.1 Non-grouped elements

4.1.1 Front
		a. Converting a Stationary_front from VGF to XML will convert both 
		   the major and minor colors.
		b. The default front is Cold_front, in case a front does not exist 
		   in one of the systems.

4.1.2 Line
           a. The default line is dashed line for normal lines, and pointed 
              arrow for special lines.
           b. A line that has more than 500 points in xml file will be             
              converted to two lines in vgf file.
           c. Will be implemented soon: some line pattern(such as arrow) is 
              smaller in Cave

4.1.3 Circle
		a. Converting from XML to VGF loses Axis and angle information. In 
		   other words, AWIPS II supports the ellipse object from which a 	   
		   circle may be represented, AWIPS does not.

4.1.4 Text
		a. Text type converting in two systems
			VGF 					XML
		   sptxtyp=0,3,4,5,10,11,13,14	General text
		   sptxtyp=1,2,6,7,8,9,12,16   	AVIATION_TEXT
		   sptxtyp=15        			MID_LEVEL_CLOUD

           b. Font converting in two systems
                 VGF 					XML
		   Courier         			Courier
		   Helvetica				Nimbus Sans L
		   Times					Liberation Serif
		   software					Courier
		   SOFTWARE					Nimbus Sans L

		c. If a VGF text has a group type CLOUD, and it is General text, the 
		   text will be converted to mid_level_cloud text in XML. The 
		   mid_level_cloud text in XML will not be reversed when converting 
		   from XML to VGF.

4.1.5 Symbol, Combo and Marker
           a. Some special symbols are not necessary registered to buttons on 
              the screen, but the systems can display them.

4.1.6 Track
		a. Converting from VGF to XML converts both major and minor colors.

4.1.7 Watch
		a. Converting from VGF to XML converts both major and minor colors.
		b. Converting from VGF to XML doesn't fill all the columns of the 
		   Counties fields in XML. Neither the Outline fields.  This 
		   shouldn't affect the Cave display.
		c. Converting from XML to VGF needs to have the mzcntys.xml and 
		   spcwatch.xml tables installed (available).

4.1.8 Sigmet
           a. No special rules.

4.1.9 CCF
           a. No special rules.

4.1.10 Jet
           a. No special rules.

4.1.11 GFA / G-AIRMET
           a. No special rules.

4.1.12 TCA
	      a. Converting from VGF to XML doesn't fill the Path field in XML. 
		   Neither the landZones fields. This shouldn't affect the CAVE 
		   displaying.

4.1.13 Volcano and Ash Clouds
	      a. If a VGF file contains Volcano and/or Ash, it contains only 
		   volcano and/or ash elements for conversion. 

	      b. The difference between volcano VGF and XML:
		   A VGF file contains one layer of volcano and/or ash elements. 
		   A XML file contains one volcano layer and four ash layers 
		   together. Layers in XML can be empty. 

	      c. The VGF will be converted to XML with possible empty layers.  
		   Layers are identified by fhr. For example, a VGF file containing 
		   only a volcano will be converted to an XML file that contains the 
		   volcano layer and 4 empty ashes layers. 
		   The XML Converter will convert all non-empty layers to ONE VGF 
		   file. For example the XML  file with a volcano layer and 4 empty 
		   ashes layers will be converted to ONE VGF file that only contains 
		   the volcano.

	      d. Converting from XML to VGF needs to have vaa.xml table in this 
	       package installed.

4.1.14 List
	      a. Converting from VGF to XML ignores the list as the List element 
		   is no longer explicitly represented in AWIPS II. Converting from 
		   XML to VGF hard codes the list from the elements that now contain 
		   an internal list.

4.1.15 VGF file header
	      a. Converting from VGF to XML ignores the VGF header. Converting 
		   from XML to VGF creates a hard coded header.


4.2 Grouped elements

4.2.1.Contour
	      a. If a VGF file contains contours, the file must contain at least 
		   one line (or circle) and one text, with group type LABEL and the 
		   same group number.

	      b. The general contour rule is one contour line with one or more 
		   texts. Multi-lines and text in a group with the same group number 
		   will be sorted to one line followed by the closest text.

	      c. Contour lines of the same color will be converted to one contour 
		   structure in the XML file. Contour test color does not count.

	      d. Symbols, when they have group type HIGH, LOW, LABEL or TROPICL, 
		   will be put into the contour structure in the XML if there are 
		   existing contours. Lines with group type ISOBARS, will be put 
		   into the contour structure in the XML if there are existing 
		   contours.

	      e. A line or a symbol without a text will not be put into the 
		   contours. They, Elements in above group type, but not contours, 
		   will be put into default collections.

		f. Contours in an XML file will be converted to VGF in group type 8.

4.2.2.Symbol/Label group, Front/label group and so on
	      a. If a VGF file contains symbols, with group type HIGH, LOW, LABEL 		   
	       or TROPICL, the VGF converter will check if they satisfy the 
		   contour conditions. If not, the VGF Converter converts every 
		   symbol and its labels to the symbol/label collection.
		   The XML Converter converts Symbol and its label to VGF  elements
		   with group number 8.

	      b. If a VGF file contains Fronts, with group type FRONT, the VGF 
		   Converter converts every front and its labels to the front/label     
		   collection. 
		   The XML Converter converts Front and its label to VGF  elements
		   with group number 3.

	      c. If a VGF file contains element/label group, or element that does 
		   not satisfy 6.1 and 6.2, the VGF converter converts them to a 
		   default collection. The type of default collection could be all 
		   kinds of group types(in letters).
		   The XML Converter converts the default collection to VGF elements
		   with corresponding group type.

4.2.3.Cloud/Turbulence
	     	a. In VGF, Special lines( line type 3 and 4) with group type CLOUD, 
		   and texts with the same group type and number, will be converted 
		   to XML Cloud collection.

	      b. In VGF, Lines, Special lines(line type 4) with group type TURB, 
		   and texts with the same group type and number, will be converted 
		   to XML turbulence collection.

	      c. If a VGF file contains Cloud, the Cloud's text could be general 
		   text or mid level cloud text. If an XML file contains Cloud, the 
		   Cloud's 	text is mid level cloud text. So the VGF converter will 
		   convert the general text to the mid level cloud text for XML.
  
		   The XML converter will not convert the mid level cloud text back 
		   to general text.  

		d. If a VGF file contains Turbulence, the Turbulence text could be 
           aviation text(may be general text).  If an XML file contains   
           Turbulence, the Turbulence's text is aviation text.   

        e. Elements in above group type, but not a Cloud or Turbulence, will  
           be put to default collections.

4.2.4.Outlook
           a. If a VGF file contains lines and symbols, or lines and texts in 
              OUTLOOK group type, they are converted to Outlook collections 
		   when VGF group types are 7(OUTLOOK), 12(HAILOTLK), 13(TORNOTLK), 
		   14(WINDOTLK), 16(FIREOUTL), 24(TSTMOLK).
  
           b. Other elements in OUTLOOK group type will be put to default    
              collections.

4.2.5. A VGF file that all elements in it are of the same vg_type
           a. No matter if it is grouped, convert the VGF as if it is not 			   
           grouped(group type 0).


4.3  Compare

4.3.1   To compare a vgf file with the converted back vgf file, vg_type, vg_class, filled, 
           closed, smooth, maj_col, points and other fields are compared.File heads are not compared.
    	              
4.3.2   To compare an xml file with the converted back xml file, color,   
		   points and other fields are compared.
