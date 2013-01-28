C Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

C *** $Id: areaparm.inc,v 1.1 2000/07/12 13:12:23 gad Exp $ ***

C  area subsystem parameters

CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  IF YOU CHANGE THESE VALUES, YOU MUST ALSO CHANGE THEM IN
C   MCIDAS.H !!
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

C  MAXGRIDPT		maximum number of grid points

C  MAX_BANDS		maximum number of bands within an area
C
      INTEGER	 MAX_BAND
      PARAMETER  (MAX_BAND = 64)


C  MAXDFELEMENTS	maximum number of elements that DF can handle
C			in an area line

      INTEGER    MAXDFELEMENTS
      PARAMETER (MAXDFELEMENTS=22000)

C  MAXOPENAREAS		maximum number of areas that the library can
C			have open (formerly called `NA')

      INTEGER    MAXOPENAREAS
      PARAMETER (MAXOPENAREAS=3)

C  NUMAREAOPTIONS	number of options settable through ARAOPT()
C			It is presently 5 because there are five options
C			that ARAOPT() knows about:
C				'PREC','SPAC','UNIT','SCAL','CALB'
C			(formerly called `NB')

      INTEGER    NUMAREAOPTIONS
      PARAMETER (NUMAREAOPTIONS=5)

C --- Size (number of words) in an area directory

      integer     MX_AREA_DIR_WORDS
      parameter ( MX_AREA_DIR_WORDS = 64 )

C	MAX_AUXBLOCK_SIZE	size (in bytes) of the internal buffers
C 				used to recieve AUX blocks during an
C				ADDE transaction

      integer	MAX_AUXBLOCK_SIZE
      parameter ( MAX_AUXBLOCK_SIZE = 10000 )

C 
C ----- MAX_AREA_NUMBER        Maximum area number allowed on system
C

      integer	MAX_AREA_NUMBER
      parameter ( MAX_AREA_NUMBER = 9999 )

C 
C ----- MAXAREARQSTLEN - max length of area request string
C

      integer	MAXAREARQSTLEN
      parameter ( MAXAREARQSTLEN = 4096 )
