	SUBROUTINE MT_WEA3 ( string, iwea, idecd, iret ) 
C************************************************************************
C* MT_WEA3                                                              *
C*                                                                      *
C* This subroutine will identify a weather group.	                *
C* The groups are stored in common, and are later translated into	*
C* GEMPAK weather numbers by PT_WNMT.					*
C* 								        *
C* MT_WEA3 ( STRING, IWEA, IDECD, IRET )                                *
C*								        *
C* Input parameters: 						        *
C*      STRING		CHAR*		Possible weather phenom field	*
C*	IWEA		INTEGER		Relative number of weather group*
C*								        *
C* Output parameters:						        *
C*	CIVALS(ICWCOD)	CHAR*		Weather character string        *
C*	RIVALS(IRNPWX)  REAL		Number of weather groups        *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = field decoded with slashes*
C*					  1 = field decoded		*
C*					  0 = field not decoded		*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return       	*
C*	                                 32 = unrecognized WX code 	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	 2/96   Cleaned up code                         *
C* D. Kidwell/NCEP 	 2/96   Add check for valid weather codes       *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP 	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 6/97   Added check for string of length .ge. 40*
C* A. Hardy/GSC          4/98   Extracted code and added DC_WTHR call   *
C* D. Kidwell/NCEP 	 4/98   New interface                           *
C* Yen&Caruso Magee/NCEP 3/04   Added check on AUTO reports possibly    *
C*                              sending '//' for wx field.		*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	string
C*
	CHARACTER 	outstr*9
	LOGICAL 	wxfnd
C------------------------------------------------------------------------
        iret  = 0
	idecd = 0
C
C*	Check for length of weather group string.
C
	lens = INDEX ( string, ' ' ) - 1
	IF ( lens .lt. 0 ) lens = LEN ( string )
C
C*      Check to see if AUTO report encoded '//' for weather field.
C
        IF ( rivals (irauto) .eq. 0. .and. string(1:2) .eq. '//' ) THEN
            idecd = 2
            RETURN
        END IF
C
C*	Send string to be validated as a weather code.
C
        CALL DC_WTHR ( string, lens, wxfnd, outstr, iret )
C
C*	If a code was found, then save the group in the common.
C
        IF ( wxfnd ) THEN
            idecd = 1
	    civals ( icwcod ( iwea ) ) = outstr
	    rivals ( irnpwx ) = iwea
        END IF
C*
	RETURN
	END
