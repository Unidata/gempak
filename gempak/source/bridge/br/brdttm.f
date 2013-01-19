	SUBROUTINE BR_DTTM ( strdat, lendat, jda, jhr, jmin, iret ) 
C************************************************************************
C* BR_DTTM                                                              *
C*                                                                      *
C* This subroutine decodes a date/time field of the form HHMM or DDHHMM.*
C* The field should end with 'Z', or it should have a trailing blank    *
C* appended before callling this routine, to satisfy the length check.	*
C* 								        *
C* BR_DTTM ( STRDAT, LENDAT, JDA, JHR, JMIN, IRET )			*
C*								        *
C* Input parameters: 						        *
C*	STRDAT		CHAR*		Date/time field			*
C*	LENDAT		INTEGER		Length of date/time field	*
C*								        *
C* Output parameters:						        *
C*	JDA		INTEGER		Day of month (DD)	        *
C*	JHR		INTEGER		Hour of day (HH)	        *
C*	JMIN		INTEGER		Minute (MM)		        *
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = field decoded ok		*
C*	                                 17 = decode problem         	*
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP      10/95                                           *
C* K. Tyle/GSC           1/97   Reorganized header and comments; change *
C*                              call to DC_WLOG                         *
C* K. Tyle/GSC           2/97   Changed error processing                *
C* D. Kidwell/NCEP 	 9/02   Renamed from MT_DTTM; removed mtcmn.cmn *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strdat
C------------------------------------------------------------------------
	iret = 0
C
C*	First check length of date/time field.
C
	IF ( ( lendat .eq. 5 ) .or. ( lendat .eq. 7 ) ) THEN
C
C*	    Get minutes.
C
	    CALL ST_INTG ( strdat ( lendat-2:lendat-1 ), jmin, jret )
	    IF ( ( jmin .lt. 0 ) .or. ( jmin .gt. 59 ) ) THEN
	        iret = 17
	        jmin = IMISSD
	    END IF
C
C*	    Get hour.
C
	    CALL ST_INTG ( strdat ( lendat-4:lendat-3 ), jhr, jret )
	    IF ( ( jhr .lt. 0 ) .or. ( jhr .gt. 23 ) ) THEN
	        iret = 17 
	        jhr  = IMISSD
	    END IF
C
C*	    Get day.
C
	    IF ( lendat .eq. 7 ) THEN
	        CALL ST_INTG ( strdat ( 1:2 ), jda, jret )
	        IF ( ( jda .lt. 1 ) .or. ( jda .gt. 31 ) ) THEN
                    iret = 17 
	            jda  = IMISSD
	        END IF
	      ELSE
C
C*	        No day given, will need to get it from system or from
C*		bulletin header.
C
	        jda = IMISSD
	    END IF
	  ELSE
C
C*	    The field length is bad.
C
	    jmin = IMISSD
	    jhr  = IMISSD
	    jda  = IMISSD
	    iret = 17 
	END IF
C*
	RETURN
	END
