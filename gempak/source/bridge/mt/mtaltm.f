	SUBROUTINE MT_ALTM ( stralt, idecd, iret )
C************************************************************************
C* MT_ALTM                                                              *
C*                                                                      *
C* This subroutine decodes the altimeter group in a METAR report.	*
C* If the group exists, the altimeter value is stored in common using	*
C* the units in which it was reported.                                  *
C* 								        *
C* MT_ALTM ( STRALT, IDECD, IRET )                                      *
C*								        *
C* Input parameters: 						        *
C*      STRALT		CHAR*		Possible altimeter field        *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRALTI)	REAL		Altimeter setting in inches     *
C*	RIVALS(IRALTM)	REAL		Altimeter setting in hPa (mb)   *
C*	IDECD		INTEGER		Decode decision flag            *
C*					     1 = field decoded       	*
C*					     0 = field not decoded    	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		     0 = normal return		*
C*	                                    13 = miscoded field		*
C*	                                    -1 = field not found       	*
C*					                                *
C**								        *
C* Log:							        	*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	 2/96   Changed altim upper bound to 32.48      *
C* K. Tyle/GSC		 1/97	Reorganized header and comments; 	*
C*				changed call to DC_WLOG			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP 	 6/97   ST_LSTR -> INDEX and ST_CRNM -> ST_INTG *
C* D. Kidwell/NCEP 	 4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stralt
C*
	LOGICAL 	alterr
C------------------------------------------------------------------------
	alterr = .false.
	iret   = 0
	idecd  = 0
C
C*	Look for group starting with 'A' (inches) or 'Q' (hectopascals)
C
	IF ( ( stralt ( 1:1 ) .eq. 'A' ) .or.
     +       ( stralt ( 1:1 ) .eq. 'Q' ) ) THEN
	    lens = INDEX ( stralt, ' ' ) - 1
	    IF ( lens .ge. 5 ) THEN
	        CALL ST_INTG ( stralt ( 2:5 ), ialtim, iret )
	        IF ( iret .eq. 0 ) THEN
		    idecd = 1
		    IF ( stralt ( 1:1 ) .eq. 'A' ) THEN
C
C*		        Check bounds, then convert to inches if good.
C
			IF ( ialtim .lt. 1000 .or. ialtim .gt. 3350 )
     +			     THEN
			    alterr = .true.
			  ELSE
			    rivals ( iralti ) = FLOAT ( ialtim ) *.01
			END IF
		      ELSE 
C
C*			Check bounds in hPa.
C
			IF ( ialtim .lt.  400 .or. ialtim .gt. 1100 )
     +			     THEN
			    alterr = .true.
			  ELSE
			    rivals ( iraltm ) = FLOAT ( ialtim )
			END IF
		    END IF
		  ELSE
		    alterr = .true.
		END IF
	      ELSE 
		alterr = .true.
	    END IF
	  ELSE 
C
C*	    No 'A' or 'Q' group was found.
C
	    iret = -1
	END IF
C
	IF ( alterr ) THEN
	    iret = 13
	    idecd = 1
	END IF
C*
	RETURN
	END
