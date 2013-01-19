	SUBROUTINE MT_VWND ( strvbw, idecd, iret )
C************************************************************************
C* MT_VWND                                                              *
C*                                                                      *
C* This subroutine decodes the range of wind directions in a variable	*
C* wind group.	The values are stored in common.			*
C*									*
C* MT_VWND ( STRVBW, IDECD, IRET )					*
C*								        *
C* Input parameters: 						        *
C*      STRVBW		CHAR*		Variable wind field    		*
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRDRC1)	REAL		First variable wind direction   *
C*	RIVALS(IRDRC2)	REAL		Second variable wind direction  *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded       	*
C*					  0 = field not decoded    	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return       	*
C*	                                  8 = miscoded field		*
C*                                       -1 = field not found   	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95	                                        *
C* K. Tyle/GSC		 1/97	Change call to DC_WLOG; reorganize	*
C*				header and comments			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP 	 6/97	Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 6/97	Added check for string of length .ge. 40*
C* D. Kidwell/NCEP 	 4/98	New interface                           *
C* D. Kidwell/NCEP 	 9/02	Replaced MT_VALD with BR_VALD           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strvbw
C------------------------------------------------------------------------
	idecd = 0
C
	CALL BR_VALD ( strvbw, 4, 4, 'V', 0, iloc, iret)
	IF ( iret .eq. 0 ) THEN
C
C*	    Decode variable wind group.
C
            idecd = 1
	    kret  = 0
C
C*	    Get first variable wind direction.
C
	    CALL ST_INTG ( strvbw ( 1:iloc-1 ), iwdir1, jret )
	    IF ( ( iwdir1 .lt. 0 ) .or. ( iwdir1 .gt. 360 ) ) kret = 8   
C
C*	    Get second variable wind direction.
C
            lens = INDEX ( strvbw, ' ' ) - 1
	    IF ( lens .lt. 0 ) lens = LEN ( strvbw )
	    IF ( iloc .lt. lens ) THEN
                CALL ST_INTG ( strvbw ( iloc+1:lens ), iwdir2, jret)
	        IF ( ( iwdir2 .lt. 0 ) .or. ( iwdir2 .gt. 360 ) ) 
     +                 kret = 8   
	      ELSE
		kret = 8
	    END IF
C
	  ELSE IF ( iret .eq. 1 ) THEN
C
C*	    Found 'V' in wrong place - might be a variable wind group -
C*	    Check for two integers separated by 'V'.
C 	 
	    kret = -1
	    IF ( strvbw ( 1:5 ) .ne. 'CAVOK' ) THEN
		lens = INDEX ( strvbw, ' ' ) - 1
		IF ( lens .lt. 0 ) lens = LEN ( strvbw )
		IF ( ( iloc .ne. 1 ) .and. ( iloc .ne. lens ) ) THEN
		    CALL ST_INTG ( strvbw ( 1:iloc-1 ), jwdir1, jr1 )
		    CALL ST_INTG ( strvbw ( iloc+1:lens ), jwdir2, jr2 )
		    IF ( ( jwdir1 .ge. 0 ) .and. ( jwdir1 .le. 360 )   
     +              .and. ( jwdir2 .ge. 0 ) .and. ( jwdir2 .le. 360 ) )
     +		    THEN  
			iwdir1 = jwdir1
			iwdir2 = jwdir2
			idecd  = 1
			kret   = 1
		    END IF
		END IF
	    END IF
          ELSE 
C
C*          No variable wind group found.
C
	    kret = -1
	END IF
	IF ( kret .eq. 0 .or. kret .eq. 1 ) THEN
	    rivals ( irdrc1 ) = FLOAT ( iwdir1 )
	    rivals ( irdrc2 ) = FLOAT ( iwdir2 )
	END IF
	IF ( kret .eq. 1 ) kret = 8
	iret = kret
C*
	RETURN
	END
