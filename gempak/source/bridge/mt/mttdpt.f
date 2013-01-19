	SUBROUTINE MT_TDPT ( strtmp, idecd, iret )
C************************************************************************
C* MT_TDPT                                                              *
C*                                                                      *
C* This subroutine will decode the temperature and dewpoint in the	*
C* mandatory section of the METAR report.  The values are stored in	*
C* common.  If the more precise T/Td field is included later in the 	*
C* remarks, its values will supersede those from this subroutine.	*
C* 								        *
C* MT_TDPT ( STRTMP, IDECD, IRET )                                      *
C*								        *
C* Input parameters: 						        *
C*      STRTMP		CHAR*		Temp/dewpt string		*
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRTMPC)  REAL		Temperature in celsius          *
C*      RIVALS(IRDWPC)  REAL		Dew point temp. in celsius      *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  1 = field decoded     	*
C*					  0 = field not decoded   	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return		*
C*	                              	 12 = miscoded field	 	*
C*					 -1 = field not found		*
C*					                                *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP 	11/96	Removed check for format 'tt'           *
C* K. Tyle/GSC		 1/97	Reorganize header and comments; change	*
C*				call to DC_WLOG				*
C* K. Tyle/GSC		 2/97	Cleaned up declarations and error codes	*
C* D. Kidwell/NCEP 	 5/97	Removed ERMISS reference to integer arg *
C* D. Kidwell/NCEP 	 6/97	Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 6/97	Added check for string of length .ge. 40*
C* D. Kidwell/NCEP 	 4/98	New interface                           *
C* D. Kidwell/NCEP 	 9/02	Replaced MT_VALD with BR_VALD           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strtmp
C*
	LOGICAL 	tmperr
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	tmperr = .false.
	iret   = 0
	idecd  = 0
C
C*	Look for '/' in column 3 or 4.
C
	CALL BR_VALD ( strtmp, 3, 4, '/', 1, iloc, iret )
C
	IF ( iret .eq. 0 ) THEN
C
C*	    Get temperature.
C
	    IF ( ( iloc .eq. 4 ).and.( strtmp ( 1:1 ) .ne. 'M' ) ) THEN
		IF ( strtmp ( 1:3 ) .ne. 'NIL' ) THEN
		    jret = 1
		  ELSE
		    jret = 0
		END IF
	      ELSE
		CALL ST_INTG ( strtmp ( iloc-2:iloc-1 ), itemp, jret )
		IF ( ( jret .eq. 0 ) .and. ( strtmp ( 1:1 ) .eq. 'M' ) )
     +		     itemp = - itemp
		IF ( jret .eq. 0 ) rivals ( irtmpc ) = FLOAT ( itemp )
	    END IF	
C
C*	    Get dew point if present.
C
	    lens = INDEX ( strtmp, ' ' ) - 1
	    IF ( lens .lt. 0 ) lens = LEN ( strtmp )
	    lendpt = lens - iloc
C
C*	    Missing dewpoint is represented (wrongly) by 'M', 'XX', 
C*	    '//' or 'NIL'.  Accept any of these.
C
	    IF ( lendpt .gt. 0 ) THEN
		IF ( ( lendpt .gt. 3 ) .or. ( ( lendpt .eq. 3 ) .and.
     +		     ( strtmp ( iloc+1:iloc+1 ) .ne. 'M' ) ) ) THEN
		    IF ( strtmp ( iloc+1:lens ) .ne. 'NIL' ) THEN
		        kret = 1
		      ELSE
			kret = 0
		    END IF
		  ELSE IF ( ( lendpt .eq. 1 ) .and. ( strtmp (lens:lens)
     +			  .eq. 'M' ) ) THEN
		    kret = 0
		  ELSE IF ( lendpt .ge. 2 ) THEN
		    CALL ST_INTG ( strtmp ( lens-1:lens ), idewpt, kret)
		    IF ( ( kret .eq. 0 ) .and. ( strtmp ( iloc+1:iloc+1)
     +		       .eq. 'M' ) ) idewpt = - idewpt
		    IF ( kret .ne. 0 ) THEN
      			IF ( strtmp ( lens-1:lens ) .eq. 'XX' ) kret = 0
			IF ( ( strtmp ( lens-1:lens ) .eq. '//' ) .and.
     +			     ( jret .eq. 0 ) ) kret = 0
		      ELSE
			rivals ( irdwpc ) = FLOAT ( idewpt )
		    END IF
		  ELSE
		    kret = 1
		END IF
	      ELSE
		kret = 0
	    END IF
C
	    IF ( ( jret .eq. 0 ) .and. ( kret .eq. 0 ) ) THEN
		idecd = 1
	      ELSE IF ( ( jret .eq. 0 ) .or. ( kret .eq. 0 ) ) THEN
		tmperr = .true.
	    END IF
	END IF
C
C*	Check temperature bounds.
C
	IF ( .not. ERMISS ( rivals ( irtmpc ) ) ) THEN
	    IF ( ( rivals ( irtmpc ) .lt. ( -80. ) ) .or. 
     +	         ( rivals ( irtmpc ) .gt. 60 ) ) THEN
		tmperr = .true.
		rivals ( irtmpc ) = RMISSD
	    END IF
	END IF
	IF ( .not. ERMISS ( rivals ( irdwpc ) ) ) THEN
	    IF ( ( rivals ( irdwpc ) .lt. ( -80. ) ) .or. 
     +	         ( rivals ( irdwpc ) .gt. 60 ) ) THEN
		tmperr = .true.
		rivals ( irdwpc ) = RMISSD
	    END IF
	END IF
C
	IF ( iret .gt. 0 .or. tmperr ) THEN
	    iret  = 12
	END IF
C*
	RETURN
	END
