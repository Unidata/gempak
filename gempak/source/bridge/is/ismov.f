	SUBROUTINE IS_MOV ( report, lenr, lenl, idir, ispd, iptr, iret )
C************************************************************************
C* IS_MOV 								*
C*									*
C* This subroutine decodes the direction and speed of a phenomenon from *
C* an international sigmet report.                                      *
C*                                                                      *
C* IS_MOV ( REPORT, LENR, LENL, IDIR, ISPD, IPTR, IRET )                *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	LENL		INTEGER		Length limit of string to search*
C*									*
C* Output parameters:							*
C*	IDIR 		INTEGER 	Direction of phenomenon (deg.)  *
C*	ISPD 		INTEGER 	Speed of phenomenon (knots)     *
C*	IPTR		INTEGER		Pointer to location after flds  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = dir. or speed not found   *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Added check for key words MOVE, STNR    *
C* D. Kidwell/NCEP	11/99	Added check for key word MOVG           *
C* D. Kidwell/NCEP	11/99	Used clo_cmpdir for compass direction   *
C* D. Kidwell/NCEP	 3/00	Added check for key word MVG            *
C* F. J. Yen/NCEP	 4/00	Added parameter lenl			*
C* F. J. Yen/NCEP	 8/01	Modified calling sequence for CLO_CMPDIR*
C* F. J. Yen/NCEP	 1/02	Added check for key word MOVING         *
C* F. J. Yen/NCEP	10/03	Added check for suffix 'WD' in direction*
C*				and non-abbreviated direction for CANADA*
C* F. J. Yen/NCEP	11/03	Added key words 'STATIONARY', 'KNOTS', &*
C*				XTNDG. Fixed false phenomenon speed when*
C*				'KT' found.  Added non-abbrev. direction*
C*				without suffix 'WARD'.	Handled possible*
C*				false key words.			*
C* F. J. Yen/NCEP	 2/04	Added key words 'XPNDG' and 'EXPANDING' *
C*				and suffix 'WDS'.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	carr (10)*14, drct*4, movkwd (8) * 10 
	CHARACTER	dirc (8)*9, dirab (8)*3, drctl*13
	INTEGER		lenkwd (8), ilc (8)
	DATA		dirc / 'NORTH', 'NORTHEAST', 'EAST',
     +			       'SOUTHEAST', 'SOUTH', 'SOUTHWEST',
     +			       'WEST', 'NORTHWEST' /
	DATA		dirab / 'N  ', 'NE ', 'E  ', 'SE ',
     +				'S  ', 'SW ', 'W  ', 'NW ' /
	DATA		movkwd / 'MOV ',    'MVG ',   'MOVE ',  'MOVG ',
     +				 'MOVING ', 'XTNDG ', 'XPNDG ',
     +				 'EXPANDING ' /
	DATA		lenkwd / 4, 4, 5, 5, 7, 6, 6, 10 /, numkwd /8/
C------------------------------------------------------------------------
	iret  = 0
	idir = IMISSD
	ispd = IMISSD
	iptr  = 0
C
C*	Look for the key word MOV, MVG, MOVE, MOVG, MOVING, XTNDG,
C*	XPNDG, or EXPANDING.
C
	len = MIN ( lenl, lenr ) 
	ifrst  = 100000
	DO i = 1, numkwd
	    ilc (i)  = INDEX ( report ( :len ),
     +			      movkwd ( i ) ( :lenkwd(i) ) )
	    IF ( ilc (i) .ne. 0 ) THEN
		IF ( ilc (i) .lt. ifrst ) THEN
		    ifrst  = ilc (i)
		    ifrstp = i
		END IF
	    END IF
	END DO
	IF ( ifrst .ne. 100000 ) THEN
	    iloc = ifrst + lenkwd (ifrstp) - 4
	  ELSE
	    iloc = 0
	END IF
	IF ( iloc .gt. 0 ) THEN
C
C*	    Look for KT or KNOTS to terminate movement information.
C
	    iend = INDEX ( report (iloc:len ), 'KT' )
	    IF ( iend .eq. 0 ) THEN
		iend = INDEX ( report (iloc:len ), 'KNOTS' )
	    END IF
C
C*	    If the 'KT' or 'KNOTS' found is not before '.' 
C*	    then it was a false "move" key word, so find the next
C*	    "move" key word.
C
	    iper = INDEX ( report (iloc:len ), '. ' )
	    IF ( iend .gt. iper ) THEN
C
C*	        Try again to look for the key word MOV, MVG, MOVE,
C*		MOVG, MOVING, or XTNDG.
C
		isecd  = 100000
		DO i = 1, numkwd
		    IF ( i .ne. ifrstp ) THEN
	    	        ilc (i)  = INDEX ( report ( :len ),
     +			               movkwd ( i ) ( :lenkwd(i) ) )
	    	        IF ( ilc (i) .ne. 0 ) THEN
		            IF ( ilc (i) .lt. isecd ) THEN
		                isecd  = ilc (i)
		                isecdp = i
		            END IF
	                END IF
		    END IF
		END DO
		IF ( isecd .ne. 100000 ) THEN
	    	    iloc = isecd + lenkwd (isecdp)
	          ELSE
	    	    iloc = 0
		END IF
		IF ( iloc .gt. 0 ) THEN
C
C*	            Look for KT or KNOTS to terminate movement
C*		    information.
C
	            iend = INDEX ( report (iloc:len ), 'KT' )
	            IF ( iend .eq. 0 ) THEN
		        iend = INDEX ( report (iloc:len ), 'KNOTS' )
	            END IF
C
C*	            If the 'KT' or 'KNOTS' found is not before '.' 
C*	            then it was a false "move" key word, so find
C*		    the next "move" key word.
C
	            iper = INDEX ( report (iloc:len ), '. ' )
	            IF ( iend .gt. iper ) iend = 0
	        END IF
	    END IF
	    IF ( iend .ge. 5 ) THEN
C
C*		Get the direction and speed.
C
		iend = iloc + iend - 1
		CALL ST_CLST ( report ( iloc+4:iend-1 ), ' ', ' ', 10,
     +			       carr, num, ier )
C
C*		The first or second field should be a compass direction.
C*		Convert it to degrees from north.  First, get rid of
C*		suffix 'WD' or 'WDS' if present (Canadian SIGMETs).
C
		ier = -1
		iw = 0
		DO WHILE ( ier .ne. 0 .and. iw .le. 2 )
		    iw = iw + 1
		CALL ST_LDSP ( carr (iw), drct, ncout, ier )
	 	IF ( ncout .ge. 3 ) THEN
		    IF ( drct ( ncout-1:ncout ) .eq. 'WD' ) THEN
			drct = drct ( 1:ncout-2 )
		      else IF ( drct ( ncout-2:ncout ) .eq. 'WDS' ) THEN
			drct = drct ( 1:ncout-3 )
		      else
			drct = carr ( iw ) ( :3 )
		    END IF
		  ELSE
		    drct = carr ( iw ) ( :3 )
		END IF
		CALL ST_NULL ( drct, drct, lens, ier )
		CALL CLO_CMPDIR ( drct, dir, ier )
		IF ( ier .eq. 0 ) idir = NINT ( dir )
		END DO
C
C*		Check for alternate specification of direction in
C*		degrees.
C
		IF ( carr ( iw ) ( :2 ) .eq. 'OR' ) THEN
		    iw = iw + 1
		    CALL ST_LSTR ( carr ( iw ), lens, ier )
		    CALL ST_INTG ( carr ( iw ) ( :lens ), ival, ier )
		    IF ( ier .eq. 0 ) idir = ival
		END IF
C
C*		Check for non-abbreviated 8 point direction
C*		(such as SOUTHEASTWARD, NORTHWESTWARD, WESTWARD,
C*		 SOUTHEAST, NORTHEAST, WEST )
C
		CALL ST_LDSP ( carr (1), drctl, ncout, ier )
		IF ( idir .eq. IMISSD .and. ncout .ge. 8 ) THEN
		    IF ( drctl ( ncout-3:ncout ) .eq. 'WARD' ) THEN
			drctl = drctl ( 1:ncout-4 )
		    END IF
		    CALL ST_FIND ( drctl, dirc, 8, ipos, ier )
		    IF ( ipos .ne. 0 ) THEN
		        drct = dirab ( ipos )
		        CALL ST_NULL ( drct, drct, lens, ier )
		        CALL CLO_CMPDIR ( drct, dir, ier )
		        IF ( ier .eq. 0 ) idir = NINT ( dir )
		    END IF
		END IF 
C
C*		Get the speed immediately preceding KT.  Check for a
C*		range of speeds.
C
		CALL ST_LSTR ( carr ( num ), lens, ier )
		ipos = INDEX ( carr ( num ), '-' )
	  	ipos = ipos + 1
		CALL ST_INTG ( carr ( num ) ( ipos:lens ), ispd, ier )
	      ELSE
		iret = -7
	    END IF
	  ELSE
C
C*	    Check for the key word STNR.
C
	    iloc = INDEX ( report ( :len ), 'STNR' )
	    IF ( iloc .eq. 0 ) iloc = INDEX ( report ( :len ),
     +			 'STATIONARY')
	    IF ( iloc .gt. 0 ) THEN
		idir = 0
		ispd = 0
		iend = iloc
	      ELSE
       	        iret = -7
	    END IF
	END IF
	IF ( iret .eq. 0 ) THEN
	    iptr = INDEX ( report ( iend:len ), ' ' ) + iend - 1
	END IF
C*
	RETURN
	END
