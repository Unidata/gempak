	SUBROUTINE IS_EGMV ( report, lenr, idir, ispd, iptr, iret )
C************************************************************************
C* IS_EGMV 								*
C*									*
C* This subroutine decodes the direction and speed of a phenomenon from *
C* an EGGY, RJAA, or NTAA international sigmet report.			*
C*                                                                      *
C* IS_EGMV ( REPORT, LENR, IDIR, ISPD, IPTR, IRET )                     *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
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
C* F. J. Yen/NCEP	 1/00	Converted from IS_MOV for EGGY		*
C* F. J. Yen/NCEP	 4/00	Added check for key word MVG		*
C* F. J. Yen/NCEP	 6/01	Added MOVING; non-abbreviated dir; no KT*
C* F. J. Yen/NCEP	 8/01	Modified calling sequence for CLO_CMPDIR*
C* F. J. Yen/NCEP	 8/01	Expanded termination of movement info.	*
C* F. J. Yen/NCEP	 1/02	Added MOVING to comment about key word	*
C* F. J. Yen/NCEP	 6/02	Added key word 'STATIONARY' & allow '.'.*
C*				Added check for direction without speed.*
C* F. J. Yen/NCEP	 6/03	Increased work array from 1000 to 2000. *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C*
	DIMENSION	ln (6)
	CHARACTER	carr (10)*6, drct*4, work*2000, car2 (10)*6
	CHARACTER	dirc (4)*5, term (6)*5
	DATA		dirc / 'NORTH', 'SOUTH', 'EAST', 'WEST' /
	DATA		term / 'KT', ' NC', ' WKN', 'INTSF', '.', '=' /
	DATA		ln   /   2,     3,      4,       5,   1,   1  /
C------------------------------------------------------------------------
	iret  = 0
	idir = IMISSD
	ispd = IMISSD 
	iptr  = 0
C
C*	Look for the key word MOV, MVG, MOVE, MOVG or MOVING.
C
    	len = lenr  
	iloc = INDEX ( report ( :len ), 'MOV ' )
	IF ( iloc. eq. 0 ) THEN
	    iloc = INDEX ( report ( :len ), 'MVG ' )
	    IF ( iloc. eq. 0 ) THEN
		iloce = INDEX ( report ( :len ), 'MOVE ' )
		ilocg = INDEX ( report ( :len ), 'MOVG ' )
		iloc  = MAX ( iloce, ilocg )
		IF ( iloc .gt. 0 ) THEN
		    iloc = iloc + 1
		  ELSE
		    iloc = INDEX ( report ( :len ), 'MOVING ')
		    IF ( iloc .gt. 0 ) iloc = iloc + 3
		END IF
	    END IF
	END IF
	IF ( iloc .gt. 0 ) THEN
C
C*	    Look for strings to terminate movement information.
C
	    iend = 0
	    ndx = 1
	    ikt = 0
	    DO WHILE ( iend .eq. 0 .and. ndx .le. 6 )
		iend = INDEX ( report (:len), term (ndx) (1:ln(ndx)) )
C
C*		ikt is the position for 'KT'
C
		IF ( ndx .eq. 1 ) ikt = iend
		ndx = ndx + 1
	    END DO
	    IF ( ( iend .ne. 0 .and. iend .lt. iloc )
     +			     .or. iend .eq. 0 ) iend = len
	    IF ( iend .ge. iloc + 5  ) THEN
		    iend = MIN ( iend, iloc + 35 )
	    END IF 

	    IF ( iend .ge. ( iloc + 5 ) ) THEN
C
C*		Get the direction and speed.
C
		CALL ST_CLST ( report ( iloc+4:iend-1 ), ' ', ' ', 10,
     +			       carr, num, ier )
C
C*		The first field should be a compass direction.  Convert
C*		it to degrees from north.
C
		drct = carr ( 1 ) ( :3 )
		CALL ST_NULL ( drct, drct, lens, ier )
		CALL CLO_CMPDIR ( drct, dir, ier )
		IF ( ier .eq. 0 ) idir = NINT ( dir )
C
C*		Check for alternate specification of direction in
C*		degrees.
C
		IF ( carr ( 2 ) ( :2 ) .eq. 'OR' ) THEN
		    CALL ST_LSTR ( carr ( 3 ), lens, ier )
		    CALL ST_INTG ( carr ( 3 ) ( :lens ), ival, ier )
		    IF ( ier .eq. 0 ) idir = ival
		END IF
C
C*		Get the speed immediately preceding KT.  Check for a
C*		range of speeds.
C
		IF ( ikt .ne. 0 ) THEN
		    CALL ST_LSTR ( carr ( num ), lens, ier )
		    ipos = INDEX ( carr ( num ), '-' )
		    IF ( ipos .eq. 0 )
     +				ipos = INDEX ( carr ( num ), '/' )
	  	    ipos = ipos + 1
		    CALL ST_INTG ( carr ( num ) ( ipos:lens ),
     +				   ispd, ier )
		END IF
C
C*		Check for non-abbreviated direction
C*		(such as SOUTH-SOUTH EAST)
C 
		IF ( idir .eq. IMISSD ) THEN
		    work = report ( :iend-1 ) 
		    DO ip = iloc+4, iend-1
			IF ( work(ip:ip) .eq. '-' .or.
     +			        work(ip:ip) .eq. '.' ) work(ip:ip) = ' '
		    END DO 
		    CALL ST_CLST ( work ( iloc+4:iend-1 ), ' ', ' ', 10,
     +			           car2, num2, ier )
		    ie = 0
		    drct = ' '
		    ic = 1
		    icmx = min ( num2, 5 )
		    DO WHILE ( ic .le. icmx )
			id = 1
			DO WHILE ( id .le. 4 )
			    IF ( car2 ( ic ) .eq. dirc ( id ) ) THEN
				ie = ie + 1
				drct ( ie:ie ) = dirc ( id ) (1:1)
				id = 5
			      ELSE
				id = id + 1
			    END IF
			END DO
			IF ( ie. ge. 3 ) THEN
			    ic = 6
			  ELSE
			    ic = ic + 1
			END IF
		    END DO
		    IF ( ie .gt. 0 ) THEN
			ien = ie + 1
			drct (ien:ien) = CHNULL
			CALL CLO_CMPDIR ( drct, dir, ier )
			IF ( ier .eq. 0 ) idir = NINT ( dir )
		    END IF
		END IF 
	      ELSE
		iret = -7
	    END IF
	  ELSE
C
C*	    Check for the key word STNR.
C
	    iloc = INDEX ( report ( :len ), 'STNR' )
	    IF ( iloc .gt. 0 ) THEN
		idir = 0
		ispd = 0
		iend = iloc
	      ELSE
C
C*	        Check for the key word STATIONARY.
C
	        iloc = INDEX ( report ( :len ), 'STATIONARY' )
	        IF ( iloc .gt. 0 ) THEN
		    idir = 0
		    ispd = 0
		    iend = iloc
	          ELSE
       	            iret = -7
		END IF
	    END IF
	END IF
	IF ( iret .eq. 0 ) THEN
	    iptr = INDEX ( report ( iend:len ), ' ' ) + iend - 1
	END IF
C*
	RETURN
	END
