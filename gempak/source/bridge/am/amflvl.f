	SUBROUTINE AM_FLVL ( string, flvl, iptr, iret )
C************************************************************************
C* AM_FLVL  								*
C*									*
C* This subroutine gets the flight level(s) for an airmet report.       *
C*                                                                      *
C* AM_FLVL ( STRING, FLVL, IPTR, IRET )                                 *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*									*
C* Output parameters:							*
C*	FLVL(2)		CHAR*		Flight level(s)                 *
C*	IPTR		INTEGER		Pointer following last flt level*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/00	                                        *
C* D. Kidwell/NCEP	11/00	Added BLO for below                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, flvl (*)
C*
	CHARACTER	carr (60)*10, clast*10
C------------------------------------------------------------------------
	iret       = 0
	flvl ( 1 ) = ' '
	flvl ( 2 ) = ' '
	iptr       = 0
C
C*	Look for the key word 'BTN', 'BTW', 'BLW', 'BLO' or 'ABV' and
C*	determine how many levels to look for.
C
	CALL ST_CLST ( string, ' ', ' ', 60, carr, numc, ier )
	CALL ST_FIND ( 'BTN', carr, numc, ibtn, ier )
	IF ( ibtn .eq. 0 ) CALL ST_FIND ( 'BTW', carr, numc, ibtn, ier )
	CALL ST_FIND ( 'BLW', carr, numc, iblw, ier )
	IF ( iblw .eq. 0 ) CALL ST_FIND ( 'BLO', carr, numc, iblw, ier )
	CALL ST_FIND ( 'ABV', carr, numc, iabv, ier )
	IF ( ( ibtn .eq. 0 ) .and. ( iblw .eq. 0 ) .and.
     +	     ( iabv .eq. 0 ) ) THEN
	    RETURN
	  ELSE
	    IF ( ibtn .eq. 0 ) ibtn = 999
	    IF ( iblw .eq. 0 ) iblw = 999
	    IF ( iabv .eq. 0 ) iabv = 999
	    ipos = MIN ( iblw, ibtn, iabv )
	    IF ( ( ipos .eq. ibtn ) .or. ( ipos .eq. iabv ) ) THEN
	        numlvl = 2
		inum   = 1
		IF ( ( ipos .eq. iabv ) .and. 
     +		     ( carr ( ipos + 2 ) .ne. 'TO' ) ) numlvl = 1
	      ELSE
	        numlvl = 1
		inum   = 2   
	    END IF
	END IF
C
C*	Get the levels.
C
	ipos  = ipos + 1
	DO ii = ipos, ipos + numlvl, 2
	    CALL ST_LSTR ( carr ( ii ), lens, ier )
	    lastln = lens
	    clast  = carr ( ii )
	    IF ( ( carr ( ii ) ( :2 ) .eq. 'FL' ) .and.
     +		 ( lens .gt. 2 ) ) THEN
     		carr ( ii ) = carr ( ii ) ( 3: lens )
		lens = lens - 2
	    END IF
	    IF ( carr ( ii ) ( lens:lens ) .eq. '.' ) lens = lens - 1
	    IF ( carr ( ii ) ( :lens ) .ne. 'FRZLVL' ) THEN
		flvl ( inum ) = carr ( ii ) ( :lens )
		IF ( lens .lt. 3 ) flvl ( inum ) = '0' // flvl ( inum )
		IF ( lens .lt. 2 ) flvl ( inum ) = '0' // flvl ( inum )
	      ELSE
		flvl ( inum ) = 'FRZL'
	    END IF
	    inum = inum + 1
	END DO
C
	CALL ST_LSTR ( string, lens, ier )
	iptr = INDEX ( string ( :lens ), clast ( :lastln ) ) + lastln
C*
	RETURN
	END
