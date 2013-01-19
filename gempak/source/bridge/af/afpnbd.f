	SUBROUTINE AF_PNBD  ( isnbd, ienbd, pnbdlt, pnbdln, iret )
C************************************************************************
C* AF_PNBD								*
C*									*
C* This subroutine decodes a location point (i.e. a "location point" is	*
C* either a navaid or a navaid/bearing/distance) from within the	*
C* location data of a PIREP report.					*
C*									*
C* AF_PNBD ( ISNBD, IENBD, PNBDLT, PNBDLN, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNBD		INTEGER		Index of "like-type" group which*
C*					contains start of location point*
C*									*
C* Output parameters:							*
C*	IENBD		INTEGER		Index of "like-type" group which*
C*					contains end of location point	*
C*	PNBDLT		REAL		Latitude of location point 	*
C*	PNBDLN		REAL		Longitude of location point 	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = error during decoding	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Allow bearing to be 360 degrees 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/97	AF_BSRC -> DC_BSRC			*
C* A. Hardy/GSC         05/98   Restructed, other format reports allowed*
C* A. Hardy/GSC         05/98   Cleaned up code                         *
C* D. Kidwell/NCEP	10/00   Improved error checking; cleaned up     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	LOGICAL         offset, other, bad
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret   = -1
	offset = .false.
	other  = .false.
	bad    = .false.
C
C*	The "like-type" group should be a 3-letter or 4-letter
C*	navaid identifier.
C
	ii = isnbd
        IF ( ( ( itypsf ( ii ) .eq. ALPHA ) .and.
     +     ( ( lensf ( ii ) .eq. 3 ) .or. ( lensf ( ii ) .eq. 4 ) ) ) 
     +                         .or. 
     +     ( ( itypsf ( ii ) .eq. NMR ) .and. 
     +       ( lensf ( ii ) .le. 2 ) ) ) THEN
C
C*          Setting conditions if the navaid came after the reported 
C*          distance and range.
C
	    IF ( itypsf ( ii ) .eq. NMR) THEN
		IF ( ( ii + 2 ) .le. nflds ) THEN
		    ii    = ii + 2
                    ienbd = ii
		    other = .true.
		    IF ( lensf ( ii ) .le. 2 ) bad = .true.
		  ELSE
		    bad = .true.
		END IF
            END IF
C
C*	    Locate the entry for this navaid in the PIREP navaids table
C*	    by keying on the last three letters of the identifier.
C
	   
	    IF ( .not. bad ) THEN
	        CALL DC_BSRC
     +	        ( fields ( ii ) ( ( lensf ( ii ) - 2 ) : lensf ( ii ) ),
     +		  pdnvid, npde, jj, ierfnd )
	        IF ( jj .eq. 0 )  THEN
		    logmsg = 'Could not find '  //
     +		             fields ( ii ) ( 1 : lensf ( ii ) )  //
     +		             ' in PIREP navaids table'
		    CALL DC_WLOG  ( 2, 'AF', 1, logmsg, ierwlg )
		    RETURN
	        END IF 
	    END IF
          ELSE 
	    bad = .true.
	END IF
C
	IF ( bad ) THEN
	    logmsg = 'navaid '  //
     +		     fields ( ii ) ( 1 : lensf ( ii ) )
	    CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
	    RETURN
	END IF
C
C*      Looking for the bearing and distance offset, if there is one.
C
	IF  ( ( .not. other ) .and. ( ( ii + 1 ) .le. nflds ) .and.
     +	      ( itypsf ( ii + 1 ) .eq. NMR ) .and.
     +	      ( lensf ( ii + 1 ) .eq. 6 )  )  THEN
	    ienbd = ii + 1
	    CALL ST_INTG  ( fields ( ii + 1 ) (1:3), ibdg, ier1 )
	    CALL ST_INTG  ( fields ( ii + 1 ) (4:6), idnm, ier2 )
	    offset = .true.
	    IF ( ( ibdg .lt. 0 ) .or. ( ibdg .gt. 360 ) .or.
     +		 ( idnm .le. 0 ) )  THEN
		logmsg = 'bearing/distance '  //
     +		         fields ( ii + 1 ) ( 1 : lensf ( ii + 1 ) )
		CALL DC_WLOG  ( 2, 'AF', 2, logmsg, ierwlg )
		RETURN
	    END IF 
          ELSE IF ( other ) THEN
C
C*          Getting bearing and distance of aircraft.
C
	    CALL AF_CPAS ( ii-1, ibdg, iercps )
	    CALL ST_INTG ( fields ( ii - 2 ) 
     +		           ( 1:lensf ( ii - 2 ) ), idnm, ier2 )
	    offset = .true.
	END IF
        IF ( offset ) THEN
C
C*	    Convert distance from nautical miles to kilometers.
C
	    dkm = PR_HGMK ( PR_HGNM ( FLOAT ( idnm ) ) )
C
C*	    Convert bearing from degrees to radians.
C
	    IF ( ibdg .ne. IMISSD ) THEN
	        brd = FLOAT ( ibdg ) * DTR
	      ELSE
		brd = RMISSD
	    END IF
C
C*	    Compute the latitude and longitude of the location
C*	    point by applying the bearing and distance offsets
C*	    to the navaid latitude and longitude.
C
	    CALL PR_RZLL  ( pdlat ( jj ), pdlon ( jj ), dkm, brd,
     +			    0, pnbdlt, pnbdln, ierzll )
	    IF ( ( ERMISS ( pnbdlt ) ) .or.
     +	    	 ( ERMISS ( pnbdln ) ) )  THEN
	 	logmsg = 'PR_RZLL'
		CALL DC_WLOG  ( 2, 'AF', 4, logmsg, ierwlg )
		RETURN
	    END IF
          ELSE
C
C*	    The location point is simply the navaid itself, so the
C*	    latitude and longitude of the location point are equal
C*	    to the latitude and longitude of the navaid.
C
	    ienbd  = ii
	    pnbdlt = pdlat ( jj )
	    pnbdln = pdlon ( jj )
	END IF
C
	iret = 0
C*
	RETURN
	END
