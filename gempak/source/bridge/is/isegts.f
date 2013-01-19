	SUBROUTINE IS_EGTS ( report, lenr, ibegin, iptrin, mxp,
     +		             stidnt, icorr, lunf, phen, clat, clon,
     +			     multar, jflflg, jflvl, ifeggy, origin, 
     +			     iret )
C************************************************************************
C* IS_EGTS 								*
C*									*
C* This subroutine decodes the fields which may follow an EGGY TS       *
C* (thunderstorm) or CB (cumulonimbus) report.                          *
C*                                                                      *
C* IS_EGTS ( REPORT, LENR, IBEGIN, IPTRIN, MXP, STIDNT, ICORR, LUNF,	*
C* 	     PHEN, CLAT, CLON, MULTAR, JFLFLG, JFLVL, IFEGGY, ORIGIN, 	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin or phenomenon*
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*	MXP   		INTEGER		Maximum number of points        *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR  		INTEGER		Correction flag                 *
C*	LUNF   		INTEGER		ASCII output file number        *
C*	PHEN		CHAR*		Phenomenon			*
C*	CLAT		REAL		Latitude of center              *
C*	CLON		REAL		Longitude of center             *
C*	MULTAR		LOGICAL		Multi-area flag for NTAA	*
C*	JFLFLG		INTEGER		Flight flag for multi-area NTAA	*
C*	JFLVL(*)	INTEGER		Flight levls for multi-area NTAA*
C*	IFEGGY		INTEGER		Country ID:  1 if EGGY; 2 if	*
C*					RJAA; 3 if NTAA; 4 if MUHA	*
C* 	ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 1/00	Converted IS_TS for EGGY.		*
C* F. J. Yen/NCEP	 3/00	Expanded for EGGY by invoking IS_EGAO.	*
C* F. J. Yen/NCEP	 8/00	Added parameter ifeggy; Changed calling	*
C*				sequence for IS_EGAR.			*
C* F. J. Yen/NCEP	11/01	Added decoding of cumulonimbus reports	*
C*				and added parameter PHEN.		*
C* F. J. Yen/NCEP	 6/02	Added multi-area reports for NTAA.	*
C* A. Hardy/NCEP	 9/02	Added origin parameter			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, phen, origin
	INTEGER		jflvl (*)
	LOGICAL		multar
C*
	CHARACTER	kdir*3
	INTEGER		iflvl (2)
	REAL   		rlat (20), rlon (20)
	LOGICAL		more
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get the observed / forecast indicator.
C
	ibeg = ibegin + iptrin - 1
	lens = lenr - ibeg + 1
	CALL IS_OBFC ( report ( ibeg:lenr ), lens, iobfc, iptr, iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	END IF
C
C*	Get the area covered by the phenomenon.
C
	ibeg = ibeg + iptr - 3
	lens = lenr - ibeg + 1
	CALL IS_EGAR ( report ( ibeg:lenr ), lens, mxp, ifeggy, npt,
     +		       rlat, rlon, irad, iptr, iret )
	IF ( iret .lt. 0 ) THEN
C
C*	    Check for and get open area
C
	    CALL IS_EGAO ( report ( ibeg:lenr ), lens, mxp, npt, rlat, 
     +		       rlon, irad, iptr, kdir, iret )
	    IF ( iret .lt. 0 ) THEN
		RETURN
	    END IF
	  ELSE
	    kdir = ' '
	END IF
C
C*	If area is a circle, get the center.
C
	IF ( ( irad .ne. IMISSD ) .and. ( ERMISS ( rlat ( 1 ) ) )
     +	     .or. ( ERMISS ( rlon ( 1 ) ) ) ) THEN
	    
	    IF ( ( .not. ERMISS ( clat) ) .and.
     +		    ( .not. ERMISS (clon) ) ) THEN
	        npt = 1
		rlat ( 1 ) = clat
		rlon ( 1 ) = clon
	      ELSE
		iret = -4
		iptr = 1
		RETURN
	    END IF
	END IF
C	
C	Set the pointer back to one in case flight level precedes area
C
   	iptr = 1
C
C*	Get the flight level.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_FLVL ( report ( ibeg:lenr ), lens, iflflg, iflvl, iptr, 
     +		       iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	END IF
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_EGMV ( report ( ibeg:lenr ), lens, idir, ispd, iptr, 
     +		      iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	END IF
C
C*	Get the change in intensity.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_CHNG ( report ( ibeg:lenr ), lens, ichng, iptr, iret )
C
C*	For NTAA, check if multi-areas.
C
	more = .true.
	IF ( ifeggy .eq. 3 ) THEN
	    IF ( iflflg .eq. IMISSD .and. multar ) THEN
	        iflflg = jflflg
	        iflvl (1) = jflvl (1)
	        iflvl (2) = jflvl (2)
	    END IF
	END IF
C
C*	Write out the thunderstorm or cumulonimbus data.
C
	CALL IS_OUT ( lunf, phen, stidnt, origin, icorr,
     +		        iflflg, iflvl, idir, ispd, kdir, irad, rlat,
     +		        rlon, npt, iret )
C*
	RETURN
	END
