	SUBROUTINE IS_VA ( report, lenr, ibegin, iptrin, mxp, stidnt,
     +		           icorr, lunf, origin, iret )
C************************************************************************
C* IS_VA 								*
C*									*
C* This subroutine decodes the fields which may follow a VA             *
C* (volcanic ash) report.                                               *
C*                                                                      *
C* IS_VA ( REPORT, LENR, IBEGIN, IPTRIN, MXP, STIDNT, ICORR, LUNF,	*
C*         IRET )                                           		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*	MXP   		INTEGER		Maximum number of points        *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR  		INTEGER		Correction flag                 *
C*	LUNF   		INTEGER		ASCII output file number        *
C*	ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/00	Created based on IS_TS                  *
C* F. J. Yen/NCEP	 5/00	Changed delimiter for location to ','   *
C* A. Hardy/NCEP	 9/02	Added origin parameter			*
C* F. J. Yen/NCEP	 9/03	Updated calling sequence to IS_AREA.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, origin
C*
	INTEGER		iflvl (2)
	REAL   		rlat (20), rlon (20)
C*
	CHARACTER	aloc*18, alat*9, alon*9
	REAL   		vlat, vlon
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
C*	Get the location of the volcano.
C
	keywd = 1
	CALL IS_VLOC ( report( ibeg:lenr), lens, keywd, mxp, iptr, vlat,
     +		vlon, ipkey, iret) 
	IF ( iret .ne. 0 ) THEN
	    keywd = 2
	    lensav = lens
	    lens = lenr
	    CALL IS_VLOC ( report(1:lenr), lens, keywd, mxp, iptr,
     +		    vlat, vlon, ipkey, iret) 
	    lens = lensav
	END IF
	IF ( ERMISS ( vlat ) .or. ERMISS ( vlon ) ) THEN
	    aloc = ' '
	  ELSE
	    WRITE ( aloc, 10 ) vlat, vlon
   10	    FORMAT ( 2F8.2 )
	    iptr = 1
	    CALL ST_RLCH ( vlat, 2, alat, ier )
	    CALL ST_LSTR ( alat, lenlat, ier )
	    CALL ST_RLCH ( vlon, 2, alon, ier )
	    CALL ST_LSTR ( alon, lenlon, ier )
	    aloc = alat ( :lenlat ) // ',' // alon ( :lenlon)
	END IF
C
C*	Get the area covered by the phenomenon.
C
	IF ( ipkey .ge. 0 ) THEN
	    ibeg = ibeg + ipkey
	  ELSE
	    ibeg = ibeg + iptr - 1
	END IF
	lens = lenr - ibeg + 1
	CALL IS_AREA ( report ( ibeg:lenr ), lens, mxp, origin, npt,
     +		       rlat, rlon, irad, iptr, iret )
   	ibegar = ibeg
   	iptrar = iptr
	IF ( iret .lt. 0 ) RETURN
C
C*	Set the pointer back to one in case flight level precedes area
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
   	    ibeg = ibegar
   	    iptr = iptrar
	END IF
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_MOV ( report ( ibeg:lenr ), lens, 110, idir, ispd, iptr, 
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
C*	Write out the volcanic ash data.
C
	CALL IS_OUT ( lunf, 'VA', stidnt, origin, icorr,
     +		      iflflg, iflvl, idir, ispd, aloc, irad, rlat,
     +		      rlon, npt, iret )
C*
	RETURN
	END
