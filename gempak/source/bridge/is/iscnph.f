	SUBROUTINE IS_CNPH ( report, lenr, ibegin, ibegds, iptrin, npt,
     +			     rlat, rlon, stidnt, icorr, lunf, phen,
     +			     clat, clon, origin, irad, iret )
C************************************************************************
C* IS_CNPH								*
C*									*
C* This subroutine decodes the fields which may follow a Canadian TS	*
C* (thunderstorm), TB, (turbulence), CT (CAT), CB or MW report.		*
C*                                                                      *
C* IS_CNPH ( REPORT, LENR, IBEGIN, IBEGDS, IPTRIN, NPT, RLAT, RLON,	*
C*	     STIDNT, ICORR, LUNF, PHEN, CLAT, CLON, ORIGIN, IRAD,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IBEGDS		INTEGER		Pointer to location of dash	*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*      NPT             INTEGER         Number of lat/lon points        *
C*      RLAT (*)        REAL            Latitude array                  *
C*      RLON (*)        REAL            Longitude array                 *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR  		INTEGER		Correction flag                 *
C*	LUNF   		INTEGER		ASCII output file number        *
C*	PHEN		CHAR*		Phenomenon			*
C*	CLAT		REAL		Latitude of center              *
C*	CLON		REAL		Longitude of center             *
C*	ORIGIN		CHAR*		Originating station id.		*
C*	IRAD		INTEGER		Radius if area is a circle      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	10/03	Created from IS_EGTM.			*
C* F. J. Yen/NCEP	12/03	Cleaned up.				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, phen, origin
	REAL   		rlat (*), rlon (*)
C*
	INTEGER		iflvl (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get the observed / forecast indicator.
C
	ibeg   = ibegin + iptrin - 1
	lens   = lenr - ibeg + 1
	istart = ibeg
	istard = ibegds + 1
	CALL IS_OBFC ( report ( ibeg:lenr ), lens, iobfc, iptr, iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	END IF
C
C*	If area is a circle, get the center.
C
	IF ( ( irad .ne. IMISSD ) .and. ( ERMISS ( rlat ( 1 ) ) ) .and.
     +	     ( ERMISS ( rlon ( 1 ) ) ) ) THEN
	    npt = 1
	    rlat ( 1 ) = clat
	    rlon ( 1 ) = clon
	END IF
C
C*	Get the flight level.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_FLVL ( report ( ibeg:lenr ), lens, iflflg, iflvl, iptr, 
     +		       iret )
C
C*      Can't find flight level, look much earlier in the report.
C*	Some phenomena such as TS which has FC, Water Spout sometimes
C*	have flight level before the phenomenon TS.
C
	IF ( iret .lt. 0 ) THEN
	    ibegfl = istard
	    ibeg = ibegfl
	    lens = lenr - istard + 1
	    CALL IS_FLVL ( report ( ibegfl:lenr ), lens, iflflg, iflvl, 
     +		           iptr, iret )
	    IF ( iret .lt. 0 ) iptr = 1
	END IF
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_MOV ( report ( ibeg:lenr ), lens, 90, idir, ispd, iptr, 
     +		      iret )
C
C*      Can't find direction and speed, look earlier in the report.
C
	IF ( iret .lt. 0 ) THEN
            CALL ST_LSTR ( report, lens, ier )
	    CALL IS_MOV ( report, lens, 400, idir, ispd, iptr, iret )
	    IF ( iret .lt. 0 ) iptr = 1
	END IF
C
C*	Get the change in intensity.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_CHNG ( report ( ibeg:lenr ), lens, ichng, iptr, iret )
C
C*      Can't find the change in intensity, look earlier in the report.
C
	IF ( iret .lt. 0 ) THEN
	    ibeg = istart
	    lens = lenr - ibeg + 1
	    CALL IS_CHNG ( report ( ibeg:lenr ), lens, ichng, iptr,
     +                     iret )
        END IF 
C
C*	Write out the phenomenon data.
C
	CALL IS_OUT ( lunf, phen, stidnt, origin, icorr,
     +		      iflflg, iflvl, idir, ispd, ' ', irad,
     +		      rlat, rlon, npt, iret )
C*
	RETURN
	END
