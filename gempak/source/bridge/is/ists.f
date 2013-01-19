	SUBROUTINE IS_TS ( report, lenr, ibegin, iptrin, mxp, stidnt,
     +		           icorr, lunf, phen, clat, clon, origin, iret )
C************************************************************************
C* IS_TS 								*
C*									*
C* This subroutine decodes the fields which may follow a TS             *
C* (thunderstorm) or CT (CAT) report or CB report.			*
C*                                                                      *
C* IS_TS ( REPORT, LENR, IBEGIN, IPTRIN, MXP, STIDNT, ICORR, LUNF,      *
C* 	   PHEN, CLAT, CLON, ORIGIN, IRET )                             *
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
C*	PHEN		CHAR*		Phenomenon			*
C*	CLAT		REAL		Latitude of center              *
C*	CLON		REAL		Longitude of center             *
C*	ORIGIN		CHAR*		Originating station id.		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Added center values to input arg. list  *
C* D. Kidwell/NCEP	11/99	Added check to see if center defined    *
C* F. J. Yen/NCEP	 4/00	Updated for new parameter in IS_MOV	*
C* A. Hardy/GSC          7/00   Added checks for info. earlier in rpt.  *
C* D. Kidwell/NCEP	10/00	Corrected checks for info earlier in rpt*
C* A. Hardy/SAIC	 9/01   Added early search for dir and spd      *
C* F. J. Yen/NCEP	11/01	Added phenomenon CT and parameter PHEN.	*
C* A. Hardy/NCEP 	 9/02   Added origin variable			*
C* F. J. Yen/NCEP	 9/03	Added phenomenon CB to prolog.  Updated	*
C*				calling sequence to IS_AREA with origin.*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, phen, origin
C*
	INTEGER		iflvl (2)
	REAL   		rlat (20), rlon (20)
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
	CALL IS_OBFC ( report ( ibeg:lenr ), lens, iobfc, iptr, iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	END IF
C
C*	Get the area covered by the phenomenon.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_AREA ( report ( ibeg:lenr ), lens, mxp, origin, npt,
     +		       rlat, rlon, irad, iptr, iret )
	IF ( iret .lt. 0 ) RETURN
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
C*      Can't find flight level, look earlier in the report.
C
	IF ( iret .lt. 0 ) THEN
	    ibeg = istart
	    lens = lenr - ibeg + 1
	    CALL IS_FLVL ( report ( ibeg:lenr ), lens, iflflg, iflvl, 
     +		           iptr, iret )
	    IF ( iret .lt. 0 ) iptr = 1
	END IF
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_MOV ( report ( ibeg:lenr ), lens, 40, idir, ispd, iptr, 
     +		      iret )
C
C*      Can't find direction and speed, look earlier in the report.
C
	IF ( iret .lt. 0 ) THEN
            CALL ST_LSTR ( report, lens, ier )
	    CALL IS_MOV ( report, lens, 300, idir, ispd, iptr, iret )
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
