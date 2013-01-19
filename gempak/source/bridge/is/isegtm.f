	SUBROUTINE IS_EGTM ( report, lenr, ibegin, iptrin, mxp, stidnt,
     +			     icorr, lunf, phenom, ifeggy, origin, iret )
C************************************************************************
C* IS_EGTM 								*
C*									*
C* This subroutine decodes the fields which may follow an EGGY TB     	*
C* (turbulence) or MW (marked mountain wave) report.			*
C*                                                                      *
C* IS_EGTM ( REPORT, LENR, IBEGIN, IPTRIN, MXP, STIDNT, ICORR, LUNF,    *
C*	     PHENOM, IFEGGY, ORIGIN, IRET )                             *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*	MXP   		INTEGER		Maximum number of points        *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR		INTEGER		Correction flag                 *
C*	LUNF		INTEGER		ASCII output file number        *
C*	PHENOM		CHAR*		Phenomenon ('TB' OR 'MW')	*
C*      IFEGGY          INTEGER         Country ID:  1 if EGGY; 2 if    *
C*                                      RJAA; 3 if NTAA                 *
C*	ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/00	Renamed isegtb and added mountain waves.*
C* F. J. Yen/NCEP        8/00   Added parameter ifeggy; Changed calling *
C*                              sequence for IS_EGAR.                   *
C* A. Hardy/NCEP	 9/02	Added origin parameter			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, phenom, origin
	CHARACTER*3     kdir
C* 
	INTEGER		iflvl (2)
	REAL   		rlat (20), rlon (20)
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
C*	Get the flight level.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_FLVL ( report ( ibeg:lenr ), lens, iflflg, iflvl, iptr,
     +		       iret )
C
C*	Set the pointer back to one in case area precedes flight level.
C
	iptr = 1
C
C*	Get the area covered by the phenomenon.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_EGAR ( report ( ibeg:lenr ), lens, mxp, ifeggy, npt, 
     +		       rlat, rlon, irad, iptr, iret )
        IF ( iret .lt. 0 ) THEN
C
C*	    Check for and get open area
C
            CALL IS_EGAO ( report ( ibeg:lenr ), lens, mxp, npt, rlat,
     +                 rlon, irad, iptr, kdir, iret )
            IF ( iret .lt. 0 ) THEN
                RETURN
            END IF
          ELSE
            kdir = ' '
        END IF
C
C*	Get the direction and speed of movement.
C
	iptr = 1
	ibeg = 1
	lens = lenr
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
C*	Write out the turbulence or mountain wave data.
C
	CALL IS_OUT ( lunf, phenom, stidnt, origin, icorr,
     +		      iflflg, iflvl, idir, ispd, kdir, irad, rlat,
     +		      rlon, npt, iret )
C*
	RETURN
	END
