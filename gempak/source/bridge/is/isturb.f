	SUBROUTINE IS_TURB ( report, lenr, ibegin, iptrin, mxp, stidnt,
     +			     icorr, lunf, phen, origin, iret )
C************************************************************************
C* IS_TURB 								*
C*									*
C* This subroutine decodes the fields which may follow a TB		*
C* (turbulence), IC (icing), SQ (squall), or MW (mountain wave) report. *
C*                                                                      *
C* IS_TURB ( REPORT, LENR, IBEGIN, IPTRIN, MXP, STIDNT, ICORR, LUNF,    *
C*	     PHEN, ORIGIN, IRET )                                       *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*	MXP   		INTEGER		Maximum number of points        *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR		INTEGER		Correction flag                 *
C*	PHEN		CHAR*		Phenomenon			*
C*	LUNF		INTEGER		ASCII output file number        *
C*	ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	11/99	Reset pointer following flight level    *
C* F. J. Yen/NCEP	 4/00	Updated for new paramter in IS_MOV	*
C* A. Hardy/NCEP	 9/02	Added origin parameter			*
C* F. J. Yen/NCEP	 9/03	Added check for FL earlier in report.	*
C*				Updated calling sequence to IS_AREA.	*
C* F. J. Yen/NCEP	 2/04	Added parameter PHEN to handle IC, SQ,	*	
C*				and MW.	 (CSC)				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, stidnt, phen, origin
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
	istart = ibeg
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
C*	Can't find flight level, look earlier in the report.
C
	IF ( iret .lt. 0 ) THEN
	    ibeg = istart
	    lens = lenr - ibeg + 1
	    CALL IS_FLVL ( report ( ibeg:lenr ), lens, iflflg, iflvl,
     +                     iptr, iret )
	    IF ( iret .lt. 0 ) iptr = 1
	END IF
C
C*	Set the pointer back to one in case area precedes flight level.
C
	iptr = 1
C
C*	Get the area covered by the phenomenon.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_AREA ( report ( ibeg:lenr ), lens, mxp, origin, npt,
     +		       rlat, rlon, irad, iptr, iret )
	IF ( iret .lt. 0 ) RETURN
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_MOV ( report ( ibeg:lenr ), lens, 40, idir, ispd, iptr, 
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
C*	Write out the phenomenon data.
C
	CALL IS_OUT ( lunf, phen, stidnt, origin, icorr,
     +		      iflflg, iflvl, idir, ispd, ' ', irad, rlat,
     +		      rlon, npt, iret )
C*
	RETURN
	END
