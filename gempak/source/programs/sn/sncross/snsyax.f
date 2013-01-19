	SUBROUTINE SNSYAX  ( ptype, vcoord, yaxis, iytype, ratio, 
     +			     rmargn, ybot, ytop, ivcord, vcord, yaxval, 
     +			     nyval, iylbfr, iyglfr, iytmfr, iret )
C************************************************************************
C* SNSYAX								*
C*									*
C* This subroutine defines the y-axis for the cross section program.	*
C* If user entered LIN, LOG or STUVE, linear, logarithmic, or kappa 	*
C* scaling will be used.  The default is logarithmic scaling.		*
C*									*
C* SNSYAX  ( PTYPE, VCOORD, YAXIS, IYTYPE, RATIO, RMARGN, YBOT, YTOP,	*
C*           IVCORD, VCORD, YAXVAL, NYVAL, IYLBFR, IYGLFR, IYTMFR,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*	PTYPE		CHAR*		Plot type			*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	YAXIS		CHAR*		Ybot/ytop/yinc (3d n/a)		*
C*									*
C* Output parameters:							*
C*	IYTYPE		INTEGER		Y axis type			*
C*					  1 = LIN (pressure)		*
C*					  2 = LOG (log pressure)	*
C*					  3 = STUVE (press ** KAPPA)	*
C* 	RATIO		REAL		Height to width ratio		*
C*	RMARGN (4)	REAL		Margins				*
C*	YBOT		REAL		Bottom pressure in millibars	*
C*	YTOP		REAL		Top pressure in millibars	*
C*	IVCORD		INTEGER		Numeric vertical coordinate	*
C*	VCORD		CHAR*		Vertical coordinate		*
C*	YAXVAL (NYVAL)	REAL		Y axis values			*
C*	NYVAL		INTEGER		Number of yaxis values 		*
C*	IYLBFR		INTEGER		Label frequency			*
C*	IYGLFR		INTEGER		Grid line frequency		*
C*	IYTMFR		INTEGER		Tick mark frequency		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-11 = Invalid PTYPE		*
C*					-12 = Invalid YAXIS		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* G. Huffman/GSC	11/88	GEMPAK4.1 globals, documentation	*
C* S. Schotz/GSC	 7/90	Add IN_PTYP; pass out margin, ratio	*
C* S. Schotz/GSC	 7/90	Add call to IN_AXIS, return yaxis att	*
C* M. desJardins/GSFC	 9/90	Add VCOORD input			*
C* K. Brill/NMC		02/92	Check for valid ivcord from LV_CORD	*
C* S. Jacobs/NMC	 4/94	Add call to ST_LCUC before LV_CORD	*
C* T. Piper/SAIC	 4/02	Fixed UMR; added GEMPRM.PRM for RMISSD	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	ptype, yaxis, vcoord, vcord
C*
	REAL		rmargn (*), yaxval (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get values from PTYPE.
C
	CALL IN_PTYP  ( ptype, iytype, ratio, rmargn, ier )
	IF  ( ( ier .ne. 0 ) .or. ( iytype .eq. 4 ) )  THEN
            iret = -11
	    CALL ER_WMSG  ( 'SNCROSS', iret, ptype, ier )
	    RETURN
        END IF
C
C*	Get vertical coordinate.
C
	CALL ST_LCUC  ( vcoord, vcord, ier )
	CALL LV_CORD  ( vcord, vcord, ivcord, ier )
	IF  ( ier .ne. 0 .or. 
     +	      ivcord .lt. 0 .or. ivcord .gt. 5 )  THEN
	    iret = -11
	    CALL ER_WMSG  ( 'LV', ier, vcoord, ier2 )
	    RETURN
	END IF
C
C*	Break out the arguments	in YAXIS.
C
	CALL IN_AXIS  ( yaxis, ivcord, .false., ' ', RMISSD, RMISSD, 
     +			1, 0, 1, ybot, ytop, yaxval, nyval, iylbfr, 
     +			iyglfr, iytmfr, iret )
        IF  ( iret .ne. 0 )  THEN
	    iret = -12
	    CALL ER_WMSG  ( 'SNCROSS', iret, yaxis, ier )
	END IF
C*
	RETURN
	END
