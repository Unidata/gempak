	SUBROUTINE PC_SSTN  ( stid, isnum, slat, slon, selv, ispri, 
     +			      ihhmm, nlev, iret )
C************************************************************************
C* PC_SSTN								*
C*									*
C* This subroutine saves the station information required for the PC	*
C* package.  								*
C*									*
C* PC_SSTN  ( STID, ISNUM, SLAT, SLON, SELV, ISPRI, IHHMM, NLEV, IRET )	*
C*									*
C* Input parameters:							*
C*	STID		CHAR*4		Station identifier		*
C*	ISNUM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority number         *
C*	IHHMM		INTEGER		Station hour and minute		*
C*	NLEV		INTEGER		Number of vertical levels	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C*				 	  -3 = invalid NLEV		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleanup up				*
C* M. desJardins/GSFC	11/89	Change from GEMPAK stntim to ihhmm	*
C* A. Hardy/GSC		 3/99	Added priority parameter ispri		*
C* A. Hardy/GSC		 3/99	Removed ispri = 0                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	stid
C------------------------------------------------------------------------
C*	Save all input information in common.
C
	kpnt   = 0
	vtbflg = .false.
	IF  ( ( nlev .le. 0 ) .or. ( nlev .gt. MAXLEV ) )  THEN
	    iret    = -3
	    tstnfl  = .false.
	  ELSE
	    tstnfl  = .true.
	    tstid   = stid
	    jsnum   = isnum
	    tlat    = slat
	    tlon    = slon
	    telv    = selv
	    jspri   = ispri
	    ithhmm  = ihhmm
	    jnumlv  = nlev
	    iret    = 0
	END IF
C*
	RETURN
	END
