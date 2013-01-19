	SUBROUTINE PC_CMVR  ( vlev, ivcord, datain, outdat, chrdat,
     +			      iret )
C************************************************************************
C* PC_CMVR								*
C*									*
C* This subroutine computes level parameters at a given vertical	*
C* level in the coordinate system specified by IVCORD.  If VLEV		*
C* is not in the data set the data will be interpolated.  The		*
C* output parameters must be defined by a call to PC_DFLV before	*
C* this subroutine is called.						*
C*									*
C* PC_CMVR ( VLEV, IVCORD, DATAIN, OUTDAT, CHRDAT, IRET )		*
C*									*
C* Input parameters:							*
C*	VLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER		Vertical coordinate of VLEV	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	OUTDAT (NOUTPM)	REAL		Computed real data		*
C*	CHRDAT (NOUTPM)	CHAR*		Computed character data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT must be called	*
C*					 -6 = PC_SSTN must be called	*
C*					 -7 = PC_DFLV must be called	*
C*					-10 = vert cord of dset is 0	*
C*					-13 = no comp for ivcord	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	11/89	Added check for conditions		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
	CHARACTER*(*)	chrdat (*)
C*
	INCLUDE		'ERMISS.FNC'
C*
C-------------------------------------------------------------------------
C*	Check for error in the input values
C
	index = 1
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF  ( .not. tstnfl )  THEN
	    iret = -6
	  ELSE IF  ( .not. tabflg (index) )  THEN
	    iret = -7
	  ELSE IF  ( (jcord .eq. 0) .and. (vlev .ne. 0.0) )  THEN
	    iret = -10
	  ELSE
	    iret = 0
	ENDIF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize output data arrays
C
	CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
C
C*	Get station elevation from height field if not in common.
C
	IF  ( ( (telv .eq. 0.) .or. ERMISS (telv) ) .and.
     +		(jhght .ne. 0) ) telv = datain ( jhght )
C
C*	Set the indices for the right tables and call PC_CMDT
C*	to compute the data.
C
	idx1 = 1
	idx2 = 2
	idx3 = 3
	CALL PC_CMDT  ( idx1, idx2, idx3, vlev, ivcord, datain,
     +							outdat, iret )
	IF  ( iret .lt. 0 )  RETURN
C
C*	Translate character data
C
	CALL PT_CNVR  ( outdat, koutpm(1), qcmp (1), qchr (1), chrfnc,
     +			chrdat, ier )
C
C*	Check conditions if they have been set.  If not, set data to
C*	missing.
C
	IF ( levcnd )  CALL PC_CLCD ( koutpm (1), outdat, chrdat, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
	END IF
C*
	RETURN
	END
