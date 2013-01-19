	SUBROUTINE PC_CMLV ( levnum, datain, outdat, chrdat, iret )
C************************************************************************
C* PC_CMLV								*
C*									*
C* This subroutine computes level parameters for a particular		*
C* data set level specified by the level number.  Only level data are	*
C* computed.  The output parameters must be defined by a call to	*
C* PC_DFLV before this subroutine is called.				*
C*									*
C* PC_CMLV ( LEVNUM, DATAIN, OUTDAT, CHRDAT, IRET )			*
C*									*
C* Input parameters:							*
C*	LEVNUM		INTEGER		Level number			*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	OUTDAT (NOUTPM)	REAL		Computed real data		*
C*	CHRDAT (NOUTPM)	CHAR*		Computed character data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT not called	*
C*					 -6 = PC_SSTN not called	*
C*					 -7 = output parms not set	*
C*					 -8 = invalid level number	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	11/89	Added check for conditions		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
	CHARACTER*(*)	chrdat (*)
C*
	REAL		datlev (MAXPRM)
C*
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
C*	Check for error in the input values.
C
	index = 1
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF  ( .not. tstnfl )  THEN
	    iret = -6
	  ELSE IF  ( .not. tabflg (index) )  THEN
	    iret = -7
	  ELSE IF ( ( levnum .lt. 1) .or. ( levnum .gt. jnumlv ) ) THEN
	    CALL PC_INID ( outdat, chrdat, koutpm (1), ier )
	    iret = -8
	  ELSE
	    iret = 0
	ENDIF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Initialize the output data arrays
C
	CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
C
C*	Get station elevation from height field if not already in common.
C
	IF  ( ( (telv .eq. 0.) .or. ERMISS (telv) ) .and.
     +		(jhght .ne. 0) ) telv = datain ( jhght )
C
C*	Pull out the requested level of data from the input dataset
C
	CALL PC_GLEV  ( levnum, datain, jdsprm, datlev, ier )
C
C*	Compute required output parameters
C
	CALL PC_COMP  ( index, datlev, outdat, ier )
C
C*	Compute the integrated parameters if there are any.
C
	IF  ( king .ne. 0 )  THEN
	    level1 = 0
	    IF  ( .not. vtbflg)  CALL PC_MVRT  ( datain, ier )
	    pres = vdata ( 1, levnum )
	    CALL PC_CING  ( datain, level1, levnum, pres, jdsprm,
     +			    outdat, ier )
	END IF
C
C*	Compute the layer parameters if any.
C
	IF  ( klayr .ne. 0 )  THEN
	    IF  ( .not. vtbflg)  CALL PC_MVRT  ( datain, ier )
	    pres = vdata ( 1, levnum )
	    CALL PC_CLYR  ( datain, level1, levnum, pres, datlev (1),
     +			    jcord, outdat, ier )
	END IF
C
C*	Translate character data
C
	CALL PT_CNVR  ( outdat, koutpm (1), qcmp (1), qchr (1),
     +			chrfnc, chrdat, ier )
C
C*	Check if level conditions have been met.  If not, set data
C*	to missing values.
C
	IF ( levcnd )  CALL PC_CLCD ( koutpm (1), outdat, chrdat, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
	END IF
C*
	RETURN
	END
