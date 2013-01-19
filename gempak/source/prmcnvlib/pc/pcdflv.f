	SUBROUTINE PC_DFLV  ( noutpm, outprm, chrflg, cmpflg, ncomp,
     +			      iret )
C************************************************************************
C* PC_DFLV								*
C*									*
C* This subroutine defines the output level parameters which will	*
C* be returned when either PC_CMLV or PC_CMVR is called.  The		*
C* output values will be computed from the parameters in the 		*
C* data set.  PC_INIT must be called before PC_DFLV.  The returned	*
C* values of CMPFLG indicate whether the parameters are computable.	*
C* NCOMP is the number of computable parameters found by this		*
C* subroutine.								*
C*									*
C* PC_DFLV  ( NOUTPM, OUTPRM, CHRFLG, CMPFLG, NCOMP, IRET )		*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*	OUTPRM (NOUTPM)	CHAR*4		Output parameters		*
C*									*
C* Output parameters:							*
C*	CHRFLG (NOUTPM)	LOGICAL		Character data flag 		*
C*	CMPFLG (NOUTPM)	LOGICAL		Computable data flag		*
C*	NCOMP		INTEGER		Number of computable parms	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT not called	*
C*					 -5 = invalid # of parms	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	10/86	Modified meaning of CMPFLG		*
C* M. desJardins/GSFC	11/87	Changed character computations		*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* M. desJardins/GSFC	 7/89	Initialize chrflg and scmflg		*
C* M. desJardins/GSFC	11/89	Added conditions			*
C* M. desJardins/GSFC	 7/90	Add layer quantities			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'pccmn.cmn'
C*
	LOGICAL		chrflg(*), cmpflg(*)
	CHARACTER*(*)	outprm (*)
C-------------------------------------------------------------------------
	ncomp = 0
C
C*	Turn off the level condition flag.
C
	levcnd = .false.
C
C*	Check for errors in input values.
C
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF ( ( noutpm .lt. 1 ) .or. ( noutpm .gt. MAXPRM ) ) THEN
	    iret = -5
	  ELSE
	    iret = 0
	ENDIF
	IF  ( iret .lt. 0 )  RETURN
C
C*	Initialize the computable and character type flags.
C
	DO  i = 1, noutpm
	    cmpflg (i) = .false.
	    chrflg (i) = .false.
	END DO
C
C*	Set up the conversion from data set parameters to output parameters
C*	using index = 1 into the tables. Return if no parameters can be
C*	computed.
C
	ncomp = 0
	index = 1
	CALL PC_METH  ( index, jdsprm, dsparm, noutpm, outprm, cmpflg, 
     +			ncomp, ier )
	IF  ( ier .lt. 0 )  THEN
	    iret = ier
	    RETURN
	END IF
C
C*	Check to see if any integrated parameters are requested.
C
	CALL PC_SING  ( noutpm, outprm, cmpflg, ncomp, ier )
C
C*	Check to see if any layer quantities are requested.
C
	CALL PC_SLYR  ( noutpm, outprm, cmpflg, ncomp, ier )
C
C*	Set the character data flags using the flags stored in common.
C*	Store the computable flags in common.
C
	DO  i = 1, noutpm
	    qcmp (i) = cmpflg (i)
	    IF  ( cmpflg (i) )  chrflg (i) = qchr (i)
	ENDDO
C
C*	If interpolation flags are on, set up the interpolation tables.
C
	IF  ( jntflg  .and.  inton  .and.  ( jcord .ne. 0 ) ) THEN
	    index1 = 2
	    index2 = 3
	    CALL  PC_SINT  ( noutpm, outprm, cmpflg, index1, index2, 
     +			     ier )
	  ELSE
	    doint = .false.
	ENDIF
C*
	RETURN
	END
