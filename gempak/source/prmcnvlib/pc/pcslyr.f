	SUBROUTINE PC_SLYR  ( noutpm, outprm, cmpflg, np, iret )
C************************************************************************
C* PC_SLYR								*
C*									*
C* This subroutine sets up the computation of layer parameters.		*
C* The computable flags for the station parameters are save in		*
C* common.								*
C*									*
C* PC_SLYR  ( NOUTPM, OUTPRM, CMPFLG, NP, IRET )			*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parms		*
C*	OUTPRM (NOUTPM) CHAR*4		Output parms			*
C*									*
C* Input and output parameters:						*
C*	CMPFLG (NOUTPM) LOGICAL		Computable flags		* 
C*	NP		INTEGER		Number of computable parms	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	outprm (*)
	LOGICAL		cmpflg (*)
C*
	CHARACTER*4	parm
C------------------------------------------------------------------------
	iret = 0
C
C*	Set up tables 5,6,7 to be used to compute the basic parameters 
C*	for the stability indices.
C
	IF  ( .not. sindxf )  THEN
	    index = 5
	    CALL PC_METH  ( index, jdsprm, dsparm, MSTNPM, splist,
     +			    scomp, n, ier )
C
C*	    If interpolation flags are on, set up the interpolation 
C*	    tables.
C
	    index1 = 6
	    index2 = 7
	    IF  ( jntflg  .and.  inton  .and.  (jcord .ne. 0) )  THEN
		CALL PC_SINT  ( MSTNPM, splist, scomp, index1, index2, 
     +				ier )
	    END IF
	    sindxf = .true.
	END IF
C
C*	Initialize common values.
C
	DO  i = 1, MAXPRM
	    qlayr  (i) = .false.
	ENDDO
	klayr = 0
C
C*	If all parameters are already computable, return.
C*	Also exit if there is no vertical coordinate system.
C
	IF ( (noutpm .eq. kfound (1)) .or. (jcord .eq. 0) ) RETURN
C
C*	Check if any non-computable parameters are layer parms.
C
	DO  i = 1, noutpm
	    IF  ( .not. cmpflg (i) )  THEN
		parm = outprm (i)
		IF  ( ( parm .eq. 'RICH' ) .and. scomp (1) .and.
     +		        scomp (2) .and. scomp (4) .and. scomp (5)
     +			.and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'BVFQ' ) .and. scomp (1)
     +			       .and. scomp (2) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'BVSQ' ) .and. scomp (1)
     +			       .and. scomp (2) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'BVPD' ) .and. scomp (1)
     +			       .and. scomp (2) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'LAPS' )
     +			       .and. scomp (2) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'STAB' ) .and. scomp (1)
     +			       .and. scomp (2) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'STAP' ) .and. scomp (1)
     +			       .and. scomp (2) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'SHRM' ) .and. scomp (4)
     +			       .and. scomp (5) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF  ( ( parm .eq. 'SHRD' ) .and. scomp (4)
     +			       .and. scomp (5) .and. scomp (6) )  THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		  ELSE IF ( ( parm .eq. 'SEPA' ) .and. scomp (2) ) THEN
		    klayr = klayr + 1
		    np    = np + 1
		    cmpflg (i) = .true.
		    prmlyr (i) = parm
		    qlayr  (i) = .true.
C------------------------------------------------------------------------
		END IF
	    END IF
	END DO
C*
	RETURN
	END
