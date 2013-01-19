	SUBROUTINE PC_FTOP ( datain, nparm, levtop, outdat, iret )
C************************************************************************
C* PC_FTOP								*
C*									*
C* This subroutine finds the highest level in the sounding which	*
C* reports pressure and temperature data.				*
C*									*
C* PC_FTOP ( DATAIN, NPARM, LEVTOP, OUTDAT, IRET )			*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*	NPARM		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	LEVTOP		INTEGER		Highest level number		*
C*	OUTDAT (NOUTPM)	REAL		Output data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/GSC		 8/97						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Lee/GSC		11/97	Used dimension MAXPRM for datlev	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
C*
	REAL		datlev (MAXPRM)
	INCLUDE		'ERMISS.FNC'
C-------------------------------------------------------------------------
	iret = 0
C
C*	Retrieve the top level index and data.  Return if the temperature
C*	and pressure data are there.
C
	CALL PC_FLVL  ( -1., 1, datain, ptop, numlev, nnext, levtyp,
     +			ier )
	CALL PC_GLEV  ( numlev, datain, nparm, datlev, ier )
	CALL PC_COMP  ( 5,    datlev, outdat, ier )
	IF  ( .not. ERMISS ( outdat (1) ) .and.
     +	      .not. ERMISS ( outdat (2) ) )  THEN
	    levtop = numlev
	    RETURN
	END IF
C
C*	Search for the valid data level from the top.
C
	DO  WHILE ( ERMISS ( outdat (2) ) .or. ERMISS ( outdat (1) )
     +		    .and.  ( numlev .gt. 1 ) )
	    numlev= numlev - 1
	    CALL PC_GLEV  ( numlev, datain, nparm, datlev, ier )
	    CALL PC_COMP  ( 5, datlev, outdat, ier )
	END DO
C*
	levtop = numlev
	RETURN
	END
