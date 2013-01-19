	SUBROUTINE DC_OSHP ( filnam, iflsrc, maxrpt, iflno, nparm, parms,
     +			     iret )
C************************************************************************
C* DC_OSHP								*
C*									*
C* This routine opens a surface ship file.				*
C*									*
C* DC_OSHP ( FILNAM, IFLSRC, MAXRPT, IFLNO, NPARM, PARMS, IRET )	*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File to be opened or created	*
C*	IFLSRC		INTEGER		File source			*
C*	MAXRPT		INTEGER		Max number of reports		*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter list			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* D. Keiser/GSC	 4/96	Added iflsrc to calling sequence	*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	filnam, parms (*)
C*
	CHARACTER	newfil*132
	LOGICAL		exist, pkflg
C------------------------------------------------------------------------
	iret = 0
C*
	CALL FL_INQR ( filnam, exist, newfil, ier )
	IF  ( exist )  THEN
C
C*	    Open the existing file.
C
	    CALL SF_OPNR ( filnam, iflno, ifsc, nparm, parms, iret )
C
C*	    Check the parameter list. If the parameter list doesn't
C*	    match, create a new file with an 'A' at the end of the
C*	    file name.
	ELSE
C
C*	    Create the surface ship file.
C
	    CALL SF_CSDP ( filnam, prmfil, iflsrc, maxrpt, .true.,
     +			   iflno, nparm, parms, pkflg, iret )
	END IF
C*
	RETURN
	END
