	SUBROUTINE DSFRNT  ( ifcod, pipsz, ipipst, ipipdr,  
     +			     jfcod, szpip, jpipst, jpipdr, iret )
C************************************************************************
C* DSFRNT								*
C* 									*
C* This subroutine sets the front attributes including the front code,	*
C* the pip size multiplier, the pip stroke multiplier, and the pip	*
C* direction.								*
C*									*
C* DSFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR,				*
C*	     JFCOD, SZPIP, JPIPST, JPIPDR, IRET)			*
C*									*
C* Input parameters:							*
C*	IFCOD		INTEGER		Front code			*
C*	PIPSZ		REAL		Pip size multiplier		*
C*	IPIPST		INTEGER		Pip stroke multiplier		*
C*	IPIPDR		INTEGER		Pip direction			*
C*					  				*
C* Output parameters:							*
C*	JFCOD		INTEGER		Front code			*
C*	SZPIP		REAL		Pip size			*
C*	JPIPST		INTEGER		Pip stroke multiplier		*
C*	JPIPDR		INTEGER		Pip direction multiplier	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	10/96	Created					*
C* E. Wehner/EAi	11/96	Eliminate parameter			*
C* S. Jacobs/NCEP	 2/97	Documentation changes			*
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C************************************************************************
	INCLUDE 'FUNCCODE.PRM'
	INCLUDE 'ERROR.PRM'
	INCLUDE 'DEVACT.CMN'
C*
	INTEGER isend (3), isnd2 (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = CSFRNT
	isend (3) = ifcod
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( pipsz, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isnd2 (1) = ipipst
 	isnd2 (2) = ipipdr
C
	CALL GPUT  ( isnd2, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Fill output variables.
C
	CALL GGET  ( ircv, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	iret   = ircv (1)
	jfcod  = ircv (2)
C
	CALL GGETR  ( szpip, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( ircv, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	jpipst = ircv (1)
	jpipdr = ircv (2)
C
C*	Fill the ACTIVE common block variables
C
	mfcod  = jfcod
	tpipsz = szpip
	mpipst = jpipst
	mpipdr = jpipdr
C*
	RETURN
	END
