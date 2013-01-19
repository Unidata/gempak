	SUBROUTINE PC_GCND  ( chrtyp, nocc, rvalue, cvalue, iret )
C************************************************************************
C* PC_GCND								*
C*									*
C* This subroutine gets the condition value input after a particular	*
C* separator.								*
C*									*
C* PC_GCND  ( CHRTYP, NOCC, RVALUE, CVALUE, IRET )			*
C*									*
C* Input parameters:							*
C*	CHRTYP		CHAR*1		Separator			*
C*	NOCC		INTEGER		Occurrence of separator		*
C*									*
C* Output parameters:							*
C*	RVALUE		REAL		Condition value			*
C*	CVALUE		CHAR*		Character condition value	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-29 = no such condition value	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* D. Kidwell/NCEP	 4/04	Added check on msgcnd                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	chrtyp, cvalue
C-------------------------------------------------------------------------
	iret   = -29
	rvalue = RMISSD
	cvalue = ' '
C
C*	Check for station or level condition type.
C
	IF  ( icndtp .eq. 1 )  THEN
	    iocc = 0
	    knt  = 1
	    i    = ilvprm
	    DO WHILE  ( ( iret .ne. 0 ) .and. ( knt .le. nlvcnd (i) ) )
		IF  ( symlev ( knt, i ) .eq. chrtyp )  THEN
		    iocc = iocc + 1
		    IF  ( iocc .eq. nocc )  THEN
			IF ( .not. msgcnd ( knt, i ) ) THEN
			    rvalue = rlvcnd ( knt, i )
			    cvalue = cclcnd ( knt, i )
			    iret   = 0
			  ELSE
			    knt = nlvcnd ( i )
			END IF
		    END IF
		END IF
		knt = knt + 1
	    END DO
	  ELSE IF  ( icndtp .eq. 2 )  THEN
	    iocc = 0
	    knt  = 1
	    i    = istprm
	    DO WHILE  ( ( iret .ne. 0 ) .and. ( knt .le. nstcnd (i) ) )
		IF  ( symstn ( knt, i ) .eq. chrtyp )  THEN
		    iocc = iocc + 1
		    IF  ( iocc .eq. nocc )  THEN
			rvalue = rstcnd ( knt, i )
			cvalue = ccscnd ( knt, i )
			iret   = 0
		    END IF
		END IF
		knt = knt + 1
	    END DO
	END IF
C*
	RETURN
	END
