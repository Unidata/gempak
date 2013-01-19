	SUBROUTINE PC_CKPM ( index, nparms, prmlst, iret )
C************************************************************************
C* PC_CKPM								*
C*									*
C* This subroutine saves the interpolation and extrapolation		*
C* flags for the PC computations.  The character flags, QCHR,		*
C* are no longer set in this subroutine.  If the parameter type		*
C* table cannot be opened or if it is too large for the buffer,		*
C* an error message will be written but no error will be returned.	*
C*									*
C* PC_CKPM ( INDEX, NPARMS, PRMLST, IRET )				*
C*									*
C* Input parameters:							*
C*	INDEX		INTEGER		Table index			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	PRMLST (NPARMS)	CHAR*4		Parameter list			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	11/87	Eliminated check for character type	*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* M. Linda/GSC		10/97	Corrected the prologue right border	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	prmlst (*)
C*
	LOGICAL		found
C------------------------------------------------------------------------
C*	Read in parameter conversion table if it has not already been
C*	read.
C
	iret = 0
	IF  ( .not. pptabl )  THEN
	    CALL TB_PRMT  ( MAXPM, jptabl, pparm, pchr, pint, pext,
     +			    pang, ier )
	    pptabl = .true.
	END IF
C
C*	Loop through each parameter.
C
	DO  i = 1, nparms
	    found = .false.
	    j = 1
C
C*	    Find this parameter in the table.
C
	    DO WHILE  ( ( .not. found ) .and. ( j .le. jptabl ) )
		IF  ( prmlst (i) .eq. pparm (j) )  THEN
		    qint (i, index) = pint (j)
		    qext (i, index) = pext (j)
		    qang (i, index) = pang (j)
		    found = .true.
		ENDIF
		j = j + 1
	    ENDDO
C
C*	    If parameter was not found, use default values.
C
	    IF  ( .not. found )  THEN
		qint (i, index) = .true.
		qext (i, index) = .false.
		qang (i, index) = .false.
	    END IF
	ENDDO
C*
	RETURN
	END
