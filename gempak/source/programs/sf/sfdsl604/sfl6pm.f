	SUBROUTINE SFL6PM  ( sfparm, cmpflg, iret )
C************************************************************************
C* SFL6PM								*
C*									*
C* This subroutine sets up parameters to process.			*
C*									*
C* SFL6PM  ( SFPARM, CMPFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	SFPARM		CHAR*		Parameter input with conditions	*
C*									*
C* Output parameters:							*
C*	CMPFLG (*)	LOGICAL		Compute flags			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = no computable parms	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	11/89	Added SFPARM to set conditions		*
C* J. Whistler/SSAI	 2/91	Added additional parameters		*
C* P. Bruehl/Unidata	 9/94   Added additional parameters		*
C* K. Tyle/GSC		 1/97	Removed several parameters; call ER_LMSG*
C* S. Lilly/SIB         09/10   Changed Combined cloud height and       *
C*                              coverage(CLDx) to Character cloud       *
C*                              coverage(xCLD)                          *
C* S. Lilly/SIB         09/10   Removed Visibility from Data and        *
C*                              rearranged the order of pararameters    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		cmpflg (*)
	CHARACTER*(*)	sfparm
C*
	CHARACTER	parms (MMPARM)*4, prmcnd (MMPARM)*16, 
     +			cndtn (MMPARM)*16
	LOGICAL		cndflg
C*
	PARAMETER	( NPARM = 16 )
	PARAMETER	( KPARM = MMPARM - NPARM )
	CHARACTER	prmlst (MMPARM) *4
	LOGICAL		chrflg (MMPARM)
C*
	DATA		PRMLST  / 'TMPF', 'DWPF', 'DRCT', 'SKNT', 
     +				  'GUST', 'LCLD', 'MCLD', 'HCLD',
     +				  'ALTI', 'PMSL', 'PT03', 'WTHR',
     +				  'P03I', 'P06I', 'P24I', 'SNOW',
     +				  KPARM * ' '/ 
C--------------------------------------------------------------------------
	iret = 0
C
C*	Check for conditions on parameters.
C
	CALL IN_PRMC  ( MMPARM, sfparm, parms, prmcnd, ncprm, ier )
	number = nparm
	cndflg = .false.
	DO  i = 1, MMPARM
	    cndtn (i) = ' '
	END DO
	DO  i = 1, ncprm
C
C*	    See if parameter is one in the list.
C
	    CALL ST_FIND  ( parms (i), prmlst, NPARM, ipos, ier )
C
C*	    If parameter is not in list, add it.
C
	    IF  ( ( ipos .eq. 0 ) .and. ( prmcnd (i) .ne. ' ' ) )  THEN
		number = number + 1
		prmlst (number) = parms  (i)
		cndtn  (number) = prmcnd (i)
		cndflg = .true.
C
C*		Otherwise, set conditions for this parameter.
C
	      ELSE IF  ( prmcnd (i) .ne. ' ' )  THEN
		cndtn  (ipos) = prmcnd (i)
		cndflg = .true.
	    END IF
	END DO
C
C*	Check to see which parameters are computable.
C
	CALL PC_DFLV  ( number, prmlst, chrflg, cmpflg, np, iret )
C
C*	Show which parameters cannot be computed.
C
	np = number
	DO  i = 1, number
	    IF  ( .not. cmpflg (i) )  THEN
		CALL ER_LMSG  ( 0, 'SFDSL604', +1, prmlst (i), iret )
		np = np - 1
	    END IF
	END DO
C
C*	No parameters computable, quit.
C
	IF  ( np .lt. 1 )  THEN
	    CALL ER_LMSG  ( 0, 'SFDSL604', -4, ' ', ier )
	    iret = -1
	    RETURN
	END IF
C
C*	Set conditions.
C
	IF  ( cndflg )  CALL PC_SLCD  ( number, cndtn, ier )
C*
	RETURN
	END
