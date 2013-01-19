	SUBROUTINE SNLINP  ( snfile, area, dattim, snparm, stndex,
     +			     levels, vcoord, output, mrgdat, iret )
C************************************************************************
C* SNLINP								*
C*									*
C* This subroutine gets the input variables for SNLIST.			*
C*									*
C* SNLINP  ( SNFILE, AREA, DATTIM, SNPARM, STNDEX, LEVELS, VCOORD,	*
C*           OUTPUT, MRGDAT, IRET )					*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87	GEMPAK4					*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* M. desJardins/GSFC	 4/89	Added MRGDAT				*
C************************************************************************
	CHARACTER*(*)	snfile, area, dattim, snparm, stndex, levels,
     +			vcoord, output, mrgdat
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SNFILE', snfile, ier1 )
	CALL IP_STR  ( 'AREA',   area,   ier2 )
	CALL IP_STR  ( 'DATTIM', dattim, ier3 )
	CALL IP_STR  ( 'SNPARM', snparm, ier4 )
	CALL IP_STR  ( 'STNDEX', stndex, ier5 )
	CALL IP_STR  ( 'LEVELS', levels, ier6 )
	CALL IP_STR  ( 'VCOORD', vcoord, ier7 )
	CALL IP_STR  ( 'OUTPUT', output, ier8 )
	CALL IP_STR  ( 'MRGDAT', mrgdat, ier9 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
