	SUBROUTINE SNOINP  ( snfile, snoutf, snparm, area, dattim, 
     +			     levels, vcoord, timstn, mrgdat, idntyp,
     +                       iret )
C************************************************************************
C* SNOINP								*
C*									*
C* This subroutine gets the input variables for SNLIST.			*
C*									*
C* SNOINP  ( SNFILE, SNOUTF, SNPARM, AREA, DATTIM, LEVELS, VCOORD,	*
C*           TIMSTN, MRGDAT, IDNTYP, IRET )				*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87	GEMPAK4					*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* S. Schotz/GSC	1/90	Now get MRGDAT for unmerged path	*
C* S. Schotz/GSC	8/90	Added IDNTYP				*
C************************************************************************
	CHARACTER*(*)	snfile, snoutf, area, dattim, snparm, levels,
     +			vcoord, timstn, mrgdat, idntyp
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SNFILE', snfile, ier1 )
	CALL IP_STR  ( 'SNOUTF', snoutf, ier2 )
	CALL IP_STR  ( 'SNPARM', snparm, ier3 )
	CALL IP_STR  ( 'AREA',   area,   ier4 )
	CALL IP_STR  ( 'DATTIM', dattim, ier5 )
	CALL IP_STR  ( 'LEVELS', levels, ier6 )
	CALL IP_STR  ( 'VCOORD', vcoord, ier7 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier8 )
        CALL IP_STR  ( 'MRGDAT', mrgdat, ier9 )
	CALL IP_STR  ( 'IDNTYP', idntyp, ier10 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 +
     +         ier7 + ier8 + ier9 + ier10
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
