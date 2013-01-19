	SUBROUTINE SNFINP  ( snfile, prmfil, stnfil, mrgdat, timstn,
     +			     iret )
C************************************************************************
C* SNFINP								*
C*									*
C* This subroutine gets input for SNCFIL.				*
C*									*
C* SNFINP  ( SNFILE, PRMFIL, STNFIL, MRGDAT, TIMSTN, IRET )		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* M. desJardins/GSFC	 5/90	Change MRGFIL to MRGDAT			*
C************************************************************************
	CHARACTER*(*)	snfile, prmfil, stnfil, timstn, mrgdat
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNOUTF', snfile, ier1 )
	CALL IP_STR  ( 'SNPRMF', prmfil, ier2 )
	CALL IP_STR  ( 'STNFIL', stnfil, ier3 )
	CALL IP_STR  ( 'MRGDAT', mrgdat, ier4 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier5 )
C
	ier = ier1 + ier2 + ier3 + ier4 + ier5 
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
                          
