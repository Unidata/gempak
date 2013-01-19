	SUBROUTINE  BFRINP  ( snefil, sffsrc, snoutf, sfoutf,
     +			      snprmf, sfprmf, area, dattim, timstn,
     +			      thndat, iret )
C************************************************************************
C* BFRINP								*
C*									*
C* This subroutine gets the input variables for BFR2GP.			*
C*									*
C* BFRINP  ( SNEFIL, SFFSRC, SNOUTF, SFOUTF, SNPRMF, SFPRMF, AREA,	*
C*	     DATTIM, TIMSTN, THINDT, IRET )				*
C**									*
C* Log:									*
C* K. F. Brill/EMC	 2/97						*
C* K. Brill/EMC		 2/97	Added AREA				*
C* K. Brill/EMC		 4/97	Added SFFSRC & DATTIM			*
C* L. Sager/NCO  	 5/05   Added THNDAT 
C************************************************************************
	CHARACTER*(*)	snefil, sffsrc, snoutf, sfoutf, snprmf, sfprmf,
     +			area, dattim, timstn, thndat
C------------------------------------------------------------------------
C*	Get the input variables.
C
   	CALL IP_STR  ( 'SNEFIL', snefil, ier1 )
	CALL IP_STR  ( 'SFFSRC', sffsrc, ier2 )
	CALL IP_STR  ( 'SNOUTF', snoutf, ier3 )
	CALL IP_STR  ( 'SFOUTF', sfoutf, ier4 )
	CALL IP_STR  ( 'SNPRMF', snprmf, ier5 )
	CALL IP_STR  ( 'SFPRMF', sfprmf, ier6 )
	CALL IP_STR  ( 'AREA',   area,   ier7 )
	CALL IP_STR  ( 'DATTIM', dattim, ier8 )
	CALL IP_STR  ( 'TIMSTN', timstn, ier9 )
  
        thndat = ' '
        if(sffsrc .eq. 'thncft') then
            sffsrc = 'aircft'
            thndat = 'YES'
        endif
        if(sffsrc .eq. 'thnwnd') then
            sffsrc = 'satwnd'
            thndat = 'YES'
        endif
        print *,' thndat is ',thndat
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
