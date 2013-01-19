	SUBROUTINE GDOINP  ( gdfile, gdoutf, gpack, gdattim, 
     +			     glevel, gvcord, gfunc, grdhdr, iret )
C************************************************************************
C* GDOINP								*
C*									*
C* This subroutine gets the input parameters for GDMOD from the TAE.	*
C*									*
C* GDOINP  ( GDFILE, GDOUTF, GPACK, GDATTIM, GLEVEL, GVCORD,    	*
C*	     GFUNC, GRDHDR, IRET )					*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/85						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* M. desJardins/GSFC	 3/89	Added grid packing			*
C* J. Hoopingarner/CAC  12/93   Added grid header info			*
C* M. Li/SAIC           04/04   Added grdhdr                            *
C************************************************************************
	CHARACTER*(*)	gdfile, gdoutf, gpack, gdattim,
     + 			glevel, gvcord, gfunc, grdhdr
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',   gdfile,   ier1 )
	CALL IP_STR  ( 'GDOUTF',   gdoutf,   ier2 )
	CALL IP_STR  ( 'GPACK',    gpack,    ier4 )
	CALL IP_STR  ( 'GDATTIM',  gdattim,  ier5 )
	CALL IP_STR  ( 'GLEVEL',   glevel,   ier6 )
	CALL IP_STR  ( 'GVCORD',   gvcord,   ier7 )
	CALL IP_STR  ( 'GFUNC',    gfunc,    ier8 )
	CALL IP_STR  ( 'GRDHDR',   grdhdr,   ier9 )
	iret = ier1 + ier2 + ier4 + ier5 + ier6 + ier7 + ier8 + ier9
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
