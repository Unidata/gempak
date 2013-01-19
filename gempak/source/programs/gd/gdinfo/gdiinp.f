	SUBROUTINE GDIINP  ( gdfile, lstall, output, gdattim, glevel,
     +		 	     gvcord, gfunc, iret )
C************************************************************************
C* GDIINP								*
C*									*
C* This subroutine gets the input parameters for GDIIST from the TAE.	*
C*									*
C* GDIINP  ( GDFILE, LSTALL, OUTPUT, GDATTIM, GLEVEL, GVCORD, GFUNC,    *
C*	     IRET )							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/84						*
C* K. Brill/NMC          9/90		Change GDLIST to LSTALL		*
C* S. Maxwell/GSC	10/96		Updated to include new parms	*
C************************************************************************
	CHARACTER*(*)	gdfile, output, gdattim, glevel, gvcord, gfunc
	LOGICAL		lstall
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_LOG  ( 'LSTALL',  lstall,  ier2 )
	CALL IP_STR  ( 'OUTPUT',  output,  ier3 )
        CALL IP_STR  ( 'GDATTIM', gdattim, ier4 )
        CALL IP_STR  ( 'GLEVEL',  glevel,  ier5 )
        CALL IP_STR  ( 'GVCORD',  gvcord,  ier6 )
        CALL IP_STR  ( 'GFUNC',   gfunc,   ier7 )
        iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
