	SUBROUTINE OACINP  ( sffile, gdfile, guess, sfparm, dtaarea,
     +			     dattim, gamma, search, npass, qcntl,
     +                       guesfun, oabnd, gdattim, gfunc, glevel,
     +			     gvcord, iret ) 
C************************************************************************
C* OACINP								*
C*									*
C* This subroutine gets the input parameters for OABSFC.		*
C*									*
C* OACINP  ( SFFILE, GDFILE, GUESS, SFPARM, DTAAREA, DATTIM, GAMMA, 	*
C*           SEARCH, NPASS, QCNTL, GUESFUN, OABND, GDATTIM, GFUNC,	*
C*	     GLEVEL, GVCORD, IRET )					*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added GUESS				*
C* K. Brill/NMC         10/90   Changed AREA to DTAAREA			*
C* T. Lee/GSC		 3/99	Added QCNTL				*
C* R. Tian/SAIC		 3/05	Added GUESFUN				*
C* J. Wu/SAIC		 4/05	add OABND				*
C* B. Yin/SAIC		 6/05	add GDATTIM, GFUNC, GLEVEL, & GVCORD	*
C************************************************************************
	CHARACTER*(*)	sffile, gdfile, guess, sfparm, dtaarea, dattim,
     +			gamma, search, npass, qcntl, guesfun, oabnd,
     +			gdattim, gfunc, glevel, gvcord
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SFFILE', sffile, ier1 )
	CALL IP_STR  ( 'GDFILE', gdfile, ier2 )
	CALL IP_STR  ( 'SFPARM', sfparm, ier3 )
	CALL IP_STR  ( 'DTAAREA', dtaarea, ier4 )
	CALL IP_STR  ( 'DATTIM', dattim, ier5 )
	CALL IP_STR  ( 'GAMMA',  gamma,  ier6 )
	CALL IP_STR  ( 'SEARCH', search, ier7 )
	CALL IP_STR  ( 'NPASS',  npass,  ier8 )
	CALL IP_STR  ( 'GUESS',  guess,  ier9 )
	CALL IP_STR  ( 'QCNTL',  qcntl,  ier10 )
	CALL IP_STR  ( 'GUESFUN', guesfun, ier11 )
	CALL IP_STR  ( 'OABND',  oabnd,  ier12 )
	CALL IP_STR  ( 'GDATTIM', gdattim, ier13 )
	CALL IP_STR  ( 'GFUNC', gfunc, ier14 )
	CALL IP_STR  ( 'GLEVEL', glevel, ier15 )
	CALL IP_STR  ( 'GVCORD', gvcord, ier16 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +         ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +	       ier16
C*
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
