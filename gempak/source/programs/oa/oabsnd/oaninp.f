	SUBROUTINE OANINP  ( snfile, gdfile, guess, snparm, stndex, 
     +			     dtaarea, dattim, levels, vcoord, gamma, 
     +			     search, npass, qcntl, guesfun, oabnd, 
     +                       iret )
C************************************************************************
C* OANINP								*
C*									*
C* This subroutine gets the input parameters for OABSFC.		*
C*									*
C* OANINP  ( SNFILE, GDFILE, GUESS, SNPARM, STNDEX, DTAAREA, DATTIM, 	*
C*           LEVELS, VCOORD, GAMMA,  SEARCH, NPASS, QCNTL, GUESFUN,	*
C*           OABND, IRET )						*
C*									*
C* Input parameters:							*	
C*	SNFILE		CHAR*		Sounding file name		*	
C*	GDFILE		CHAR*		Grid file name			*
C*	GUESS		CHAR*		First guess			*
C*	SNPARM		CHAR*		Sounding parameters		*
C*	STNDEX		CHAR*		Station index			*
C*	DTAAREA		CHAR*		Data area			*
C*	DATTIM		CHAR*		Date time			*
C*	LEVELS		CHAR*		Data levels			*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	GAMMA		CHAR*		Gamma				*
C*	SEARCH		CHAR*		Search criteria			*
C*	NPASS		CHAR*		Number of passes		*
C*	QCNTL		CHAR*		Quality control flag		*
C*	GUESFUN		CHAR*		Guess function			*
C*	OABND		CHAR*		Bounds files used for blocking	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Status return			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added GUESS				*
C* K. Brill/NMC         10/90   Changed AREA to DTAAREA			*
C* T. Lee/GSC		 3/99	Added QCNTL				*
C* T. Piper/GSC		 3/99	Filled in prolog parameters		*
C* R. Tian/SAIC		 3/05	Added GUESFUN				*
C* J. Wu/SAIC		 4/05	add OABND				*
C************************************************************************
	CHARACTER*(*)	snfile, gdfile, guess, snparm, dtaarea, dattim,
     +			gamma, search, npass, levels, vcoord, stndex,
     +			qcntl, guesfun, oabnd
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE',  snfile,  ier1 )
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier2 )
	CALL IP_STR  ( 'SNPARM',  snparm,  ier3 )
	CALL IP_STR  ( 'STNDEX',  stndex,  ier4 )
	CALL IP_STR  ( 'DTAAREA', dtaarea, ier5 )
	CALL IP_STR  ( 'DATTIM',  dattim,  ier6 )
	CALL IP_STR  ( 'LEVELS',  levels,  ier7 )
	CALL IP_STR  ( 'VCOORD',  vcoord,  ier8 )
	CALL IP_STR  ( 'GAMMA',   gamma,   ier9 )
	CALL IP_STR  ( 'SEARCH',  search,  ier10 )
	CALL IP_STR  ( 'NPASS',   npass,   ier11 )
	CALL IP_STR  ( 'GUESS',   guess,   ier12 )
	CALL IP_STR  ( 'QCNTL',   qcntl,   ier13 )
	CALL IP_STR  ( 'GUESFUN', guesfun, ier14 )
	CALL IP_STR  ( 'OABND',   oabnd,   ier15 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
     +		    + ier9 + ier10 + ier11 + ier12 + ier13 + ier14
     +              + ier15
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
