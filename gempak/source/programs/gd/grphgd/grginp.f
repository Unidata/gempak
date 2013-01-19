	SUBROUTINE GRGINP  ( gdfile, guess, proj, gdarea, kxky, maxgrd,
     +			     cpyfil, anlyss, cntrfl, gdatim, gparm, 
     +			     glevel, gvcord, keycol, gglims, hstgrd,
     +			     bounds, type, gamma, search, npass, qcntl, 
     +			     guesfun, catmap, discrete, dlines, ggvgf, 
     +			     edgeopts, olkday, keylin, iret )
C************************************************************************
C* GRGINP								*
C*									*
C* This subroutine gets the input parameters for GRPHGD.		*
C*									*
C* GRGINP  ( GDFILE, GUESS, PROJ, GDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS, *
C*	     CNTRFL, GDATIM, GPARM, GLEVEL, GVCORD, KEYCOL, GGLIMS, 	*
C* 	     HSTGRD, BOUNDS, TYPE, GAMMA, SEARCH, NPASS, QCNTL, 	*
C*	     GUESFUN, CATMAP, DISCRETE, DLINES, GGVGF, EDGEOPTS,       	*
C*	     OLKDAY, KEYLIN, IRET )					*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 9/98						*
C* D.W.Plummer/NCEP	 9/98	Add GLEVEL and GVCORD			*
C* D.W.Plummer/NCEP	 9/98	Changed GPARM to GFUNC			*
C* D.W.Plummer/NCEP	 2/99	Added KEYCOL				*
C* D.W.Plummer/NCEP	 9/99	Added GGLIMS				*
C* D.W.Plummer/NCEP	10/99	Added HISTGRD				*
C* W.D.Plummer/NCEP	12/02	Added BOUNDS 				*
C* T. Lee/SAIC		 7/04	Added TYPE, OAATTR and OAGUESS		*
C* H. Zeng/SAIC		03/05	Added CATMAP				*
C* H. Zeng/SAIC		04/05	Added DISCRETE&DLINES			*
C* M. Li/SAIC		04/05	Repl OAATTR, OAGUESS w/ GUESS, GUESFUN, *
C*                              CGAMMA, CSRCH, PASS, and CQCNTL		*
C* m.gamazaychikov/SAIC	12/05	Added GGVGF				*
C* M. Li/SAIC		03/07	Added EDGEOPTS				*
C* F. J. Yen/NCEP	01/08	Added OLKDAY and KEYLINE		*
C************************************************************************
	CHARACTER*(*)	gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +                  anlyss, cntrfl, gdatim, gparm, glevel, gvcord,
     +			keycol, gglims, hstgrd, bounds, type, guess, 
     +			catmap, discrete, dlines, gamma, search, 
     +			npass, qcntl, guesfun, ggvgf, edgeopts,
     +			olkday, keylin
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDOUTF',  gdfile, ier1  )
	CALL IP_STR  ( 'PROJ',    proj,   ier2  )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier3  )
	CALL IP_STR  ( 'KXKY',    kxky,   ier4  )
	CALL IP_STR  ( 'MAXGRD',  maxgrd, ier5  )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier6  )
	CALL IP_STR  ( 'ANLYSS',  anlyss, ier7  )
	CALL IP_STR  ( 'CNTRFL',  cntrfl, ier8  )
	CALL IP_STR  ( 'GDATTIM', gdatim, ier9  )
	CALL IP_STR  ( 'GFUNC',   gparm,  ier10 )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier11 )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier12 )
	CALL IP_STR  ( 'KEYCOL',  keycol, ier13 )
	CALL IP_STR  ( 'GGLIMS',  gglims, ier14 )
	CALL IP_STR  ( 'HISTGRD', hstgrd, ier15 )
	CALL IP_STR  ( 'BOUNDS',  bounds, ier16 )
	CALL IP_STR  ( 'TYPE',    type,   ier17 )
	CALL IP_STR  ( 'GUESS',   guess,  ier18 )
	CALL IP_STR  ( 'GUESFUN', guesfun, ier19 )
	CALL IP_STR  ( 'CATMAP',  catmap, ier20 )
	CALL IP_STR  ( 'GAMMA',   gamma,  ier21 )
        CALL IP_STR  ( 'SEARCH',  search, ier22 )
        CALL IP_STR  ( 'NPASS',   npass,  ier23 )
        CALL IP_STR  ( 'QCNTL',   qcntl,  ier24 )
        CALL IP_STR  ( 'DISCRETE', discrete, ier25 )
        CALL IP_STR  ( 'DLINES',  dlines, ier26 )
        CALL IP_STR  ( 'GGVGF',   ggvgf,  ier27 )
        CALL IP_STR  ( 'EDGEOPTS', edgeopts,  ier28 )
        CALL IP_STR  ( 'OLKDAY', olkday,  ier29 )
        CALL IP_STR  ( 'KEYLINE', keylin, ier30 )

C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +		ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
     +          ier16+ ier17 + ier18 + ier19 + ier20 + ier21 + ier22 +
     +          ier23 + ier24 + ier25 + ier26 + ier27 + ier28 + ier29 +
     +		ier30
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
