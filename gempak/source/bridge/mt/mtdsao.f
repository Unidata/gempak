	SUBROUTINE MT_DSAO ( irpntr, coun, ihr, iret ) 
C************************************************************************
C* MT_DSAO								*
C*									*
C* This routine will decode surface observations corresponding to the	*
C* SAO format and write the data to the common interface. 		*
C* The station and time must have been previously set.			*
C*									*
C* MT_DSAO ( IRPNTR, COUN, IHR, IRET )		        		*
C*									*
C* Input parameters:							*
C*	IRPNTR		INTEGER		Pointer to start of report	*
C*	COUN		CHAR*		Country				*
C*	IHR		INTEGER		Report hour			*
C*									*
C* Output parameters:							*
C*	RIVALS(*)	REAL		Real interface values array     *
C*	CIVALS(*)	CHAR*		Char. interface values array    *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* K. Tyle/GSC		11/96	 					*
C* K. Tyle/GSC		 1/97	Reorganized parameter declarations;	*
C*				use pmsl in return from RA_DECD		*
C* D. Kidwell/NCEP	 4/98	New interface; split out SF write calls *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'mtcmn.cmn'
C*
	CHARACTER*(*) 	coun
C*
	PARAMETER 	( MAXCLD = 5 )
	REAL 		cldtyp ( MAXCLD ), cldhgt ( MAXCLD )
	LOGICAL		asoflg
	CHARACTER 	wcod*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	asoflg = .true.
	CALL RA_DECD  ( irpntr, coun, asoflg, maxcld,
     +			ihr, cldtyp, cldhgt,
     +			ncld, vsby, wcod, wnum, pmsl,
     +			tmpf, dwpf, sknt, drct, gust,
     +			alti, p03d, p03i, p06i, hsun,
     +			csyl, csym, csyh, snow, weqs,
     +			t06x, t12x, t24x, tmdx, t06n,
     +			t12n, t24n, tmdn, p24i, iret )
	ctyl = csyl
	ctym = csym
	ctyh = csyh
	IF ( .not. ERMISS ( csym ) ) ctym = csym - 10.
	IF ( .not. ERMISS ( csyh ) ) ctyh = csyh - 20.
C
C*	Convert from Fahrenheit to Celsius.
C
	tmpc = PR_TMFC ( tmpf )
	dwpc = PR_TMFC ( dwpf )
C
	IF ( wnum .eq. 0.0 ) wnum = RMISSD
	IF ( iret .eq. 0 )  THEN
C
C*	    Compute low, mid and high clouds.
C
	    CALL RA_CLEV  ( cldtyp, cldhgt, ncld,
     +			    chc1, chc2, chc3, ier )
C
C*	    Move data into interface array.
C
	    rivals ( irpmsl )       = pmsl
	    rivals ( iralti )       = alti
	    rivals ( irtmpc )       = tmpc
	    rivals ( irdwpc )       = dwpc
	    rivals ( irsknt )       = sknt
	    rivals ( irdrct )       = drct
	    rivals ( irgust )       = gust
	    IF ( .not. ERMISS ( wnum ) ) rivals ( irnpwx ) = 1
	    civals ( icwcod ( 1 ) ) = wcod
	    IF ( .not. ERMISS ( chc1 ) ) rivals ( irnsky ) = 1
	    rivals ( ircmtn ( 1 ) ) = chc1
	    IF ( .not. ERMISS ( chc2 ) ) rivals ( irnsky ) = 2
	    rivals ( ircmtn ( 2 ) ) = chc2
	    IF ( .not. ERMISS ( chc3 ) ) rivals ( irnsky ) = 3
	    rivals ( ircmtn ( 3 ) ) = chc3
	    IF ( .not. ERMISS ( vsby ) ) rivals ( irnvsb ) = 1
	    rivals ( irvsby ( 1 ) ) = vsby
	    rivals ( irp03d )       = p03d
	    rivals ( irp03i )       = p03i
	    rivals ( irmsun )       = hsun
	    rivals ( irsnow )       = snow
	    rivals ( irweqs )       = weqs
	    rivals ( irp24i )       = p24i
	    rivals ( irtdxc )       = t24x
	    rivals ( irtdnc )       = t24n
	    rivals ( irctyl )       = ctyl
	    rivals ( irctym )       = ctym
	    rivals ( irctyh )       = ctyh
	    rivals ( irp06i )       = p06i
	    rivals ( irt6xc )       = t06x
	    rivals ( irt6nc )       = t06n
	END IF
C*
	RETURN
	END
