	SUBROUTINE PC_FUNC  ( nfunc, funcs, ipos, idim, iout, setlev, 
     +			      locout, data, iret )
C************************************************************************
C* PC_FUNC								*
C*									*
C* This subroutine computes meteorological parameters.  Information	*
C* about the functions to call is stored in PC tables.			*
C* Note that this subroutine must match functions defined in the	*
C* GEMPAK table PCCONV.TBL						*
C*									*
C* PC_FUNC  ( NFUNC, FUNCS, IPOS, IDIM, IOUT, SETLEV, LOCOUT, DATA, 	*
C*		IRET )							*
C*									*
C* Input parameters:							*
C*	NFUNC		INTEGER		Number of functions to compute	*
C*	FUNCS (NFUNC)	CHAR*		Functions			*
C*	IPOS (IDIM,*)	INTEGER		Data locations			*
C*	IDIM		INTEGER		Dimension of internal array	*
C*	IOUT (*)	INTEGER		Output locations		*
C*	SETLEV		LOGICAL		Calc of actual output flag	*
C*	LOCOUT (NFUNC)	INTEGER		Location in final output	*
C*									*
C* Output parameters:							*
C*	DATA (*)	REAL		Output data array		*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	11/87	Added null call for PT functions	*
C* M. desJardins/GSFC	 9/88	Cleaned up for GEMPAK4			*
C* K. F. Brill/GSC       6/89   Added PR_AMSL				*
C* S. Schotz/GSC	10/89   Added PR_HGSF,PR_D100,PR_INMM,PR_MMIN,	*
C*                              PR_M100,PR_COMT,PR_COML,PR_COMM,	*
C*				PR_COMH,PR_CLCX,PR_CLHX			*
C* M. desJardins/GSFC	 4/90	Send TELV as elevation to PR_PMSL	*
C* S. Schotz/GSC	 4/90   Added PR_NSYM, PR_6SYM			*
C* M. desJardins/GSFC	 7/90	Added layer parms, conds, new funcs	*
C* M. desJardins/GSFC	12/90	Fixed calling sequence for ZMSL		*
C* M. desJardins/GSFC	10/91	Added sky cover parameters		*
C* K. Brill/NMC		10/91	Added cloud cover parameters		*
C* M. desJardins/NMC	11/91	Added PR_WIND				*
C* K. Brill/NMC		11/91	Added PR_PTND, PR_PTSY  and PR_P03C	*
C* K. Brill/NMC		11/91	Added PR_CTCC				*
C* K. Brill/NMC		12/91	Added PR_SKYX				*
C* S. Jacobs/EAI	 3/93	Added PR_HEAT, PR_HMTR, PR_WCEQ		*
C* S. Jacobs/NMC	 4/94	Added PR_TMWB				*
C* S. Jacobs/NCEP	11/96	Removed PR_RWSH, PR_6SYM		*
C* D. Kidwell/NCEP	 5/97	Added PR_VSKN                           *
C* T. Lee/GSC		11/97	Added PR_THWC				*
C* D. Kidwell/NCEP	 4/98	Added PR_HGSK, cleaned up               *
C* T. Piper/GSC		11/98	Updated prolog				*
C* D. Kidwell/NCEP	12/98	Added PR_WAVx, x = 2, 4, 5, and PR_WVDD *
C* D. Kidwell/NCEP	 2/99	Added PR_WPHM, PR_WVSW; PR_WVPH->PR_WPHF*
C* D. Kidwell/NCEP	 3/99	Added PR_DMAX, PR_DMIN                  *
C* S. Jacobs/NCEP	 3/99	Added PR_TPFR				*
C* A. Hardy/GSC		 4/99   Added PR_PR6X, PR_DPRN		        *
C* J. Green/AWC		 6/99   Added PR_XVFR                           *
C* G. Grosshans/SPC	10/99   Added PR_KNMH                           *
C* D. Kidwell/NCEP	 3/00	Added PR_IGRO                           *
C* D. Kidwell/NCEP	11/00	Added PR_PR24                           *
C* K. Brill/HPC		 9/01	Added PR_QUOT				*
C* T. Lee/SAIC		 9/01	Added PR_WCHT, PR_MHKN			*
C* D. Kidwell/NCEP	 5/03	Added PR_CMSL, PR_MOBS, PR_WCFR,        *
C*				PR_WXVF, PR_TPWN, PR_AWNM               *
C* T. Lee/SAIC		 6/03	Added PR_FOSB				*
C* D. Kidwell/NCEP	 8/03	Added PR_WCMS                           *
C* S. Chiswell/Unidata	 5/04	Added PR_PANY				*
C* T. Piper/SAIC	12/04	Check ipos for greater than zero	*
C* M. Li/SAIC		07/06   Added PR_CLDB				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	funcs (*)
	INTEGER		ipos ( idim, * ), iout (*), locout (*)
	REAL		data (*)
C*
	CHARACTER	fff*8, cvalue*20
 	LOGICAL		setlev
C-----------------------------------------------------------------------
	iret = 0
C
C*	Compute all the required functions.
C
	IF  ( setlev )  icndtp = 1
	DO  i = 1, nfunc
	  IF  ( setlev )  ilvprm = locout (i)
	  fff = funcs (i)
	  d1 = RMISSD
	  d2 = RMISSD
	  d3 = RMISSD
	  d4 = RMISSD
	  if ( ipos(1,i) .gt. 0 ) d1 = data (ipos (1,i) )
	  if ( ipos(2,i) .gt. 0 ) d2 = data (ipos (2,i) )
	  if ( ipos(3,i) .gt. 0 ) d3 = data (ipos (3,i) )
	  if ( ipos(4,i) .gt. 0 ) d4 = data (ipos (4,i) )
C*
	  IF  ( fff .eq. 'SAMEPARM' ) THEN
	    data ( iout (i) ) = d1
C*
	  ELSE IF ( fff .eq. ' ' )  THEN
C*
	  ELSE IF ( fff (1:2) .eq. 'PT' ) THEN
	    data ( iout (i) ) = d1
C*
	  ELSE IF ( fff .eq. 'PR_TMCF' ) THEN
	    data ( iout (i) ) = PR_TMCF ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_TMFC' ) THEN
	    data ( iout (i) ) = PR_TMFC ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_TMCK' ) THEN
	    data ( iout (i) ) = PR_TMCK ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_TMKC' ) THEN
	    data ( iout (i) ) = PR_TMKC ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_TMFK' ) THEN
	    data ( iout (i) ) = PR_TMFK ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_TMKF' ) THEN
	    data ( iout (i) ) = PR_TMKF ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_ALTI' ) THEN
	    data ( iout (i) ) = PR_ALTI ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_ALTM' ) THEN
	    data ( iout (i) ) = PR_ALTM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_PALT' ) THEN
	    data ( iout (i) ) = PR_PALT ( d1, telv )
C*
	  ELSE IF ( fff .eq. 'PR_PMSL' ) THEN
	    data ( iout (i) ) = PR_PMSL ( d1, d2, d3, telv )
C*
	  ELSE IF ( fff .eq. 'PR_PANY' ) THEN
	    data ( iout (i) ) = PR_PANY ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_AMSL' ) THEN
	    data ( iout (i) ) = PR_AMSL ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_ALTP' ) THEN
	    data ( iout (i) ) = PR_ALTP ( d1, telv )
C*
	  ELSE IF ( fff .eq. 'PR_SALI' ) THEN
	    data ( iout (i) ) = PR_SALI ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_DWDP' ) THEN
	    data ( iout (i) ) = PR_DWDP ( d1 , d2 )
C*
	  ELSE IF ( fff .eq. 'PR_RELH' ) THEN
	    data ( iout (i) ) = PR_RELH ( d1 , d2 )
C*
	  ELSE IF ( fff .eq. 'PR_RHDP' ) THEN
	    data ( iout (i) ) = PR_RHDP ( d1 , d2 )
C*
	  ELSE IF ( fff .eq. 'PR_MIXR' ) THEN
	    data ( iout (i) ) = PR_MIXR ( d1 , d2 )
C*
	  ELSE IF ( fff .eq. 'PR_THTA' ) THEN
	    data ( iout (i) ) = PR_THTA ( d1 , d2 )
C*
	  ELSE IF ( fff .eq. 'PR_THTE' ) THEN
	    data ( iout (i) ) = PR_THTE ( d1 , d2 , d3 )
C*
	  ELSE IF ( fff .eq. 'PR_DRCT' ) THEN
	    data ( iout (i) ) = PR_DRCT ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_SPED' ) THEN
	    data ( iout (i) ) = PR_SPED ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_UWND' ) THEN
	    data ( iout (i) ) = PR_UWND ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_VWND' ) THEN
	    data ( iout (i) ) = PR_VWND ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WIND' ) THEN
	    data ( iout (i) ) = PR_WIND ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WCMP' ) THEN
	    CALL PC_GCND ( '$', 1, dcmp, cvalue, ier )
	    IF  ( ier .ne. 0 )  dcmp = 90.
	    data ( iout (i) ) = PR_WCMP ( d1, d2, dcmp )
C*
	  ELSE IF ( fff .eq. 'PR_WNML' ) THEN
	    CALL PC_GCND ( '$', 1, dcmp, cvalue, ier )
	    IF  ( ier .ne. 0 )  dcmp = 90.
	    data ( iout (i) ) = PR_WNML ( d1, d2, dcmp )
C*
	  ELSE IF ( fff .eq. 'PR_KNMS' ) THEN
	    data ( iout (i) ) = PR_KNMS ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_KNMH' ) THEN
	    data ( iout (i) ) = PR_KNMH ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_MHKN' ) THEN
	    data ( iout (i) ) = PR_MHKN ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_MSKN' ) THEN
	    data ( iout (i) ) = PR_MSKN ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_PRES' ) THEN
	    data ( iout (i) ) = PR_PRES ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_TMPK' ) THEN
	    data ( iout (i) ) = PR_TMPK ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_HEAT' ) THEN
	    data ( iout (i) ) = PR_HEAT ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_HMTR' ) THEN
	    data ( iout (i) ) = PR_HMTR ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WCEQ' ) THEN
	    data ( iout (i) ) = PR_WCEQ ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WCHT' ) THEN
	    data ( iout (i) ) = PR_WCHT ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_DWPT' ) THEN
	    data ( iout (i) ) = PR_DWPT ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_TLCL' ) THEN
	    data ( iout (i) ) = PR_TLCL ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_PLCL' ) THEN
	    data ( iout (i) ) = PR_PLCL ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_PKDD' ) THEN
	    data ( iout (i) ) = PR_PKDD ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_PKSS' ) THEN
	    data ( iout (i) ) = PR_PKSS ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_PSPD' ) THEN
	    data ( iout (i) ) = PR_PSPD ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_HGFM' ) THEN
	    data ( iout (i) ) = PR_HGFM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGMF' ) THEN
	    data ( iout (i) ) = PR_HGMF ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_LATI' ) THEN
	    data ( iout (i) ) = PR_LATI ( tlat, tlon, d1, d2, telv )
C*
	  ELSE IF ( fff .eq. 'PR_LONI' ) THEN
	    data ( iout (i) ) = PR_LONI ( tlat, tlon, d1, d2, telv )
C*
	  ELSE IF ( fff .eq. 'PR_LHVP' ) THEN
	    data ( iout (i) ) = PR_LHVP ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_DDEN' ) THEN
	    data ( iout (i) ) = PR_DDEN ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_TVRK' ) THEN
	    data ( iout (i) ) = PR_TVRK ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_VAPR' ) THEN
	    data ( iout (i) ) = PR_VAPR ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CMBC' ) THEN
	    data ( iout (i) ) = PR_CMBC ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_CLOA' ) THEN
	    data ( iout (i) ) = PR_CLOA ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CLCT' ) THEN
	    data ( iout (i) ) = PR_CLCT ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_CTCC' ) THEN
	    data ( iout (i) ) = PR_CTCC ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_CFCT' ) THEN
	    data ( iout (i) ) = PR_CFCT ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CTCF' ) THEN
	    data ( iout (i) ) = PR_CTCF ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_SKYX' ) THEN
	    data ( iout (i) ) = PR_SKYX ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_WCCV' ) THEN
	    data ( iout (i) ) = PR_WCCV ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CSYL' ) THEN
	    data ( iout (i) ) = PR_CSYL ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CSYM' ) THEN
	    data ( iout (i) ) = PR_CSYM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CSYH' ) THEN
	    data ( iout (i) ) = PR_CSYH ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CSYT' ) THEN
	    data ( iout (i) ) = PR_CSYT ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_PTSY' ) THEN
	    data ( iout (i) ) = PR_PTSY ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_P03C' ) THEN
	    data ( iout (i) ) = PR_P03C ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGKM' ) THEN
	    data ( iout (i) ) = PR_HGKM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGMD' ) THEN
	    data ( iout (i) ) = PR_HGMD ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGMK' ) THEN
	    data ( iout (i) ) = PR_HGMK ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_COMX' ) THEN
	    data ( iout (i) ) = PR_COMX ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_DDEP' ) THEN
	    data ( iout (i) ) = PR_DDEP ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_ZMSL' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 1013.25 )
C*
	  ELSE IF ( fff .eq. 'PR_Z000' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 1000.0 )
C*
	  ELSE IF ( fff .eq. 'PR_Z950' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 950.0 )
C*
	  ELSE IF ( fff .eq. 'PR_Z900' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 900.0 )
C*
	  ELSE IF ( fff .eq. 'PR_Z850' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 850.0 )
C*
	  ELSE IF ( fff .eq. 'PR_Z800' ) THEN
	    data ( iout (i) ) = PR_ZALT ( d1, 800.0 )
C*
          ELSE IF ( fff .eq. 'PR_HGFS' ) THEN
	    data ( iout (i) ) = PR_HGFS ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGSF' ) THEN
            data ( iout (i) ) = PR_HGSF ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_D100' ) THEN
 	    data ( iout (i) ) = PR_D100 ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_M100' ) THEN
	    data ( iout (i) ) = PR_M100 ( d1 ) 
C*
	  ELSE IF ( fff .eq. 'PR_INMM' ) THEN
	    data ( iout (i) ) = PR_INMM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_MMIN' ) THEN
	    data ( iout (i) ) = PR_MMIN ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGKS' ) THEN
C
C*	    Statute miles to feet; then feet to meters; finally meters
C*	    to kilometers.
C
 	    d1 = PR_HGSF ( d1 )
	    d1 = PR_HGFM ( d1 )
	    data ( iout (i) ) = PR_HGMK ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_HGSK' ) THEN
C
C*	    Kilometers to meters; then meters to feet; finally feet
C*	    to statute miles.
C
	    d1 = PR_HGKM ( d1 )
	    d1 = PR_HGMF ( d1 )
	    data ( iout (i) ) = PR_HGFS ( d1 )
C*
          ELSE IF ( fff .eq. 'PR_COMT' ) THEN
	    data ( iout (i) ) = PR_COMT ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_COML' ) THEN
	    data ( iout (i) ) = PR_COML ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_COMM' ) THEN
	    data ( iout (i) ) = PR_COMM ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_COMH' ) THEN
	    data ( iout (i) ) = PR_COMH ( d1, d2, d3 )
C*
          ELSE IF ( fff .eq. 'PR_CLDB' ) THEN
            data ( iout (i) ) = PR_CLDB ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_CLCX' ) THEN
	    data ( iout (i) ) = PR_CLCX ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_CLHX' ) THEN
	    data ( iout (i) ) = PR_CLHX ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_NSYM' ) THEN
	    data ( iout (i) ) = PR_NSYM ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_LTMP' ) THEN
	    CALL PC_GCND  ( '!', 1, pres, cvalue, ier )
	    IF  ( ( pres .le. 0. ) .or. ( ier .ne. 0 ) )  pres = 500.
	    data ( iout (i) ) = PR_LTMP ( d1, d2, pres )
C*
	  ELSE IF ( fff .eq. 'PR_STDZ' ) THEN
	    data ( iout (i) ) = PR_STDZ ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_THWC' ) THEN
	    data ( iout (i) ) = PR_THWC ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_TMWB' ) THEN
	    data ( iout (i) ) = PR_TMWB ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_VSKN' ) THEN
	    data ( iout (i) ) = PR_VSKN ( d1 )
C*
	  ELSE IF ( fff .eq. 'PR_WAV2' ) THEN
	    data ( iout (i) ) = PR_WPHF ( d1, d2, 2. )
C*
	  ELSE IF ( fff .eq. 'PR_WAV4' ) THEN
	    data ( iout (i) ) = PR_WPHF ( d1, d2, 4. )
C*
	  ELSE IF ( fff .eq. 'PR_WAV5' ) THEN
	    data ( iout (i) ) = PR_WPHF ( d1, d2, 5. )
C*
	  ELSE IF ( fff .eq. 'PR_WVDD' ) THEN
	    data ( iout (i) ) = PR_WVDD ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WPHM' ) THEN
	    data ( iout (i) ) = PR_WPHM ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_WVSW' ) THEN
	    data ( iout (i) ) = PR_WVSW ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_DMAX' ) THEN
	    data ( iout (i) ) = PR_DMAX ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_DMIN' ) THEN
	    data ( iout (i) ) = PR_DMIN ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_TPFR' ) THEN
	    data ( iout (i) ) = PR_TPFR ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_PR6X' ) THEN
	    data ( iout (i) ) = PR_PR6X ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_DPRN' ) THEN
	    data ( iout (i) ) = PR_PR6X ( d1, d2, RMISSD, RMISSD )
C*
	  ELSE IF ( fff .eq. 'PR_XVFR' ) THEN
	    data ( iout (i) ) = PR_XVFR ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_IGRO' ) THEN
	    data ( iout (i) ) = PR_IGRO ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_PR24' ) THEN
	    data ( iout (i) ) = PR_PR24 ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_QUOT' ) THEN
	    data ( iout (i) ) = PR_QUOT ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_CMSL' ) THEN
	    data ( iout (i) ) = PR_CMSL ( d1, telv )
C*
	  ELSE IF ( fff .eq. 'PR_MOBS' ) THEN
	    data ( iout (i) ) = PR_MOBS ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_WCFR' ) THEN
	    data ( iout (i) ) = PR_WCFR ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_WXVF' ) THEN
	    data ( iout (i) ) = PR_WXVF ( d1, d2 )
C*
	  ELSE IF ( fff .eq. 'PR_TPWN' ) THEN
	    data ( iout (i) ) = PR_TPWN ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_AWNM' ) THEN
	    data ( iout (i) ) = PR_AWNM ( d1, d2, d3, d4 )
C*
	  ELSE IF ( fff .eq. 'PR_FOSB' ) THEN
	    data ( iout (i) ) = PR_FOSB ( d1, d2, d3 )
C*
	  ELSE IF ( fff .eq. 'PR_WCMS' ) THEN
	    data ( iout (i) ) = PR_WCMS ( d1, d2 )
C*
	  ELSE
	    data ( iout (i) ) = RMISSD
	  END IF
	END DO
C*
	RETURN
	END
