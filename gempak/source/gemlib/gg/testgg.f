	PROGRAM TESTGG
C************************************************************************
C* TESTGG								*
C*									*
C* This program tests the GG library subroutines.			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/85						*
C* S. Jacobs/NMC	 7/94	Removed AOI projection			*
C* S. Jacobs/NMC	 8/94	Added GG_RVEC				*
C* M. Linda/GSC		12/95	Removed GG_SMAP and GG_SNPG		*
C* D. Keiser/GSC	12/95	Added GG_SPLT				*
C* S. Jacobs/NCEP	 3/96	Added GG_MOTF				*
C* S. Jacobs/NCEP	 3/96	Updated call to GG_MAPS			*
C* G. Krueger/EAI	 5/96	Corrected GG_MAPS & GG_SSAT usage	*
C* S. Jacobs/NCEP	 7/96	Removed GG_SSAT				*
C* S. Maxwell/GSC	 1/97	Added GG_ZARE				*
C* S. Maxwell/GSC	 1/97	Added GG_DLTN and GG_DMAP		*
C* D. Kidwell/NCEP	 4/99	Added GG_WLBL				*
C* S. Jacobs/NCEP	 5/99	Added map point filter flag		*
C* S. Jacobs/NCEP	 5/99	Added GG_WTCH and GG_WARN		*
C* A. Hardy/GSC		 6/99   Added two flags to GG_WTCH and GG_WARN  *
C* S. Jacobs/NCEP	 6/99	Added GG_CPNL				*
C* S. Jacobs/NCEP	 8/99	Added status line color to GG_WTCH	*
C* A. Hardy/GSC		 9/99   Added GG_HRCN				*
C* D. Kidwell/NCEP	10/99	Added GG_ISIG				*
C* D. Kidwell/NCEP	11/99	Added GG_CTRD				*
C* D. Kidwell/NCEP	11/99	Modified GG_ISIG calling sequence       *
C* D. Kidwell/NCEP	12/99	Added GG_LTNG				*
C* D. Kidwell/NCEP	 2/00	Modified GG_LTNG calling sequence       *
C* S. Jacobs/NCEP	 3/00	Changed calls to GG_WTCH, GG_WARN,	*
C*				GG_HRCN, GG_ISIG			*
C* F. J. Yen/NCEP	 5/00	Added VA and MW to GG_ISIG		*
C* D. Kidwell/NCEP	 5/00	Added some attributes for VA and MW     *
C* D. Kidwell/NCEP	 6/00	Added GG_ATCF, GG_MISC                  *
C* S. Jacobs/NCEP	 6/00	Added GQMPRJ				*
C* D. Kidwell/NCEP	 7/00	Added GG_AIRM                           *
C* A. Hardy/GSC		 8/00   Added tropical cyclone info to GG_ISIG  *
C* D. Kidwell/NCEP	 8/00	Added GG_NCON                           *
C* A. Hardy/GSC		12/00   Added lat/lon label locations, format	*
C* F. J. Yen/NCEP	12/00	Added handling of SLS watches and	*
C*			     	updated GG_MISC for airmets and non_conv*
C* F. J. Yen/NCEP	 5/01	Added GG_NATC				*
C* S. Jacobs/NCEP	 5/01	Added GG_ARC				*
C* A. Hardy/GSC		 6/01   Added GG_WCUR and GG_INFO		*
C* D. Kidwell/NCEP	 7/01	Added GG_TCMG                           *
C* S. Jacobs/NCEP	 7/01	Added GG_QSCT				*
C* F. J. Yen/NCEP	 1/02	Added more phenomena to GG_ISIG		*
C* R. Tian/SAIC		 2/02	Added a new argument for GG_WARN	*
C* D. Kidwell/NCEP	 2/02	Changed GG_TCMG call sequence           *
C* A. Hardy/NCEP	 8/02	Added GG_CSIG				*
C* M. Li/SAIC		 8/02	Added GG_WSTM				*
C* D.W.Plummer/NCEP	 9/02	Added fill option for GG_WARN & GG_WSTM	*
C* D.Kidwell/NCEP	11/02	Changed fill to outline                 *
C* A. Hardy/NCEP         2/03   Added GG_WWCR 				*
C* A. Hardy/NCEP         3/03   Changed calling sequence for GG_WWCR	*
C* M. Li/SAIC		 4/03	Add the second color for QSCT		*
C* M. Li/SAIC		 5/03	Added time stamp for QSCT		*
C* M. Li/SAIC		 5/03	Added color2 and level filter to AIRM	*
C* M. Li/SAIC            5/03   Added data filter to NCON               *
C* A. Hardy/NCEP         7/03   Added 'mndif' to GG_WWCR		*
C* A. Hardy/NCEP         7/03   Added GG_WCCK and GG_WCVF		*
C* M. Li/SAIC		 8/03	Added QSCT new format and SEAWINDS data	*
C* m.gamazaychikov/SAIC	10/03	Removed the call to GG_ATCF		*
C* F. J. Yen/NCEP	10/03	Added low level wind shear to GG_ISIG	*
C* m.gamazaychikov/SAIC	01/04	Made changes for new type SW in ggairm	*
C* m.gamazaychikov/SAIC	01/04	Updated the changes to GG_MISC		*
C* A. Hardy/NCEP	 1/04   Added parms to GG_WCCK and GG_WWCR	*
C* B. Yin/SAIC		 3/04	Changed SS_GTIM to CSS_GTIM		*
C* A. Hardy/NCEP	 3/04	Added GG_ZSRT				*
C* F. J. Yen/NCEP	 6/04	Added arrows for QSCT and removed SEAWND*
C* F. J. Yen/NCEP	 6/04	Added 1-hr and 2-hr extrapolated CSIG	*
C* B. Yin/SAIC		 7/04	Added forecast hour flags for HRCN	*
C* F. J. Yen/NCEP	 7/04   Added AMBG1, AMBG2, AMBG3, and AMBG4.   *
C* m.gamazaychikov/SAIC 08/04   Added new Issue Time flag for GG_AIRM   *
C* T. Piper/SAIC	08/04	Added GG_SCAL				*
C* M. Li/SAIC		10/04	Added outline widths for GG_WSTM	*
C* M. Li/SAIC		10/04	Added color numbers for QSCT wind & rain*
C* M. Li/SAIC		12/04	Modified watch flags			*
C* M. Li/SAIC		01/05   Add WOU and WCN; modified GG_MISC flags	*
C* F. J. Yen/NCEP	 4/05   Added WCP				*
C* m.gamazaychikov/SAIC	04/05	Changed GG_MISC CS, prompts for GG_NATC *
C* F. J. Yen/NCEP        4/05   Added union flag for WOU and WCN        *
C* F. J. Yen/NCEP        5/05   Comment out union flag for WOU and WCN  *
C* S. Jacobs/NCEP	 6/05	Added iznflg to GG_WCCK call		*
C* S. Gilbert/NCEP	 6/05	Added prompt for rain circle for GG_QSCT*
C* F. J. Yen/NCEP        7/05   Enabled union flag for WOU and WCN	*
C* m.gamazaychikov/SAIC	02/06	Changed CS and prompts for GG_NATC 	*
C* m.gamazaychikov/SAIC	02/06	Added WACT and WFPS			*
C* m.gamazaychikov/SAIC	03/06	Made changes to WACT and WFPS prompts	*
C* C. Bailey/HPC	 4/06	Added FFA, modified GG_MISC		*
C* S. Gilbert/NCEP       5/06   Added new fcst hours for HRCN data      *
C* F. J. Yen/NCEP        3/07   Increased dim. of sacod from 20 to 100;	*
C*				Replaced dim. LLSTFL & 400 with MAX_CNTY*
C*				Added GG_WWFO				*
C* H. Zeng/SAIC         06/07   changed CS to gg_natc                   *
C* T. Piper/SAIC	06/07	Added storm-based warning flag to WARN	*
C* F. J. Yen/NCEP	 7/07	Initialize prefs.tbl due to GG_QSCT mods*
C* T. Piper/SAIC	01/08	Added GD_INIT; removed from IN_BDTA	*
C* T. Piper/SAIC	08/08	Increased array sizes for GG_NATC to 25	*
C* S. Jacobs/NCEP	12/09	Added TRAK1, TRAKE and TRAK2		*
C* L. Hinson/AWC        06/12   Add ASDI                                *
C* L. Hinson/AWC        10/12   Add EDR                                 *
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ggcmn.cmn'
C*
	CHARACTER 	device*72, proj*72, garea*72, map*72, alias*4
	CHARACTER	str*80, cprj*72, latlon*72, clrbar*72, text*72
	CHARACTER	satfil*72, refvec*72, string*12, stnplt*72
	CHARACTER	mapfil*72, endtim*20, frametim*20, strnam*72
        CHARACTER       modls (20)*20
	CHARACTER	sys*1, systim*20, attnln*180, datatyp*10
        CHARACTER       wtnm*4, type*3, tbeg*20, tstp*20, stzstr*256 
        CHARACTER       tzone(100)*3, tmissu(100)*20, tmstrt(100)*20,
     +			tmstp(100)*20, cnty(MAX_CNTY)*6, mscale*(LLMXLN)
	CHARACTER	filtyp*20, emodls (20)*20, usrtim*20,strtim*20,
     +                  cnties(MAX_CNTY)*128, wfocty(MAX_CNTY)*128,
     +                  ircntm*20, sacod(100)*4, answ*1, usrtim2*20,
     +			wtnum*120, wfolst(180)
        CHARACTER       mode*2, depdest*2, sites*125 
        REAL            xlat(20), xlon(20), wlevs(4) 
	REAL		angle (3), zmarg (4), flvl (50), x(2), y(2),
     +			dlltln(2), alat (7), alon (7), ppmark (3), 
     +			pnmark (3), ssize(25), arwsiz(20), ahdsiz(20),
     +                  aloc (2), xout(100), yout(100)
	INTEGER		ifcolr (50), ifreq(2), itimes (50), isorc(5),
     +			iclrs(20), mrktyp(25), iwidth(25), iflags(22),
     +			iawdth(20), lwidth(25),icolr2 (20),ifcolr2(50),
     +			icolr3 (20), icolr4 (20), icolr5 (25),
     +			icolr6 (2,20), icolrgam(2,20), icolr7 (20),
     +                  icolr8(20),lvfil(2), icolr9 (20), icolr10 (20), 
     +                  icolr11 (20), icolr12(20), icolr13 (20), 
     +                  iuwtch(1000), icolr14(20), icolr15(20), 
     +                  icolr16(20), icolr17(20), iclr18(20), 
     +                  iclr19 (20), iclr20(20), iclr1k(20), iclrek(20), 
     +                  iclr2k(20), wcnty(MAX_CNTY)
        INTEGER         ktminc(2), kcolrs(2), htinc(2), htcolrs(2),
     +                  numc, tlimit, ecolrs(2), esymb1(2), esymb2(2),
     +                  esymbsz1(2), esymbsz2(2), enumc
        REAL            evinc(2)
	LOGICAL		ang, etmchg, aoa180fl
	INTEGER		nums, itype
C------------------------------------------------------------------------
C*      Initialize GEMPAK common blocks 
C
        CALL IN_BDTA  ( iret )
C
C*      Initialize grid library common area grdcmn.cmn
C
        CALL GD_INIT  ( iret )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE(6,20)
20	    FORMAT
     +      ('  1 = GG_INIT   2 = GG_SDEV   3 = GG_PROJ   4 = GG_MOTF'/
     +       '  5 = GG_MAP    6 = GG_PANL   7 = GG_BOX    8 = GG_WSTR'/
     +       '  9 = GG_SGRF  10 = GG_ZARE  11 = GG_LTLN  12 = GG_CBAR'/
     +       ' 13 = GG_MAPS  14 = GG_DMAP  15 = GG_RVEC  16 = GG_SPLT'/
     +       ' 17 = GG_DLTN  18 = GG_WLBL  19 = GG_WTCH  20 = GG_WARN'/
     +       ' 21 = GG_HRCN  22 = GG_CPNL  23 = GG_ISIG  24 = GG_CTRD'/
     +       ' 25 = GG_LTNG  26 = GG_SCAL  27 = GG_MISC  28 = GG_AIRM'/
     +       ' 29 = GG_GAIRM 30 = GG_NCON  31 = GG_NATC  32 = GG_ARC'/
     +       ' 33 = GG_WCUR  34 = GG_INFO  35 = GG_TCMG  36 = GG_QSCT'/
     +       ' 37 = GG_CSIG  38 = GG_WSTM  39 = GG_WWCR  40 = GG_WCCK'/
     +       ' 41 = GG_WCVF  42 = GG_ZSRT  43 = GG_WWOU/GG_WWCN'/
     +       ' 44 = GG_WACT  45 = GG_WFPS  46 = GG_WWFO'/
     +       ' 50 = GCLEAR   51 = IN_TEXT  52 = GQMPRJ   53 = GG_WCP'/
     +       ' 54 = GG_FFA   55 = GG_ASDI  56 = GG_EDR')
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier ) 
	IF ( ier .eq. 2 ) THEN
	   iostat = -1
	   numsub = -1
	END IF
C
C*      Initialize preferences table.
C
        CALL CTB_PFREAD  ( iret )
        IF  ( iret .ne. 0 )  THEN
            CALL ER_WMSG  ( 'GPMAP', -1, ' ', ier )
            CALL SS_EXIT
        END IF
C------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
		WRITE(6,*)' Enter mode'
		READ (5, *) mode
		CALL GG_INIT ( mode, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
		WRITE(6,*)' Enter device name'
		READ (5, 2) device
		CALL GG_SDEV ( device, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
	        WRITE (6,*)' Enter PROJ'
	        READ (5, 2) proj
	        CALL GG_PROJ  ( proj, cprj, angle, zmarg, ang, iret )
	        WRITE (6,*) ' PROJ:  ', cprj
	        WRITE (6, *) 'ANGLE: ', angle
	        WRITE (6, *) 'ZMARG: ', zmarg
	        WRITE (6, *) 'ANGFLG: ', ang
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 4) THEN
		WRITE(6,*)' Enter window name'
		READ (5, 2) device
		CALL GG_MOTF ( device, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 5) THEN
		WRITE(6,*)' Enter color/line type/line width/filt flg'
		READ (5, 2) map
		CALL GG_MAP ( map, iret )			
	        CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
	        WRITE (6,*) 'Enter panel'
	        READ (5,2) str
	        CALL GG_PANL ( str, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 7) THEN
	        WRITE (6,*)' Enter an region name (D, N, V, P) '
	        READ (5, 2) proj
	        WRITE (6,*)' Enter icolor, ilntyp, ilnwid'
	        READ (5,*) icolor, ilntyp, ilnwid
	        CALL GG_BOX (proj, icolor, ilntyp, ilnwid, ier)
	        CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 8) THEN
		WRITE (6,*) 'Enter string'
		READ (5,2) str
	        WRITE(6,*)' Enter line no'
	        READ (5,*) ilin
		CALL GG_WSTR ( str, ilin,  iret )			
	        CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 9) THEN
	        WRITE (6,*)' Enter a graphics area:'
	        READ (5, 2) garea
	        WRITE (6,*)' Enter projection'
	        READ (5, 2) proj
	        CALL GG_SGRF (proj, garea, iret)
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10) THEN
		WRITE (6,*) 'Enter x(1)'
		READ  (5,*)  x(1) 
		WRITE (6,*) 'Enter y(1)'
		READ  (5,*)  y(1) 
		WRITE (6,*) 'Enter x(2)'
		READ  (5,*)  x(2) 
		WRITE (6,*) 'Enter y(2)'
		READ  (5,*)  y(2) 
		CALL GG_ZARE  ( x, y, garea, proj, iret )
		WRITE (6,*) garea, proj, iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 11) THEN
		WRITE (6,*) 'Enter LATLON'
		READ  (5,2)  latlon
		CALL GG_LTLN  ( latlon, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL GEPLOT (ier)
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 12) THEN
		DO  mm = 1, 50
		    flvl (mm)   = -9999.0
		    ifcolr (mm) = 0
		END DO
		WRITE (6,*) 'Enter CLRBAR'
		READ  (5,2)  clrbar
		WRITE (6,*) 'Enter the number of levels'
		READ  (5,*)  nflvl
		WRITE (6,*) 'Enter ', nflvl, ' level values'
		READ  (5,*) ( flvl (mm), mm = 1, nflvl )
		WRITE (6,*) 'Enter ', nflvl+1, ' level colors'
		READ  (5,*) ( ifcolr (mm), mm = 1, nflvl+1 )
		CALL GG_CBAR  ( clrbar, nflvl, flvl, ifcolr, ier )
	        CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 13 ) THEN
		WRITE(6,*)' Enter projection'
		READ (5, 2) proj
		WRITE(6,*)' Enter graphics area'
		READ (5, 2) garea
		WRITE(6,*)' Enter satellite filename'
		READ (5, 2) satfil
		CALL GG_MAPS ( proj, garea, satfil, idrpfl, iret )
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'IDRPFL = ', idrpfl
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 14 ) THEN
		WRITE (6,*) 'Enter map file name:'
		READ  (5,2)  mapfil
		WRITE (6,*) 'Enter map line color:'
		READ  (5,*)  mapcol
		WRITE (6,*) 'Enter map line type:'
		READ  (5,*)  maptyp
		WRITE (6,*) 'Enter map line width:'
		READ  (5,*)  mapwid
		WRITE (6,*) 'Enter map point filter flag (0=NO,1=YES):'
		READ  (5,*)  mapflt
		CALL GG_DMAP ( mapfil, mapcol, maptyp, mapwid, mapflt,
     +			       iret )
	        CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 15 ) THEN
		WRITE (6,*) 'Enter REFVEC'
		READ  (5,2)  refvec
		WRITE (6,*) 'Enter the default text string'
		READ  (5,2)  string
		CALL GG_RVEC  ( refvec, string, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL GEPLOT (ier)
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 16 ) THEN
		WRITE (6,*) 'Enter STNPLT'
		READ  (5,2) stnplt
		CALL GG_SPLT ( stnplt, iret )
		CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 17) THEN
                WRITE (6,*) 'Enter line color:'
                READ  (5,*)  lincol
                WRITE (6,*) 'Enter line type:'
                READ  (5,*)  lintyp
                WRITE (6,*) 'Enter line width:'
                READ  (5,*)  linwid
                WRITE (6,*) 'Enter frequency of labels:'
                READ  (5,*)  ifreq(1), ifreq(2)
                WRITE (6,*) 'Enter frequency of lat/lon lines:'
                READ  (5,*)  dlltln(1), dlltln(2)
                WRITE (6,*) 'Enter lat/lon format :'
                READ  (5,*)  ifrmat
                WRITE (6,*) 'Enter lat/lon label locations:'
                READ  (5,*)  aloc(1), aloc(2)
                CALL GG_DLTN ( lincol, lintyp, linwid, ifreq, dlltln,
     +                         aloc, ifrmat, iret )
		CALL GEPLOT ( ier )
                WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG  ( 'IN', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 18) THEN
		WRITE (6,*) 'Enter number of points for bounded area'
		READ  (5,*)  np 
		DO  mm = 1, np
		    WRITE (6,*) 'Enter latitude and longitude', mm
		    READ  (5,*)  alat (mm), alon (mm)
		END DO
		CALL GG_WLBL  ( np, alat, alon, alat, alon, iret )
		WRITE (6,*) 'ALAT, ALON, IRET = ', alat, alon, iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 19) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*) 'Enter color for thunderstorm watches'
		READ  (5,*)  iclrs(1) 
		WRITE (6,*) 'Enter width for thunderstorm watches'
		READ  (5,*)  iwidth(1) 
C
		WRITE (6,*) 'Enter color for tornado watches'
		READ  (5,*)  iclrs(2) 
		WRITE (6,*) 'Enter width for tornado watches'
		READ  (5,*)  iwidth(2) 
C
		WRITE (6,*) 'Enter color for the status lines'
		READ  (5,*)  iclrs(3) 
		WRITE (6,*) 'Enter width for the status lines'
		READ  (5,*)  iwidth(3) 
		WRITE (6,*) 'Enter size for the status lines'
		READ  (5,*)  ssize(3) 
C
		WRITE (6,*) 'Enter flag for watch box times (0=no,1=yes)'
		READ  (5,*)  iflags(1) 
		WRITE (6,*) 'Enter flag for status line time(0=no,1=yes)'
		READ  (5,*)  iflags(2)
		WRITE (6,*) 'Enter flag for watch box number(0=no,1=yes)'
		READ  (5,*)  iflags(3)
                WRITE (6,*) 'Enter flag for status line # (0=no,1=yes)'
                READ  (5,*)  iflags(4)
                WRITE (6,*) 'Enter flag for most recent status line'
                READ  (5,*)  iflags(6)
C
		CALL GG_WCUR  ( endtim, iuwtch, iunum, iret )
		CALL GG_WTCH  ( endtim, iclrs, ssize, iwidth, 
     +                          iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
                nums = 2
		WRITE (6,*) 'Enter type (1 for warning, 2 for SLS)'
		READ  (5,*)  ifltyp
		IF ( ifltyp .ne. 1 .and. ifltyp .ne. 2 ) THEN
		    WRITE (6,*) 'Invalid type.  Assume type is warning'
		    ifltyp = 1
		END IF
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*)
     +  'Enter color for thunderstorm warnings or SLS watches'
		READ  (5,*)  icolr2(1) 
		WRITE (6,*) 'Enter marker type (17-21) for t-storm'
		READ  (5,*)  mrktyp(1)
		WRITE (6,*) 'Enter marker size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter marker width'
		READ  (5,*)  iwidth(1)
C
		WRITE (6,*) 'Enter color for tornado warnings or SLS watches'
		READ  (5,*)  icolr2(2) 
		WRITE (6,*) 'Enter marker type (17-21) for tornado'
		READ  (5,*)  mrktyp(2)
		WRITE (6,*) 'Enter marker size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter marker width'
		READ  (5,*)  iwidth(2)
C
		IF ( ifltyp .eq. 1 ) THEN
		    alias = 'WARN'
		    nums = 3
		    WRITE (6,*) 'Enter color for flash flood warnings'
		    READ  (5,*)  icolr2(3) 
		    WRITE (6,*) 'Enter marker type (17-21) for flash flood'
		    READ  (5,*)  mrktyp(3)
		    WRITE (6,*) 'Enter marker size'
		    READ  (5,*)  ssize(3)
		    WRITE (6,*) 'Enter marker width'
		    READ  (5,*)  iwidth(3)
		  ELSE
		    alias = 'SVRL'
		END IF
C
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(1) 
		WRITE (6,*) 'Enter flag for label (0=no,1=yes)'
		READ  (5,*)  iflags(2)
		WRITE (6,*) 'Enter flag for outline (0=no,1=yes)'
		READ  (5,*)  iflags(3)
		WRITE (6,*) 'Enter flag for storm-based warnings
     + (0=no,1=yes)'
		READ  (5,*)  iflags(4)
C
		CALL GG_WARN  ( alias, endtim, nums, icolr2, mrktyp, ssize,
     +				iwidth, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*) 'Enter color for tropical depressions'
		READ  (5,*)  icolr3(1) 
		WRITE (6,*) 'Enter special symbol type (1-40) for '
     +                       // 'tropical depressions '
		READ  (5,*)  mrktyp(1)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(1)
C
		WRITE (6,*) 'Enter color for tropical storms'
		READ  (5,*)  icolr3(2) 
		WRITE (6,*) 'Enter special symbol type (1-40) for '
     +                       // 'tropical storms'
		READ  (5,*)  mrktyp(2)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(2)
C
		WRITE (6,*) 'Enter color for hurricanes'
		READ  (5,*)  icolr3(3) 
		WRITE (6,*) 'Enter special symbol type (1-40) for ' 
     +                       // 'hurricanes '
		READ  (5,*)  mrktyp(3)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(3)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(3)
C
		WRITE (6,*) 'Enter color for directional arrow'
		READ  (5,*)  icolr3(4) 
		WRITE (6,*) 'Enter arrow size'
		READ  (5,*)  arwsiz(4)
		WRITE (6,*) 'Enter arrow head size'
		READ  (5,*)  ahdsiz(4)
		WRITE (6,*) 'Enter arrow width'
		READ  (5,*)  iawdth(4)
C
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(1) 
		WRITE (6,*) 'Enter flag for label (0=no,1=yes)'
		READ  (5,*)  iflags(2)
		WRITE (6,*) 'Enter flag for motion (0=no,1=yes)'
		READ  (5,*)  iflags(3)
		WRITE (6,*) 'Enter flag for wind/sea quadrant (0=no,1=yes)'
		READ  (5,*)  iflags(4)
		WRITE (6,*) 'Enter flag for 00 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(5)
		WRITE (6,*) 'Enter flag for 06 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(6)
		WRITE (6,*) 'Enter flag for 12 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(7)
		WRITE (6,*) 'Enter flag for 18 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(8)
		WRITE (6,*) 'Enter flag for 24 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(9)
		WRITE (6,*) 'Enter flag for 30 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(10)
		WRITE (6,*) 'Enter flag for 36 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(11)
		WRITE (6,*) 'Enter flag for 42 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(12)
		WRITE (6,*) 'Enter flag for 48 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(13)
		WRITE (6,*) 'Enter flag for 54 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(14)
		WRITE (6,*) 'Enter flag for 60 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(15)
		WRITE (6,*) 'Enter flag for 66 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(16)
		WRITE (6,*) 'Enter flag for 72 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(17)
		WRITE (6,*) 'Enter flag for 78 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(18)
		WRITE (6,*) 'Enter flag for 84 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(19)
		WRITE (6,*) 'Enter flag for 90 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(20)
		WRITE (6,*) 'Enter flag for 96 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(21)
		WRITE (6,*) 'Enter flag for 120 hour forecast (0=no,1=yes)'
		READ  (5,*)  iflags(22)
C
		WRITE (6,*) 'Enter the storm name'
		READ  (5,2)  strnam
C
		CALL GG_HRCN  ( endtim, icolr3, mrktyp, ssize, iwidth,
     +				arwsiz, ahdsiz, iawdth, iflags,
     +				strnam, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 22) THEN
		WRITE (6,*) 'Enter panel'
		READ (5,2) str
		CALL GG_CPNL ( str, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 23) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter color for thunderstorms'
		READ  (5,*)  icolr4(1)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(1)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(1)
C
		WRITE (6,*) 'Enter color for turbulence'
		READ  (5,*)  icolr4(2)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(2)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(2)
C
		WRITE (6,*) 'Enter color for hurricanes'
		READ  (5,*)  icolr4(3)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(3)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(3)
C
		WRITE (6,*) 'Enter color for tropical storms'
		READ  (5,*)  icolr4(4)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(4)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(4)
C
		WRITE (6,*) 'Enter color for tropical depressions'
		READ  (5,*)  icolr4(5)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(5)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(5)
C
		WRITE (6,*) 'Enter color for volcanic ash'
		READ  (5,*)  icolr4(6)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(6)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(6)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(6)
C
		WRITE (6,*) 'Enter color for marked mountain waves'
		READ  (5,*)  icolr4(7)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(7)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(7)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(7)
C
		WRITE (6,*) 'Enter color for tropical cyclones'
		READ  (5,*)  icolr4(8)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(8)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(8)
C
		WRITE (6,*) 'Enter color for squall lines'
		READ  (5,*)  icolr4(9)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(9)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(9)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(9)
C
		WRITE (6,*) 'Enter color for CAT'
		READ  (5,*)  icolr4(10)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(10)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(10)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(10)
C
		WRITE (6,*) 'Enter color for icing'
		READ  (5,*)  icolr4(11)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(11)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(11)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(11)
C
		WRITE (6,*) 'Enter color for hail'
		READ  (5,*)  icolr4(12)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(12)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(12)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(12)
C
		WRITE (6,*) 'Enter color for duststorm'
		READ  (5,*)  icolr4(13)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(13)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(13)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(13)
C
		WRITE (6,*) 'Enter color for sandstorm'
		READ  (5,*)  icolr4(14)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(14)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(14)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(14)
C
		WRITE (6,*) 'Enter color for cumulonimbus'
		READ  (5,*)  icolr4(15)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(15)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(15)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(15)
C
		WRITE (6,*) 'Enter color for low level wind shear'
		READ  (5,*)  icolr4(16)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(16)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(16)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(16)
C
		WRITE (6,*) 'Enter flag for plotting symbols or storm '
     +		   	     // 'names (0=no,1=yes)'
		READ  (5,*)  iflags(1)
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for message id (0=no,1=yes)'
		READ  (5,*)  iflags(3)
		WRITE (6,*) 'Enter flag for dir and speed (0=no,1=yes)'
		READ  (5,*)  iflags(4)
		WRITE (6,*) 'Enter flag for flight level/Press-MxWnd '
     +		 // ' (0=no,1=yes)'
		READ  (5,*)  iflags(5)
C
		CALL GG_ISIG  ( endtim, icolr4, ssize, iwidth, lwidth,
     +			        iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24) THEN
		WRITE (6,*) 'Enter number of points for bounded area'
		READ  (5,*)  np 
		DO  mm = 1, np
		    WRITE (6,*) 'Enter latitude and longitude', mm
		    READ  (5,*)  alat (mm), alon (mm)
		END DO
		CALL GG_CTRD  ( np, alat, alon, alat, alon, iret )
		WRITE (6,*) 'ALAT, ALON, IRET = ', alat, alon, iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 25) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter the number of colors'
		READ  (5,*)  numclr
		WRITE (6,*) 'Enter ', numclr, ' time increments (min.)'
		READ  (5,*)  ( itimes (mm), mm = 1, numclr )
		WRITE (6,*) 'Enter ', numclr, ' colors'
		READ  (5,*)  ( ifcolr (mm), mm = 1, numclr )
		WRITE (6,*) 'Enter positive marker, size, width'
		READ  (5,*)  ppmark
		WRITE (6,*) 'Enter negative marker, size, width'
		READ  (5,*)  pnmark
		DO mm = 1, 5
		    isorc (mm) = 1
		END DO
		CALL GG_LTNG ( endtim, itimes, ifcolr, numclr, ppmark, 
     +			       pnmark, isorc, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 26 )  THEN
		WRITE (6,*) 'Enter the MSCALE attributes'
		WRITE (6,*) 'See mscale.hl2 for syntax'
		READ  (5,2)  mscale
		CALL GG_SCAL ( mscale, iret )
		CALL GEPLOT (ier)
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 27) THEN
		WRITE (6,*) 'Enter NUMC, the max no. of colors/models'
		READ  (5,*) numc
                WRITE (6,*) 'Enter NUMW, max# of colors for QSCT wind'
                READ  (5,*) numw
                WRITE (6,*) 'Enter NUMR, max# of colors for QSCT rain'
                READ  (5,*) numr
		CALL GG_MISC ( numc,  iclrs,   icolr2,  icolr3,  icolr4,
     +			      icolr5, modls,   icolr6,  icolrgam, 
     +                        icolr7,  icolr8, icolr9, icolr10, icolr11,
     +                        icolr12, icolr13, icolr14, icolr15, 
     +                        icolr16, icolr17, iclr18, iclr19, emodls,
     +                        iclr20, iclr1k, iclrek, iclr2k, iret )
		WRITE (6,30) 'WTCH', ( iclrs (i), i = 1, numc )
30		FORMAT ( A, 20I3 )
		WRITE (6,30) 'WARN', ( icolr2 (i), i = 1, numc )
		WRITE (6,30) 'HRCN', ( icolr3 (i), i = 1, numc ) 
		WRITE (6,30) 'ISIG', ( icolr4 (i), i = 1, numc )
		WRITE (6,*) 'ATCF Model       Color'
		WRITE (6,40) ( modls (i), icolr5 (i), i = 1, numc )
		WRITE (6,30) 'AIRM1', ( icolr6 (1,i), i = 1, numc ) 
		WRITE (6,30) 'AIRM2', ( icolr6 (2,i), i = 1, numc )
                WRITE (6,30) 'GAIRM1', ( icolrgam (1,i), i = 1, numc )
                WRITE (6,30) 'GAIRM2', ( icolrgam (2,i), i = 1, numc ) 
		WRITE (6,30) 'NCON', ( icolr7 (i), i = 1, numc )
		WRITE (6,30) 'SVRL', ( icolr8 (i), i = 1, numc )
		WRITE (6,30) 'TCMG', ( icolr9 (i), i = 1, numc )
                WRITE (6,30) 'WSTM', ( icolr10 (i), i = 1, numc )
                WRITE (6,30) 'WOUs', ( icolr11 (i), i = 1, numc )
                WRITE (6,30) 'WOU Fill', ( icolr12 (i), i = 1, numc )
                WRITE (6,30) 'WCNs', ( icolr13 (i), i = 1, numc )
                WRITE (6,30) 'WCN Fill', ( icolr14 (i), i = 1, numc )
                WRITE (6,30) 'CSIG', ( icolr15 (i), i = 1, numc )
                WRITE (6,30) 'QSCT Wind', ( icolr16 (i), i = 1, numw )
                WRITE (6,30) 'QSCT Rain', ( icolr17 (i), i = 1, numr )
                WRITE (6,30) 'WCP', ( iclr18 (i), i = 1, numc )
		WRITE (6,30) 'FFA', ( iclr20 (i), i = 1, numc )
		WRITE (6,30) 'TRAK1', ( iclr1k (i), i = 1, numc )
		WRITE (6,30) 'TRAK2', ( iclr2k (i), i = 1, numc )
		WRITE (6,30) 'TRAKE', ( iclrek (i), i = 1, numc )
		WRITE (6,*) 'ENCY Model       Color'
		WRITE (6,40) ( emodls (i), iclr19 (i), i = 1, numc )
40		FORMAT ( A, I3 )
		WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 28) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter color for IFR'
		READ  (5,*)  icolr4(1)
		WRITE (6,*) 'Enter text size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter text width'
		READ  (5,*)  iwidth(1)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(1)
C
		WRITE (6,*) 'Enter color for mountain obscuration'
		READ  (5,*)  icolr4(2)
		WRITE (6,*) 'Enter text size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter text width'
		READ  (5,*)  iwidth(2)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(2)
C
		WRITE (6,*) 'Enter color for turbulence'
		READ  (5,*)  icolr4(3)
                WRITE (6,*) 'Enter color2 for turbulence'
                READ  (5,*)  itbclr2 
                WRITE (6,*) 'Enter flight level for turbulence'
                READ  (5,*)  lvlfl 
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(3)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(3)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(3)
C
		WRITE (6,*) 'Enter color for icing'
		READ  (5,*)  icolr4(4)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(4)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(4)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(4)
C
		WRITE (6,*) 'Enter color for sustained winds'
		READ  (5,*)  icolr4(5)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(5)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(5)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(5)
C
		WRITE (6,*) 'Enter color for low-level wind shear'
		READ  (5,*)  icolr4(6)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(6)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(6)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(6)
C
                WRITE (6,*) 'Enter lower filter level'
                READ  (5,*)  lvfil(1) 
                WRITE (6,*) 'Enter upper filter level'
                READ  (5,*)  lvfil(2) 
C
		WRITE (6,*) 'Enter flag for plotting symbols or type '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(1)
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for flight levels (0=no,1=yes)'
		READ  (5,*)  iflags(3)
                WRITE (6,*) 'Enter flag for using issue time to plot'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(4)
C
		CALL GG_AIRM  ( endtim, icolr4, itbclr2, lvlfl, lvfil, 
     +				ssize, iwidth, lwidth, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 29) THEN
                WRITE (6,*) 'Enter the frame time'
		READ  (5,2)  frametim
		WRITE (6,*) 'Enter color for IFR'
		READ  (5,*)  icolr4(1)
		WRITE (6,*) 'Enter text size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter text width'
		READ  (5,*)  iwidth(1)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(1)
C
		WRITE (6,*) 'Enter color for mountain obscuration'
		READ  (5,*)  icolr4(2)
		WRITE (6,*) 'Enter text size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter text width'
		READ  (5,*)  iwidth(2)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(2)
C
		WRITE (6,*) 'Enter color for turbulence'
		READ  (5,*)  icolr4(3)
                WRITE (6,*) 'Enter color2 for turbulence'
                READ  (5,*)  itbclr2 
                WRITE (6,*) 'Enter flight level for turbulence'
                READ  (5,*)  lvlfl 
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(3)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(3)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(3)
C
		WRITE (6,*) 'Enter color for icing'
		READ  (5,*)  icolr4(4)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(4)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(4)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(4)
C
		WRITE (6,*) 'Enter color for sustained winds'
		READ  (5,*)  icolr4(5)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(5)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(5)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(5)
C
		WRITE (6,*) 'Enter color for low-level wind shear'
		READ  (5,*)  icolr4(6)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(6)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(6)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(6)
C
                WRITE (6,*) 'Enter lower filter level'
                READ  (5,*)  lvfil(1) 
                WRITE (6,*) 'Enter upper filter level'
                READ  (5,*)  lvfil(2) 
C
		WRITE (6,*) 'Enter flag for plotting symbols or type '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(1)
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for flight levels (0=no,1=yes)'
		READ  (5,*)  iflags(3)
                WRITE (6,*) 'Enter flag for a time offset of -2 hours'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(4)
                WRITE (6,*) 'Enter flag for a time offset of -1 hours'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(5)
                WRITE (6,*) 'Enter flag for a time offset of  0 hours'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(6)
                WRITE (6,*) 'Enter flag for a time offset of +1 hours'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(7)
                WRITE (6,*) 'Enter flag for a time offset of +2 hours'
     +                       // '(0=no,1=yes)'
                READ  (5,*)  iflags(8)
C
		CALL GG_GAIRM ( frametim, icolr4, itbclr2, lvlfl, lvfil, 
     +				ssize, iwidth, lwidth, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 30) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter color for icing'
		READ  (5,*)  icolr4(1)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(1)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(1)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(1)
C
		WRITE (6,*) 'Enter color for turbulence'
		READ  (5,*)  icolr4(2)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(2)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(2)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(2)
C
		WRITE (6,*) 'Enter color for duststorm/sandstorm'
		READ  (5,*)  icolr4(3)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(3)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(3)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(3)
C
		WRITE (6,*) 'Enter color for volcanic ash'
		READ  (5,*)  icolr4(4)
		WRITE (6,*) 'Enter symbol size'
		READ  (5,*)  ssize(4)
		WRITE (6,*) 'Enter symbol width'
		READ  (5,*)  iwidth(4)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(4)
C
		WRITE (6,*) 'Enter flag for plotting symbols '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(1)
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for message id (0=no,1=yes)'
		READ  (5,*)  iflags(3)
		WRITE (6,*) 'Enter flag for flight levels (0=no,1=yes)'
		READ  (5,*)  iflags(4)
		WRITE (6,*) 'Enter flag for November data (0=no,1=yes)'
                READ  (5,*)  iflags(5)
                WRITE (6,*) 'Enter flag for Oscar data (0=no,1=yes)'
                READ  (5,*)  iflags(6)
                WRITE (6,*) 'Enter flag for Papa data (0=no,1=yes)'
                READ  (5,*)  iflags(7)
                WRITE (6,*) 'Enter flag for Quebec data (0=no,1=yes)'
                READ  (5,*)  iflags(8)
                WRITE (6,*) 'Enter flag for Romeo data (0=no,1=yes)'
                READ  (5,*)  iflags(9)
                WRITE (6,*) 'Enter flag for Uniform data (0=no,1=yes)'
                READ  (5,*)  iflags(10)
                WRITE (6,*) 'Enter flag for Victor data (0=no,1=yes)'
                READ  (5,*)  iflags(11)
                WRITE (6,*) 'Enter flag for Whiskey data (0=no,1=yes)'
                READ  (5,*)  iflags(12)
                WRITE (6,*) 'Enter flag for Xray data (0=no,1=yes)'
                READ  (5,*)  iflags(13)
                WRITE (6,*) 'Enter flag for Yankee data (0=no,1=yes)'
                READ  (5,*)  iflags(14)

C
		CALL GG_NCON  ( endtim, icolr4, ssize, iwidth, lwidth,
     +			        iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF ( numsub .eq. 31) THEN
		WRITE (6,*) 'Enter the initial time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter colors for model tracks (upto 25 values)'
		READ  (5,*) icolr5
		WRITE (6,*) 'Enter marker symbol type (1-22) (upto 25 vals)'
		READ  (5,*) mrktyp
		WRITE (6,*) 'Enter marker size (upto 25 vals)'
		READ  (5,*) ssize
		WRITE (6,*) 'Enter marker line width (upto 25 vals)'
		READ  (5,*) iwidth
		WRITE (6,*) 'Enter track line width (upto 25 vals)'
		READ  (5,*) lwidth
C
		WRITE (6,*) 'Enter the storm name'
		READ  (5,2)  strnam
C
                IF ( strnam .eq. 'ENS_CYC' ) THEN
		   WRITE(6,*)'Enter flag to plot DD/HH    (0=no,1=yes)'
		   READ (5,*)  iflags(1) 
		   WRITE(6,*)'Enter flag to plot pressure (0=no,1=yes)'
		   READ (5,*) iflags(2)
		   WRITE(6,*)'Enter flag to plot markers  (0=no,1=yes)'
		   READ (5,*)  iflags(3)
		   WRITE(6,*)'Enter flag to color tracks (0=no,1=yes)'
		   READ (5,*)  iflags(4)
                   WRITE(6,*)'Enter wind levels for color coding:'
                   WRITE(6,*)'Enter lower level'
                   READ (5,*)  wlevs(1)
                   WRITE(6,*)'Enter middle level'
                   READ (5,*)  wlevs(2)
                   WRITE(6,*)'Enter upper middle level'
                   READ (5,*)  wlevs(3)
                   WRITE(6,*)'Enter upper level'
                   READ (5,*)  wlevs(4)
		   WRITE(6,*)'Enter optional single forecast hour (0,6,12,...)'
		   READ (5,*)  ifcsth
                 ELSE
		   WRITE(6,*)'Enter flag for plotting time (0=no,1=yes)'
		   READ (5,*)  iflags(1) 
		   WRITE(6,*)'Enter flag for storm id (0=no,1=yes)'
		   READ (5,*)  iflags(2)
		   WRITE(6,*)'Enter flag for forecast speed(0=no,1=yes)'
		   READ (5,*)  iflags(3)
		   WRITE(6,*)'Enter flag for track markers (0=no,1=yes)'
		   READ (5,*)  iflags(4)
		   ifcsth = -1
                END IF
C
		CALL GG_NATC ( endtim, icolr5, mrktyp, ssize, iwidth,
     +			           lwidth, iflags, wlevs, ifcsth, strnam, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 32) THEN
	      	WRITE (6,*) 'Enter System Coord (S,D,N,V,P,M,G)'
		READ  (5,2) sys
		WRITE (6,*) 'Enter the center point X and Y'
		READ  (5,*) xcent, ycent
		WRITE (6,*) 'Enter the radius point X and Y'
		READ  (5,*) xcrm, ycrm
		WRITE (6,*) 'Enter the number of points for the arc'
		WRITE (6,*) '      (Max of 100 for testing)'
		READ  (5,*) np
		WRITE (6,*) 'Enter the start and end angles'
		READ  (5,*) ang1, ang2
		WRITE (6,*) 'Enter the accuracy factor'
		READ  (5,*) iaccf
C
		CALL GG_ARC ( sys, xcent, ycent, xcrm, ycrm, np,
     +			      ang1, ang2, iaccf, xout, yout, iret )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
		IF  ( iret .eq. 0 )  THEN
		    CALL GQLINE ( iltyp, ilthw, iwidth, iwhw, ier )
		    CALL GQCOLR ( icolr, ier )
		    CALL GQSMTH ( ismtyp, dens, ier )
C
		    CALL GSLINE ( 1, 0, 3, 0, ier )
		    CALL GSCOLR ( 2, ier )
		    CALL GSSMTH ( 0, 0.0, ier )
C
		    CALL GLINE  ( sys, np, xout, yout, ier )
C
		    CALL GSLINE ( iltyp, ilthw, iwidth, iwhw, ier )
		    CALL GSCOLR ( icolr, ier )
		    CALL GSSMTH ( ismtyp, dens, ier )
C
		    CALL GEPLOT ( ier )
		END IF
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 33) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		CALL GG_WCUR  ( endtim, iuwtch, iunum, iret )
		WRITE (6,*) 'Number of current watches : ', iunum
		WRITE (6,*) 'Current watches : '
                DO ii = 1, iunum
 		    WRITE (6,*) '    ',iuwtch(ii)
                END DO
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 34) THEN
		WRITE (6,*) 'Enter a watch number '
		READ  (5,2)  wtnm
                CALL ST_NUMB ( wtnm, iwtch, ier )
C
		WRITE (6,*) 'Current watch information : '
                DO ii = 1, nwtch
                    iend = 0
                    CALL ST_NUMB ( wnum(ii), inumb, ier )
                    IF ( iwtch .eq. inumb  ) THEN
		        CALL GG_INFO  ( iwtch, ii, type, tbeg, tstp, 
     +                              inpt, xlat, xlon, ntest, iret )
		        WRITE (6,*) iwtch,' ',type, tbeg, tstp, inpt,
     +                               ' ', ntest
                        IF ( ( ( type .eq. 'TS' ) .or. 
     +                                 ( type .eq.'TN') )  .and. 
     +                                         ( inpt .eq. 0 ) ) THEN
                             iend = 5
                          ELSE
                             iend = inpt
                        END IF
                        IF ( iend .gt. 0 ) THEN
                            DO jj = 1, iend
		                WRITE (6,*) xlat(jj),' ',xlon(jj)
                            END DO
                        END IF 
                    END IF
                END DO
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 35) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter color for tropical symbols'
		READ  (5,*)  icolr9(1)
		WRITE (6,*) 'Enter color for track arrows'
		READ  (5,*)  icolr9(2)
		WRITE (6,*) 'Enter color for ten tropical systems'
		READ  (5,*)  (icolr9(jj),jj=3,12)
C
		WRITE (6,*) 'Enter hurricane center name (CPHC or TPC)'
		READ  (5,2) text 
C
		ssize (1)  = 0.0
		iwidth (1) = 0
		slsize     = 0.0
		iswdth     = 0
C
		CALL GG_TCMG  ( endtim, icolr9, ssize, iwidth, slsize,
     +				iswdth, text, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 36) THEN
		WRITE(6,*) 'Enter data type(QSCT, QSCT_HI, AMGB1, AMBG2,'
     +			// ' AMBG3, or AMBG4)'
                READ  (5,2)  datatyp 
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*) 'Enter the number of colors'
		READ  (5,*)  numclr
		WRITE (6,*) 'Enter ', numclr, ' speed increments (m/s)'
		READ  (5,*)  ( itimes (mm), mm = 1, numclr )
		WRITE (6,*) 'Enter ', numclr, ' colors'
		READ  (5,*)  ( ifcolr (mm), mm = 1, numclr )
		WRITE (6,*) 'Enter ', numclr, ' colors2'
                READ  (5,*)  ( ifcolr2 (mm), mm = 1, numclr )
C
		WRITE (6,*) 'Enter arrow/barb size'
		READ  (5,*)  brbsiz
		WRITE (6,*) 'Enter arrow/barb width'
		READ  (5,*)  ibwid
		WRITE (6,*) 'Enter type: 3=Dir Arrow 4=Reg Arrow 5=Barb'
		READ  (5,*)  ityp
		IF ( ityp .eq. 3 .or. ityp .eq. 4 .or. ityp .eq. 1 ) THEN
		    WRITE (6,*) 'Enter arrow head size'
		    READ  (5,*)  ahdsz
		  ELSE
		    ahdsz = 0.4
		    ityp = 5
		END IF
C
		WRITE (6,*) 'Enter skip value'
		READ  (5,*)  iskip
C
		WRITE (6,*) 'Enter time stamp interval'
                READ  (5,*)  interv 
		WRITE (6,*) 'Enter time stamp color'
                READ  (5,*)  itmclr 
		WRITE (6,*) 'Enter line width of time stamp'
                READ  (5,*)  ilnwid 
C
		WRITE (6,*) 'Enter flag for high speeds (0=no,1=yes)'
		READ  (5,*)  iflags(1) 
		WRITE (6,*) 'Enter flag for low speeds (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for rain algorithm (0=no,1=yes)'
		READ  (5,*)  iflags(3) 
		WRITE (6,*) 'Enter flag for data available (0=no,1=yes)'
		READ  (5,*)  iflags(4) 
		WRITE (6,*) 'Enter flag for 2nd rain color (0=no,1=yes)'
                READ  (5,*)  iflags(5)
		WRITE (6,*) 'Enter flag for plotting rain circles (0=no,1=yes)'
                READ  (5,*)  iflags(6)
C
		CALL GG_QSCT ( datatyp, endtim, itimes, ifcolr, ifcolr2,
     +			    numclr, brbsiz, ibwid, ahdsz, ityp, iskip,
     +			    interv, itmclr, ilnwid, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 37) THEN
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*) 'Enter color for initial hour CSIG'
		READ  (5,*)  icolr4(1)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(1)
C
		WRITE (6,*) 'Enter color for 1-hour extrapolated CSIG'
		READ  (5,*)  icolr4(2)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(2)
C
		WRITE (6,*) 'Enter color for 2-hour extrapolated CSIG'
		READ  (5,*)  icolr4(3)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(3)
C
		WRITE (6,*) 'Enter color for outlooks'
		READ  (5,*)  icolr4(4)
		WRITE (6,*) 'Enter line width'
		READ  (5,*)  lwidth(4)
C
		WRITE (6,*) 'Enter flag for plotting 0-hr sequence ID '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(1)
		WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
		READ  (5,*)  iflags(2) 
		WRITE (6,*) 'Enter flag for direction/speed(0=no,1=yes)'
		READ  (5,*)  iflags(3)
		WRITE (6,*) 'Enter flag for flight levels (0=no,1=yes)'
		READ  (5,*)  iflags(4)
		WRITE (6,*) 'Enter flag for intensity level(0=no,1=yes)'
		READ  (5,*)  iflags(5)
		WRITE (6,*) 'Enter flag for plotting 1-hr sequence ID '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(6)
		WRITE (6,*) 'Enter flag for plotting 2-hr sequence ID '
     +		   	     // '(0=no,1=yes)'
		READ  (5,*)  iflags(7)
C
		CALL GG_CSIG  ( endtim, icolr4, lwidth, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET = ', iret
	        CALL ER_WMSG ( 'GG', iret, ' ', ierr)
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 38) THEN
                WRITE (6,*) 'Enter the end time'
                READ  (5,2)  endtim
C
                WRITE (6,*) 'Enter color for winter storm warning'
                READ  (5,*)  icolr2(1)
                WRITE (6,*) 'Enter marker type (17-21) for warning'
                READ  (5,*)  mrktyp(1)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(1)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(1)
                WRITE (6,*) 'Enter line width for winter storm warning'
                READ  (5,*)  lwidth(1)		
C
                WRITE (6,*) 'Enter color for winter storm watch'
                READ  (5,*)  icolr2(2)
                WRITE (6,*) 'Enter marker type (17-21) for storm watch'
                READ  (5,*)  mrktyp(2)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(2)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(2)
                WRITE (6,*) 'Enter line width for winter storm watch'
                READ  (5,*)  lwidth(2)
C
                WRITE (6,*) 'Enter color for winter storm advisory'
                READ  (5,*)  icolr2(3)
                WRITE (6,*) 'Enter marker type (17-21) for advisory '
                READ  (5,*)  mrktyp(3)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(3)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(3)
                WRITE (6,*) 'Enter line width for winter storm advisory'
                READ  (5,*)  lwidth(3)
C
                WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
                READ  (5,*)  iflags(1)
                WRITE (6,*) 'Enter flag for label (0=no,1=yes)'
                READ  (5,*)  iflags(2)
                WRITE (6,*) 'Enter flag for outline (0=no,1=yes)'
                READ  (5,*)  iflags(3)
C
                CALL GG_WSTM  ( endtim, icolr2, mrktyp, ssize,
     +                          iwidth, lwidth, iflags, iret )
                CALL GEPLOT ( ier )
                WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 39) THEN
		WRITE (6,*) 'Enter the directory alias (eg. WOU or WCN)'
		READ  (5,2)  alias
C
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
C
		WRITE (6,*) 'Enter the minutes difference from end time'
		READ  (5,*)  mndif
C
		CALL GG_WWCR  ( endtim, alias, mndif, iuwtch, iunum, 
     +				tzone, tmissu, tmstrt, tmstp, iret )
 		WRITE (6,*) 'Number of unique ',alias, 
     +                       'watch numbers ', iunum
                IF ( iunum .gt. 0 ) THEN
		    WRITE (6,*) 'Current watches, issue, starting and ' 
     +                          // ' ending times: '
                    DO ii = 1, iunum
 		        WRITE (6,23) iuwtch(ii),tmissu(ii), tmstrt(ii),
     +                               tmstp(ii)
                    END DO
 23                 FORMAT(3X, I4, 3X, A15, 3X, A15, 3X, A15)
                END IF
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 40) THEN

		WRITE (6,*) 'Enter the watch number'
		READ  (5,*)  inumb
		WRITE (6,*) 'Enter the end time'
		READ  (5,2)  endtim
		WRITE (6,*) 'Enter if marine zones are to be removed:'
		WRITE (6,*) '1 - yes  0 - no'
		READ  (5,2)  irmzn
		itype = 1
                CALL CSS_GTIM ( itype, systim, iret )
C
		CALL GG_WCCK  (inumb, endtim, systim, irmzn,
     +			       iznflg, icancl, attnln, stzstr, iret ) 
                IF ( iret .eq. 0 ) THEN
		    WRITE (6,*) 'Watch ',inumb, 
     +				' Cancelation flag : ', icancl,
     +				' Marine Zone flag : ', iznflg
                    CALL ST_LSTR ( attnln, lens, ier )
                    CALL ST_LSTR ( stzstr, lenz, ier )
                    IF ( icancl .eq. 0 ) THEN
 		        WRITE (6,*) 'ORIG. ATTN  LINE : ',attnln(:lens)
 		        WRITE (6,*) 'ORIG. STATES LINE : ',stzstr(:lenz)
                    END IF
                END IF
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 41 ) THEN
                WRITE (6,*) 'Enter the watch number'
		READ  (5,*)  inumb
                CALL GG_WCVF ( inumb, ier )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 42 ) THEN
                WRITE (6,*) 'Enter the number of counties to sort'
		READ  (5,*)  inumb
                DO ii = 1, inumb
                    WRITE (6,*) 'Enter county #',ii
		    READ  (5,*) cnty(ii) 
                END DO 
                CALL GG_ZSRT ( cnty, inumb, iret )
                DO ii = 1, inumb
                    WRITE(6,*)cnty(ii)
                END DO
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 43) THEN
                WRITE (6,*) 'Enter file type (WOU or WCN)'
                READ  (5,2) filtyp 
C
                WRITE (6,*) 'Enter the end time'
                READ  (5,2)  endtim
C
                WRITE (6,*) 'Enter the number of colors'
                READ  (5,*)  numclr
C
                WRITE (6,*) 'Enter ', numclr, ' colors'
                READ  (5,*)  ( ifcolr (mm), mm = 1, numclr )
                WRITE (6,*) 'Enter ', numclr, ' colors2'
                READ  (5,*)  ( ifcolr2 (mm), mm = 1, numclr )
C
                WRITE (6,*)'Enter color for thunderstorm'
                READ  (5,*)  icolr2(1)
                WRITE (6,*) 'Enter marker type (17-21) for t-storm'
                READ  (5,*)  mrktyp(1)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(1)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(1)
C
                WRITE (6,*) 'Enter color for tornado'
                READ  (5,*)  icolr2(2)
                WRITE (6,*) 'Enter marker type (17-21) for tornado'
                READ  (5,*)  mrktyp(2)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(2)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(2)
C
                WRITE (6,*) 'Enter flag for watch box times(0=no,1=yes)'
                READ  (5,*)  iflags(1)
                WRITE (6,*) 'Enter flag for label (0=no,1=yes)'
                READ  (5,*)  iflags(2)
                WRITE (6,*) 'Enter flg for watch box number(0=no,1=yes)'
                READ  (5,*)  iflags(3)
                WRITE (6,*) 'Enter flag for color code (0=no,1=yes)'
                READ  (5,*)  iflags(4)
                WRITE (6,*) 'Enter flag for marker (0=no,1=yes)'
                READ  (5,*)  iflags(5)
                WRITE (6,*) 'Enter flag for outline (0=no,1=yes)'
                READ  (5,*)  iflags(6)
                WRITE (6,*) 'Enter flag for fill (0=no,1=yes)'
                READ  (5,*)  iflags(7)
 		WRITE (6,*) 'Enter flag for union (0=no,1=yes)'
                READ  (5,*)  iflags(8)
                IF ( filtyp .eq. 'WOU' ) THEN
                    CALL GG_WWOU  ( filtyp, endtim, numclr, ifcolr, 
     +                    ifcolr2, mrktyp, ssize, iwidth, iflags, iret )
                  ELSE
                    CALL GG_WWCN  ( filtyp, endtim, numclr, ifcolr, 
     +                    ifcolr2, mrktyp, ssize, iwidth, iflags, iret )
                END IF
                CALL GEPLOT ( ier )
                WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 44 ) THEN
                WRITE (6,*) 'Enter the watch number'
		READ  (5,*)  inumb
		WRITE (6,*) 'Enter the user specified time'
		READ  (5,2)  usrtim
		itype = 1
		CALL CSS_GTIM ( itype, systim, ier )
		CALL TI_STAN ( usrtim, systim, usrtim2, ier )
		WRITE (6,*) 'To use this time as search time enter 1'
		READ  (5,*)  isystim
                IF ( isystim .eq. 1 ) systim = ' ' 
		CALL GG_WACT ( inumb,usrtim2,systim,type,strtim,endtim,
     +                         ncnty,cnties,wfocty,knt,etmchg,itst,
     +                         sacod,numarr,ircntm,iret )
		WRITE (6,*) 'IRET = ', iret
                IF ( iret .eq. 0 ) THEN
		   WRITE (6,*) 'Watch start time:           ', strtim
		   WRITE (6,*) 'Watch end   time:           ', endtim
		   WRITE (6,*) 'Most recent start time:     ',ircntm
		   WRITE (6,*) 'Number of counties in array:', ncnty
		   WRITE (6,*) 'Number of WOU in array:     ', knt
		   WRITE (6,*) 'Flag for ending time change:',etmchg
		   WRITE (6,*) 'Test flag:                  ',itst
		   WRITE (6,*) 'Number of action codes:     ',numarr
                   IF ( numarr .gt. 0 ) THEN
		      WRITE (6,*) 'Action codes:'
                      DO inum = 1, numarr
		         WRITE (6,*) sacod (inum)
		      END DO
		   END IF
                   IF ( ncnty .gt. 0 ) THEN
                      WRITE (6,*) 'Print active counties (y/n)?'
		      READ  (5,*)  answ
                      IF ( answ .eq. 'y' ) THEN
		         WRITE (6,*) 'Active county array:'
                         DO incnty = 1, ncnty
		            WRITE (6,*) cnties (incnty)(:95)
		         END DO
		      END IF
		   END IF
                   IF ( knt .gt. 0 ) THEN
                      WRITE (6,*) 'Print WOU array (y/n)?'
		      READ  (5,*)  answ
                      IF ( answ .eq. 'y' ) THEN
		         WRITE (6,*) 'WOU array:'
                         DO iknt = 1, knt
		            WRITE (6,*) wfocty (iknt) (:95)
		         END DO
		      END IF
		   END IF
		END IF
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 45 ) THEN
                WRITE (6,*) 'Enter the watch number'
		READ  (5,*)  inumb
		WRITE (6,*) 'Enter the user specified time'
		READ  (5,2)  usrtim
		itype = 1
                CALL CSS_GTIM ( itype, systim, ier )
		CALL TI_STAN ( usrtim, systim, usrtim2, ier )
                CALL GG_WFPS (inumb, usrtim2, systim, nmcnty,wcnty,iret)
		WRITE (6,*) 'IRET = ', iret
		IF ( nmcnty .ne. 0 ) THEN
		   WRITE (6,*) 'Number of WCN FIPS = ', nmcnty
                   WRITE (6,*) 'Print WCN FIPS codes (y/n)?'
		   READ  (5,*)  answ
                   IF ( answ .eq. 'y' ) THEN
		      WRITE (6,*) 'WCN FIPS codes:'
                      DO inmcnty = 1, nmcnty
		         WRITE (6,*) wcnty (inmcnty)
		      END DO
		   END IF
		END IF
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 46 ) THEN
                WRITE (6,*) 'Enter watch numbers separated by a space'
		READ  (5,200)  wtnum
 200		FORMAT (a120)
		WRITE (6,*) 'watch numbers:  ', wtnum
		CALL GG_WWFO ( wtnum, wfolst, iret )
		WRITE (6,*) 'IRET = ', iret
		WRITE (6,*) 'WFO list:  ', wfolst
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 50) THEN
		CALL GCLEAR (ier)
	        CALL GEPLOT (ier)
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 51) THEN
		WRITE (6,*) 'Enter TEXT info'
		READ  (5,2)  text
		CALL IN_TEXT ( text, iret )
		WRITE (6,*) 'IRET = ', iret
		CALL ER_WMSG  ( 'IN', iret, ' ', ierr )
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 52) THEN
		CALL GQMPRJ ( proj, ang1, ang2, ang3,
     +			      dlatll, dlonll, dlatur, dlonur, iret )
		WRITE (6,*) 'PROJ   = ', proj
		WRITE (6,*) 'ANGLES = ', ang1, ang2, ang3
		WRITE (6,*) 'GAREA  = ', dlatll, dlonll, dlatur, dlonur
		WRITE (6,*) 'IRET   = ', iret
                CALL ER_WMSG ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 53) THEN
C
                WRITE (6,*) 'Enter the end time'
                READ  (5,2)  endtim
C
                WRITE (6,*) 'Enter color for thunderstorm watches'
                READ  (5,*)  iclr18(1)
                WRITE (6,*) 'Enter width for thunderstorm watches'
                READ  (5,*)  iwidth(1)
C
                WRITE (6,*) 'Enter color for tornado watches'
                READ  (5,*)  iclr18(2)
                WRITE (6,*) 'Enter width for tornado watches'
                READ  (5,*)  iwidth(2)
C
                WRITE (6,*) 'Enter number of colors for color coding'
                READ  (5,*)  numclr
		IF ( numclr .lt. 1 .or. numclr .gt. 10 ) numclr = 10
                WRITE (6,*) 'Enter ', numclr, ' colors'
		nmclrs = numclr + 2
                READ  (5,*)  ( iclr18 (mm), mm = 3, nmclrs )
		WRITE (6,*) 'Enter ', numclr, ' line widths'
		READ  (5,*)  ( iwidth (mm), mm = 3, nmclrs )
C
                WRITE (6,*) 'Enter flag for watch box times(0=no,1=yes)'
                READ  (5,*)  iflags(1)
                WRITE (6,*) 'Enter flag for watch box no. (0=no,1=yes)'
                READ  (5,*)  iflags(2)
                WRITE (6,*) 'Enter flag for color code(0=no,1=yes)'
                READ  (5,*)  iflags(3)
                CALL GG_WCP  ( endtim, iclr18, iwidth, iflags, iret )
	        CALL GEPLOT ( ier )
		WRITE (6,*) 'IRET   = ', iret
                CALL ER_WMSG ( 'GG', iret, ' ', ierr )
C------------------------------------------------------------------------
	    ELSE IF ( numsub .eq. 54 ) THEN
		WRITE (6,*) 'Enter the end time'
                READ  (5,2)  endtim
C
                WRITE (6,*) 'Enter color for flash flood watch'
                READ  (5,*)  iclr20(1)
                WRITE (6,*) 'Enter marker type (17-21) for flash flood'
                READ  (5,*)  mrktyp(1)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(1)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(1)
                WRITE (6,*) 'Enter line width for flash flood watch'
                READ  (5,*)  lwidth(1)          
C
                WRITE (6,*) 'Enter color for areal flood watch'
                READ  (5,*)  iclr20(2)
                WRITE (6,*) 'Enter marker type (17-21) for areal flood'
                READ  (5,*)  mrktyp(2)
                WRITE (6,*) 'Enter marker size'
                READ  (5,*)  ssize(2)
                WRITE (6,*) 'Enter marker width'
                READ  (5,*)  iwidth(2)
                WRITE (6,*) 'Enter line width for areal flood watch'
                READ  (5,*)  lwidth(2)
C
                WRITE (6,*) 'Enter flag for plotting times (0=no,1=yes)'
                READ  (5,*)  iflags(1)
                WRITE (6,*) 'Enter flag for label (0=no,1=yes)'
                READ  (5,*)  iflags(2)
		WRITE (6,*) 'Enter flag for immediate cause(0=no,1=yes)'
		READ  (5,*)  iflags(3)
                WRITE (6,*) 'Enter flag for outline (0=no,1=yes)'
                READ  (5,*)  iflags(4)
C
                CALL GG_FFA   ( endtim, iclr20, mrktyp, ssize,
     +                          iwidth, lwidth, iflags, iret )
                CALL GEPLOT ( ier )
                WRITE (6,*) 'IRET = ', iret
                CALL ER_WMSG  ( 'GG', iret, ' ', ierr )
            ELSE IF ( numsub .eq. 55) THEN
              WRITE (6,*) 'Enter the frame time'
              READ  (5,2) frametim
              WRITE (6,*) 'Enter a time (mins) or height increment'
              READ  (5,*) ktminc(1)
              WRITE (6,*) 'Enter color for the time or height increment'
              READ  (5,*) kcolrs(1)
              WRITE (6,*) 'Enter a time limit'
              READ  (5,*) tlimit
              WRITE (6,*) 'Enter the mode T or H'
              READ  (5,2) mode
              WRITE (6,*) 'Enter D/A/B to plot Depart/Arrivals/Both'
              READ  (5,2) depdest
              WRITE (6,*) 'Enter ALL to plot all sites,or a 3-letter ID'
              READ  (5,2) sites
              numc = 1
              CALL GG_ASDI ( frametim, ktminc, kcolrs, numc, tlimit,
     +                       mode, depdest, sites, iret )
              CALL GEPLOT ( ier )
              WRITE (6,*) 'IRET = ', iret
              CALL ER_WMSG ( 'GG', iret, ' ', ierr)
            ELSE IF ( numsub .eq. 56) THEN
              WRITE (6,*) 'Enter the frame time'
              READ  (5,2) frametim
              WRITE (6,*) 'Enter a height increment'
              READ  (5,*) htinc(1)
              WRITE (6,*) 'Enter color for height increment'
              READ  (5,*) htcolrs(1)
              WRITE (6,*) 'Enter a time limit'
              READ  (5,*) tlimit
              WRITE (6,*) 'Enter an EDR increment'
              READ  (5,*) evinc(1)
              WRITE (6,*) 'Enter an EDR Color'
              READ  (5,*) ecolrs(1)
              WRITE (6,*) 'Enter symbol number for BLO FL180'
              READ  (5,*) esymb1(1)
              WRITE (6,*) 'Enter symbol number for AOA FL180'
              READ  (5,*) esymb2(1)
              WRITE (6,*) 'Enter a symbol size for BLO FL180'
              READ  (5,*) esymbsz1(1)
              WRITE (6,*) 'Enter a symbol size for AOA FL180'
              READ  (5,*) esymbsz2(1)
              aoa180fl = .true.
              numc = 1
              enumc = 1
              CALL GG_EDR ( frametim, ktminc, kcolrs, numc, tlimit,
     +                      evinc, ecolrs, esymb1, esymb2, esymbsz1,
     +                      esymbsz2, aoa180fl, iret)
              CALL GEPLOT ( ier )
              WRITE (6,*) 'IRET = ', iret
              CALL ER_WMSG ( 'GG', iret, ' ', ierr )               
C-----------------------------------------------------------------------
              
	    END IF
	END DO
C
2	FORMAT (A)
	END
