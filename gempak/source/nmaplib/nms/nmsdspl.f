	SUBROUTINE NMS_DSPL ( panel, dattim, alias, isbcat, filnam,
     +			      numc, ionoff, iclrs, iclrs2, numf, iflgs, 
     +			      fvalu, fline, fsym1, fsym2, farrw, ititl, 
     +			      iret )
C************************************************************************
C* NMS_DSPL								*
C*									*
C* This routine plots the requested MISC data to the current display	*
C* device.								*
C*									*
C* NMS_DSPL ( PANEL, DATTIM, ALIAS, ISBCAT, FILNAM, NUMC, IONOFF, ICLRS,*
C*	      ICLRS2, NUMF, IFLGS, FVALU, FLINE, FSYM1, FSYM2, FARRW,	*
C*	      ITITL, IRET )						*
C*									*
C* Input parameters:							*
C*	PANEL		CHAR*		GEMPAK panel			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	ALIAS		CHAR*		Alias for SFC data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	FILNAM		CHAR*		Data file name/Storm name	*
C*	NUMC		INTEGER		Number of colors		*
C*	IONOFF(*)	INTEGER		On-off array			*
C*	ICLRS (*)	INTEGER		Color array			*
C*	ICLRS2(*)	INTEGER		Second Color array		*
C*	NUMF		INTEGER		Number of flags			*
C*	IFLGS (*)	INTEGER		Flags array			*
C*	FVALU (*)	REAL		Value array			*
C*	FLINE (*)	REAL		Line attribute array		*
C*	FSYM1 (*)	REAL		Symbol 1 attribute array	*
C*	FSYM2 (*)	REAL		Symbol 2 attribute array	*
C*	FARRW (*)	REAL		Arrow attribute array		*
C*	ITITL		INTEGER		Title line			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  +1 = no match for time	*
C*					  -3 = no files found		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 9/99	Created					*
C* A. Hardy/GSC		10/99	Added filnam to GG_HRCN for storm name	*
C* S. Jacobs/NCEP	11/99	Added calls to CDS_SFILL, CDS_GFILL	*
C*				and FL_PATH				*
C* S. Jacobs/NCEP	12/99	Changed call to include attributes	*
C* D. Kidwell/NCEP	 2/00	Changed calling sequence for GG_LTNG	*
C* S. Jacobs/NCEP	 3/00	Changed calling sequence for GG_ funcs	*
C* D. Kidwell/NCEP	 6/00	Added call to GG_ATCF			*
C* D. Kidwell/NCEP	 7/00	Added call to GG_AIRM			*
C* D. Kidwell/NCEP	 8/00	Added call to GG_NCON			*
C* F. J. Yen/NCEP	 1/01	Added SVRL				*
C* F. J. Yen/NCEP	 5/01	Changed ATCF to ATCO and added call to	*
C*				GG_NATC for new format			*
C* S. Jacobs/NCEP	 7/01	Added call to GG_QSCT			*
C* J. Wu/SAIC		11/01	remove unnecessary call to crg_init	*
C* R. Tian/SAIC		 2/02	added argument to gg_warn		*
C* A. Hardy/NCEP	 8/02	Added call to GG_CSIG			*
C* M. Li/SAIC		 8/02	Added call to GG_WSTM			*
C* M. Li/SAIC		02/03	Added call to GG_WWOU & GG_WWCN		*
C* M. Li/SAIC		03/03	Increase NU to 12			*
C* M. Li/SAIC		04/03	Added the second color array		*
C* M. Li/SAIC		05/03	Added time stamp to GG_QSCT		*
C* M. Li/SAIC		05/03	Added colr2 and 3 levels to AIRM	*
C* M. Li/SAIC		08/03	Add Quikscat new format and SEAWND data	*
C* m.gamazaychikov/SAIC	10/03	Removed reference to ATCO		*
C* A. Hardy/NCEP	 4/04	Changed fvalu (5)&(6) to fvalu(6) & (7)	*
C* F. J. Yen/NCEP	 6/04	Added QSCT arrows & removed SEAWND.	*
C* F. J. Yen/NCEP	 7/04	Added AMBG1, AMBG2, AMBG3, and AMBG4.	*
C* M. Li/SAIC		10/04	Added lwid to GG_WSTM			*
C* M. Li/SAIC		01/05	Added iclrs2 to GG_WWOU and GG_WWCN	*
C* F. J. Yen/NCEP	 4/05	Added call to GG_WCP.			*
C* m.gamazaychikov/SAIC	04/05	Added code to handle ens cyclone data	*
C* S. Gilbert/NCEP	06/05	Re-Add SEAWND data			*
C* T. Piper/SAIC	12/05	Removed CDS_INIT; called in nmap.c	*
C* m.gamazaychikov/SAIC	02/06	Added windgr to GG_NATC calling sequence*
C* C. Bailey/HPC	04/06	Added GG_FFA				*
C* S. Gilbert/NCEP	05/06	Increased MAXPLT from 20 to 25		*
C* T. Piper/SAIC	07/06	Added SGWH and SSHA support		*
C* G. McFadden/SAIC	06/06	Added WindSAT				*
C* G. McFadden/SAIC	03/07	Added call to GG_ASCT			*
C* H. Zeng/SAIC		06/07	Changed no. of wind levels for ENS_CYC	*
C* G. McFadden/SAIC	07/07	Added ASCT_HI				*
C* G. McFadden/SAIC	07/07	Added AAMBG1 and AAMBG2			*
C* J. Lewis/AWC		08/07	Changed fvalu(6)&(7) to fvalu(7)&(8)	*
C* G. McFadden/SAIC	09/07	Added AAMBG1_HI and AAMBG2_HI		*
C* G. McFadden/SAIC	01/08	Added SGWHE and SGWHG			*
C* G. McFadden/SAIC	06/08	Added SGWH2 (Jason2 sig wave height)    *
C* G. McFadden/SAIC	12/08	Added TRAK1 (Jason-1), TRAKE (ENVISAT), *
C* 				and TRAK2 (Jason-2) Ground Track	*
C*				Predictions				*
C* S. Jacobs/NCEP	 1/10	Add EXperimental ASCT data types	*
C* L. Hinson/AWC        04/10   Add GAIRM                               *
C* G. McFadden/IMSG	 9/10	Added OSCAT				*
C* G. McFadden/IMSG	11/10	Added AAMBG3_HI and AAMBG4_HI		*
C* G. McFadden/IMSG	 7/11	Added SGWHC                  		*
C* G. McFadden/IMSG	 1/12	Added OSCT_HI                  		*
C* L. Hinson/AWC         4/12   Add ASDI                                *
C* G. McFadden/IMSG	 7/12	Added OAMBG1_HI, OAMBG2_HI, OAMBG3_HI,	*
C*                              and OAMBG4_HI                       	*
C* S. Jacobs/NCEP	10/12	Consolidate check for ASCT data types	*
C* L. Hinson/AWC        10/12   Add EDR                                 *
C* G. McFadden/IMSG	11/13	Added SGWHA, WSPDA, WSPD2, WSPDC, and   *
C*                              GG_WSPD                                 *
C* G. McFadden/IMSG	01/14	Added TRAKS and TRAKC                   *
C* L. Hinson/AWC        10/18   Update EDR with tracksfl                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MAXPLT = 25, NU = 12 )
C*
	CHARACTER*(*)	panel, dattim, alias, filnam
	INTEGER		ionoff(*), iclrs(*), iclrs2(*), iflgs(*)
	REAL		fvalu(*), fline(*), fsym1(*), fsym2(*),
     +			farrw(*)
C*
	CHARACTER	ttlstr*80, newfil*160, dirnam*160
	INTEGER		ktminc(LLCLEV), kwninc(LLCLEV), 
     +                  jclrs(LLCLEV+1), htinc(LLCLEV+1),
     +                  htclr(LLCLEV+1), evclr(LLCLEV+1),
     +                  esymb1(LLCLEV+1), esymb2(LLCLEV+1)
        REAL            evinc(LLCLEV+1), esymbsz1(LLCLEV+1),
     +                  esymbsz2(LLCLEV+1)
	INTEGER		jclrs2(LLCLEV+1), lvfil(2)
	REAL		ppmark(3), pnmark(3), windgr(4)
C
	INTEGER		isym(MAXPLT), iwid(MAXPLT), iawd(MAXPLT),
     +			lwid(MAXPLT)
	REAL		ssiz(MAXPLT), asiz(MAXPLT), hsiz(MAXPLT),
     +			slsz(MAXPLT)
        CHARACTER       mode*2
        CHARACTER       depdest*2
        INTEGER         tlimit
        LOGICAL         aoa180fl
        LOGICAL         tracksfl
	CHARACTER       sites*120
	CHARACTER       arpts(20)*12
        DATA            arpts / 'MCI', 'ATL', 'DEN', 'MSP', 'DTW',
     +                          'MIA', 'MCO', 'PHL', 'CLT', 'BOS',
     +                          'FLL', 'TPA', 'SAN', 'MEM', 'ORD;MDW',
     +                          'JFK;EWR;EGA', 'IAD;DCA;BWI',
     +                          'SEA;SFO;LAX', 'LAS;SLC;PHX', 
     +                          'DFW;IAH' /
C------------------------------------------------------------------------
	iret   = 0
	newfil = filnam
C
C*	Save the current settings for color and text.
C
	CALL GQCOLR  ( jclr, ier )
	CALL GQTEXT  ( jtxfn, jtxhw, sztxt, jtxwid, jbrdr,
     +		       jrrotn, jjust, ier )
C
C*	Set the panel.
C
	CALL GG_PANL ( panel, ier )
C
C*	Plot the different types of data.
C
	IF  ( alias .eq. 'WTCH' )  THEN
	    DO  i = 1, numc
		slsz(i) = fline((i-1)*2+1)
		lwid(i) = fline((i-1)*2+2)
	    END DO
	    CALL GG_WTCH ( dattim, iclrs, slsz, lwid, iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'WCP' )  THEN
	    DO  i = 1, numc
		lwid(i) = fline((i-1)*2+2)
	    END DO
	    CALL GG_WCP ( dattim, iclrs, lwid, iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'WARN' .or. alias .eq. 'SVRL')  THEN
	    DO  i = 1, numc
		isym(i) = fsym1((i-1)*3+1)
		ssiz(i) = fsym1((i-1)*3+2)
		iwid(i) = fsym1((i-1)*3+3)
	    END DO
	    CALL GG_WARN ( alias, dattim, numc, iclrs, isym, ssiz, iwid,
     +			   iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'WSTM' )  THEN
	    DO  i = 1, numc
		isym(i) = fsym1((i-1)*3+1)
		ssiz(i) = fsym1((i-1)*3+2)
		iwid(i) = fsym1((i-1)*3+3)
		lwid(i) = fline((i-1)*2+2)
	    END DO
	    CALL GG_WSTM ( dattim, iclrs, isym, ssiz, iwid, lwid, 
     +			   iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'FFA' )  THEN
	    DO  i = 1, numc
	        isym(i) = fsym1((i-1)*3+1)
                ssiz(i) = fsym1((i-1)*3+2)
                iwid(i) = fsym1((i-1)*3+3)
                lwid(i) = fline((i-1)*2+2)
            END DO
	    CALL GG_FFA ( dattim, iclrs, isym, ssiz, iwid, lwid,
     +                     iflgs, ier )
C
          ELSE IF  ( alias .eq. 'WOU' )  THEN
            DO  i = 1, numc
                isym(i) = fsym1((i-1)*3+1)
                ssiz(i) = fsym1((i-1)*3+2)
                iwid(i) = fsym1((i-1)*3+3)
            END DO
            CALL GG_WWOU ( alias, dattim, NU, iclrs, iclrs2, isym, ssiz, 
     +		           iwid, iflgs, ier )
C
          ELSE IF  ( alias .eq. 'WCN' )  THEN
            DO  i = 1, numc
                isym(i) = fsym1((i-1)*3+1)
                ssiz(i) = fsym1((i-1)*3+2)
                iwid(i) = fsym1((i-1)*3+3)
            END DO
            CALL GG_WWCN ( alias, dattim, NU, iclrs, iclrs2, isym, ssiz,
     +                     iwid, iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'HRCN' )  THEN
            IF ( newfil .eq. 'NONE' ) newfil = ' '
	    DO  i = 1, numc
		isym(i) = fsym1((i-1)*3+1)
		ssiz(i) = fsym1((i-1)*3+2)
		iwid(i) = fsym1((i-1)*3+3)
C
		asiz(i) = farrw((i-1)*4+1)
		hsiz(i) = farrw((i-1)*4+2)
		iawd(i) = farrw((i-1)*4+3)
	    END DO
	    CALL GG_HRCN ( dattim, iclrs, isym, ssiz, iwid,
     +			   asiz, hsiz, iawd, iflgs, newfil, ier )
C
	  ELSE IF  ( ( alias .eq. 'ISIG' ) .or.
     +		     ( alias .eq. 'AIRM' ) .or.
     +               ( alias .eq. 'GAIRM' ) .or.
     +		     ( alias .eq. 'NCON' ) )  THEN
	    DO  i = 1, numc
		ssiz(i) = fsym1((i-1)*3+2)
		iwid(i) = fsym1((i-1)*3+3)
C
		lwid(i) = fline((i-1)*2+2)
	    END DO
C
	    IF  ( alias .eq. 'ISIG' )  THEN
	        CALL GG_ISIG ( dattim, iclrs, ssiz, iwid, lwid, iflgs,
     +			       ier )
	      ELSE IF  ( alias .eq. 'AIRM' )  THEN
		IF ( iclrs(3) .le. 0 ) THEN
		    itbclr2 = 0
		ELSE
		    itbclr2 = iclrs2 ( 3 ) 
		END IF
C
		lvlfl = fvalu ( 3 )
		lvfil (1) = fvalu ( 7 )
                lvfil (2) = fvalu ( 8 )
C
	        CALL GG_AIRM ( dattim, iclrs, ssiz, iwid, lwid, 
     +			       itbclr2, lvlfl, lvfil, iflgs, ier )
              ELSE IF  (alias .eq. 'GAIRM') THEN
	        IF ( iclrs(3) .le. 0 ) THEN
		    itbclr2 = 0
		ELSE
		    itbclr2 = iclrs2 ( 3 ) 
		END IF
C
		lvlfl = fvalu ( 3 )
		lvfil (1) = fvalu ( 7 )
                lvfil (2) = fvalu ( 8 )
		CALL GG_GAIRM ( dattim, iclrs, ssiz, iwid, lwid, 
     +			       itbclr2, lvlfl, lvfil, iflgs, ier )
	      ELSE
		CALL GG_NCON ( dattim, iclrs, ssiz, iwid, lwid, iflgs, 
     +			       ier )
	    END IF
C
	  ELSE IF  ( alias .eq. 'CSIG' ) THEN
	    DO  i = 1, numc
		lwid(i) = fline((i-1)*2+2)
	    END DO
            CALL  GG_CSIG ( dattim, iclrs, lwid, iflgs, ier )
C
	  ELSE IF  ( alias .eq. 'LTNG' )  THEN
	    DO  i = 1, 3
		ppmark(i) = fsym1(i)
		pnmark(i) = fsym2(i)
	    END DO
	    nn = numc
	    DO  i = numc, 1, -1
		ktminc(i) = fvalu(i)
		IF  ( iclrs(i) .eq. 0 )  nn = i - 1
	    END DO
	    CALL GG_LTNG ( dattim, ktminc, iclrs, nn,
     +			   ppmark, pnmark, iflgs, ier )
C
          ELSE IF ( alias .eq. 'ASDI_T' .or. alias .eq. 'ASDI_H' ) THEN
	    IF (alias .eq. 'ASDI_T') THEN
              nn = numc
              DO i = numc, 1, -1
        	ktminc(i) = fvalu(i)
        	IF ( iclrs(i) .eq. 0 )  nn = i - 1
              END DO
              tlimit = 12
              mode = "T"
	    ELSE
	      nn = numc - 1
              nn = numc - 1
              DO i = numc - 1, 1, -1
        	ktminc(i) = fvalu(i)
        	IF (iclrs(i) .eq. 0 ) nn = i - 1 
              END DO
              tlimit = fvalu(numc)
              mode = "H"
	    END IF
     
            IF (iflgs(1) .eq. 1) THEN
              depdest = "D"
            ELSE IF (iflgs(2) .eq. 1) THEN
              depdest = "A"
            ELSE
              depdest = "B"
            END IF
	    IF (iflgs(1) .eq. 1 .and. iflgs(2) .eq. 1) THEN
	      depdest = "B"
	    END IF
	    IF (iflgs(3) .eq. 1) THEN
	      sites = 'ALL'
	    ELSE
	      sites = ' '
	      ipos = 1  
	      DO i = 4, numf
		IF (iflgs(i) .eq. 1) THEN
		  CALL ST_LSTR(arpts(i-3), ilen, ier)
	          sites(ipos:ipos+ilen) = arpts(i-3)
		  sites(ipos+ilen:ipos+ilen) = ';'
		  ipos=ipos+ilen+1
		END IF
	      END DO
	    END IF
	    CALL ST_LSTR ( sites, ilen, ier )
	    IF ( ilen .eq. 0 ) THEN
	      sites = 'ALL'
	    ELSE
	      IF ( sites(ilen:ilen) .eq. ';' ) THEN
	        sites(ilen:ilen) = ' '
	      END IF
	    END IF	    
            CALL GG_ASDI ( dattim, ktminc, iclrs, nn, tlimit, mode,
     +                     depdest, sites, iret )
          ELSE IF ( alias .eq. 'EDR' ) THEN
C*          Assume 9 Height colors, followed by Time Limit, followed by 5
C*          EDR colors.
            nn = 9 - 1
            DO i = 9, 1, -1
        	htinc(i) = fvalu(i)
                htclr(i) = iclrs(i)
            END DO
            
            tlimit = fvalu(10)
            
            DO i = 15, 11, -1
              evinc(i-10) = fvalu(i)
              evclr(i-10) = iclrs(i)
            END DO
            
            DO i = 11, 15
              esymb1(i-10) = fsym1((i-1)*3+1)
              esymb2(i-10) = fsym2((i-1)*3+1)
              esymbsz1(i-10) = fsym1((i-1)*3+2)
              esymbsz2(i-10) = fsym2((i-1)*3+2)
            END DO

            IF (iflgs(1) .eq. 1) THEN
              aoa180fl = .true.
            ELSE
              aoa180fl = .false.
            END IF
            IF (iflgs(2) .eq. 1) THEN
              tracksfl = .true.
            ELSE
              tracksfl = .false.
            END IF
            CALL GG_EDR ( dattim, htinc, htclr, 9, tlimit, evinc,
     +                    evclr, esymb1, esymb2, esymbsz1, esymbsz2, 5,
     +                    aoa180fl, tracksfl, iret )
C   
          ELSE IF  ( ( alias .eq. 'ATCF'   ) .or.
     +               ( alias .eq. 'ENS_CYC') )  THEN
            IF ( alias .eq. 'ENS_CYC' ) THEN
               newfil = alias
             ELSE IF ( newfil .eq. 'NONE' ) THEN
               newfil = ' '
            END IF
	    DO  i = 1, numc
		isym(i) = fsym1((i-1)*3+1)
		ssiz(i) = fsym1((i-1)*3+2)
		iwid(i) = fsym1((i-1)*3+3)
C
		lwid(i) = fline((i-1)*2+2)
	    END DO
	    windgr (1) = fvalu ( 1 )
	    windgr (2) = fvalu ( 2 )
	    windgr (3) = fvalu ( 3 )
	    windgr (4) = fvalu ( 4 )
	    ifcsth     = NINT ( fvalu ( 24 ) )
	    IF ( ionoff(24) .eq. 0 )  ifcsth = -1
	    CALL GG_NATC ( dattim, iclrs, isym, ssiz, iwid, lwid, 
     +                     iflgs, windgr, ifcsth, newfil, ier )
C
C
	  ELSE IF  ( alias(1:4) .eq. 'ASCT' .or. 
     +               alias(1:5) .eq. 'AAMBG' .or.
     +	  	     alias(1:6) .eq. 'EXASCT' .or.
     +               alias(1:7) .eq. 'EXAAMBG' ) THEN
	    IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
                interv = 0
              ELSE
                interv = NINT ( fvalu(2) )
            END IF
	    itmclr = iclrs(2)
C
	    itmwid  = fline ((2-1)*2+2) 
C
	    brbsiz = farrw ((3-1)*4+1)
	    ahsiz  = farrw ((3-1)*4+2)
	    ibwid  = farrw ((3-1)*4+3)
	    ityp   = farrw ((3-1)*4+4)
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
		jclrs2 (i-2) = iclrs2(i)
	    END DO
C
	    CALL GG_ASCT ( alias, dattim, kwninc, jclrs, jclrs2, nn, 
     +		   brbsiz, ibwid, ahsiz, ityp, iskip, interv, itmclr,
     +		   itmwid, iflgs, ier )
C
C
	  ELSE IF  ( alias .eq. 'OSCT'      .or. 
     +               alias .eq. 'OSCT_HI'   .or.
     +               alias .eq. 'OAMBG1_HI' .or.
     +               alias .eq. 'OAMBG2_HI' .or.
     +               alias .eq. 'OAMBG3_HI' .or.
     +               alias .eq. 'OAMBG4_HI' ) THEN
	    IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
                interv = 0
              ELSE
                interv = NINT ( fvalu(2) )
            END IF
	    itmclr = iclrs(2)
C
	    itmwid  = fline ((2-1)*2+2) 
C
	    brbsiz = farrw ((3-1)*4+1)
	    ahsiz  = farrw ((3-1)*4+2)
	    ibwid  = farrw ((3-1)*4+3)
	    ityp   = farrw ((3-1)*4+4)
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
		jclrs2 (i-2) = iclrs2(i)
	    END DO
C
	    CALL GG_OSCT ( alias, dattim, kwninc, jclrs, jclrs2, nn, 
     +		   brbsiz, ibwid, ahsiz, ityp, iskip, interv, itmclr,
     +		   itmwid, iflgs, ier )
C
C
	  ELSE IF  ( alias .eq. 'QSCT' .or. alias .eq. 'QSCT_HI' .or.
     +		     alias .eq. 'SEAWND' .or. alias .eq. 'SEAWND_HI' .or.
     +		     alias .eq. 'AMBG1' .or. alias .eq. 'AMBG2' .or.
     +		     alias .eq. 'AMBG3' .or. alias .eq. 'AMBG4' ) THEN
	    IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
                interv = 0
              ELSE
                interv = NINT ( fvalu(2) )
            END IF
	    itmclr = iclrs(2)
C
	    itmwid  = fline ((2-1)*2+2) 
C
	    brbsiz = farrw ((3-1)*4+1)
	    ahsiz  = farrw ((3-1)*4+2)
	    ibwid  = farrw ((3-1)*4+3)
	    ityp   = farrw ((3-1)*4+4)
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
		jclrs2 (i-2) = iclrs2(i)
	    END DO
C
	    CALL GG_QSCT ( alias, dattim, kwninc, jclrs, jclrs2, nn, 
     +		   brbsiz, ibwid, ahsiz, ityp, iskip, interv, itmclr,
     +		   itmwid, iflgs, ier )
C
C
	  ELSE IF  ( alias .eq. 'WSAT' .or. alias .eq. 'WAMBG1' .or.
     +               alias .eq. 'WAMBG2' .or. alias .eq. 'WAMBG3' .or.
     +               alias .eq.  'WAMBG4' ) THEN 
	    IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
                interv = 0
              ELSE
                interv = NINT ( fvalu(2) )
            END IF
	    itmclr = iclrs(2)
C
	    itmwid  = fline ((2-1)*2+2) 
C
	    brbsiz = farrw ((3-1)*4+1)
	    ahsiz  = farrw ((3-1)*4+2)
	    ibwid  = farrw ((3-1)*4+3)
	    ityp   = farrw ((3-1)*4+4)
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
		jclrs2 (i-2) = iclrs2(i)
	    END DO
C
	    CALL GG_WSAT ( alias, dattim, kwninc, jclrs, jclrs2, nn, 
     +		   brbsiz, ibwid, ahsiz, ityp, iskip, interv, itmclr,
     +		   itmwid, iflgs, ier )
C
C
	ELSE IF  ( alias .eq. 'SGWH' .or. alias .eq. 'SGWHE' .or. 
     +             alias .eq. 'SGWHG' .or. alias .eq. 'SSHA' .or.
     +             alias .eq. 'SGWH2' .or. alias .eq. 'SGWHC' .or.
     +             alias .eq. 'SGWHA' )  THEN
	  IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
		interv = 0
	      ELSE
		interv = NINT ( fvalu(2) )
	    END IF
	    itmclr = iclrs(2)
C
	    IF ( alias .eq. 'SSHA' )  THEN
		mrktyp = fsym1((2-1)*3+1)
		sizmrk = fsym1((2-1)*3+2)
		mrkwid = fsym1((2-1)*3+3)
	    END IF
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
	    END DO
C
	    CALL GG_WAVE ( alias, dattim, kwninc, jclrs, nn,
     +		mrktyp, sizmrk, mrkwid, iskip, interv, itmclr,
     +		ier )
C
C
	ELSE IF  ( alias .eq. 'WSPDA' .or. alias .eq. 'WSPD2' .or. 
     +             alias .eq. 'WSPDC' ) THEN 
	  IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	      ELSE
		iskip = NINT ( fvalu(1) )
	    END IF
	    IF  ( iclrs(2) .eq. 0 )  THEN
		interv = 0
	      ELSE
		interv = NINT ( fvalu(2) )
	    END IF
	    itmclr = iclrs(2)
C
	    nn = numc - 2
	    DO  i = numc, 3, -1
		kwninc(i-2) = fvalu(i)
		jclrs (i-2) = iclrs(i)
	    END DO
C
	    CALL GG_WSPD ( alias, dattim, kwninc, jclrs, nn,
     +		iskip, interv, itmclr, ier )
C
C
	  ELSE IF  ( alias .eq. 'TRAK1' .or. alias .eq. 'TRAKE' .or.
     +               alias .eq. 'TRAK2' .or. alias .eq. 'TRAKS' .or.
     +               alias .eq. 'TRAKC' ) THEN
            icolor = iclrs(1)
	    IF  ( iclrs(1) .eq. 0 )  THEN
		iskip = 0
	    ELSE
		iskip = NINT ( fvalu(2) )
	    END IF
            CALL GG_TRAK ( alias, dattim, icolor, iskip, ier )
C
C
	  ELSE IF  ( alias .eq. 'VGF' )  THEN
	    CALL CDS_GFILL ( jflflg, ier )
	    CALL CDS_SFILL ( iflgs(1), ier )
C
	    CALL GG_DVGF ( filnam, iclrs(1), ier )
C
	    CALL CDS_SFILL ( jflflg, ier )
C
	    CALL FL_PATH ( filnam, dirnam, newfil, ier )
C
	  ELSE
	    iret = -2
	    RETURN
	END IF
C
C*	Set constant values for color and text for the title.
C
	CALL GSCOLR  ( 31, ier )
	CALL GSTEXT  ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
C*	Create and plot the title string.
C
	CALL ST_LSTR ( alias, lena, ier )
        IF  ( ( newfil .eq. 'NONE' ) .or. ( newfil .eq. ' ' ) .or.
     +        ( newfil .eq. 'ENS_CYC' ) )  THEN
	    IF  ( alias .eq. 'VGF' )  THEN
		ttlstr = alias(1:lena)
	      ELSE
		ttlstr = alias(1:lena) // ' ' // dattim
	    END IF
	  ELSE
	    CALL ST_LSTR ( newfil, lenf, ier )
	    IF  ( alias .eq. 'VGF' )  THEN
		ttlstr = alias(1:lena) // ' ' // newfil(1:lenf)
	      ELSE
		ttlstr = alias(1:lena) // ' ' //
     +			 newfil(1:lenf) // ' ' // dattim
	    END IF
	END IF
	CALL GG_WSTR ( ttlstr, ititl, ier )
C
C*	Reset the saved settings for color and text.
C
	CALL GSCOLR  ( jclr, ier )
	CALL GSTEXT  ( jtxfn, jtxhw, sztxt, jtxwid, jbrdr,
     +		       jrrotn, jjust, ier )
C*
	RETURN
	END
