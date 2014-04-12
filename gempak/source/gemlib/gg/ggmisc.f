        SUBROUTINE GG_MISC ( numc, lclrwt, lclrwn, lclrhn, lclris,
     +			     lclrat, atmodl, lclram, lclrgam, lclrnc,
     +                       lclrsv, lclrtc, lclrws, lclrwo, lclrof, 
     +                       lclrwu, lclruf, lclcsg, lclrqw, lclrqr, 
     +                       lclrwp, lclrww, lclrwr, lclraw, lclrar, 
     +                       lclren, enmodl, lclrff, lclr1k, lclrek, 
     +                       lclr2k, lclrow, lclror, lclsg1, lclsgc,
     +                       lclsge, lclsgg, lclsg2, lclsga, lclwsa,
     +                       lclws2, lclwsc, lclrsk, lclrck, iret )
C************************************************************************
C* GG_MISC                                                              *
C*                                                                      *
C* This subroutine reads a table of miscellaneous data type attributes  *
C* to get the default color numbers for each type of miscellaneous data *
C* and the model names for ATCF and ensemble cyclone data.              *
C*                                                                      *
C* GG_MISC ( NUMC, LCLRWT, LCLRWN, LCLRHN, LCLRIS, LCLRAT, ATMODL,	*
C*	     LCLRAM, LCLRGAM, LCLRNC, LCLRSV, LCLRTC, LCLRWS, LCLRWO,   *
C*           LCLROF, LCLRWU, LCLRUF, LCLCSG, LCLRQW, LCLRQR, LCLRWP,    *
C*           LCLRWW, LCLRWR, LCLRAW, LCLRAR, LCLREN, ENMODL, LCLRFF,    *
C*           LCLR1K, LCLREK, LCLR2K, LCLROW, LCLROR, LCLSG1, LCLSGC,    *
C*           LCLSGE, LCLSGG, LCLSG2, LCLSGA, LCLWSA, LCLWS2, LCLWSC,    *
C*           LCLRSK, LCLRCK, IRET )	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      NUMC            INTEGER		Maximum number of colors/models *
C*                                                                      *
C* Output parameters:                                                   *
C*	LCLRWT(*)	INTEGER		Color numbers for watches       *
C*	LCLRWN(*)	INTEGER		Color numbers for warnings      *
C*	LCLRHN(*)	INTEGER		Color numbers for hurricanes    *
C*	LCLRIS(*)	INTEGER		Color numbers for intl SIGMETS  *
C*	LCLRAT(*)	INTEGER		Color numbers for ATCF tracks   *
C*	ATMODL(*)	CHAR*		ATCF model names                *
C*	LCLRAM(2,*)	INTEGER		Color numbers for airmets       *
C*      LCLRGAM(2,*)    INTEGER         Color numbers for g-airmets     *
C*	LCLRNC(*)	INTEGER		Color numbers for non-conv sgmts*
C*	LCLRSV(*)	INTEGER		Color numbers for SLS watches   *
C*	LCLRTC(*)	INTEGER		Color numbers for trop cy danger*
C*	LCLRWS(*)	INTEGER		Color numbers for winter storm	*
C*	LCLRWO(*)	INTEGER		Color numbers for WOU markers 	*
C*					and outline			*
C*	LCLROF(*)       INTEGER         Color num for WOU county fill	*
C*	LCLRWU(*)	INTEGER		Color numbers for WCN markers	*
C*					outline			   	*
C*	LCLRUF(*)       INTEGER         Color num for WCN county fill	*
C*	LCLCSG(*)	INTEGER		Color numbers for conv sigmets 	*
C*	LCLRQW(*)	INTEGER		Color numbers for QSCT wind	*
C*	LCLRQR(*)	INTEGER		Color numbers for QSCT rain	*
C*	LCLRWP(*)	INTEGER		Color numbers for WCP		*
C*	LCLRWW(*)	INTEGER		Color numbers for WSAT wind	*
C*	LCLRWR(*)	INTEGER		Color numbers for WSAT rain	*
C*	LCLRAW(*)	INTEGER		Color numbers for ASCT wind	*
C*	LCLRAR(*)	INTEGER		Color numbers for ASCT QC fail	*
C*      LCLREN(*)       INTEGER         Color nums for ens cyclone tks  *
C*      ENMODL(*)       CHAR*           Ensemble cyclone model names    *
C*	LCLRFF(*)	INTEGER		Color numbers for FFA		*
C*	LCLR1K(*)	INTEGER		Color number for TRAK1		*
C*	LCLREK(*)	INTEGER		Color number for TRAKE		*
C*	LCLR2K(*)	INTEGER		Color number for TRAK2		*
C*	LCLROW(*)	INTEGER		Color numbers for OSCT wind	*
C*	LCLROR(*)	INTEGER		Color numbers for OSCT rain	*
C*	LCLSG1(*)	INTEGER		Color numbers for Jason-1    	*
C*	                                significant wave height      	*
C*	LCLSGC(*)	INTEGER		Color numbers for CryoSat    	*
C*	                                significant wave height      	*
C*	LCLSGE(*)	INTEGER		Color numbers for Envisat    	*
C*	                                significant wave height      	*
C*	LCLSGG(*)	INTEGER		Color numbers for GFO        	*
C*	                                significant wave height      	*
C*	LCLSG2(*)	INTEGER		Color numbers for Jason-2    	*
C*	                                significant wave height      	*
C*	LCLSGA(*)	INTEGER		Color numbers for Altika     	*
C*	                                significant wave height      	*
C*	LCLWSA(*)	INTEGER		Color numbers for Altika     	*
C*	                                wind speed		      	*
C*	LCLWS2(*)	INTEGER		Color numbers for Jason-2    	*
C*	                                wind speed		      	*
C*	LCLWSC(*)	INTEGER		Color numbers for Cryosat     	*
C*	                                wind speed		      	*
C*	LCLRSK(*)	INTEGER		Color number for TRAKS		*
C*	LCLRCK(*)	INTEGER		Color number for TRAKC		*
C*      IRET            INTEGER         Return code                     *
C*                                         0 = normal return            *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP      6/00                                            *
C* D. Kidwell/NCEP      7/00 	Added color numbers for airmets         *
C* D. Kidwell/NCEP      8/00 	Added color numbers for non-conv sigmets*
C* F. J. Yen/NCEP      12/00	Added color numbers for SLS watches	*
C* F. J. Yen/NCEP	5/01	Added ATCO (Old ATCF)			*
C* D. Kidwell/NCEP      7/01 	Added color numbers for tropical cyclone*
C*				danger areas; fixed ATCO bug            *
C* M. Li/SAIC		8/02	Added LCLRWS for winter storm		*
C* A. Hardy/NCEP	2/03	Added LCLRWO for WOUs; LCLRWU for WCNs	*
C* m.gamazaychikov/SAIC	10/03	Removed GEMPAK variable ATCO		*
C* m.gamazaychikov/SAIC	01/04	Added color numbers for conv sigmets	*
C* m.gamazaychikov/SAIC	01/04	Fixed default color setting for airmets *
C* M. Li/SAIC		10/04	Added LCLRQW and LCLRQR			*
C* M. Li/SAIC		01/05	Added lclrof, and lclruf		*
C* F. J. Yen/NCEP	 4/05	Addded lclrwp				*
C* m.gamazaychikov/SAIC 04/05   Added lclren and enmodl                 *
C* D. Kidwell/NCEP	 6/05	Changed default lclren 1 -> 0 for ggency*
C* C. Bailey/HPC	 4/06	Added lclrff				*
C* G. McFadden/SAIC	 6/06	Added LCLRWW and LCLRWR and alias WSAT	*
C* T. Piper/SAIC	07/06	Declare type, ldum, and ldum1 w/numc	*
C* G. McFadden/SAIC	01/07	Added LCLRAW and LCLRAR and alias ASCT	*
C* G. McFadden/SAIC	07/07	Changed description of LCLRAR from rain *
C* 				to quality control (QC) fail	 	*
C* G. McFadden/SAIC	12/08	Added LCLR1K, LCLREK, and LCLR2K        *
C*                              and aliases TRAK1, TRAKE, and TRAK2     *
C* L. Hinson/AWC        04/10   Add LCLRGAM for G-AIRMET                *
C* G. McFadden/IMSG	 9/10	Added LCLROW and LCLROR and alias OSCT	*
C* G. McFadden/IMSG	 7/11	Added LCLSG1, LCLSGC, LCLSGE, LCLSGG,   *
C* 				LCLSG2, and aliases SGWH, SGWHC, SGWHE, *
C*                              SGWHG, SGWH2                            *
C* G. McFadden/IMSG	 7/13	Added LCLSGA, LCLWSA, and aliases SGWHA *
C*                              and WSPDA                               *
C* G. McFadden/IMSG	11/13	Added LCLWS2, LCLWSC, and aliases WSPD2 *
C*                              and WSPDC                               *
C* G. McFadden/IMSG	 1/14	Added LCLRSK, LCLRCK, and aliases TRAKS *
C*                              and TRAKC                               *
C************************************************************************
        CHARACTER*(*)   atmodl (*),  enmodl (*)
	INTEGER		lclrwt (*), lclrwn (*), lclrhn (*), lclris (*),
     +			lclrat (*), lclram (2,*), lclrgam (2,*), 
     +                  lclrnc (*), lclrsv (*), lclrtc (*), lclrws (*),
     +                  lclrwo (*), lclrof (*), lclrwu (*), lclruf (*), 
     +                  lclcsg (*), lclrqw (*), lclrqr (*), lclrwp (*), 
     +                  lclrww(*), lclrwr(*),  lclraw (*), lclrar (*), 
     +                  lclren (*), lclrff (*),lclr1k (*), lclrek (*), 
     +                  lclr2k (*), lclrow (*), lclror (*), lclsg1 (*),
     +                  lclsgc (*), lclsge (*), lclsgg (*), lclsg2 (*),
     +                  lclsga (*), lclwsa (*), lclws2 (*), lclwsc (*),
     +                  lclrsk (*), lclrck (*)
C*
	PARAMETER	( NUMALS = 35 )
C*
        CHARACTER       alias(NUMALS)*7, buffer*80, carr(2)*8, 
     +			type(numc)*20
	INTEGER		ldum1(numc), ldum(numc)
	LOGICAL		found
C*
	DATA		alias / 'WTCH', 'WARN', 'HRCN', 'ISIG', 'ATCF',
     +			        'AIRM', 'GAIRM', 'NCON', 'SVRL', 'TCMG',
     +                          'WSTM', 'WOU' , 'WCN' , 'CSIG', 'QSCT',
     +                          'WCP', 'ENS_CYC', 'FFA', 'WSAT', 'ASCT',
     +                          'TRAK1', 'TRAKE', 'TRAK2', 'OSCT',
     +                          'SGWH', 'SGWHC', 'SGWHE', 'SGWHG',
     +                          'SGWH2', 'SGWHA', 'WSPDA', 'WSPD2',
     +                          'WSPDC', 'TRAKS', 'TRAKC' /
C------------------------------------------------------------------------
        iret = 0
C
	DO ii = 1, numc
	    lclrwt ( ii ) = 1
	    lclrwn ( ii ) = 1
	    lclrhn ( ii ) = 1
	    lclris ( ii ) = 1
	    lclrat ( ii ) = 1
	    atmodl ( ii ) = ' '
	    lclram ( 1, ii ) = 1
	    lclram ( 2, ii ) = 1
            lclrgam( 1, ii ) = 1
            lclrgam( 2, ii ) = 1
	    lclrnc ( ii ) = 1
	    lclrsv ( ii ) = 1
	    lclrtc ( ii ) = 1
	    lclrws ( ii ) = 1
	    lclrwo ( ii ) = 1
	    lclrof ( ii ) = 1
	    lclrwu ( ii ) = 1
	    lclruf ( ii ) = 1
	    lclcsg ( ii ) = 1
	    lclrqw ( ii ) = 1
	    lclrqr ( ii ) = 1
	    lclrwp ( ii ) = 1
	    lclrww ( ii ) = 1
	    lclrwr ( ii ) = 1
	    lclraw ( ii ) = 1
	    lclrar ( ii ) = 1
	    lclren ( ii ) = 0
            enmodl ( ii ) = ' '
	    lclrff ( ii ) = 1
	    lclr1k ( ii ) = 1
	    lclrek ( ii ) = 1
	    lclr2k ( ii ) = 1
 	    lclrow ( ii ) = 1
 	    lclror ( ii ) = 1
 	    lclsg1 ( ii ) = 1
 	    lclsgc ( ii ) = 1
 	    lclsge ( ii ) = 1
 	    lclsgg ( ii ) = 1
 	    lclsg2 ( ii ) = 1
 	    lclsga ( ii ) = 1
 	    lclwsa ( ii ) = 1
 	    lclws2 ( ii ) = 1
 	    lclwsc ( ii ) = 1
            lclrsk ( ii ) = 1
            lclrck ( ii ) = 1
	END DO
C
C*      Open table.
C
        CALL FL_TBOP ( 'miscset.tbl', 'config', lun, ier )
        IF ( ier .ne. 0 ) THEN
            CALL ER_WMSG  ( 'FL', ier, 'miscset.tbl', ierr )
            RETURN
        END IF
C
2       FORMAT ( A )
C
        iostat = 0
	icount = 0
C
C*      Match alias with its attribute list.
C
        DO WHILE  ( ( iostat .eq. 0 ) .and. ( icount .le. NUMALS ) )
            READ ( lun, 2, IOSTAT = iostat ) buffer
            IF ( ( iostat .eq. 0 ) .and. ( buffer (:5) .eq. 'ALIAS' ) )
     +             THEN
		CALL ST_CLST ( buffer, ' ', ' ', 2, carr, num, ier )
		found = .true.
		IF ( carr ( 2 ) .eq. alias ( 1 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrwt, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 2 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrwn, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 3 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrhn, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 4 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclris, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 5 ) ) THEN 
		    CALL GG_ALIS ( lun, numc, lclrat, ldum, atmodl,ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 6 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
	            DO ii = 1, numc
	              lclram ( 1, ii ) = ldum1 ( ii )
	              lclram ( 2, ii ) = ldum  ( ii )
	            END DO
                  ELSE IF ( carr ( 2 ) .eq. alias ( 7 ) ) THEN
                    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
	            DO ii = 1, numc
	              lclrgam ( 1, ii ) = ldum1 ( ii )
	              lclrgam ( 2, ii ) = ldum  ( ii )
	            END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 8 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrnc, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 9 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrsv, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 10 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrtc, ldum, type, ier )
                  ELSE IF ( carr ( 2 ) .eq. alias ( 11 ) ) THEN
                    CALL GG_ALIS ( lun, numc, lclrws, ldum, type, ier )
                  ELSE IF ( carr ( 2 ) .eq. alias ( 12 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc
                      lclrwo (ii)  = ldum1 ( ii )
                      lclrof (ii)  = ldum  ( ii )
                    END DO
                  ELSE IF ( carr ( 2 ) .eq. alias ( 13 ) ) THEN
                    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc
                      lclrwu (ii)  = ldum1 ( ii )
                      lclruf (ii)  = ldum  ( ii )
                    END DO
                  ELSE IF ( carr ( 2 ) .eq. alias ( 14 ) ) THEN
                    CALL GG_ALIS ( lun, numc, lclcsg, ldum, type, ier )
                  ELSE IF ( carr ( 2 ) .eq. alias ( 15 ) ) THEN
                    CALL GG_ALIS (lun, numc, ldum1, ldum, type, ier)
                    DO ii = 1, numc - 2
                        lclrqw (ii) = ldum1 (ii + 2)
                        lclrqr (ii) = ldum  (ii + 2) 
                    END DO
                  ELSE IF ( carr ( 2 ) .eq. alias ( 16 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrwp, ldum, type, ier )
                  ELSE IF ( carr ( 2 ) .eq. alias ( 17 ) ) THEN
                    CALL GG_ALIS ( lun, numc, lclren, ldum, enmodl,ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 18 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrff, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 19 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclrww (ii) = ldum1 (ii + 2)
                        lclrwr (ii) = ldum  (ii + 2) 
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 20 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclraw (ii) = ldum1 (ii + 2)
                        lclrar (ii) = ldum  (ii + 2) 
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 21 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclr1k, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 22 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrek, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 23 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclr2k, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 24 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclrow (ii) = ldum1 (ii + 2)
                        lclror (ii) = ldum  (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 25 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsg1 (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 26 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsgc (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 27 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsge (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 28 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsgg (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 29 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsg2 (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 30 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclsga (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 31 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclwsa (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 32 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclws2 (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 33 ) ) THEN
		    CALL GG_ALIS ( lun, numc, ldum1, ldum, type, ier )
                    DO ii = 1, numc - 2
                        lclwsc (ii) = ldum1 (ii + 2)
                    END DO
		  ELSE IF ( carr ( 2 ) .eq. alias ( 34 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrsk, ldum, type, ier )
		  ELSE IF ( carr ( 2 ) .eq. alias ( 35 ) ) THEN
		    CALL GG_ALIS ( lun, numc, lclrck, ldum, type, ier )
		  ELSE
		    found = .false.
		END IF
		IF ( found )  icount = icount + 1
	    END IF
	END DO
C
C*      Close table.
C
        CALL FL_CLOS ( lun, ier )
C*
	RETURN
	END
