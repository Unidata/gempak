	SUBROUTINE GG_EDRP (lunf, dattm2, datfil, datgrp, nhts, 
     +			    htinc, htcolrs, tlimit, enumc, evinc,
     +                      esymb1, esymb2, esymbsz1, esymbsz2, 
     +                      evcolrs, aoa180fl, tracksfl, iret)
C************************************************************************
C* GG_EDRP                                                              *
C*                                                                      *
C* This subroutine reads and plots EDR tracking data from a single      *
C* file, using specified colors and time and/or height increments.      *
C*                                                                      *
C* GG_EDRP ( LUNF, DATTM2, DATFIL, DATGRP, NHTS, HTINC, HTCOLRS         *
C*           TLIMIT, ENUMC, EVINC, ESYMB1, ESYMB2, ESYMBSZ1, ESYMBSZ2,  *
C*           EVCOLRS, AOA180FL, IRET )                                  *
C*                                                                      *
C* Input parameters:                                                    *
C*	LUNF		INTEGER		Logical unit number for input   *
C*	DATTM2		CHAR*		GEMPAK ending time for data     *
C*	DATFIL		CHAR*		GEMPAK time for current data    *
C*	DATGRP(*)	CHAR*		GEMPAK break times for colors   *
C*	NHTS		INTEGER 	Number of heights               *
C*      HTINC(*)        INTEGER         Height Values                   *
C*	HTCOLRS(*)	INTEGER		Color for each height           *
C*      TLIMIT          INTEGER         Time Length Limit to Tracks     *
C*      ENUMC           INTEGER         Number of EDR Values            *
C*      EVINC(*)        REAL            EDR Values                      *
C*      ESYMB1(*)       INTEGER         Symbols BLO 180                 *
C*      ESYMB2(*)       INTEGER         Symbols AOA FL180               *
C*      ESYMBSZ1(*)     REAL            Symbol size BLO 180             *
C*      ESYMBSZ2(*)     REAL            Symbol size AOA FL180           *
C*      EVCOLRS(*)      INTEGER         Color for each EDR Value        *
C*      AOA180FL        LOGICAL         FLAG to Plot 18 KFT +           *
C*      TRACKSFL        LOGICAL         FLAG to Plot Tracks             * 
C*                                                                      *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* L. Hinson/AWC        09/12                                           *
C* L. Hinson/AWC        10/18 Updated to add tracks flag                *
C************************************************************************ 
	INCLUDE 'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattm2, datfil, datgrp (*)
	INTEGER         htinc (*)
	INTEGER		htcolrs (*)
        INTEGER         tlimit
        INTEGER         enumc
        REAL            evinc  (*)
        INTEGER         esymb1 (*), esymb2 (*)
        REAL            esymbsz1 (*), esymbsz2 (*)
        INTEGER         evcolrs (*)
        LOGICAL         aoa180fl
        LOGICAL         tracksfl
        INTEGER         ht, ixoff, iyoff
C*
	PARAMETER	( MAXSTK = LLMXPT * 2 )
C*
	REAL            lats (LLMXPT), lons (LLMXPT), ages(LLMXPT)        
        REAL            edr (LLMXPT), edrv
        INTEGER         hts (LLMXPT)
	
	CHARACTER       buffer*100
	CHARACTER       carr (11)*20
	CHARACTER*(20)  key
        CHARACTER*(4)   htstr
	INTEGER         numpts, age1, age2
        INTEGER         ii, jj
	LOGICAL         found
        REAL            latw(3), lonw(3)
C-----------------------------------------------------------------------
        ier = 0
	found = .false.
	CALL GQCOLR (jcolr, ier)
	CALL GQLINE (jtype, jtyhw, jwidth, jwdhw, ier)
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GQTURB ( szturb, jtuwid, ier )
	CALL GQICNG ( szicng, jicwid, ier )
	CALL GQSPCL ( szsswn, jswwid, ier )
        CALL GQMRKR ( jmark, jmkhw, szmark, jkmwid, ier )
	iostat = 0
	iltyp = 1
	ilwidth = 1
	
C*      Loop on the records in the file...
        DO WHILE (iostat .eq. 0)
	  READ ( lunf, 2, IOSTAT = iostat ) buffer
2	  FORMAT ( A )
	  IF ( iostat .eq. 0) THEN
	    CALL ST_CLST (buffer, '|', ' ', 2, carr, num, ier )
      	    key = carr(1)
	    CALL ST_NUMB ( carr ( 2 ), numpts, ier )
C*            Read the lat/lon coordinates from the file.
            jostat = 0
            DO ii = 1, numpts
	      READ ( lunf, 2, IOSTAT = jostat ) buffer
              IF ( jostat .eq. 0) THEN
                CALL ST_CLST (buffer, '|', ' ', 5, carr, num, ier )
		CALL ST_CRNM ( carr (1), ages(ii), iret )
		CALL ST_CRNM ( carr (2), lats(ii), iret )
		CALL ST_CRNM ( carr (3), lons(ii), iret )
                CALL ST_NUMB ( carr (4), hts(ii), iret )
                CALL ST_CRNM ( carr (5), edr(ii), iret )
              END IF
            END DO
            ier = 0
C           DO QC here, and return ier=0 for success...
C           Plot Line Segments First, EDR Intensities 2nd.       
            IF ( ier .eq. 0 ) THEN
              IF ( tracksfl ) THEN
                DO ii = 1, numpts
                  IF ( ii .eq. 1 ) THEN
                    latw(1) = lats(ii)
                    lonw(1) = lons(ii)
                  ELSE
                    latw(2) = lats(ii)
                    lonw(2) = lons(ii)
                    age2 = ages(ii)
                    ht = hts(ii)
                    IF (ht .gt. 0 .and. ht .lt. htinc(1)) THEN
                        ic = htcolrs(1)
                    ELSE 
                      DO jj = 1, nhts - 1
                        IF (ht .gt. htinc(jj)) THEN
                          ic = htcolrs (jj+1)
                        END IF
                      END DO
                    END IF
                    IF (age2 .lt. tlimit) THEN
                      CALL GSCOLR (ic, ier)
                      CALL GSLINE ( iltyp, 0, ilwidth, 0, ier )
		      CALL GLINE ( 'M', 2, latw, lonw, ier )
                    END IF
                    latw(1) = latw(2)
                    lonw(1) = lonw(2)
                  END IF
                ENDDO
              END IF
C             Now Plot the Intensities, if any...
C             Cycle through the intensities primary,
C               Actual observation second to accomplish layer effect...
              DO jj = 1, enumc - 1
                DO ii = 1, numpts
                  edrv = edr(ii)
                  age1 = ages(ii)                  
                  IF ( age1 .le. tlimit .and. 
     +               ( (aoa180fl .and. hts(ii) .ge. 180) .or.
     +                 (.not. aoa180fl) ) ) THEN
C                   Here, we only plot the color jj, if edrv exceeds it
                    IF (edrv .ge. evinc(jj)) THEN
                      ic = evcolrs(jj + 1)                      
                      CALL GSCOLR ( ic, ier )
                      IF (hts(ii) .lt. 180) THEN
                        CALL GSMRKR ( esymb1(jj+1), 0, esymbsz1(jj+1),
     +                                1, ier )
                      ELSE
                        CALL GSMRKR ( esymb2(jj+1), 0, esymbsz2(jj+1),
     +                                1, ier )
                      END IF
                      
                      CALL GMARK ('M',1,lats(ii),lons(ii),ier)
C                     Label the height...
                      ixoff = -2
                      iyoff = -3
                      IF (edrv .ge. evinc(2)) THEN
                        CALL GSTEXT ( 1, 2, 1.0, 1, 111, 1, 1, ier)
                        WRITE (htstr,'(I3)') hts(ii)
                        CALL GTEXT ('M', lats(ii), lons(ii), htstr,
     +                               0., ixoff, iyoff, ierr)
                      END IF 
                               
                    END IF
                  END IF
                ENDDO
              ENDDO                  
            END IF
	  END IF
	END DO
	CALL FL_CLOS (lunf, ier )
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	CALL GSTURB ( szturb, jtuwid, ier )
	CALL GSICNG ( szicng, jicwid, ier )
	CALL GSSPCL ( szsswn, jswwid, ier )
        CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
C*	
	RETURN
	
	END


	
