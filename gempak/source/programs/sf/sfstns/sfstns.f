	PROGRAM SFSTNS
C************************************************************************
C* PROGRAM SFSTNS							*
C*									*
C* This program updates the geographic information in a surface file.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* S. Schotz/GSC	10/90	Added warning if no stations updated	*
C* M. desJardins/GSFC	12/90	Skip blank stid and missing stnm	*
C* K. Brill/NMC		 8/93	Changes for 8-char station id		*
C* K. Brill/NMC		 8/93	Added ISPRI to TB_RSTN & SF_ASTN calls	*
C* L. Williams/EAI	 7/94	Removed call to SFSUPD			*
C* D. Keiser		12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Changed calling sequence for TB_RSTN    *
C*                              to add character string parameter       *
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 3/99   Added priority parameter to SF_USTN     *
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	sffile*(LLMXLN), stnfil*(LLMXLN),
     +			keynam*(LLMXLN)
	LOGICAL		addstn
C*
	CHARACTER	prm (MMPARM)*4, tbid*8, filnam*72
	CHARACTER	tbsta*2, tbcoun*2, name*32, astid (LLSTFL)*8
	CHARACTER	addcn (LLSTFL), addsta (LLSTFL)*2, tbchrs*20
	INTEGER		idadd (LLSTFL), iadpri (LLSTFL)
	REAL		addlat (LLSTFL), addlon (LLSTFL), 
     +			addelv (LLSTFL)
	LOGICAL		respnd, proces, done, ok
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFSTNS', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFSTNS', ier )
C
C*	Main loop.
C
	done = .false.
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFSINP  ( sffile, stnfil, addstn, keynam, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFSTNS', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Check for valid keynam.
C
	    CALL ST_LCUC  ( keynam, keynam, ier )
	    IF  ( ( keynam .ne. 'STID' ) .and. ( keynam .ne. 'STNM' ) )
     +							THEN
		proces = .false.
		CALL ER_WMSG  ( 'SFSTNS', -3, keynam, ier )
	    END IF
C
C*	    Open the surface file.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( sffile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SF_OPNF  ( filnam, .true., isffln, is, nparm, prm, 
     +				ier )
		IF  ( ier .ne. 0 )  THEN
		    proces = .false.
		END IF
	    END IF
C
C*	    Open the table file.
C
	    IF  ( proces )  THEN
		CALL FL_TBOP  ( stnfil, 'stns', lunstn, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'FL', iret, stnfil, ier )
		    iret = -4
		    CALL ER_WMSG  ( 'SFSTNS', iret, stnfil, ier )
		    proces = .false.
		    CALL SF_CLOS  ( isffln, ier )
		END IF
	    END IF
C
C*	    Read each station in the table.
C
	    IF  ( proces )  THEN
		iout = 0
		ns   = 0
		nadd = 0
		DO WHILE  ( iout .eq. 0 ) 
		    CALL TB_RSTN  ( lunstn, tbid, name, id, tbsta, 
     +				    tbcoun, tlat, tlon, telv,
     +				    ispri, tbchrs, iout )
		    ok = .true.
		    IF  ( ( keynam .eq. 'STID' ) .and. 
     +			  ( tbid .eq. ' ' ) )  ok = .false.
		    IF  ( ( keynam .eq. 'STNM' ) .and.
     +			  ( ( id .eq. IMISSD ) .or. ( id .eq. 0 ) ) )
     +						ok = .false.
		    IF   ( ( iout .eq. 0 ) .and. ok ) THEN
C
C*			Check for station in the file.
C
			CALL SF_USTN  ( isffln, tbid, id, tlat, tlon, 
     +					telv, ispri, tbsta, tbcoun, 
     +					keynam, iret )
			IF  ( iret .eq. 0 )  THEN
			    ns = ns + 1
			    WRITE  ( 6, 40 )  tbid, id, ns
40			    FORMAT ( '+Station: ', A, ' ', I7, I7, 
     +                               ' updated')
C
C*			     If not found, add station if requested.
C
			  ELSE IF (addstn .and. ( iret .eq. -10 )) THEN
			    nadd = nadd + 1
			    astid  (nadd) = tbid
			    idadd  (nadd) = id
			    addlat (nadd) = tlat
			    addlon (nadd) = tlon
			    addelv (nadd) = telv
			    iadpri (nadd) = ispri
			    addsta (nadd) = tbsta
			    addcn  (nadd) = tbcoun
			  ELSE IF  ( iret .ne. -10 )  THEN
			    CALL ER_WMSG  ( 'SF', iret, ' ', ier )
			    iout = -1
			END IF
		    END IF
		END DO
C
C*		If there are stations to add, do that now.
C
		IF  ( nadd .gt. 0 )  THEN
		    WRITE  ( 6, 1000 ) nadd
1000	            FORMAT ( ' Attempting to add ', I6, ' stations' )
		    CALL SF_ASTN  ( isffln, nadd, astid, idadd, addlat, 
     +				    addlon, addelv, addsta, addcn, 
     +				    iadpri, nad, ier )
		    IF  ( ier .ne. 0 )  CALL ER_WMSG  
     +						( 'SF', ier, ' ', ierr )
		    WRITE  ( 6, 1001 ) nad
1001		    FORMAT ( ' There were ', I6, ' stations added.' )
		END IF
C
C*              Write out warning message if no stations updated or 
C*              added.
C
		IF  ( ( nadd .eq. 0 ) .and. ( ns .eq. 0 ) )  THEN
		    CALL ER_WMSG ( 'SFSTNS', 1, ' ', ier )
		END IF
C
C*		Close the surface file and the table file.
C
		CALL SF_CLOS  ( isffln, iret )
		CALL FL_CLOS  ( lunstn, iret )
	    END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
