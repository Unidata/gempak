	PROGRAM SNSTNS
C************************************************************************
C* PROGRAM SNSTNS							*
C*									*
C* This program updates the geographic information in a surface file.	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* S. Schotz/GSC	10/90	Add warning for  no stations updated	*
C* M. desJardins/GSFC	12/90	Dont use blank stid or missing stnm	*
C* K. Brill/NMC		 8/93	tbid*4 -> tbid*8; astid*4 -> astid*8	*
C* K. Brill/NMC		 8/93	Added ISPRI to TB_RSTN call		*
C* L. Williams/EAI	 7/94	Removed call to SNNUPD			*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* L. Sager/NCEP         6/96   Changed calling sequence for TB_RSTN    *
C*                              to add character string parameter       *
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	12/99	Changed size of tbchrs 14->20		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), stnfil*(LLMXLN),
     +			idntyp*(LLMXLN)
C*
	LOGICAL		addstn
C*
	CHARACTER	prm (MMPARM)*4, tbid*8, tbchrs*20
	CHARACTER	tbsta*2, tbcoun*2, name*32, astid (LLSTFL)*8
	CHARACTER	addcn (LLSTFL), addsta (LLSTFL)*2, filnam*72
	INTEGER		idadd (LLSTFL)
	REAL		addlat (LLSTFL), addlon (LLSTFL), 
     +			addelv (LLSTFL)
	LOGICAL		respnd, proces, done, mrgdat, ok
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SNSTNS', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SNSTNS', ier )
C
C*	Main loop.
C
	done = .false.
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SNNINP  ( snfile, stnfil, addstn, idntyp, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SNSTNS', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Check for valid idntyp.
C
	    CALL ST_LCUC  ( idntyp, idntyp, ier )
	    IF  ( ( idntyp .ne. 'STID' ) .and. ( idntyp .ne. 'STNM' ) )
     +							THEN
		proces = .false.
		CALL ER_WMSG  ( 'SNSTNS', -3, idntyp, ier )
	    END IF
C
C*	    Open the surface file.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SN_OPNF  ( filnam, .true., isnfln, is, nparm, prm, 
     +				ivert, mrgdat, ier )
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
		    CALL ER_WMSG  ( 'SNSTNS', iret, stnfil, ier )
		    proces = .false.
		    CALL SN_CLOS  ( isnfln, ier )
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
C
C*			Check for station in the file.
C
                    ok = .true.
                    IF  ( ( idntyp .eq. 'STID' ) .and. 
     +                    ( tbid .eq. ' ' ) )  ok = .false.
                    IF  ( ( idntyp .eq. 'STNM' ) .and.
     +                    ( ( id .eq. IMISSD ) .or. ( id .eq. 0 ) ) )
     +                                          ok = .false.
		    IF   ( ( iout .eq. 0 ) .and. ok ) THEN
			CALL SN_USTN  ( isnfln, tbid, id, tlat, tlon, 
     +					telv, tbsta, tbcoun, idntyp,
     +					iret )
			IF  ( iret .eq. 0 )  THEN
			    ns = ns + 1
			    WRITE  ( 6, 40 )  tbid, id, ns
40			    FORMAT ( '+Station: ', A, ' ', I7, I7, 
     +                               ' updated')
C
C*			     If not found, add station if requested.
C
			  ELSE IF (addstn .and. ( iret .eq. -11 )) THEN
			    nadd = nadd + 1
			    astid  (nadd) = tbid
			    idadd  (nadd) = id
			    addlat (nadd) = tlat
			    addlon (nadd) = tlon
			    addelv (nadd) = telv
			    addsta (nadd) = tbsta
			    addcn  (nadd) = tbcoun
			  ELSE IF  ( iret .ne. -11 )  THEN
			    CALL ER_WMSG  ( 'SN', iret, ' ', ier )
			    iout = -1
			END IF
		    END IF
		END DO
C
C*		If there are stations to add, do that now.
C
		IF  ( nadd .gt. 0 )  THEN
		    WRITE  ( 6, 1000 ) nadd
1000	            FORMAT ( ' Attemting to add ', I6, ' stations' )
		    CALL SN_ASTN  ( isnfln, nadd, astid, idadd, addlat, 
     +				    addlon, addelv, addsta, addcn, 
     +				    nad, ier )
		    IF  ( ier .ne. 0 )  CALL ER_WMSG  
     +						( 'SN', ier, ' ', ierr )
		    WRITE  ( 6, 1001, IOSTAT = iostat )  nad
1001		    FORMAT ( ' There were ', I4, ' stations added.' )
		END IF
C
C*		Warn user if no stations were updated or added.
C
		IF  ( ( nadd .eq. 0 ) .and. ( ns .eq. 0 ) )  THEN
		    CALL ER_WMSG ( 'SNSTNS', 1, ' ', ierr )
		END IF
C
C*		Close the surface file and the table file.
C
		CALL SN_CLOS  ( isnfln, iret )
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
