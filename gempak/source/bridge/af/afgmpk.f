	SUBROUTINE AF_GMPK  ( irptdt, gemfil, iadstn, maxtim, 
     +			      cprms, imnem, numprm, report, lenr, iret )
C************************************************************************
C* AF_GMPK								*
C*									*
C* This subroutine retrieves interface-stored data, converts it into	*
C* GEMPAK output, and then writes the GEMPAK output to the GEMPAK	*
C* output stream.							*
C*									*
C* AF_GMPK ( IRPTDT, GEMFIL, IADSTN, MAXTIM, CPRMS, IMNEM, NUMPRM,      *
C*	     REPORT, LENR, IRET)                                        *
C*									*
C* Input parameters:							*
C*	IRPTDT (5)	INTEGER		Report time (YYYY,MM,DD,HH,MM)  *
C*	GEMFIL		CHAR*		GEMPAK output file name template*
C*	IADSTN		INTEGER		Number of additional stations   *
C*	MAXTIM		INTEGER		Number of times in output file  *
C*	CPRMS (*)	CHAR*		GEMPAK parms chosen for output  *
C*	IMNEM (*)	INTEGER		Subscript mapping,data to GEMPAK*
C*	NUMPRM		INTEGER		Count of chosen GEMPAK parms    *
C*	REPORT		CHAR*		Report text                     *
C*	LENR		INTEGER		Length of report                *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		08/97						*
C* D. Kidwell/NCEP	09/97	Changed interface, cleaned up           *
C* D. Kidwell/NCEP	 6/99	Modified turbulence and icing processing*
C* D. Kidwell/NCEP	 7/99	Added text, moved PIREP height checks   *
C* D. Kidwell/NCEP	 8/99	Added interface convert for CLC parms   *
C* D. Kidwell/NCEP	11/99	Removed XX999 reports                   *
C* D. Kidwell/NCEP	 1/00	Changed PIREP report id, added ATP1     *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C* S. Chiswell/Unidata	 5/01	Added check for duplicate location	*
C* D. Kidwell/NCEP	 1/02	Allowed missing elevation		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	gemfil, cprms (*), report
	INTEGER		irptdt (*), imnem (*)
C*
	CHARACTER	parms ( MMPARM )*4, dattim*12, filnam*100,
     +			rpid*8, coun*4, stat*4
	CHARACTER	logln*(LLMXLN)
	REAL		rdata ( MMPARM )
	INTEGER		nparm, iwrkdt (5), iclam (14)
C*
	SAVE		nparm, parms
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA		iclam 
     +			/ 1, 6, 6, 2, 2, 3, 3, 3, 4, 5, 9, 2, 3, 6 /
C*
C*----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
C
	DO ii = 1, MMPARM
	    rdata ( ii ) = RMISSD
	END DO
C
C*	Do not create GEMPAK output if the latitude or longitude is
C*	missing.
C
	IF  (  ( ERMISS ( rivals ( irslat ) ) )  .or.
     +	       ( ERMISS ( rivals ( irslon ) ) )  )  RETURN
	xlat = rivals ( irslat )
	xlon = rivals ( irslon )
C
C*	Use the flight level as the station elevation if it exists.
C*	Otherwise, use the pressure altitude if it exists.
C
	IF  ( .not. ERMISS ( rivals ( irflvl ) ) )  THEN
	    xelv = rivals ( irflvl )
	  ELSE IF  ( .not. ERMISS ( rivals ( irpsal ) ) )  THEN
	    xelv = rivals ( irpsal )
	  ELSE
	    xelv = RMISSD
	END IF
	xelv = PR_HGFM ( xelv )
C
C*	Set the report identifier.  Discard XX999 military reports per
C*	AWC request.
C
	rpid = ' '
	IF  ( civals ( icacid ) (1:1) .ne. ' ' )  THEN
	    rpid = civals ( icacid ) (1:8)
	  ELSE IF  ( civals ( icrpid ) (1:1) .ne. ' ' )  THEN
	    rpid = civals ( icrpid ) (1:8)
	  ELSE IF  ( civals ( icrsid ) (1:1) .ne. ' ' )  THEN
	    rpid = civals ( icrsid ) (1:8)
	END IF
	IF ( rpid .eq. 'XX999   ' ) RETURN
C
C*	Use the report date-time to generate a GEMPAK output filename.
C
	iwrkdt (1) = irptdt (1)
	iwrkdt (2) = irptdt (2)
	iwrkdt (3) = irptdt (3)
	iwrkdt (4) = irptdt (4)
	iwrkdt (5) = 0
C
C*	Round hour forward if minutes are greater than or equal to 45.
C
	IF ( irptdt (5) .ge. 45 ) THEN
	    iwrkdt (4) = iwrkdt (4) + 1
	    IF ( iwrkdt (4) .ge. 24 ) THEN
		iwrkdt (4) = iwrkdt (4) - 24
		CALL TI_ADDD ( iwrkdt, iwrkdt, ier )
	    END IF
	END IF
	CALL TI_ITOC  ( iwrkdt, dattim, iertoc )
	IF  ( iertoc .ne. 0 )  THEN
	    CALL DC_WLOG  ( 2, 'TI', iertoc, ' ', ierwlg )
	    RETURN
	END IF
	CALL FL_MNAM  ( dattim, gemfil, filnam, iermnm )
C
C*	Open the GEMPAK output file.
C
	iflsrc = MFSHIP + MFTEXT
	CALL DC_FCYL  ( filnam, iflsrc, ' ', iadstn, maxtim, lunf,
     +			nparm, parms, iercyl )
	IF  ( iercyl .ne. 0 )  THEN
	    CALL DC_WLOG  ( 2, 'SF', iercyl, filnam, ierwlg )
	    RETURN
	END IF
C
C*	Locate the GEMPAK parameters within the packing table.
C
        DO  k = 1, numprm
C
C*	    Check for the parameter in the list.
C
            CALL ST_FIND  ( cprms ( k ), parms, nparm, ilc, ier )
C
            IF ( ilc .eq. 0 )  THEN
		CALL DC_WLOG ( 1, 'DCACFT', -3, cprms(k), ier )
	      ELSE   
C
C*		Retrieve the requested data and store it into the
C*		GEMPAK array.
C
	   	IF ( imnem ( k ) .ne. 0 ) THEN
      		    IF ( cprms ( k ) .eq. 'WNUM' ) THEN
C
C*		        Convert present weather character code to 
C*			GEMPAK.
C
		        IF ( civals ( imnem ( k ) ) .ne. ' ' ) THEN
             		    rdata (ilc) = PT_WNMT ( civals (imnem(k)) )
			    IF ( rdata ( ilc ) .eq. 0. ) THEN
C
C*			    Weather code is not valid.
C
			        rdata ( ilc ) = RMISSD
			        rivals ( irhbwx (1) ) = RMISSD
			        rivals ( irhtwx (1) ) = RMISSD
			    END IF
		        END IF
C
		      ELSE IF ( cprms ( k ) .eq. 'ATP1' ) THEN
C
C*			Convert character aircraft type to GEMPAK.
C
		        IF ( civals ( imnem ( k ) ) .ne. ' ' ) THEN
			    rdata (ilc) = PT_PATN ( civals (imnem(k)) )
			END IF
C
		      ELSE
C
C*		        Store real value from interface to GEMPAK.
C
      		        rdata ( ilc ) = rivals ( imnem ( k ) )
C
		        IF ( cprms ( k ) ( :3 ) .eq. 'CLC' ) THEN
C
C*		            Convert interface for CLC1, CLC2 to Gempak.
C*			    The interface values are from WMO bufr code
C*			    table 020011 (cloud amount).
C
			    IF ( .not. ERMISS ( rdata ( ilc ) ) ) THEN
			        ixclam = NINT ( rdata ( ilc ) ) + 1 
			        IF ( ( ixclam .ge. 1 ) .and.
     +				     ( ixclam .le. 14 ) ) THEN
				    rdata (ilc) = FLOAT (iclam(ixclam))
			          ELSE
			            rdata ( ilc ) = RMISSD
			        END IF
			    END IF
		        END IF
		    END IF
C
		  ELSE IF ( cprms ( k ) .eq. 'ACRT' ) THEN
C
C*		    Convert aircraft report type (bulletin type) to
C*		    GEMPAK.
C
		    IF ( bultyp .eq. AIREP ) THEN
			rdata ( ilc ) = 1.
		      ELSE IF ( bultyp .eq. PIREP ) THEN
			rdata ( ilc ) = 2.
		      ELSE IF ( bultyp .eq. RECCO ) THEN
			rdata ( ilc ) = 3.
		      ELSE IF ( bultyp .eq. AMDAR ) THEN
			rdata ( ilc ) = 4.
		    END IF
		END IF
	    END IF
        END DO
C
C*	Initialize following parameters to not available.
C
	istnum = 0
	coun = ' '
	stat = ' '
C
C*	Get hours and minutes of report.
C
	ihhmm = irptdt ( 4 ) * 100 + irptdt ( 5 )
C
C* 	If the id is all-numeric, set istnum = ID so individual
C*	stations can be listed with sflist.
C
	CALL ST_NUMB ( rpid, irpid, ier )
	IF ( ier .eq. 0 ) istnum = irpid
C
C*	Check for duplicate report
C
	CALL AF_DUPE ( lunf, rpid, dattim, xlat, xlon, xelv, ier)
C
	IF (ier .ne. 0) THEN
	   write(logln,1000) 'Duplicate location ', rpid, dattim,
     +				xlat,xlon,xelv
1000	   FORMAT ( 1X, A, A, A, 3F10.3 )
	   CALL ST_RXBL ( logln, logln, ilen, ier )
	   CALL DC_WLOG  ( 1, 'AF', 1, logln, ierwlg )
	   RETURN
	END IF
C
C*	Write the decoded report data to GEMPAK aircraft data file.
C
	CALL SF_WSDD  ( lunf, dattim, rpid, istnum, xlat, xlon, xelv,
     +			stat, coun, ihhmm, rdata, iersdd )
C
C*	Write the text to the GEMPAK file.
C
	CALL SF_WSTR ( lunf, ihhmm, report ( :lenr ), ierstr )
C
	IF  ( ( iersdd .ne. 0 ) .or. ( ierstr .ne. 0 ) ) THEN
	    CALL DC_WLOG  ( 2, 'SF', iersdd, ' ', ierwlg )
	END IF
C*
	RETURN
	END
