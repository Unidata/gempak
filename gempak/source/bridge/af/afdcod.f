	SUBROUTINE AF_DCOD  ( curtim, gemfil, pnvtbl, awptbl, prmfil,
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* AF_DCOD								*
C*									*
C* This routine decodes aircraft bulletins into GEMPAK format.		*
C*									*
C* AF_DCOD  ( CURTIM, GEMFIL, PNVTBL, AWPTBL, PRMFIL, IADSTN, MAXTIM,   *
C*	      NHOURS, IRET )						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Date-time from command line 	*
C*	GEMFIL		CHAR*		GEMPAK output file name template*
C*	PNVTBL		CHAR*		PIREP navaids table 		*
C*	AWPTBL		CHAR*		AIREP waypoints table 		*
C*	PRMFIL		CHAR*		Parameter packing table 	*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Max. # of times allowed	in file *
C*	NHOURS		INTEGER		No. of hours before sys. time   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		11/96	Decode bulletin day/hour from header	*
C*				for AMDAR, add logic to decode RECCO 	*
C* J. Ator/NP12		01/97	Reduce decoder log output 		*
C* J. Ator/NP12		02/97	Reduce decoder log output 		*
C* J. Ator/NP12		08/97	New interface format, cleaned up code 	*
C* D. Kidwell/NCEP	09/97	Cleaned up doc, mods to GEMPAK interface*
C* J. Ator/NCEP		03/98	Check ierbif before storing RECCO RPID	*
C* A. Hardy/GSC         04/98   Moved GEMPAK time check call            *
C* J. Ator/NCEP		04/98	AF_TMCK -> DC_TMCK			*
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to call sequences  *
C* J. Ator/NCEP		12/98	Don't decode RECCO ID from 2nd hdr line	*
C* A. Hardy/GSC         04/99   Added check for UAXX10 EGRR bull. hdrs  *
C* D. Kidwell/NCEP       7/99   Added text output, removed irptr arg.   *
C* D. Kidwell/NCEP      10/00   Calling sequence & time check changes;  *
C*				cleaned up                              *
C* D. Kidwell/NCEP       4/03   Checked for error before calling DC_TMCK*
C* B. Yin/SAIC		 3/04	Changed SS_GTIM to CSS_GTIM  		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	curtim, gemfil, pnvtbl, awptbl, prmfil
C*
	CHARACTER	bull*(DCMXBF), bullx*(DCMXBF), report*(DCMXBF),
     + 			seqnum*8, buhd*8, cicli*8, bulldt*8, bbb*8,
     + 			rundt*12, sysdt*12, cprms(MMPARM)*4,
     +			rimnem(NRIMN)*8, cimnem(NCIMN)*8
	INTEGER		irundt (5), irptdt (5), imnem (MMPARM), 
     +			ibuldt (5), itype
	LOGICAL		bullok,	rptok
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set the pointers for the interface arrays.
C
	CALL AF_IFSP  ( rimnem, cimnem, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Read the PIREP navaids table.
C
	CALL AF_PTOR  ( pnvtbl, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Read the AIREP waypoints table.
C
	CALL AF_ATOR  ( awptbl, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
	CALL DC_FINT  ( 3, 2, prmfil, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'DC', ier, ' ', ierr )
	    RETURN
	END IF
C
C*	Initialize the GEMPAK parameter array and set up links to
C*	the interface mnemonics.
C
	CALL AF_INTF ( rimnem, cimnem, cprms, imnem, numprm, iret )
C
	DO WHILE  ( .true. )
C
C*	    Get a new bulletin from the input pipe.
C
	    CALL DC_GBUL  ( bull, lenb, ifdtyp, ier )
	    IF  ( ier .ne. 0 )  THEN
C
C*		A time-out occurred while waiting for a new bulletin
C*		on the input pipe.  Shut down the decoder and exit.
C
		CALL DC_WLOG  ( 0, 'DC', ier, ' ', ierr )
		CALL DC_FCLS  ( ier )
		CALL FL_CLAL  ( ier )
		RETURN
	    END IF
	    bullok = .true.
C
C*	    Decode the header information from this bulletin.
C
	    IF  ( ifdtyp .eq. 0 )  THEN
C
C*		Decode WMO products.
C
	        CALL DC_GHDR  ( bull, lenb, seqnum, buhd, cicli,
     +				bulldt, bbb, ibptr, ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'DC', ier, ' ', ierr )
		    bullok = .false.
		END IF
	      ELSE
C
C*		Do not decode AFOS products.
C
		bullok = .false.
	    END IF
	    IF  ( bullok )  THEN
C
C*		What type of bulletin is this?
C
		CALL AF_BLTP  ( buhd, cicli, ier )
		IF  ( bultyp .eq. LEMON )  THEN
		    logmsg = buhd  //  ' '  // cicli
		    CALL DC_WLOG  ( 2, 'DCACFT', 2, logmsg, ier )
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		Remove unprintable characters from this bulletin.
C
		lenbxo = lenb - ibptr
		CALL ST_UNPR  ( bull ( ibptr + 1 : lenb ), lenbxo,
     +				bullx, lenbxn, ier )
		lenbx = lenbxn
		ibxptr = 1
C
C*		Decode any extra information from the beginning of
C*		this bulletin.
C
		CALL AF_XBIF  ( bullx, lenbx, ibxptr, iahday, ier )
		IF  ( ier .ne. 0 )  THEN
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		Get the system time.
C
	 	itype = 1
		CALL CSS_GTIM  ( itype, sysdt, ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'SS', ier, ' ', ierr )
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		If a date-time was entered on the command line, then
C*		use it as the run date-time.  Otherwise, use the
C*		system time as the run date-time.
C
		IF  ( curtim .eq. 'SYSTEM' )  THEN
		    rundt = sysdt
		  ELSE
		    CALL TI_STAN  ( curtim, sysdt, rundt, ier )
		    IF  ( ier .ne. 0 )  THEN
			CALL DC_WLOG  ( 2, 'TI', ier, ' ', ierr )
			bullok = .false.
		    END IF
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		Convert the run date-time to integer.
C
		CALL TI_CTOI  ( rundt, irundt, ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'TI', ier, ' ', ierr )
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		Get and decode the bulletin day, bulletin hour and
C*		bulletin minute from the bulletin header.
C
		CALL AF_OBDH  ( bulldt, ibday, ibhr, ibmin, ier )
		IF  ( ier .ne. 0 )  THEN
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*		Use the run date-time, bulletin day, bulletin hour, and
C*		bulletin minute to construct a bulletin date-time.
C
		ndyb = nhours / 24 + 1
		CALL DC_RTIM  ( irundt, ibday, ibhr, ibmin, ndyb,
     +				ibuldt, ier )
		IF  ( ier .ne. 0 )  THEN
		    bullok = .false.
		END IF
	    END IF
	    IF  ( ( bullok ) .and. ( bultyp .eq. AMDAR ) )   THEN
C
C*		Check that the AMDAR header day is within one day of
C*		the bulletin date-time.
C
		CALL DC_RTIM  ( ibuldt, iahday, 0, 0, 1, irptdt, ier )
		IF  ( ier .ne. 0 )  THEN
		    bullok = .false.
		END IF
	    END IF
C
	    DO WHILE  ( bullok )
C
C*		Get the next report from this bulletin.
C*              Added UAXX10 EGRR check to get reports that
C*              do not end with '='.
C
                IF ( (buhd .eq. 'UAXX10') .and. 
     +                                   (cicli .eq. 'EGRR' ) ) THEN
                   CALL AF_FARP (  bullx, lenbx, ibxptr, 
     +                             report, lenr, ier )
                  ELSE 
		   CALL AF_GRPT  ( bullx, lenbx, ibxptr, report, 
     +			           lenr, ier )
                END IF
		IF  ( ier .ne. 0 )  THEN
C
C*		    There are no more reports in this bulletin.
C
		    bullok = .false.
		END IF
		IF  ( bullok )  THEN
		    CALL AF_IFIV  ( ier )
C
C*		    Start an entry for this report in the decoder log.
C
		    logmsg = seqnum // buhd // cicli // bulldt // bbb
		    CALL DC_WLOG  ( 2, 'DCACFT', 1, logmsg, ier )
C
C*		    Write the report to the decoder log.
C
		    CALL ST_LDSP ( report ( :lenr), report, lenr, iret )
		    CALL DC_WLOG ( 2, ' ', 1, report (1:lenr), ier )
C
C*		    Based upon the bulletin type, decode the report.
C
		    IF  ( bultyp .eq. AMDAR )  THEN
			CALL AF_DADR  ( report, lenr, ier )
		      ELSE IF  ( bultyp .eq. AIREP )  THEN
			CALL AF_DARP  ( report, lenr, cicli, ier )
		      ELSE IF  ( bultyp .eq. PIREP )  THEN
			CALL AF_DPRP  ( report, lenr, ier )
		      ELSE IF  ( bultyp .eq. RECCO )  THEN
			CALL AF_DRCO  ( report, lenr, ier )
		    END IF
		    rptok = .true.
C
C*		    Use the run date-time, bulletin date-time, report
C*		    day (if it exists), report hour, and report minute
C*		    to construct a report date-time.
C
		    CALL AF_RTIM  ( irundt, ibuldt, irptdt, ierr )
		    IF ( ierr .eq. 0 ) THEN
C
C*		        Do not create GEMPAK output for reports that
C*		        are more than NHOURS hours before or more than
C*		        1 hour after the run time.
C
	                CALL DC_TMCK  ( 2, irundt, irptdt, nhours, 60, 
     +				        ier )
		        IF  ( ier .ne. 0 ) rptok = .false.
		      ELSE
			rptok = .false.
		    END IF
C
		    IF  ( rptok )  THEN
C
C*			Write data for this report to the decoder log.
C
			logmsg = '<-----BEGIN INTERFACE OUTPUT----->'
			CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ier )
			CALL AF_IFPT  ( rimnem, cimnem, ier )
			logmsg = '<-----END INTERFACE OUTPUT----->'
			CALL DC_WLOG  ( 3, 'AF', 1, logmsg, ier )
C
C*		        Convert data for report to GEMPAK format
C*		        and write it to the GEMPAK output file.
C
		        CALL AF_GMPK  ( irptdt, gemfil, iadstn,
     +			                maxtim, cprms, imnem, numprm, 
     +					report, lenr, ier )
		    END IF
		END IF
	    END DO
C
	END DO
C*
	RETURN
	END
