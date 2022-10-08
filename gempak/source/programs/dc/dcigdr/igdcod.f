	SUBROUTINE IG_DCOD  ( cldt, bufrta, gemfil, nhours, iret )
C************************************************************************
C* IG_DCOD								*
C*									*
C* This routine decodes bulletins containing IGDR wind/wave BUFR	*
C* messages from France into GEMPAK ASCII file format.			*
C*									*
C* IG_DCOD ( CLDT, BUFRTA, GEMFIL, NHOURS, IRET )			*
C*									*
C* Input parameters:							*
C*	CLDT		CHAR*		Date-time from command line	*
C*	BUFRTA		CHAR*		IGDR BUFR table file		*
C*	GEMFIL		CHAR*		GEMPAK SWH output file		*
C*	NHOURS		INTEGER		Max # of hours before run time	*
C*					for creating BUFR output	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP	10/05 						*
C* C. Caruso Magee/NCEP	02/06  Replace IUPBS1 with IUPBS01 and modify	*
C*					input args appropriately.	*
C* T. Piper/SAIC	03/06	Modified for ASCII output.		*
C* S. Chiswell/Unidata	10/07	Added use of output file template	*
C* T. Piper/SAIC	08/08	Modified to support output template	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'igcmn.cmn'
C
C*  Number of non-NCEP BUFR tables files.
C
	PARAMETER	( NBUFRT = 1 )
C
	CHARACTER*(*)	cldt, bufrta, gemfil 
C
	CHARACTER	bull*(DCMXBF), cbull*(DCMXBF), bfstyp*8,
     +			seqnum*8, buhd*8, cborg*8, bulldt*8, bbb*8,
     +			rundt*12, sysdt*12, rimnem(NRIMN)*8, 
     +			filnam*132
C
C
	INTEGER		irundt ( 5 ), irptdt ( 5 ),
     +			iubfmf, nxdsc, 	ibull ( DCMXBF / 4 )
C
	LOGICAL		bullok
C
	EQUIVALENCE	( cbull (1:4), ibull (1) )
C
C*  Number of expected descriptors within Section 3 of each
C*  type of non-NCEP BUFR message.
C
	PARAMETER	( NXDSCA = 73 )
C
C*  The following array will hold the list of expected descriptors
C*  within Section 3 of each type of non-NCEP BUFR message.
C*  The first dimension of this array must be at least as large
C*  as the largest of the above NXDSC values.
C
	CHARACTER 	cxdsc( NXDSCA )*6
C
C*  Expected descriptors within Section 3 of an IGDR BUFR message.
C
	DATA		cxdsc 
     +		/ '001007', '025060', '001033', '002048',
     +		  '002048', '005040', '201134', '007001',
     +		  '201000', '202131', '007005', '202000',
     +		  '301011', '301012', '004007', '005001',
     +		  '006001', '008029', '008074', '008012',
     +		  '025095', '025096', '025097', '204001',
     +		  '031021', '022070', '204000', '008023',
     +		  '022070', '021128', '123002', '008076',
     +		  '204001', '031021', '201129', '021062',
     +		  '201000', '204000', '008023', '021062',
     +		  '204001', '031021', '201134', '007001',
     +		  '201000', '204000', '202131', '007005',
     +		  '202000', '008023', '202131', '007001',
     +		  '202000', '021128', '204001', '031021',
     +		  '002173', '204000', '107003', '201130',
     +		  '002121', '201000', '204001', '031021',
     +		  '012163', '204000', '104002', '002023',
     +		  '202129', '011012', '202000', '013090', '013091'/ 
C
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
	nxdsc = NXDSCA
C
C*  Set the pointers for the interface arrays.
C
	CALL IG_IFSP  ( rimnem, ierfsp )
	IF  ( ierfsp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*  Open the tables file.
C
	CALL FL_TBOP  ( bufrta, 'bufr', iubfta, ierspn ) 
	IF  ( ierspn .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'FL', ierspn, bufrta, ierwlg )
	    RETURN
	END IF
C
C*  Open the messages file.
C
	CALL FL_GLUN  ( iubfmf, iergln )
	IF  ( iergln .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'FL', iergln, ' ', ierwlg )
	    RETURN
	END IF
	OPEN  ( UNIT = iubfmf, FILE = '.dummy.dcigdr',
     +          FORM = 'UNFORMATTED' )
C
C*  Connect the tables and messages files.
C
	CALL OPENBF  ( iubfmf, 'IN', iubfta )
C
C*  Close the tables file.
C
        CALL FL_CLOS  ( iubfta, iercls )
	IF  ( iercls .ne. 0 )  THEN
            CALL DC_WLOG  ( 0, 'FL', iercls, ' ', ierwlg )
        END IF
C
C*	Hanlded below (Chiswell)
C	CALL ST_NULL(gemfil, gemfil, lens, ier)
C
	DO WHILE  ( .true. )
C
C*  Get a new bulletin from the input pipe.
C
	    CALL DC_GBUL  ( bull, lenb, ifdtyp, iergbl )
	    IF  ( iergbl .ne. 0 )  THEN
C
C*  A time-out occurred while waiting for a new bulletin
C*  on the input pipe.  Shut down the decoder and exit.
C
		CALL DC_WLOG  ( 0, 'DC', iergbl, ' ', ierwlg )
		CALL CLOSBF  ( iubfmf )
		CALL FL_CLAL  ( iercal )
		RETURN
	    END IF
C
	    bullok = .true.
C
C*  Decode the header information from this bulletin.
C
	    IF  ( ifdtyp .eq. 0 )  THEN
C
C*  Decode WMO products.
C
		CALL DC_GHDR  ( bull, lenb, seqnum, buhd, cborg,
     +				 bulldt, bbb, ibptr, ierghd )
		IF  ( ierghd .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'DC', ierghd, ' ', ierwlg )
		    bullok = .false.
		ELSE
C
C*  Start an entry for this bulletin in the decoder log.
C
		    logmsg = '####################' //
     +			     '####################'
		    CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
		    logmsg = seqnum // buhd // cborg // bulldt // bbb
		    CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
		END IF
	    ELSE
C
C*  Do not decode AFOS products.
C
		bullok = .false.
	    END IF
	    IF  ( bullok )  THEN
C
C*  Get the system time.
C
		itype = 1
		CALL CSS_GTIM  ( itype, sysdt, iergtm )
		IF  ( iergtm .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'SS', iergtm, ' ', ierwlg )
		    bullok = .false.
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*  If a date-time was entered on the command line, then
C*  use it as the run date-time.  Otherwise, use the
C*  system time as the run date-time.
C
		IF  ( cldt .eq. 'SYSTEM' )  THEN
		    rundt = sysdt
		ELSE
		    CALL TI_STAN  ( cldt, sysdt, rundt, ierstn )
		    IF  ( ierstn .ne. 0 )  THEN
			CALL DC_WLOG  ( 2, 'TI', ierstn, ' ', ierwlg )
			bullok = .false.
		    END IF
		END IF
	    END IF
	    IF  ( bullok )  THEN
C
C*  Convert the run date-time to integer.
C
		CALL TI_CTOI  ( rundt, irundt, iercto )
		IF  ( iercto .ne. 0 )  THEN
		    CALL DC_WLOG  ( 2, 'TI', iercto, ' ', ierwlg )
		    bullok = .false.
		END IF
	    END IF
	    DO WHILE  ( bullok )
C
C*  Locate the next BUFR message within the bulletin,
C*  and store it within an equivalenced integer array.
C
		ipt1 = INDEX ( bull ( ibptr : lenb ), 'BUFR' )
		IF  ( ipt1 .eq. 0 )  THEN
C
C*  There are no more BUFR messages within the bulletin.
C
		    bullok = .false.
		ELSE
		    istart = ibptr + ipt1 - 1
		    ibptr = istart + 4
		    cbull = bull ( istart : lenb )
C
		    nrept = 0
C
C*  Retrieve the Section 3 descriptors from this IGDR BUFR message
C*  and compare it with the list of expected descriptors
C
		    CALL UT_CBS3  ( 3, ibull, cxdsc, nxdsc, iercs3 )
		    IF  ( iercs3 .ne. 0 )  THEN
			  bullok = .false.
			  logmsg = 'message has unknown format'
			  CALL DC_WLOG  ( 3, 'DC', 2, logmsg, ierwlg )
	            END IF
C
		    IF  ( bullok )  THEN
C
C*  Open this BUFR message.
C
			CALL READERME  ( ibull, iubfmf,
     +					 bfstyp, ibfdt, ierrme )
			IF  ( ierrme .ne. 0 )  THEN
			  bullok = .false.
			END IF
		    ELSE
			logmsg = 'message has unknown format'
			CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
		    END IF
C
		    DO WHILE  ( bullok )
C
C*  Get the next report from this BUFR message.
C
			CALL READSB  ( iubfmf , ierrsb )
			IF  ( ierrsb .ne. 0 )  THEN
C
C*  There are no more reports in this message.
C
			  bullok = .false.
C
C*  Print a count of the number of reports processed.
C
			  WRITE  ( UNIT = logmsg, FMT = '( A, I4, A )' )
     +				'contained ', nrept, ' reports'
			  CALL DC_WLOG  ( 2, 'DC', 2, logmsg, ierwlg )
C
			ELSE
			  nrept = nrept + 1
C
C*  Initialize the interface arrays.
C
			  CALL IG_IFIV  ( ierifi )
C
C*  Decode the report into the interface arrays.
C
			  CALL IG_BFIF  ( iubfmf , cborg, ierbif )
C
C*  Write data for this report to the decoder log.
C
			  CALL IG_IFPT  ( rimnem, ierifp )
C
C*  Do not create output for reports that are more than
C*  NHOURS before or more than 3 hours after the run time.
C
			  IF  (  ( ERMISS ( rivals ( iryear ) ) ) .or.
     +				 ( ERMISS ( rivals ( irmnth ) ) ) .or.
     +				 ( ERMISS ( rivals ( irdays ) ) ) .or.
     +				 ( ERMISS ( rivals ( irhour ) ) ) .or.
     +				 ( ERMISS ( rivals ( irminu ) ) )  )
     +				THEN
			    iertmk = -1
			  ELSE
			    irptdt (1) = INT ( rivals ( iryear ) )
			    irptdt (2) = INT ( rivals ( irmnth ) )
			    irptdt (3) = INT ( rivals ( irdays ) )
			    irptdt (4) = INT ( rivals ( irhour ) )
			    irptdt (5) = INT ( rivals ( irminu ) )
			    CALL DC_TMCK  ( 2, irundt, irptdt, nhours,
     +					    180, iertmk )
			  END IF
			  IF  ( iertmk .eq. 0 )  THEN
C
C*  Write out.
C
			    IF ( INT (rivals(irlsql)) .eq. 0 )  THEN
C
C*				Handle template in file name (Chiswell)
C
				CALL TI_ITOC ( irptdt, dattim, ier )
				CALL FL_MNAM ( dattim, gemfil, filnam, 
     +						ier)
	                        CALL ST_NULL(filnam, filnam, lens, ier)
C
				CALL IG_ASCII ( filnam, irptdt,
     +					rivals(irslat), rivals(irslon),
     +					rivals(irsgw1), iret)

			    END IF
			  END IF
C
			  logmsg = '-----------------------------------'
			  CALL DC_WLOG  ( 3, 'DC', 2, logmsg, ierwlg )
			END IF
		    END DO
		END IF
	    END DO
	END DO
C
	RETURN
	END
