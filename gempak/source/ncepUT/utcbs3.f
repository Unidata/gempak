	SUBROUTINE UT_CBS3  ( loglev, ibufr, cxdesc, nxdesc, iret )
C************************************************************************
C* UT_CBS3								*
C*									*
C* This subroutine unpacks the descriptors contained within Section 3	*
C* of a given BUFR message and then compares this list against a given	*
C* list of expected descriptors.  Any differences are written to the	*
C* decoder log at a verbosity level specified by the user.  Note that,	*
C* when Section 3 of the input BUFR message is unpacked for comparison	*
C* with the expected list, sequence descriptors will not be recursively	*
C* resolved; therefore, the expected list should contain exactly what	*
C* is expected to appear within Section 3.				*
C*									*
C* UT_CBS3  ( LOGLEV, IBUFR, CXDESC, NXDESC, IRET )			*
C*									*
C* Input parameters:							*
C*	LOGLEV		INTEGER		Verbosity level			*
C*	IBUFR(*)	INTEGER		BUFR message			*
C*	CXDESC(*)	CHARACTER*6	Expected descriptors		*
C*	NXDESC		INTEGER		Number of expected descriptors	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = Descriptor list within	*
C*					      Section 3 of IBUFR	*
C*					      matches CXDESC		*
C*					 -1 = Descriptor list within	*
C*					      Section 3 of IBUFR	*
C*					      does not match CXDESC	*
C**									*
C* Log:									*
C* J. Ator/NCEP		05/01						*
C************************************************************************
	INTEGER		ibufr(*)
C*
	CHARACTER*6	cdesc(300), cxdesc(*)
C*
	CHARACTER*100	logmsg
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Unpack the descriptors from Section 3 of the BUFR message.
C
	CALL UPDS3  ( ibufr, cdesc, ndesc )
C
C*	Does the number of descriptors equal the number expected?
C
	IF  ( ndesc .ne. nxdesc )  THEN
	    WRITE  ( UNIT = logmsg, FMT = '( A, I3, A, I3, A )' )
     +		'UT_CBS3:  BUFR message contained ', ndesc,
     +		' descriptors (expected ', nxdesc, ')'
	    CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
	    iret = -1
	    RETURN
	END IF
C
C*	Compare each unpacked descriptor with the corresponding one
C*	from the expected list, and log any differences.
C
	DO ii = 1, ndesc
	    IF  ( cdesc (ii) .ne. cxdesc (ii) )  THEN
		WRITE  ( UNIT = logmsg, FMT = '( 3A, I3, 3A )' )
     +		    'UT_CBS3:  found descriptor ', cdesc (ii),
     +		    ' at position ', ii, ' (expected ', cxdesc (ii), ')'
		CALL DC_WLOG  ( loglev, 'DC', 2, logmsg, ierwlg )
		iret = -1
	    END IF
	ENDDO
C*
	RETURN
	END
