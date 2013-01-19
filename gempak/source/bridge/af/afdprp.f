	SUBROUTINE AF_DPRP  ( report, lenr, iret )
C************************************************************************
C* AF_DPRP								*
C*									*
C* This subroutine decodes a PIREP report.				*
C*									*
C* AF_DPRP  ( REPORT, LENR, IRET )					*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	LENR		INTEGER		Length of REPORT 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Decode "/IC" and "/WX" TEI's, prevent	*
C*				misdecode of "/OVC" and "/SKC" as TEI's *
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/98	Initialize rpids, lrpids, teis, lteis,	*
C*				via DATA stmts				*
C* D. Kidwell/NCEP	 6/99	Decode "/SK" TEI                        *
C* D. Kidwell/NCEP	 7/99	Decode "/RM", irptr out of call sequence*
C* D. Kidwell/NCEP	 1/00	Added call to AF_PRSI                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	PARAMETER	( NRPIDS = 2 )
C*
	CHARACTER	rpids ( NRPIDS )*3
	INTEGER		lrpids ( NRPIDS )
C*
	PARAMETER	( NTEIS = 11 )
C*
	CHARACTER	teis ( NTEIS )*4
	INTEGER		lteis ( NTEIS )
C*
	DATA		rpids
     +		    / 'UUA', 'UA' /
	DATA		lrpids
     +		    / 3, 2 /
C*
	DATA		teis
     +		    / '/OV ', '/TM', '/FL', '/TP', '/SK ', '/WX',
     +		      '/TA' , '/WV', '/TB', '/IC', '/RM' /
	DATA		lteis
     +		    / 4, 3, 3, 3, 4, 3,
     +		      3, 3, 3, 3, 3 /
C-----------------------------------------------------------------------
	iret  = 0
	irptr = 1
C
C*	Look for a 'UUA' or 'UA' indicator to denote the start of
C*	the report. 
C
	CALL ST_NXTS  ( report, irptr, lenr, rpids, lrpids,
     +			NRPIDS, ipt1, irpid, iernxt )
	IF  ( iernxt .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Decode and store the reporting station id.
C
	IF ( ipt1 .gt. 4 ) THEN
	    CALL AF_PRSI ( report ( :ipt1 - 1 ), ierrsi )
	END IF
	irptr = irptr + ipt1 + ( lrpids ( irpid ) ) - 1
C
C*	Find the first TEI within the report.
C
	CALL ST_NXTS  ( report, irptr, lenr, teis, lteis,
     +			NTEIS, ipt1, itei1, iernxt )
	IF  ( iernxt .ne. 0 )  THEN
	    RETURN
	END IF
	istei = ipt1 + lteis ( itei1 ) 
C
	DO WHILE  ( istei .le. lenr )
C
C*	    Find the end of the data for this TEI by finding the next
C*	    TEI within the report.
C
	    CALL ST_NXTS  ( report, istei, lenr, teis, lteis,
     +			    NTEIS, ipt2, itei2, iernxt )
	    IF  ( iernxt .ne. 0 )  THEN
C
C*		This is the last TEI within the report, so set the end
C*		of the data for this TEI equal to the end of the report.
C
		ietei = lenr
	    ELSE
		ietei = ipt2 - 1
	    END IF
C
	    IF  ( itei1 .eq. 1 )  THEN
C
C*		Decode and store the "/OV" (i.e. location) data.
C
		CALL AF_PLOC  ( report, istei, ietei, ierloc )
	    ELSE IF  ( itei1 .eq. 2 )  THEN
C
C*		Decode and store the "/TM" (i.e. time) data.
C
		CALL AF_PTIM  ( report, istei, ietei, iertim )
	    ELSE IF  ( itei1 .eq. 3 )  THEN
C
C*		Decode and store the "/FL" (i.e. flight level) data.
C
		CALL AF_PFLV  ( report, istei, ietei, ierflv )
	    ELSE IF  ( itei1 .eq. 4 )  THEN
C
C*		Decode and store the "/TP" (i.e. aircraft type) data.
C
		CALL AF_PATP  ( report, istei, ietei, ieratp )
	    ELSE IF  ( itei1 .eq. 5 )  THEN
C
C*		Decode and store the "/SK" (i.e. sky cover) data.
C
		CALL AF_PSKY  ( report, istei, ietei, iersky )
	    ELSE IF  ( itei1 .eq. 6 )  THEN
C
C*		Decode and store the "/WX" (i.e. weather) data.
C
		CALL AF_PWXV  ( report, istei, ietei, ierwxv )
	    ELSE IF  ( itei1 .eq. 7 )  THEN
C
C*		Decode and store the "/TA" (i.e. temperature) data.
C
		CALL AF_PTMP  ( report, istei, ietei, ieratp )
	    ELSE IF  ( itei1 .eq. 8 )  THEN
C
C*		Decode and store the "/WV" (i.e. wind) data.
C
		CALL AF_PWND  ( report, istei, ietei, ierwnd )
	    ELSE IF  ( itei1 .eq. 9 )  THEN
C
C*		Decode and store the "/TB" (i.e. turbulence) data.
C
		CALL AF_PTRB  ( report, istei, ietei, iertrb )
	    ELSE IF  ( itei1 .eq. 10 )  THEN
C
C*		Decode and store the "/IC" (i.e. icing) data.
C
		CALL AF_PICG  ( report, istei, ietei, iericg )
	    ELSE IF  ( itei1 .eq. 11 )  THEN
C
C*		Decode and store the "/RM" (i.e. remarks) data.
C
		CALL AF_PRMK  ( report, istei, ietei, ierrmk )
	    END IF
C
	    IF  ( iernxt .ne. 0 )  THEN
		istei = lenr + 1
	    ELSE
		ipt1 = ipt2
		itei1 = itei2
		istei = ipt1 + lteis ( itei1 ) 
	    END IF
	END DO
C*
	RETURN
	END
