        SUBROUTINE NAMINV ( bufrfl, iret )
C************************************************************************
C* NAMINV								*
C*									*
C* This subroutine lists the parameters in a MODEL BUFR file using its	*
C* associated table stored in the first BUFR message.			*
C*									*
C* The output is written to a file named bufr_table.dump.		*
C*									*
C* NAMINV  ( BUFRFL, IRET )						*
C*									*
C* Input parameters:							*
C*	BUFRFL		CHAR*		BUFR file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -4 = cannot open BUFR file	*
C*				 	-53 = cannot bufr_table.dump	*
C**									*
C* Log:									*
C* K. Brill/EMC		9/98						*
C* D. Kidwell/NCEP     12/98	SNMINV -> NAMINV, SNMODL -> NAMSND      *
C************************************************************************
	CHARACTER*(*)	bufrfl
C*
	PARAMETER	( MXTBLA = 1 )
	CHARACTER*8	tblas (MXTBLA), seqs (80), pnams (80), pseq
C-----------------------------------------------------------------------
	iret = 0
C*
	CALL FL_GLUN ( lunbuf, ier )
	IF ( ier .ne. 0 ) THEN
     	    CALL ER_WMSG ( 'FL', ier, ' ', ire )
	    iret = -34
	    RETURN
	END IF
	CALL FL _SUNK ( 'bufr_table.dump', lunout, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -53
	    RETURN
	END IF
C
C*	CALL JB_OPEN to open the BUFR file.
C
  	CALL JB_OPEN ( bufrfl, lunbuf, ' ', ier )
	IF ( ier .ne. 0 ) THEN
	    ier = ier - 50
     	    CALL ER_WMSG ( 'NAMSND', ier, bufrfl, iret )
	    iret = -4
	END IF
C*
	np = MXTBLA
  	CALL JTB_INIT ( lunbuf, np, tblas, ier )
	IF ( ier .ne. 0 ) THEN
	    ier = ier - 40
 	    CALL ER_WMSG ( 'NAMSND', ier, ' ', iret )
	    iret = -1
	    RETURN
	END IF
C
C*	Get the TABLE D names for the TABLE A entry - there should
C*	only be one TABLE A entry.
C
	nseq = 1
	seqs ( 1 ) = tblas ( 1 )
  	CALL JTB_SPLT ( seqs, nseq, pseq, pnams, np, iret )
	IF ( iret .ne. 0 ) THEN
	    iermsg = iret - 40
 	    CALL ER_WMSG ( 'NAMSND', iermsg, pseq, ier )
	    RETURN
	END IF
C
C*	Find the sequences and parameters.
C
	IF ( np .ne. 0 ) THEN
	    CALL JTB_PDMP (lunout, pseq, pnams, np, ier )
	END IF
	DO WHILE ( nseq .ne. 0 )
	    CALL JTB_SPLT ( seqs, nseq, pseq, pnams, np, ier )
	    IF ( ier .ne. 0 ) THEN
		WRITE (6,*) 'JTB_SPLT ... ier = ', ier
		STOP
	    END IF
	    CALL JTB_PDMP (lunout, pseq, pnams, np, ier )
	    IF ( ier .ne. 0 ) THEN
		WRITE (6,*) 'JTB_PDMP ... ier = ', ier
		STOP
	    END IF
	END DO
C*
	CALL FL_CLOS ( lunbuf, ier )
	CALL FL_CLOS ( lunout, ier )
C*
	RETURN
	END
