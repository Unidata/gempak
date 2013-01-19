	SUBROUTINE JTB_SPLT ( seqs, nseqs, pseq, pnams, np, iret )
C************************************************************************
C* JTB_SPLT								*
C*									*
C* This subroutine returns the parameter names associated with the	*
C* first sequence mnemonic found in SEQS.  Any additional sequence	*
C* mnemonics are added to the top of the FIFO stack (SEQS) of sequence 	*
C* mnemonics.  If NP = 0, there are no parameters names associated	*
C* with the sequence mnemonic on top of the stack.			*
C*									*
C* If NSEQ = 0, no further processing is needed for the initial		*
C* TABLE A entry name.							*
C*									*
C* JTB_SPLT ( SEQS, NSEQS, PSEQ, PNAMS, NP, IRET )			*
C*									*
C*									*
C* Input and output parameters						*
C*	SEQS (NSEQS)	CHAR*8		FIFO stack of sequence mnemonics*
C*	NSEQS		INTEGER		Current length of stack		*
C*									*
C* Output parameters:							*
C*	PSEQ		CHAR		Seq mnemonic just processed	*
C*	PNAMS  (NP)	CHAR		Parameter names assoc w PSEQ	*
C*	NP		INTEGER		Number of parm names		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = seq name not found	*
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	pnams (*), seqs(*), pseq
C*
	LOGICAL		found
C-----------------------------------------------------------------------
	iret = 0
	np = 0
C
C*	Take sequence name from top of stack.
C
	pseq = seqs (1)
	nseqs = nseqs - 1
	IF ( nseqs .gt. 0 ) THEN
	    DO i = 1, nseqs
		seqs (i) = seqs (i+1)
	    END DO
	END IF
C
C*	Search for mnemonic sequence name on top of stack
C
	found = .false.
	i = 0
	DO WHILE ( .not. found .and. i .lt. numseq )
	    i = i + 1
	    CALL ST_LSTR ( pseq, l1, ier )
	    CALL ST_LSTR ( seqnam (i), l2, ier )
	    IF ( pseq (1:l1) .eq. seqnam (i) (1:l2) ) THEN
		found = .true.
		is = 0
		DO j = 1, nsqprm (i)
		    IF ( sqpflg (i,j) ) THEN
			np = np + 1
			pnams (np) = seqprm (i,j)
		    ELSE
C
C*			Add new sequence name on stack.
C
			IF ( nseqs .eq. 0 ) THEN
			    is = 1
			    nseqs = 1
			    seqs (1) = seqprm (i,j)
			ELSE
			   nseqs = nseqs + 1
			   DO k = nseqs, 2+is, -1
				seqs (k) = seqs (k-1)
			   END DO
			   is = is + 1
			   seqs (is) = seqprm (i,j)
			END IF
		    END IF
		END DO
	    END IF
	END DO
	IF ( .not. found ) THEN
	    iret = -9
	    RETURN
	END IF
C*
	RETURN
	END
