	SUBROUTINE JTB_LDSQ ( nsqmx, np, name, sseq, nseq,  iret )
C************************************************************************
C* JTB_LDSQ								*
C*									*
C* This subroutine scans information dumped by SUBROUTINE GETABDB to	*
C* get the sequence associated with each TABLE D entry.			*
C*									*
C* If NP = 0, no Table D entries were found.				*
C*									*
C* JTB_LDSQ ( NSQMX, NP, NAME, SSEQ, NSEQ, IRET )			*
C*									*
C* Input parameter:							*
C*	NSQMX		INTEGER		Max # of parms in a sequence	*
C*									*
C* Input and output parameters:						*
C*	NP		INTEGER		Max number of  names		*
C*					OUTPUT:  actual # of names	*
C*									*
C* Output parameters:							*
C*	NAME   (NP)	CHAR*		Array of sequence mnemonics	*
C*	SSEQ (NP,NSQMX)	CHAR*		Array of parameters for NAME	*
C*	NSEQ   (NP)	INTEGER		Number of parameters for NAME	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = sequence has no members	*
C*					 -6 = too many sequences found	*
C*					 -7 = too many parms / sequence *
C**									*
C* Log:									*
C* K. Brill/EMC		 9/98						*
C************************************************************************
	INCLUDE		'jtb.cmn'
C*
	CHARACTER*(*)	name (*), sseq(np,*)
	INTEGER		nseq (*)
C*
	CHARACTER*1	ctype
	CHARACTER*8	lastd, dntry
	CHARACTER*8	substr (64)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Use entries marked by D.
C
	lastd = ' '
	ie = 0
	ip = 0
	ctype = 'D'
	DO WHILE ( ie .lt. nntrys .and. ctype .eq. 'D' )
	    ie = ie + 1
	    ctype = entrys (ie) (1:1)
	    IF ( ctype .eq. 'D' ) THEN
		dntry = entrys (ie) (3:10)
		CALL ST_CLST ( entrys (ie) (12: ), ' ', ' ', 64,
     +			       substr, num, ier )
		IF ( num .eq. 0 ) THEN
		    iret = -3
		    RETURN
		END IF
		IF ( dntry .ne. lastd ) THEN
		    ns = 0
		    ip = ip + 1
		    IF ( ip .gt. np ) THEN
			iret = -6
			RETURN
		    END IF
		    name (ip) = dntry
		END IF
		DO i = 1, num
		    ns = ns + 1
		    IF ( ns .gt. nsqmx ) THEN
			iret = -7
			RETURN
		    END IF
		    sseq (ip,ns) = substr (i)
		END DO
		nseq (ip) = ns
		lastd = dntry
	    END IF
	END DO
	np = ip
C*
	RETURN
	END
