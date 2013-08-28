	SUBROUTINE DM_PART  ( iflno, prtnam, lenhdr, ityprt, nparms,
     +			      prmnam, iscale, ioffst, nbits, iret )
C************************************************************************
C* DM_PART								*
C*									*
C* This subroutine returns information for a specific part.		*
C*									*
C* DM_PART  ( IFLNO, PRTNAM, LENHDR, ITYPRT, NPARMS, PRMNAM, ISCALE,	*
C*            IOFFST, NBITS, IRET )					*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	PRTNAM		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	LENHDR		INTEGER		Length of data header		*
C*	ITYPRT		INTEGER		Data type			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	PRMNAM (NPARMS)	CHAR*4		Parameter names			*
C*	ISCALE (NPARMS)	INTEGER		Scaling term			*
C*	IOFFST (NPARMS)	INTEGER		Offset				*
C*	NBITS  (NPARMS)	INTEGER		Number of bits			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					-10 = invalid part name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/87						*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	prtnam, prmnam (*)
	INTEGER		iscale (*), ioffst (*), nbits (*)
C-----------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Find this part name.
C
	knt = 0
	DO  i = 1, kprt ( iflno )
	    IF  ( kprtnm ( i, iflno ) .eq. prtnam )  knt = i
	END DO
C
C*	Return if part name was not found.
C
	IF  ( knt .eq. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Retrieve part information from common area.
C
	lenhdr = klnhdr ( knt, iflno )
	ityprt = ktyprt ( knt, iflno )
	nparms = kparms ( knt, iflno )
	DO  i = 1, nparms
	    prmnam ( i ) = kprmnm ( i, knt, iflno )
	    iscale ( i ) = kscale ( i, knt, iflno )
	    ioffst ( i ) = koffst ( i, knt, iflno )
	    nbits  ( i ) = kbits  ( i, knt, iflno )
	END DO
C*
	RETURN
	END
