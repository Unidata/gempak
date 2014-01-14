	SUBROUTINE DA_RDTR ( iflno, irow, icol, part,
     +			     idthdr, rdata, nword, iret )
C************************************************************************
C* DA_RDTR								*
C*									*
C* This subroutine reads real data from a non-GEMPAK data source.	*
C*									*
C* DA_RDTR  ( IFLNO, IROW, ICOL, PART, IDTHDR, RDATA, NWORD, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	IDTHDR (*)	INTEGER		Data header			*
C*	RDATA (NWORD)	REAL		Data				*
C*	NWORD		INTEGER		Length of data array		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	part
	REAL		rdata (*)
	INTEGER		idthdr (*)
C*
	CHARACTER	dattim*20, stid*8
	INTEGER		istid(2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the Date and Time values for this row
C
	idate = kheadr ( 1, irow, iflno )
	itime = kheadr ( 2, irow, iflno )
	CALL TI_CDTM ( idate, itime, dattim, iret )
	CALL ST_NULL ( dattim, dattim, ld, ier )
C
C*	Get the Station ID for this column
C
	jj = krow(iflno) + icol
	istid(1) = kheadr ( 1, jj, iflno )
	istid(2) = kheadr ( 8, jj, iflno )
	CALL ST_ITOS ( istid, 2, nc, stid, ier )
	CALL ST_NULL ( stid, stid, ls, ier )
C
C*	Get the Part number
C
	iptnum = 0
	DO  ii = 1, kprt(iflno)
	    IF  ( kprtnm(ii,iflno) .eq. part )  iptnum = ii
	END DO
C
C*	Request the data
C
	CALL DA_GETDTR ( stid, dattim, iptnum, iflno,
     +				idthdr, rdata, nword, iret )
C*
	RETURN
	END
