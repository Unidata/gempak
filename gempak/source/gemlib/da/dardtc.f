	SUBROUTINE DA_RDTC ( iflno, irow, icol, part,
     +			     idthdr, cdata, nchar, iret )
C************************************************************************
C* DA_RDTC								*
C*									*
C* This subroutine reads character data from a non-GEMPAK data source.	*
C*									*
C* DA_RDTC ( IFLNO, IROW, ICOL, PART, IDTHDR, CDATA, NCHAR, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:                                                   *
C*	IDTHDR (*)	INTEGER		Data header			*
C*	CDATA		CHAR*NCHAR	Data				*
C*	NCHAR		INTEGER		Length of string		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/13	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	part, cdata
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
C*      Get the Part number
C
        iptnum = 0 
	DO  ii = 1, kprt(iflno)
	    IF  ( kprtnm(ii,iflno) .eq. part )  iptnum = ii
	END DO
C
C*	Request the data
C
	CALL DA_GETDTC ( stid, dattim, iptnum, iflno, 
     +				idthdr, cdata, nc, iret )
	CALL ST_RNUL ( cdata, cdata, nchar, ier )
C*
	RETURN
	END
