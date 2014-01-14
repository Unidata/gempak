	SUBROUTINE DA_RDTRGD ( iflno, irow, icol, part,
     +			     idthdr, rdata, nword, iret )
C************************************************************************
C* DA_RDTRGD								*
C*									*
C* This subroutine reads real data from a non-GEMPAK data source.	*
C*									*
C* DA_RDTRGD  ( IFLNO, IROW, ICOL, PART, IDTHDR, RDATA, NWORD, IRET )	*
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
C* S. Jacobs/NCEP	10/13	Fixed vertical coordinate conversion	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'../dm/dmcmn.cmn'
C*
	CHARACTER*(*)	part
	REAL		rdata (*)
	INTEGER		idthdr (*)
C*
	CHARACTER	dattim*20, gvcd*8, gprm*20, cfcst*12,
     +			clev1*8, clev2*8
	INTEGER		iftime(2), ifdtm(3), iprm(3)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the Date/Time and Forecast values
C
	jj = krow(iflno) + icol
	iftime(1) = kheadr ( 1, jj, iflno )
	iftime(2) = kheadr ( 2, jj, iflno )
	CALL TG_FTOI ( iftime, ifdtm, iret )
	CALL TI_CDTM ( ifdtm(1), ifdtm(2), dattim, ier )
	CALL ST_NULL ( dattim, dattim, ld, ier )
C
	ihm = MOD ( ifdtm(3), 100000 )
	ih = ihm / 100
	im = MOD ( ihm, 100 )
	ifcst = ( ih * 60 + im ) * 60
	CALL ST_INCH ( ifcst, cfcst, ier )
	CALL ST_NULL ( cfcst, cfcst, lf, ier )
C
C*	Get the Level1/2 and Vertical Coordinate values
C
	lev1 = kheadr ( 5, jj, iflno )
	lev2 = kheadr ( 6, jj, iflno )
	ivcd = kheadr ( 7, jj, iflno )
C
	CALL ST_INCH ( lev1, clev1, ier )
	CALL ST_NULL ( clev1, clev1, lf, ier )
C
	CALL ST_INCH ( lev2, clev2, ier )
	CALL ST_NULL ( clev2, clev2, lf, ier )
C
	CALL LV_CCRD ( ivcd, gvcd, ier )
	CALL ST_NULL ( gvcd, gvcd, ls, ier )
C
C*	Get the Parameter value
C
	iprm(1) = kheadr (  8, jj, iflno )
	iprm(2) = kheadr (  9, jj, iflno )
	iprm(3) = kheadr ( 10, jj, iflno )
	CALL ST_ITOS ( iprm, 3, nch, gprm, ier )
	CALL ST_NULL ( gprm, gprm, lg, ier )
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
	CALL DA_GETDTRGD ( dattim, cfcst, clev1, clev2, gvcd,
     +			   gprm, iflno, idthdr, rdata, nword,
     +			   iret )
C*
	RETURN
	END
