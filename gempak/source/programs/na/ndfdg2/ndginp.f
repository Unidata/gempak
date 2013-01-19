	SUBROUTINE NDGINP  ( gbfile, gdoutf, maxgrd, garea, 
     +					gskip, output, overwr, iret )
C************************************************************************
C* NDGINP								*
C*									*
C* This subroutine gets the input parameters for NDFDG2.		*
C*									*
C* NDGINP ( GBFILE, GDOUTF, MAXGRD, GAREA, GSKIP, OUTPUT, OVERWR, IRET )*
C**									*
C* Log:									*
C* T. Piper/SAIC	10/02	Modified from NAGINP			*
C* T. Piper/SAIC	04/03	Added gskip				*
C* m.gamazaychikov/SAIC	09/05	Added overwr				*
C************************************************************************
	CHARACTER*(*)	gbfile, gdoutf,	maxgrd, garea, gskip, output
	LOGICAL		overwr
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GBFILE',  gbfile,  ier1 )
	CALL IP_STR  ( 'GDOUTF',  gdoutf,  ier2 )
	CALL IP_STR  ( 'MAXGRD',  maxgrd,  ier3 )
	CALL IP_STR  ( 'GAREA',   garea,   ier4 )
	CALL IP_STR  ( 'GSKIP',   gskip,   ier5 )
	CALL IP_STR  ( 'OUTPUT',  output,  ier6 )
	CALL IP_LOG  ( 'OVERWR',  overwr,  ier7 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
