	SUBROUTINE MA_CGWX ( string, iret )
C************************************************************************
C* MA_CGWX								*
C*									*
C* This subroutine decodes the weather data in one report.  If there    *
C* are multiple weather codes included with the report, only the most   *
C* significant will be saved.  The weather code is saved in common	*
C* /rintfv/.								*
C*                                                                      *
C* MA_CGWX  ( STRING, IRET )                                            *
C*                                                                      *
C* Input parameters:                                                    *
C*	STRING		CHAR*		string containing sky cover data*
C*                                                                      *
C* Output parameters:                                                   *
C*	RIVALS(IRWWMO)	REAL		weather from a manned station	*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP	 4/01	Original Author.			*
C* F. J. Yen/NCEP	 4/01	Rewrote and renamed from CG_GTWX.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	string
C*
	PARAMETER	( NCOD = 57 )
	CHARACTER*4	wcod ( NCOD ) 
	INTEGER		nch ( NCOD ), wmo ( NCOD )
C
C*	Maintain the order of the following weather strings
C
	DATA	wcod	/ 'TSTM', 'TSRA', 'TSSN', 'TRW',  'TSGR',
     +		          'TSGS', 'TSDS', 'TSSS', 'SQ',   'FC',
     +		  	  'ZL',   'FZDZ', 'DS',   'SS',   'BR',
     +			  'SR',   'RS',   'RASN', 'SNRA', 'DZSN',
     +			  'SNDZ', 'LR',   'RL',   'RADZ', 'DZRA',
     +			  'FZRA', 'ZR',   'SHGS', 'GS',   'SP',
     +			  'SHGR', 'GR',   'RW',   'SHRA', 'R-',
     +			  'RA',   'R',    'SW',   'SHSN', 'SG',
     +			  'SN',   'S',    'PL',   'PE',   'L-',
     +			  'L',    'DZ',   'K',    'FU',   'FOG',
     +			  'FG',   'F',    'HZ',   'H',    'IC',
     +			  'DU',   'PO' /
	DATA	nch   / 4, 4, 4, 3, 4,	  4, 4, 4, 2, 2,
     +			2, 4, 2, 2, 2,	  2, 2, 4, 4, 4,
     +			4, 2, 2, 4, 4,	  4, 2, 4, 2, 2,
     +			4, 2, 2, 4, 2,	  2, 1, 2, 4, 2,
     +			2, 1, 2, 2, 2,	  1, 2, 1, 2, 3,
     +			2, 1, 2, 1, 2,	  2, 2 /	     			   	
	DATA	wmo  / 95., 95., 95., 95., 96., 96., 98., 98., 18., 19., 
     +		       57., 57., 31., 31., 10., 69., 69., 69., 69., 69.,
     +		       69., 59., 59., 59., 59., 67., 67., 88., 88., 88.,
     +		       90., 90., 81., 81., 61., 63., 63., 86., 86., 77.,
     +		       73., 73., 79., 79., 51., 53., 53.,  4.,  4., 45.,
     +		       45., 45.,  5.,  5., 76.,  6.,  8. /              
C------------------------------------------------------------------------
	iret = 0
	ipos = 0
	i = 1
	DO WHILE ( ipos .eq. 0 .and. i .le. NCOD )
	    ipos = index (string, wcod (i) (1:nch(i)))
	    IF ( ipos .ne. 0 ) rivals (irwwmo) = wmo (i)
	    i = i + 1
	END DO
C*
	RETURN
	END
