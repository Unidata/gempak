	SUBROUTINE MA_CGCL ( lenb, bulltn, iret )
C************************************************************************
C* MA_CGCL                                                              *
C*                                                                      *
C* This subroutine cleans up the CGR data before decoding.  It attempts *
C* to standardize such text as the report header line, missing char     *
C* indicators, etc.  It is very important that the length of            *
C* the bulletin is not changed when replacing text.
C*                                                                      *
C* MA_CGCL  ( LENB, BULLTN, IRET )                                      *
C*                                                                      *
C* Input parameters:                                                    *
C*      LENB            INTEGER         Length of bulletin in bytes     *
C*      BULLTN          CHAR*           Bulletin                        *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*					  1 = ID not found		*
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP  4/01	Original Author				*
C* F. J. Yen/NCEP	 4/01	Rewrote & renamed from CG_CLUP.  Handled*
C*				more data variation. Added ',' & 'RMK'.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	bulltn
C*
	CHARACTER	titbas (10)*16, rplchr (10)*16, chwxvs (6)*18
	CHARACTER	carrtn, substr (36)*16, repstr (36)*16
	LOGICAL		more
	INTEGER		nmbsch (10), nmrpch (10), lensub (36)
	DATA		chwxvs / 'WXVSB',   'WXVIS', 'OXVSB',
     +				 'WX',      'VSB',   'VIS' / 
	DATA		titbas / 'WIND',    'WIN0',  'WAV',    'SEA',
     +				 'SST',	    'AIR',   'PRES',   'BARO',
     +				 'REMARKS', 'RMK' /
	DATA		nmbsch /    4,        4,       3,        3,
     +			            3,        3,       4,        4,
     +				    7,        3 /
	DATA		nmrpch /    5,        5,       4,        4,
     +				    4,        4,       5,        5,
     +				    4,        4 / 
	DATA		rplchr / '/WIND',   '/WIND',  '/WAV',   '/SEA',
     +				 '/SEA',    '/AIR',   '/PRES',  '/PRES',
     +				 '/RMK',    '/RMK' /
	DATA		substr / '///',   'MISG',   'MSG',   'UNK',
     +				 'MMM',   'MM',     'XX',    'INOP',
     +				 'NRPT',  'NONE',   'NO REPORT GIVING',
     +				 'NO REPORT',  'NO RPT/',  'NO RPT5',
     +				 'NO RPT','NOT AVAILABLE','UNAVAILABLE',
     +				 'UNAVAIL', 'UNAVIL', 'UNAVL', 'UNAV',
     +				 '--.--',   '------', '-----', '----',
     +				 '---',     '--',     '/-/',   ' OVC',
     +				 ' CYC',    ' PCC',   'C/',    ', ',
     +				 'GROUP ',  ' STN ',   'S C A ' /
	DATA		repstr / '???',    'M',      'M',      'M',
     +				 'M',      'M',      'M',      'M',
     +				 'M',      ' ',      ' ',
     +				 ' ',      'M/',     ' ',
     +				 ' ',      ' ',      ' ',
     +				 ' ',      ' ',      ' ',      ' ',
     +				 ' ',      ' ',      ' ',      ' ',
     +				 ' ',      ' ',      '/M/',    '  CY',
     +				 '  CY',   '  PC',   'C',      ' ',
     +				 ' ',      ' ',      'SCA' /
	DATA		lensub / 3,  4,  3,  3,     3,  2,  2,  4, 
     +				 4,  4, 16,         9,  7,  7, 
     +				 6, 13, 11,         7,  6,  5,  4,
     +				 5,  6,  5,  4,     3,  2,  3,  4,
     +				 4,  4,  2,  2,	    6,  5,  6 / 				 
C------------------------------------------------------------------------
	iret = 0
	carrtn = CHAR(13)
C
C*	Check raw bulletin for unusual formats in header line, which
C*	should look like:
C*	    ID  WXVSB   /WIND /WAV /SEA/AIR/PRES REMARKS        STATION
C*	First, clean up the header line.
C
	ipid = index ( bulltn (1:lenb-45), CHLF // 'ID ' )
	IF ( ipid .eq. 0 ) THEN
	    iret = 1
	    RETURN
	  ELSE
	    ipend = index ( bulltn ( ipid + 1:lenb ), CHLF )
	    ipenda = ipend + ipid
	END IF
	ist = ipid + 3
C
C*	In the header line, check for keywords (in titbas) such as WIND,
C*	WIN0, WAV, SEA, SST, REMARKS, and RMK.  Replace the keyword
C*	with the standardized keyword (in rplchr) prefixed with a '/'
C*	if missing or if there are spaces between the '/' and the
C*	keyword.
C
	DO i = 1, 10 
	    ib = index ( bulltn (ist:ipenda), titbas(i)(1:nmbsch(i)))
	    IF ( ib .ne. 0 ) THEN
		more = .true.
		js = ist + ib - 2
		j = js
		nsp = -1
		DO WHILE ( more .and. j .gt. ist ) 
		    IF ( bulltn (j:j) .eq. ' ' ) THEN
		        nsp = nsp + 1
		      ELSE IF ( bulltn (j:j) .eq. '/' ) THEN
			more = .false.
			nsp = nsp + 1
			js = j
		      ELSE
			more = .false.
			nsp = 0
		    END IF
		    j = j - 1
		END DO
		nsp = nsp + nmrpch (i)
		je = js + nsp
		j = je
		more = .true.
		DO WHILE ( more .and. j .lt. ipenda)
		    IF ( bulltn (j:j) .eq. '/' ) THEN
			more = .false.
		      ELSE IF ( bulltn (j:j) .eq. carrtn ) THEN
			more = .false.
		      ELSE IF ( bulltn (j:j) .ne. ' ' ) THEN
			nsp = nsp + 1
		      ELSE
			more = .false.
		    END IF
		    j = j + 1			
		END DO 
 	 	bulltn(js:js+nsp-1) = rplchr(i)(1:nsp )
	    END IF 
	END DO
	more = .true.
	i = 1
C
C*	Replace various forms and spacing of the weather visibility
C*	heading such as 'WXVIS', 'WX  VSB', and 'WX    VIS'
C*	with the standard 'WXVSB'.
C
	DO WHILE ( more ) 
	    ib = index ( bulltn (ist:ipenda), chwxvs(i)(1:5) )
	    IF ( ib .ne. 0 ) THEN
		more = .false.
		IF ( i .ne. 1 ) THEN
		    js = ist + ib - 1
		    bulltn(js:js+4) = chwxvs(1)
	 	END IF
	      ELSE
		IF ( i .ge. 3 ) more = .false.
		i = i + 1
	    END IF
	END DO
	more = .true.
	IF ( more .and. i .ge. 4 ) THEN
	    ib = index ( bulltn (ist:ipenda), chwxvs(4)(1:2) )
	    IF ( ib .ne. 0 ) THEN
		i = 5
		DO WHILE (more) 
		    ib2 = index ( bulltn (ist:ipenda), chwxvs(i)(1:3) )
		    IF ( ib2 .ne. 0 ) THEN
	        	js = ist + ib + 1
			nsp = ib2 - ib
			bulltn(js:js+nsp) = chwxvs(5)(1:nsp+1)

			more = .false.
		      ELSE
			IF ( i .ge. 6 ) more = .false.
			i = i + 1
		    END IF
		END DO
	    END IF 
	END IF
C
C*	Check for different formats within report lines.  Replace
C*	strings in substr with corresponding strings in repstr.
C*	This replacment includes (1) replacing ids encoded as missing
C*	('///') with '???' to keep from confusing the slashes with
C*	field separators, (2) replacing various forms of missing data
C*	indicators with a single 'M', (3) replacing bad data
C*	formats with standard characters (e.g. replace 'OVC' with
C*	' CY'), and (4)	replacing various forms of 'NO RPT' with blanks.
C
	DO i = 1, 36
	    ipos = -1
	    ip = ipenda + 1
	    DO WHILE ( ipos .ne. 0 .and. ip .lt. lenb - lensub(i) + 1 )
		CALL ST_RPSL ( bulltn (ip:lenb), substr(i), lensub(i),
     +		     repstr(i), lensub(i), ipos, bulltn(ip:lenb), iret )	
		IF ( ipos .ne. 0 ) ip = ip + ipos - 1
	    END DO
	END DO
C*
      RETURN
      END
