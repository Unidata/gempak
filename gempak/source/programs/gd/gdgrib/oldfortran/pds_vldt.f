	SUBROUTINE PDS_VLDT  ( flgdtm, lasttm, idt, b13_25, i13_25,
     +			       iret )
C************************************************************************
C* PDS_VLDT								*
C*									*
C* This subroutine uses the full GEMPAK time (YYYYMMDD/HHHHFhhhmm) for	*
C* grids to set PDS bytes 13 through 25.				*
C*									*
C* Note that the input date must have a 4-digit year.			*
C*									*
C* PDS_VLDT  ( FLGDTM, LASTTM, IDT, B13_25, I13_25, IRET )		*
C*									*
C* Input parameters:							*
C*	FLGDTM (2)	CHAR*		GEMPAK grid times		*
C*	LASTTM		CHAR*		Last grid time			*
C*	IDT		INTEGER		Time interval from parm (hours) *
C*									*
C* Output parameters:							*
C*	B13_25 (13)	CHAR*1		PDS byte values 13--25		*
C*	I13_25 (13)	INTEGER		Integer value of bytes 13--25	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-89 = cannot handle dual times	*
C*					-90 = 4-digit year required	*
C*					-91 = forecast must be in hours *
C*					-97 = fhr not dvsble by 3, 6, 12*
C*					-98 = idt not dvsble by 3, 6, 12*
C**									*
C* Log:									*
C* K. Brill/HPC		 7/99						*
C* K. Brill/HPC		01/06	Allow time units 3, 6, 12 (PDS byte 18) *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	flgdtm (2), lasttm
	CHARACTER*1	b13_25 (13)
	INTEGER		i13_25 (13)
C*
	INTEGER		intdtm (3)
	CHARACTER*1	ctype
	CHARACTER*6	chm
	CHARACTER*20	hlddtm
C------------------------------------------------------------------------
	iret = 0
	DO i = 1, 13
	    i13_25 (i) = 255
	    b13_25 (i) = CHAR (255)
	END DO
C*
	IF ( flgdtm (2) .ne. ' ' ) THEN
	    iret = -89
	    RETURN
	END IF
	islash = INDEX ( flgdtm (1), '/' )
        IF ( islash .ne. 9 ) THEN
	    iret = -90
	    RETURN
	END IF
C*
	hlddtm = flgdtm (1) (3: )
	CALL GFULTM ( flgdtm (1), lasttm, hlddtm, ier )
	CALL TG_CTOI ( hlddtm, intdtm, ier )
	CALL TG_CFTM ( intdtm (3), ctype, chm, ier )
	CALL ST_LSTR ( chm, lng, ier )
	IF ( lng .gt. 3 ) THEN
	    iret = -91
	    RETURN
	END IF
C
C*	Split the GEMPAK time into integers.
C
	CALL ST_NUMB ( flgdtm (1) (1:2), icy, ier )
	iyr = intdtm (1) / 10000
	IF ( iyr .eq. 0 ) THEN
	    iyr = 100
	ELSE
	    icy = icy + 1
	END IF
	imh = MOD ( ( intdtm (1) / 100 ), 100 )
        idy = MOD ( intdtm (1), 100 )
	ihr = intdtm (2) / 100
	imn = MOD ( intdtm (2), 100 )
	CALL ST_NUMB ( chm, ifh, ier )
	mhr = 0
	i18 = 9
	jfh = ifh
	DO WHILE ( jfh .gt. 255 .and. i18 .lt. 12 )
	    i18 = i18 + 1
	    mhr = mhr + 3
	    IF ( MOD ( ifh, mhr ) .eq. 0 ) THEN
		jfh = ifh / mhr
	    END IF
	END DO
	IF ( jfh .gt. 255 ) THEN
	    iret = -97
	    RETURN
	END IF
	ifh = jfh
	IF ( mhr .ne. 0 ) THEN
	    IF ( MOD ( idt, mhr ) .ne. 0 ) THEN
		iret = -98
		RETURN
	    END IF
	    idt = idt / mhr
	ELSE
	    i18 = 1
	END IF
C
C*	Start assigning output integers.
C
	i13_25 (1) = iyr
	i13_25 (2) = imh
	i13_25 (3) = idy
	i13_25 (4) = ihr
	i13_25 (5) = imn
	i13_25 (6) = i18
	IF ( idt .le. 0 ) THEN
	    i13_25 (7) = ifh
	    i13_25 (8) = 0
	    i13_25 (9) = 0
	ELSE
	    i13_25 (7) = ifh - idt
	    i13_25 (8) = ifh
	    i13_25 (9) = 4
	END IF
	i13_25 (10) = 0
	i13_25 (11) = 0
	i13_25 (12) = 0
	i13_25 (13) = icy
	DO i = 1, 13
	    b13_25 (i) = CHAR ( i13_25 (i) )
	END DO
C*
	RETURN
	END
