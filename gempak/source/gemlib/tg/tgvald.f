	SUBROUTINE TG_VALD  ( gdattm, vdattm, iret )
C************************************************************************
C* TG_VALD								*
C*									*
C* This subroutine converts a grid time to the valid date/time.		*
C* The input string must be a full grid time.  The output time will	*
C* contain only the date and time.  The input and output strings	*
C* may be the same.							*
C*									*
C* TG_VALD  ( GDATTM, VDATTM, IRET )					*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Grid date/time			*
C*									*
C* Output parameters:							*
C*	VDATTM		CHAR*		Valid date/time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -2 = invalid forecast type	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89						*
C* M. desJardins/GSFC	 5/89	Fixed return codes			*
C* K. Brill/NMC		11/91	Remove string index range from sub call *
C* S. Jacobs/EAI	 9/93	Changed output format: yymmdd/hhmmVhhhmm*
C* M. desJardins/NMC	10/94	Fix output for analysis time		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* B. Yin/SAIC		 5/05	Check the length of the input string	*
C************************************************************************
	CHARACTER*(*)	vdattm, gdattm
C*
	CHARACTER	ftime*12, ccc*12, ftype*1
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	iret = 0
	lengdt =  LEN ( gdattm )
C
C*	If the input string has less than 13 characters (GEMPAK format)
C*	return with the original time string
C
	IF  ( lengdt .lt. 13 )  THEN
	    vdattm = gdattm ( :lengdt)
	    RETURN
	END IF
C
C*	If the forecast type is V replace it with a blank time.
C
	ftype = gdattm (12:12)
	CALL ST_LCUC  ( ftype, ftype, ier )
	IF  ( ftype .eq. 'V' )  THEN
	    vdattm = gdattm
	    RETURN
	  ELSE
	    ftime = gdattm (13: )
	END IF
C
C*	Return if the forecast time is blank.
C
	IF  ( ftime .eq. ' ' )  THEN
	    vdattm = gdattm ( :12)
	    RETURN
	END IF
C
C*	Get the forecast range.  A sets the i in ihhhmm to zero.
C
	CALL TG_IFTM  ( 'A', ftime, ihhhmm, iret )
	ihour = ihhhmm / 100
	imin  = ihhhmm - ihour * 100
	IF  ( iret .ne. 0 )  THEN
	    vdattm = ' '
	    RETURN
	END IF
C
C*	Break the date/time into 5 integers.
C
	ccc = gdattm ( :11 )
	CALL TI_CTOI  ( ccc, idtarr, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret   = -1
	    vdattm = ' '
	    RETURN
	END IF
C
C*	Add forecast minutes.
C
	idtarr (5) = idtarr (5) + imin
	DO  WHILE  ( idtarr (5) .ge. 60 )
	    idtarr (5) = idtarr (5) - 60
	    idtarr (4) = idtarr (4) + 1
	END DO
C
C*	Add forecast hours.
C
	idtarr (4) = idtarr (4) + ihour
	DO  WHILE  ( idtarr (4) .ge. 24 )
	    idtarr (4) = idtarr (4) - 24
	    CALL TI_ADDD  ( idtarr, idtarr, ier )
	END DO
C
C*	Convert the date/time back to character.
C
	CALL TI_ITOC  ( idtarr, vdattm, ier )
	CALL ST_LSTR  ( vdattm, lenvld, ier )
	vdattm = vdattm (:lenvld) // 'V' // ftime
C*
	RETURN
	END
