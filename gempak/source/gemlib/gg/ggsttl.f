	SUBROUTINE GG_STTL  ( ttlstr, iret )
C************************************************************************
C* GG_STTL								*
C*									*
C* This subroutine sets the plot title based on information found in	*
C* the image area header.						*
C*									*
C* GG_STTL  ( TTLSTR, IRET )						*
C*									*
C* Output parameters:							*
C*	TTLSTR		CHAR*		Title string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-15 = invalid image		*
C*					-16 = invalid image time	*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 9/94 						*
C* J. Cowie/COMET	12/94	Ignore comment line in table		*
C* J. Cowie/COMET	 1/95	Changed ggcmn to imgdef			*
C* J. Cowie/COMET	 2/95	Use new common variable names		*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* D.W.Plummer/NCEP      5/97   Remove seconds from time; add "Z"	*
C* J. Cowie/COMET	12/97	Added beam elevation for radar images	*
C* A. Hardy/GSC          4/99   Changed title output to GEMPAK format   *
C* S. Jacobs/NCEP	 2/01	Increased length of cbelv from 12 to 22	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	ttlstr
C*
	CHARACTER	chh*2, cmm*2, css*2, ctmp*2, cdate*8,
     +			cbelv*22, tmpst*12, dattim*13
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Return if the common variables have not been set.
C
	IF  ( imsorc .eq. IMISSD )  RETURN
C
C*	Start title with image source, type
C
	CALL ST_LSTR ( cmsorc, len1, ier )
	CALL ST_LSTR ( cmtype, len2, ier )
C
C*	Get day, month, year, time
C
	CALL ST_INCH ( imdate, cdate, ier )
C
        ihh = imtime/10000
        imm = MOD ( imtime/100, 100 )
        iss = MOD ( imtime, 100 )
C
        CALL ST_INCH ( ihh, chh, ier )
        IF  ( ihh .lt. 10 )  THEN
	    ctmp = '0'//chh(1:1)
	    chh  = ctmp
	END IF
C
        CALL ST_INCH ( imm, cmm, ier )
        IF  ( imm .lt. 10 )  THEN
	    ctmp = '0'//cmm(1:1)
	    cmm  = ctmp
	END IF
C
        CALL ST_INCH ( iss, css, ier )
        IF  ( iss .lt. 10 )  THEN
	    ctmp = '0'//css(1:1)
	    css  = ctmp
	END IF
C
C*	Insert the beam elevation for radar images.
C
	cbelv = ' '
	len3  = 1
	IF  ( ( imradf .gt. 0 ) .and. ( rmbelv .ne. RMISSD ) )  THEN
	    CALL ST_RLCH ( rmbelv, 2, cbelv, ier )
	    IF  ( ier .eq. 0 )  THEN
		CALL ST_LSTR ( cbelv, len3, ier )
		tmpst = ' ' // cbelv (1:len3) // ' DEG'
		cbelv = tmpst
		CALL ST_LSTR ( cbelv, len3, ier )
		len3 = len3 + 2
	    END IF
	END IF
C
C*      Setting date/time into GEMPAK format.
C
	dattim  =  cdate (3:8) // '/' // chh // cmm
	CALL ST_LSTR ( dattim, len4, ier )
C
C*	Set the title string based on the satellite ID.
C
	ttlstr = dattim ( :len4 ) // ' ' // cmsorc ( :len1 ) 
     +           // '  ' // cmtype ( :len2 ) // cbelv ( :len3 ) 
C*
	RETURN
	END
