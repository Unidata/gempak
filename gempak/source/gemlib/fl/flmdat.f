	SUBROUTINE FL_MDAT ( filnam, templt, defdat, dattim, iret )
C************************************************************************
C* FL_MDAT								*
C*									*
C* This function constructs a GEMPAK date/time string using the		*
C* template and a file name.						*
C*									*
C* Valid substrings for the template include:				*
C*									*
C*	YYYY		Year with the century				*
C*	YY		Year without the century			*
C*	MMM		Month 3 letter abbreviation			*
C*	MM		Month number					*
C*	DD		Day						*
C*	HH		Hour						*
C*	NN		Minute						*
C*	FFFFF		5-character forecast hour/min			*
C*	FFF		3-character forecast hour			*
C*	FF		2-character forecast hour			*
C*									*
C* FL_MDAT ( FILNAM, TEMPLT, DEFDAT, DATTIM, IRET )			*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	TEMPLT		CHAR*		File name template		*
C*	DEFDAT		CHAR*		Default date/time string	*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK date/time string		*
C*	IRET		INTEGER		Return code			*
C*					= 0 - normal			*
C*					= -14 - unable to decode	*
C**									*
C* G. Krueger/EAI	 8/96						*
C* S. Maxwell		12/96	Capitialized several words		*
C* D.W.Plummer/NCEP	 2/98	Added forecast hour processing		*
C* T. Lee/GSC		 5/99	Initialized month/date to 0101		*
C* S. Jacobs/NCEP	 9/99	Changed call to accept default date/time*
C* D.W.Plummer/NCEP	11/99	Added error checking while decoding	*
C* K. Brill/HPC		03/06	First replace *'s in template with the	*
C*				corresponding strings from the file	*
C* S. Jacobs/NCEP	 8/14	Added support for FFFFF template 	*
C***********************************************************************/
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, templt, filnam, defdat
C*
	LOGICAL		done
	CHARACTER	monams*47, pname*(MXFLSZ), tstr*8
	CHARACTER	tplate*(MXFLSZ)
	CHARACTER	tmpplt*(MXFLSZ)
	CHARACTER	aftr*1, aftr2*2
	DATA		monams /
     +		'JAN/FEB/MAR/APR/MAY/JUN/JUL/AUG/SEP/OCT/NOV/DEC' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize return value.
C
	dattim = defdat
C
C*	Find the file name contained in the path.
C
	CALL ST_LSTR ( filnam, ncnt, ier )
	done = .false.
	ipnt = ncnt + 1
	DO WHILE ( ( .not. done ) .and. ( ncnt .gt. 0 ) )
	    ipnt = ipnt - 1
	    IF ( ( filnam (ipnt:ipnt) .eq. '/' ) .or.
     +		 ( filnam (ipnt:ipnt) .eq. ':' ) ) done = .true.
	    ncnt = ncnt - 1
	END DO
C
	IF ( .not. done ) THEN
	    pname = filnam
	ELSE
	    pname = filnam (ipnt + 1:)
	END IF
C
C*	Replace any '*' in the template with actual characters from
C*	the input file name.
C
	CALL ST_LSTR ( pname, lnn, ier )
	tplate = templt
	istar = 1
	DO WHILE ( istar .ne. 0 )
	    tmpplt = tplate
	    CALL ST_LSTR ( tmpplt, lnt, ier )
	    istar = INDEX ( tmpplt, '*' )
	    IF ( istar .ne. 0 .and. istar .lt. lnt ) THEN
C
C*	        Find the template character that follows the star.
C
	        isp1 = istar + 1
	        isp2 = istar + 2
	        aftr = tmpplt (isp1:isp1)
	        aftr2 = tmpplt (isp1:isp2)
	        IF ( aftr2 .eq. 'YY' .or. aftr2 .eq. 'MM' .or.
     +		     aftr2 .eq. 'HH' .or. aftr2 .eq. 'FF' .or.
     +		     aftr2 .eq. 'DD' .or. aftr2 .eq. 'NN' .or.
     +		     aftr2 .eq. 'DW' ) THEN
C
C*		    The only unambiguous case is for '*' followed by
C*		    a non-symbolic string.  This condition could
C*		    be relaxed by checking here for strings like YY,
C*		    YYYY, MM, DD, DWK,  HH, FF, FFF or FFFFF following '*'
C*		    and trying to parse past them.  That is not done
C*		    in this implementation (03/06).
C
		    iret = -17
		    RETURN
	        END IF
C
C*	        Replace the star position with first character from
C*		the string in the file name.
C
	        i = istar
		tplate (i:i) = pname (i:i)
C
C*	        Loop over file name, storing star replacement string
C*		after the star position.
C
		i = i + 1
	        DO WHILE ( i .le. lnn .and. pname (i:i) .ne. aftr )
		    tplate (i:i) = pname (i:i)
		    i = i + 1
	        END DO
	        IF ( i .lt. lnt ) THEN
	            tplate (i:) = tmpplt (isp1:)
	        END IF
	    ELSE
	        istar = 0
	    END IF
	END DO
C
C*	Extract the 2 digits of a YY year from the file name.
C
	ipyy = INDEX ( tplate, 'YY' )
	IF ( ipyy .ne. 0 )  THEN
	    tstr = pname (ipyy:ipyy+1)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (1:2) = pname (ipyy:ipyy+1)
	    END IF
	END IF
C
C*	Extract the last 4 digits of a YYYY year from the file name.
C
	ipyy = INDEX ( tplate, 'YYYY' )
	IF ( ipyy .ne. 0 )  THEN
	    tstr = pname (ipyy+2:ipyy+3)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (1:2) = pname (ipyy+2:ipyy+3)
	    END IF
	END IF
C
C*	Extract the 2 digits of an MM month from the file name.
C
	ipmm = INDEX ( tplate, 'MM' )
	IF ( ipmm .ne. 0 )  THEN
	    tstr = pname (ipmm:ipmm+1)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (3:4) = pname (ipmm:ipmm+1)
	    END IF
	END IF
C
C*	Extract the month value from a 3 character MMM month from the
C*	file name.
C
	ipmm = INDEX ( tplate, 'MMM' )
	IF ( ipmm .ne. 0 ) THEN
	    dattim (3:4) = '00'
	    tstr = pname (ipmm:ipmm+2)
	    CALL ST_LCUC ( tstr, tstr, ier )
	    ipmon = INDEX ( monams, tstr )
	    CALL ST_INLN ( ipmon / 4 + 1, tstr, lnum, ier )
	    dattim (5-lnum:4) = tstr
	END IF
C
C*	Extract the 2 digits of a DD day from the file name.
C*	If the template does not have a DD day, e.g., climo data, it 
C*	will remain to be 01.
C
	ipdd = INDEX ( tplate, 'DD' )
	IF ( ipdd .ne. 0 )  THEN
	    tstr = pname (ipdd:ipdd+1)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (5:6) = pname (ipdd:ipdd+1)
	    END IF
	END IF
C
C*	Extract the 2 digits of an HH hour from the file name.
C
	iphh = INDEX ( tplate, 'HH' )
	IF ( iphh .ne. 0 )  THEN
	    tstr = pname (iphh:iphh+1)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (8:9) = pname (iphh:iphh+1)
	    END IF
	END IF
C
C*	Extract the 2 digits of an NN minute from the file name.
C
	ipmn = INDEX ( tplate, 'NN' )
	IF ( ipmn .ne. 0 )  THEN
	    tstr = pname (ipmn:ipmn+1)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (10:11) = pname (ipmn:ipmn+1)
	    END IF
	END IF
C
C*	Extract the 5 digits of an FFFFF forecast hour+min
C*	from the file name.
C
	ipfffff = INDEX ( tplate, 'FFFFF' )
	IF ( ipfffff .ne. 0 )  THEN
	    tstr = pname (ipfffff:ipfffff+4)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (12:12) = 'F'
	        dattim (13:17) = pname (ipfffff:ipfffff+4)
	    END IF
	ELSE
C
C*	  Extract the 3 digits of an FFF forecast hour from the file name.
C
	  ipfff = INDEX ( tplate, 'FFF' )
	  IF ( ipfff .ne. 0 )  THEN
	    tstr = pname (ipfff:ipfff+2)
	    CALL ST_NUMB ( tstr, inumb, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -14
		RETURN
	    ELSE
	        dattim (12:12) = 'F'
	        dattim (13:15) = pname (ipfff:ipfff+2)
	    END IF
	  ELSE
C
C*	    Extract the 2 digits of an FF forecast hour from the file name.
C
	    ipff = INDEX ( tplate, 'FF' )
	    IF ( ipff .ne. 0 )  THEN
	        tstr = pname (ipff:ipff+1)
	        CALL ST_NUMB ( tstr, inumb, ier )
	        IF ( ier .ne. 0 )  THEN
		    iret = -14
		    RETURN
	        ELSE
	            dattim (12:13) = 'F0'
	            dattim (14:15) = pname (ipff:ipff+1)
	        END IF
	    END IF
	  END IF
	END IF
C*
	RETURN
	END
