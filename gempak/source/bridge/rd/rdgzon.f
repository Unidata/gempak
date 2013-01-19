	SUBROUTINE RD_GZON ( segmnt, lens, ispnt, zonecd, nzone, iret )
C***********************************************************************
C* RD_GZON								*
C*									*
C* This subroutine finds and decodes the zone code list for the segment.*
C*									*
C* RD_GZON  ( SEGMNT, LENS, ISPNT, ZONECD, NZONE, IRET )		*
C*									*
C* Input parameters:							*
C*      SEGMNT		CHAR*		Segment				*
C*      LENS		INTEGER		Length of segment		*
C*									*
C* Output parameters:							*
C*	ISPNT		INTEGER		Segment pointer			*
C*      ZONECD (NZONE)	CHAR*		Zone codes for digital zone fcst*
C*	NZONE		INTEGER		Number of zones in forecast area*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = zone list not found	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C************************************************************************
	INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	segmnt, zonecd (*)
C*
	CHARACTER	zonstr*700, exptim*6, stlist(2)*1,
     +			sgmtpr*(DCMXBF)
	INTEGER		ilens(2)
	DATA		stlist / '-', '>' /
	DATA		ilens / 1, 1 /
C------------------------------------------------------------------------
	iret = 0
	nzone = 0
C*
    	CALL ST_UNPR ( segmnt, lens, sgmtpr, lensn, ier )
	IF ( lensn .gt. 170 ) THEN
	    ilend = lensn - 150
	  ELSE
	    ilend = lensn
	END IF
	CALL ST_NXTS ( sgmtpr, 1, ilend, stlist, ilens, 2, ipos,
     +		istrg, ier )
	IF ( ier .eq. 0 ) THEN
	    ipos = ipos - 6
    	    CALL SV_CNTT ( sgmtpr, ipos, lensn, zonstr, exptim, jx, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
 	    CALL BR_CNTY ( zonstr, zonecd, nzone, ier )
	    IF ( lens .gt. 170 ) THEN
	        ilnd = lens - 150
	      ELSE
	        ilnd = lens
	    END IF
	    CALL ST_NXTS ( segmnt, 1, ilnd, stlist, ilens, 2, ispos,
     +		    istrg, ier )
	    lndif = ispos - ipos
	    ispnt = jx + lndif - 3
	  ELSE
	    iret = -1
	END IF
C*
	RETURN
	END
