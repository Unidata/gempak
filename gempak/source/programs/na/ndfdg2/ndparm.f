	SUBROUTINE ND_PARM ( discpln, pds, parm, ihzrmp, idrct, iret )
C************************************************************************
C* ND_PARM								*
C*									*
C* This subroutine converts the GRIB2 discipline, parameter category, 	*
C* parameter number, and product definition template number into the 	*
C* four-character GEMPAK parameter name.				*
C*									*
C* ND_PARM  ( DISCPLN, PDS, PARM, IHZRMP, IDRCT, IRET )			*
C*									*
C* Input parameters:                                                    *
C*	DISCPLN		INTEGER		GRIB2 Master Table Number	*
C*      PDS(*)	     	INTEGER         GRIB2 PDS information		*
C*									*
C* Output parameters:                                                   *
C*      PARM(4)         CHAR*           GEMPAK parameter name           *
C*	IHZRMP		INTEGER		Horizontal remapping		*
C*	IDRCT		INTEGER		Directional flag		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       +4 = invalid parm category     *
C**									*
C* Log:									*
C* T. Piper/SAIC        10/02   Created					*
C* T. Piper/SAIC	12/02	Added SNOW				*
C* T. Piper/SAIC	05/03	Rewrote to get from table		*
C* T. Piper/SAIC	08/03	Added CPC support			*
C* T. Piper/SAIC	10/03	Fixed bug with Probability of Precip.	*
C* M. Li/SAIC		04/04	Added ihzrmp, and idrct			*
C************************************************************************
	INTEGER       discpln, pds(*) 
	CHARACTER*(*) parm
	CHARACTER   chh*2, rrr*3, lparm*12
C-------------------------------------------------------------------
C*	If template 9, could be CPC product
	IF ( pds(8) .eq. 9 .and. pds(35) .ne. 255 )  THEN
	   ipdtn = (pds(35) * 10) + pds(8)
	ELSE
	   ipdtn = pds(8)
	END IF
 	CALL CTB_G2GNAM(discpln, pds(10), pds(11), ipdtn, lparm,
     +			ihzrmp, idrct, ier)
	IF ( ier .eq. 0 )  THEN
	    iret = 0
	    CALL ST_RNUL ( lparm, lparm, lenp, ier )
	    ipos = INDEX(lparm, "--")
	    IF ( ipos .ne. 0 )  THEN
		IF ( pds(8) .eq. 8 )  THEN
		    indx = 49
		ELSE IF ( pds(8) .eq. 9 )  THEN
		   indx = 62
		END IF
C*	Average, accumulation, and/or extreme values ...
		irange = pds(indx+1)
C*	Length of the time range over which statistical processing is done
C*	Indicator of unit of time range over which statistical processing is 
C*	done (See Code Table 4.4)
		IF ( pds(indx) .eq. 0 )  THEN
		    ihh = irange / 60
		ELSE IF ( pds(indx) .eq. 1 )  THEN
		    ihh = irange
		ELSE IF ( pds(indx) .eq. 2 )  THEN
		    ihh = irange * 24 
		ELSE IF ( pds(indx) .eq. 10 )  THEN
		    ihh = irange * 3
		ELSE IF ( pds(indx) .eq. 11 )  THEN
		    ihh = irange * 6
		ELSE IF ( pds(indx) .eq. 12 )  THEN
		    ihh = irange * 12
		ELSE IF ( pds(indx) .eq. 13 )  THEN
		    ihh = irange / 3600
		ELSE
		    ihh = 0
		END IF
		CALL ST_INCH(ihh+100, rrr, ier)
		chh = rrr(2:3)
		parm = lparm(1:ipos-1) // chh // lparm(ipos+2:lenp)
	    ELSE
	        parm = lparm
	    END IF
C*	Non-Supported Parameter Category
	ELSE
	    iret = +4 
	END IF
C
	RETURN
	END
