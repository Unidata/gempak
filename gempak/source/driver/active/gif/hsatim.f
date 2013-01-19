	SUBROUTINE HSATIM ( fil, ixout0, iyout0, ixout1, iyout1,
     +                      ixspc0, iyspc0, ixspc1, iyspc1,  iret )
C************************************************************************
C* HSATIM - GIF 							*
C*                                                                      *
C* This subroutine displays a satellite image from a file.              *
C*                                                                      *
C* HSATIM ( FIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1,			*
C*          IXSPC0, IYSPC0, IXSPC1, IYSPC1, IRET )                      *
C*                                                                      *
C* Input parameters:                                                    *
C*      FIL		CHAR*           File name                       *
C*      IXOUT0          INTEGER         Image space                     *
C*      IYOUT0          INTEGER         Image space                     *
C*      IXOUT1          INTEGER         Image space                     *
C*      IYOUT1          INTEGER         Image space                     *
C*      IXSPC0          INTEGER         View space                      *
C*      IYSPC0          INTEGER         View space                      *
C*      IXSPC1          INTEGER         View space                      *
C*      IYSPC1          INTEGER         View space                      *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* J. Whistler/SSAI     12/91                                           *
C* G. Krueger/EAI       12/93   Modified hrest -> hsatim                *
C* S. Jacobs/NMC         7/94   General clean up                        *
C* J. Cowie/COMET        1/95   Added image subsetting arguments        *
C* J. Cowie/COMET        5/95   Removed them                            *
C* T. Lee/GSC		 7/00	Renamed xsatim to wsatim		*
C* R. Tian/SAIC		05/02	Added fax image type			*
C* F. J. Yen/NCEP	 8/04	Set gfplot to .TRUE.			*
C* T. Piper/SAIC	09/06	Replaced st_lstr with st_null           *
C************************************************************************
	INCLUDE 	'driver.cmn'
        CHARACTER*(*)   fil
C*
	CHARACTER	imgnam*133
C------------------------------------------------------------------------
C
C*	Get appropriate range of color table
C
	IF ( imgtyp .EQ. 1 ) THEN
	    ist  = ibgsat - 1
	    inum = nbwsat
	ELSE IF ( imgtyp .EQ. 2 ) THEN
	    ist  = ibgrad - 1
	    inum = nbwrad
	ELSE IF ( imgtyp .EQ. 3 ) THEN
	    ist  = ibgfax - 1
	    inum = nbwfax
	ELSE
	    ist  = 1
	    inum = 32
	END IF		
C
C*      Drop the satellite image.
C
        CALL ST_NULL ( fil, imgnam, ilen, ier )
	CALL WSATIM  ( imgnam, ixout0, iyout0, ixout1, iyout1, 
     +		      ist, inum, iret )
	gfplot = .TRUE.
C*
        RETURN
        END
