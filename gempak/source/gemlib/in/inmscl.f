	SUBROUTINE IN_MSCL ( mscale, sclflg, icolor, units, clat, 
     +			     nval, values, just, pos, size, lblfrq, 
     +			     legnd, iret )
C************************************************************************
C* IN_MSCL								*
C*									*
C* This subroutine converts the user input for the MSCALE variable	*
C* (character string) into the foreground/background/mask colors, units,*
C* true latitude, values, position, size, label frequency, and scale 	*
C* legend line variables.						*
C*									*
C* The MSCALE input must be of the form:				*
C*									*
C*	fgc;bgc;mask/units/lat;hide/values/anchor/x;y/len;wid/freq	*
C*									*
C* IN_MSCL ( MSCALE, SCLFLG, ICOLOR, UNITS, CLAT, NVAL, VALUES,  	*
C*			JUST, POS, SIZE, LBLFQ, LEGND, IRET )		*
C*									*
C* Input parameters:							*
C*	MSCALE	CHAR*		MSCALE input				*
C*									*
C* Output parameters:							*
C*	SCLFLG	   LOGICAL	Scale legend on/off flag		*
C*	ICOLOR(2)  INTEGER	Foreground/Background Colors		*
C*	UNITS	   CHAR*2	Scale legend units			*
C*	CLAT	   REAL		Latitude for distance calculation	*
C*	NVAL	   INTEGER	Number of values in values array	*
C*	VALUES	   REAL		List of values to display		*
C*	JUST(2)	   INTEGER	Horizontal justification		*
C*				  1 = Left				*
C*				  2 = Center				*
C*				  3 = Right				*
C*				Vertical justification			*
C*				  1 = Bottom				*
C*				  2 = Center				*
C*				  3 = Top				*
C*	POS(2)	REAL		Pos of scale legend in view coord	*
C*	SIZE(2)	REAL		Size of scale legend in view coord	*
C*	LBLFQ	INTEGER		Scale legend label frequency		*
C*	LEGND	LOGICAL		'True at' display flag			*
C*	IRET	INTEGER		Return code				*
C*				  0 = normal return			*
C**									*
C* Log:									*
C* T. Piper/SAIC	08/04	Created; modeled after in_cbar		*
C* T. Piper/SAIC	08/04	Changed valid central latitude range	*
C* T. Piper/SAIC	10/04	Added checks on increment and end values*
C* T. Piper/SAIC	10/04	Added check on nval & values; simplified*
C* T. Piper/SAIC	12/04	Added foreground AND background color 	*
C* T. Piper/SAIC	01/05	Added mask color & legnd		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	mscale
C
	REAL		endinc(2), pos(*), size(*), values(*)
	INTEGER		icolor(3), just(2)
	LOGICAL		legnd, sclflg
	CHARACTER	chide*1, cjust*2, group(8)*100, units*2,
     +			hide(2)*10
C------------------------------------------------------------------------
	iret = 0
	circ = 40000
C
C*	Check to see whether a scale legend should be drawn.
C
	IF ( mscale .eq. ' ' .or. mscale .eq. '0' ) THEN
	    sclflg = .false.
	    RETURN
	ELSE
	    sclflg = .true.
	END IF
C
C*	Break list into groups.
C
	CALL ST_CLST  ( mscale, '/', ' ', 8, group, ng, ier )
C
C*      Decode the color.
C
	IF  ( group(1) .eq. ' ' )  THEN
	    icolor(1) = 1
	    icolor(2) = 101
	ELSE
	    CALL IN_COLR ( group(1), 3, icolor, ier )
	    IF ( icolor(1) .eq. icolor(2) ) icolor(2) = 101
	    IF ( icolor(1) .eq. 0 ) icolor(1) = 1
	    IF ( icolor(3) .eq. icolor(1) .or.
     +		 icolor(3) .eq. icolor(2) ) icolor(3) = IMISSD 
	END IF
C
C*	Decode the units.
C
	CALL ST_LCUC ( group(2)(1:2), units, ier )
C
C*	Decode latitude for distance calculation
C
	CALL ST_RLST ( group(3), ';', RMISSD, 1, clat, numlat, ier )
	IF  ( ABS(clat) .gt. 85.00 )  clat = RMISSD
C
C*	Decode 'True at' display flag
C
	CALL ST_CLST ( group(3), ';', 'N', 2, hide, numstr, ier )
	CALL ST_LCUC ( hide(2)(1:1), chide, ier )
	IF ( chide .ne. 'Y' )  THEN
	    legnd = .true. 
	ELSE
	    legnd = .false. 
	END IF
C	
C*	Decode the values.
C
	CALL ST_RLST ( group(4), '-', RMISSD, 2, endinc, nval, ier )
	IF ( ier .eq. 0 )  THEN
	    IF ( endinc(1) .eq. 0 .or. endinc(1) .gt. circ/4  .or.
     +		 endinc(2) .eq. 0 .or. endinc(2) .gt. circ )  THEN
		nval = 0
C
C*	Increment-End case
C
	    ELSE IF ( nval .gt. 0 )  THEN
		IF ( nval .eq. 1 )  endinc(2) = endinc(1) * 4
		IF ( endinc(1) .eq. RMISSD )  THEN
		    endinc(1) = endinc(2) / 4
		ELSE IF ( endinc(1) .gt. endinc(2) )  THEN
		    endinc(1) = endinc(2)
		END IF
		nval = endinc(2) / endinc(1)
		IF ( nval .gt. 20 )  nval = 20
		values(1) = endinc(1)
		DO ii = 2, nval
		    values(ii) = values(ii-1) + values(1)
		END DO
	    END IF
C
C*	List of values case
C
	ELSE
 	    CALL ST_RLST ( group(4), ';', RMISSD, 20, values, 
     +						nval, ier )
	    IF ( ier .eq. 0 )  THEN 
		DO ii = 1, nval
		    IF ( values(ii) .gt. circ )  values (ii) = circ
		END DO
	    ELSE
		 nval = 0
	    END IF
	END IF
C
C*	Decode the justification (anchor).
C
	CALL ST_LCUC ( group(5)(1:2), cjust, ier )
C
C*	Set the X justification.
C
	IF  ( cjust(2:2) .eq. 'R' ) THEN
	    just(1) = 3
	ELSE IF ( cjust(2:2) .eq. 'C' ) THEN
	    just(1) = 2
	ELSE
	    just(1) = 1
	ENDIF
C
C*	Set the Y justification.
C
	IF  ( cjust(1:1) .eq. 'U' ) THEN
	    just(2) = 3
	ELSE IF ( cjust(1:1) .eq. 'C' ) THEN
	    just(2) = 2
	ELSE
	    just(2) = 1
	ENDIF
C
C*	Decode the position (x;y).
C
	CALL ST_RLST ( group(6), ';', RMISSD, 2, pos, numpos, ier )
	IF  ( pos(1) .lt. 0.0 .or. pos(1) .gt. 1.0 )  pos(1) = 0.025
	IF  ( pos(2) .lt. 0.0 .or. pos(2) .gt. 1.0 )  pos(2) = 0.005
C
C*	Decode the size (length;width).
C
	CALL ST_RLST ( group(7), ';', RMISSD, 2, size, numsiz, ier )
	IF  ( size(1) .lt. 0.0 .or. size(1) .gt. 1.0 )  size(1) = 0.4
	IF  ( size(2) .lt. 0.0 .or. size(2) .gt. 1.0 )  size(2) = 0.01
C
C*	Decode the label frequency.
C
	IF  ( group(8) .eq. ' ' )  THEN
	    lblfrq = 1
	ELSE
	    CALL ST_LSTR ( group(8), len1, ier )
	    CALL ST_INTG ( group(8)(:len1), lblfrq ,ier)
	    IF  ( lblfrq .eq. 0 )  lblfrq = 1
	END IF
C*
	RETURN
	END
