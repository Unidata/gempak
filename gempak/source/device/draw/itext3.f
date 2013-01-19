	SUBROUTINE ITEXT3 ( mfont, ix, iy, text, ixoff, iyoff, rotat,
     +                      size, iret )
C************************************************************************
C* ITEXT3 								*
C*									*
C* This subroutine computes the line segments necessary to draw a	*
C* text string in software character font numbers 3 and above. 		*
C*									*
C* ASCII character codes for the following are supported:		*
C*									*
C* BS LF CR SP ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9	*
C* : ; < = > ? @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z	*
C* [ \ ] ^ _ ` a b c d e f g h i j k l m n o p q r s t u v w x y z	*
C* { | } ~								*
C*									*
C* ASCII character codes outside the above set are translated into	*
C* a space. 								*
C*									*
C* ITEXT3 ( IX, IY, TEXT, IXOFF, IYOFF, ROTAT, SIZE, IRET )		*
C*									*
C* Input parameters:							*
C*	MFONT		INTEGER	Coded font number			*
C*	IX		INTEGER	x coord. in device units		*
C*	IY		INTEGER	y coord. in device units		*
C*	TEXT		CHAR*	Text string				*
C*	IXOFF		INTEGER	x offset in device units along rotated 	*
C*				x axis					*
C*	IYOFF		INTEGER	y offset in device units along rotated 	*
C*				y axis					*
C*	ROTAT		REAL	Rotation angle of text string, positive	*
C*				counter-clockwise from horizontal	*
C*	SIZE		REAL	Text string size multiplier 		*
C*									*
C* Output parameters:							*
C*	IRET    	INTEGER Return code				*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	10/07	Created					*
C************************************************************************
	CHARACTER*(*)	text
C*
	PARAMETER	( MXCHAR = 200 )
C!					Number of characters per font
	PARAMETER	( MXCHPT = 150 )
C!					Number of points per character
	PARAMETER	( SZMULT = 3.30 )
C!					Scaling factor compared to the
C!					base software font
	PARAMETER	( dtr = .0174532925 )
C!					Degrees to radians
C*
	INTEGER		iascii(MXCHAR), npnts(MXCHAR),
     +			ixmin(MXCHAR), ixmax(MXCHAR),
     +			ixc (MXCHPT*MXCHAR), iyc (MXCHPT*MXCHAR)
C
	INTEGER		ixp (MXCHPT), iyp(MXCHPT)
C*
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get the Hershey Font from the stored information.
C
	maxpt = MXCHPT
	CALL CTB_HFGETFONT ( mfont, maxpt, numchr, iascii, ixmin,
     +			     ixmax, npnts, ixc, iyc, ier )
     	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG ( 'CTB', ier, ' ', ier2 )
	    iret = NOFNTFL
	    RETURN
	END IF
C
C*	Compute sine and cosine of rotation angle.
C
	cosrot = COS( rotat * dtr )
	sinrot = SIN( rotat * dtr )
C
C*	Compute requested origin offsets and initialize spacing offsets.
C
	IF ( MOD( ixoff, 2 ) .ne. 0 ) THEN
	    ixo = ( ixoff - 1 ) / 2 * (7*SZMULT) + (4*SZMULT)
	ELSE
	    ixo = ixoff / 2 * (7*SZMULT)
	END IF
C*
	IF ( MOD( iyoff, 2 ) .ne. 0 ) THEN
	    iyo = ( iyoff - 1 ) / 2 * (9*SZMULT) + (5*SZMULT)
	ELSE
	    iyo = iyoff / 2 * (9*SZMULT)
	END IF
C*
	ixr  = 0
	ixs  = 1
	iys  = 1
C
C*	Loop over the number of characters in the text string.
C
	DO  ic = 1, LEN( text )
C
C*	    Determine the ascii code for this character
C
	    icode = ICHAR( text(ic:ic) ) 
C
C*	    For compilers that return signed integer
C*	    when ascii code value > 127
C
	    IF  ( icode .lt. 0 )  THEN
		icode = icode + 256
	    END IF 
C
C*	    Find the character in the array.
C
	    jc = 1
	    DO WHILE ( (iascii(jc) .ne. icode) .and. (jc .le. numchr) )
	    	jc = jc + 1
	    END DO
C
C*	    If the ascii code is for a drawn character,
C*	    then draw the character.
C
	    IF  ( jc .le. numchr )  THEN
C
C*		Compute the spacing offset for this character.
C
		IF  ( ic .gt. 1 )  THEN
		    ixs = ixs + ixr + ABS(ixmin(jc))
		ENDIF
C
C*		Loop over the points.
C
		np = 0
C*
		DO  ip = 1, npnts(jc)
C
C*		    Set the index to the point arrays.
C
		    idx = ( jc - 1 ) * MXCHPT + ip
C
C*	            If a 'pen up' flag is found, then draw the current
C*		    line and reset the point count.
C
	            IF ( ( ( ixc(idx) .eq. -99 ) .and.
     +			   ( iyc(idx) .eq. -99 ) ) .and.
     +			 ( np .gt. 0 ) ) THEN
	                CALL ILINE ( np, ixp, iyp, ier )
	                np = 0
		    ELSE
C
C*		    Compute the x and y coordinates for this point.
C
C*	    Coordinates are computed in three steps. 
C*		    
C*	    - First, X and Y coordinate components are computed as the 
C*	      sum of the matrix character coordinate, origin offset and 
C*	      spacing offset times the size multiplier.  The negative 
C*	      of the y origin offset is used to be consistent with the 
C*	      convention that character spacing is positive downward.
C*
C*	    - Next, X' and Y' coordinate components are computed for a 
C*	      coordinate system rotated to the specified angle.  The 
C*	      negative of the y component is used in this calculation to
C*	      be consistent with the convention that character spacing 
C*	      is positive downward.
C*		
C*	    - Finally, the X and Y coordinates are computed to be the 
C*	      sum of the requested coordinate and the nearest integer
C*	      to the product of the rotated coordinates and a factor 
C*	      which accounts for direction of increasing or decreasing
C*	      X or Y coordinate.
C
			np = np + 1
C*
			xd = ( ixc(idx) + ixo + ixs ) * (size/SZMULT)
			yd = ( iyc(idx) - iyo + iys ) * (size/SZMULT)
C*
			xprimd = xd * cosrot + yd * sinrot
			yprimd = xd * sinrot - yd * cosrot 
C*
			ixp(np) = ix + NINT( xprimd * aspect ) * ispanx
			iyp(np) = iy + NINT( yprimd ) * ispany
	            END IF
C*
	        END DO
C
C*	        Draw the last line.
C
	        CALL ILINE ( np, ixp, iyp, ier )
C
C*		Save the extent of the right side of the character.
C
		ixr = ixmax(jc)
C*
	    ELSE IF ( icode .EQ. 8 ) THEN
C
C*	    	Compute the spacing offset for a backapace.
C
		ixs = ixs - 7*SZMULT
C*	
	    ELSE IF ( icode .EQ. 10 ) THEN
C
C*	    	Compute the spacing offset for a line feed.
C
	        iys = iys + (9.5)*SZMULT
C*
	    ELSE IF ( icode .EQ. 13 ) THEN
C
C*	    	Compute the spacing offset for a carriage return.
C
		ixs = 0
C*
	    ELSE IF ( icode .EQ. 32 ) THEN
C
C*	    	Compute the spacing offset for a space.
C
	        ixs = ixs + 7*SZMULT
C*
	    ELSE 
C
C*		This is a non-supported ascii code.  
C
C*	    	Compute the spacing offset for a space.
C
	        ixs = ixs + 7*SZMULT
C*                                                            
	    END IF
C*
	END DO
C*
	RETURN
	END
