	SUBROUTINE GH_KGOV ( alat, alon, ipt, offset, dir, nknt, flip,
     +			     xx, yy, xkey, ykey, daylab, nlabel, xlab,
     +			     ylab, xlin, ylin, iret )
C************************************************************************
C* GH_KGOV								*
C*									*
C* This subroutine checks whether a label overlaps another label, the   *
C* key (legend box), the track, or a connecting line.  If it does, an   *
C* attempt is made to reposition the label so no overlap occurs.  The   *
C* label and label-connecting line are plotted if a good location is    *
C* found.                                                               *
C*									*
C* GH_KGOV ( ALAT, ALON, IPT, OFFSET, DIR, NKNT, FLIP, XX, YY, XKEY,    *
C*	     YKEY, DAYLAB, NLABEL, XLAB, YLAB, XLIN, YLIN, IRET )       *
C*									*
C* Input parameters:							*
C*      ALAT (NKNT)	REAL		Current/track latitudes         *
C*	ALON (NKNT)	REAL		Current/track longitudes        *
C* 	IPT		INTEGER		Current/track point number      *
C*	OFFSET		REAL		Length of connecting line (m)   *
C*	DIR		REAL		Proposed dir. for label line    *
C*      NKNT		INTEGER		Number of track points   	*
C*	FLIP		LOGICAL		Flag to flip label to opp. side *
C*	XX (NKNT)	REAL		Norm. x coords of track points  *
C*	YY (NKNT)	REAL		Norm. y coords of track	points  *
C*	XKEY (*)	REAL		Norm. x coords of legend box    *
C*	YKEY (*)	REAL		Norm. y coords of legend box    *
C*	DAYLAB		CHAR*		Day/time label                  *
C*                                                                      *
C* Input and output parameters:                                         *
C*	NLABEL		INTEGER		Number of labels plotted        *
C*	XLAB (5,*)	REAL		Norm. x coords of label boxes   *
C*	YLAB (5,*)	REAL		Norm. y coords of label boxes   *
C*	XLIN (2,*)	REAL		Norm. x coords of conn. lines   *
C*	YLIN (2,*)	REAL		Norm. y coords of conn. lines   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = no label plotted          *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/02	                                        *
C* D. Kidwell/NCEP	 3/03	Increased allowable number of points    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( BOXHI = .018, BOXWD = .11 )
	CHARACTER*(*)	daylab
	LOGICAL		flip
        REAL		alat (*), alon (*), xx (*), yy (*), xkey (*),
     +			ykey (*), xlab (5,*), ylab (5,*), xlin (2,*),
     +			ylin (2,*)
C*
	CHARACTER	sys*2
	LOGICAL		good, bad, bad2
      	REAL		bxlab (5), bylab (5), slat (2), slon (2),
     +	    		xxx (2), yyy (2), xt (2,7), yt (2,7)
C-----------------------------------------------------------------------
      	iret = 0
	sys  = 'N'
	CALL ST_NULL ( sys, sys, lens, ier )
C
C*	Get the number of possible label positions for this point.
C
	IF ( nknt .eq. ipt ) THEN
	    nposmx = 5
	  ELSE IF ( flip ) THEN
	    nposmx = 4
	  ELSE
	    nposmx = 2
	END IF
C
	direct = dir
	ipos   = 1
	good   = .false.
C
C*	Look for a good label position.
C
	DO WHILE ( .not. good )
C
C*          Get the label location, offsets and justification.
C
            CALL GH_KGLC ( alat (ipt), alon (ipt), offset, direct, blat,
     +                     blon, idir, just, ixoff, iyoff, ier )
C
C*	    Adjust the x offset and justification if necessary.
C
	    IF ( ( ipos .eq. 3 ) .or. ( ipos .eq. 4 ) .or.
     +		 ( ( ipos .eq. 2 ) .and. ( nposmx .eq. 2 ) ) ) THEN
		just  = 4 - just
		ixoff = -ixoff
	    END IF
C
C*	    Get the label position and label-connecting line endpoints
C*	    in normalized coordinates.
C
            CALL GTRANS ( 'M', 'N', 1, blat, blon, bxlab (1), bylab (1),
     +		          ier )
	    xxx ( 1 ) = xx ( ipt )
	    yyy ( 1 ) = yy ( ipt )
	    xxx ( 2 ) = bxlab ( 1 ) 
 	    yyy ( 2 ) = bylab ( 1 )
C
C*	    Define default label limits as a function of offset.
C
	    xoff = FLOAT ( ixoff )
	    IF ( IABS ( ixoff ) .ne. 1 ) THEN
 	        bxlab (1) = bxlab (1) - ( 1. - xoff / 9. ) * BOXWD / 2.
	        xoff = 1.
	    END IF
	    bxlab ( 2 ) = bxlab ( 1 ) 
	    bxlab ( 3 ) = bxlab ( 1 ) + xoff * BOXWD
	    bxlab ( 4 ) = bxlab ( 3 )
	    bxlab ( 5 ) = bxlab ( 1 ) 
	    yoff = FLOAT ( iyoff )
	    IF ( iyoff .gt. 1 ) THEN
 	        bylab (1) = bylab (1) + ( ( yoff - 1. ) / 2. ) * BOXHI
	        yoff = 1.
	    END IF
	    bylab ( 2 ) = bylab ( 1 ) + yoff * BOXHI
	    bylab ( 3 ) = bylab ( 2 )
	    bylab ( 4 ) = bylab ( 1 )
	    bylab ( 5 ) = bylab ( 1 )
C
C*	    Bypass overlap checks if this is the last possible position
C*	    for the final label.
C
	    IF ( ipos .lt. 5 ) THEN
		bad = .false.
	      ELSE
		bad = .true.
	    END IF
C
C*	    Check for overlap of new label and legend box (key).
C
	    IF ( .not. bad ) THEN
	        CALL CGR_POLYINT ( sys, 5, bxlab, bylab, sys, 5, xkey,
     +			           ykey, intxn, ier )
C
 	        IF ( intxn .eq. 1 ) bad = .true.
	    END IF
C
C*	    Check for overlap of new label and a previous label.
C
	    IF ( .not. bad ) THEN
	        DO ii = 1, nlabel
	            CALL CGR_POLYINT ( sys, 5, bxlab, bylab, sys, 5,
     +			          xlab (1,ii), ylab (1,ii), intxn, ier )
 	            IF ( intxn .eq. 1 )  bad = .true.
	        END DO
C
	    END IF
C
C*	    Check for overlap of new label and track line.
C
	    IF ( .not. bad ) THEN
	        xbmin  = AMIN1 ( bxlab ( 1 ), bxlab ( 3 ) )
	        xbmax  = AMAX1 ( bxlab ( 1 ), bxlab ( 3 ) )
	        ybmin  = AMIN1 ( bylab ( 1 ), bylab ( 2 ) )
	        ybmax  = AMAX1 ( bylab ( 1 ), bylab ( 2 ) )
	        ntklns = nknt - 1
	        DO ii = 1, ntklns
		    xt ( 1, ii ) = xx ( ii )
		    xt ( 2, ii ) = xx ( ii + 1 )
		    yt ( 1, ii ) = yy ( ii ) 
		    yt ( 2, ii ) = yy ( ii + 1 )
	        END DO
	        CALL GH_KGIX ( xbmin, xbmax, ybmin, ybmax, bxlab, bylab, 
     +			       ntklns, xt, yt, bad, ier )
C
	    END IF
C
C*	    Check for overlap of new label and a previous 
C*	    label-connecting line.
C
	    IF ( .not. bad ) THEN
	        CALL GH_KGIX ( xbmin, xbmax, ybmin, ybmax, bxlab, bylab, 
     +			       nlabel, xlin, ylin, bad, ier )
C
	    END IF
C
C*	    Check for overlap of new label-connecting line and a
C*	    previous label.
C
	    IF ( .not. bad ) THEN
	        DO ii = 1, nlabel
	            xbmin  = AMIN1 ( xlab ( 1, ii ), xlab ( 3, ii ) )
	            xbmax  = AMAX1 ( xlab ( 1, ii ), xlab ( 3, ii ) )
	            ybmin  = AMIN1 ( ylab ( 1, ii ), ylab ( 2, ii ) )
	            ybmax  = AMAX1 ( ylab ( 1, ii ), ylab ( 2, ii ) )
		    CALL GH_KGIX ( xbmin, xbmax, ybmin, ybmax, 
     +			           xlab ( 1, ii ), ylab ( 1, ii ) ,
     +			           1, xxx, yyy, bad2, ier )
		    IF ( bad2 ) bad = .true.
	        END DO
C
	    END IF
C
C*	    Check for overlap of new label-connecting line and track 
C*	    line.
C
	    IF ( .not. bad ) THEN
	        DO ii = 1, ntklns
		    IF ( ( ( alat ( ipt ) .ne. alat ( ii ) ) .or.
     +		           ( alon ( ipt ) .ne. alon ( ii ) ) ) .and.
     +		         ( ( alat ( ipt ) .ne. alat ( ii + 1 ) ) .or.
     +		           ( alon ( ipt ) .ne. alon ( ii + 1 ) ) ) )THEN
C
C*		        If track line is horizontal or vertical, adjust
C*		        slightly to ensure that CGR_SEGINT works right.
C
		        IF ( ABS ( xt(1,ii) - xt(2,ii) ) .lt. .005 )THEN
		            IF ( xt ( 1, ii ) .lt. xt ( 2, ii ) ) THEN
			        xt ( 1, ii ) = xt ( 1, ii ) - .005
			        xt ( 2, ii ) = xt ( 2, ii ) + .005
	                      ELSE
			        xt ( 1, ii ) = xt ( 1, ii ) + .005
			        xt ( 2, ii ) = xt ( 2, ii ) - .005
		            END IF
		        END IF
		        IF ( ABS ( yt(1,ii) - yt(2,ii) ) .lt. .005 )THEN
		            IF ( yt ( 1, ii ) .lt. yt ( 2, ii ) ) THEN
			        yt ( 1, ii ) = yt ( 1, ii ) - .005
			        yt ( 2, ii ) = yt ( 2, ii ) + .005
		              ELSE
			        yt ( 1, ii ) = yt ( 1, ii ) + .005
			        yt ( 2, ii ) = yt ( 2, ii ) - .005
		            END IF
		         END IF
C
		        CALL CGR_SEGINT ( sys, xxx, yyy, sys, xt (1,ii),
     +			        yt (1,ii), sys, xint, yint, intxn, ier ) 
		        IF ( intxn .eq. 1 ) bad = .true.
		    END IF
	        END DO
C
	    END IF
C
C*	    Check for overlap of new label-connecting line and a 
C*	    previous label-connecting line.
C
	    IF ( .not. bad ) THEN
	        DO ii = 1, nlabel
		    CALL CGR_SEGINT ( sys, xxx, yyy, sys, xlin ( 1,ii ),
     +			    ylin ( 1,ii ), sys, xint, yint, intxn, ier ) 
		    IF ( intxn .eq. 1 ) bad = .true.
	        END DO
C
	    END IF
C
	    IF ( ( .not. bad ) .or.  ( ipos .eq. 5 ) ) good = .true.
 	    IF ( good ) THEN
                CALL ST_LSTR ( daylab, lend, ier )
                CALL GSTEXT ( 0, 0, 0., 0, 0, 0, just, ier )
C
C*              Plot the label for the current location.
C
                CALL GTEXT ( 'M', blat, blon, daylab (:lend),
     +                       0.0, ixoff, iyoff, ier )
                slat (1) = alat (ipt)
                slon (1) = alon (ipt)
                slat (2) = blat
                slon (2) = blon
C
C*              Draw the label-connecting line.
C
                CALL GLINE ( 'M', 2, slat, slon, ier )
	      ELSE
		ipos = ipos + 1
		IF ( ipos .gt. nposmx ) THEN
		   good = .true.
		   iret = -1
		  ELSE
		    IF ( ipos .eq. 5 ) THEN
			IF ( nknt .gt. 1 ) THEN
			    CALL CLO_DIRECT ( alat (nknt), alon (nknt),
     +				     alat (nknt - 1), alon (nknt - 1),
     +				     direct, ier )
			END IF
		      ELSE IF ( flip ) THEN
			direct = idir + 180.
		    END IF
		    IF ( direct .ge. 360. ) direct = direct - 360.
		END IF
 	    END IF
	END DO    
C
C*	Save the label envelope and label-connecting line for later 
C*	checks.
C
	IF ( iret .eq. 0 ) THEN
	    IF ( nlabel .lt. 5 ) THEN
		nlabel = nlabel + 1
		DO ii = 1, 5
		    xlab ( ii, nlabel ) = bxlab ( ii )
		    ylab ( ii, nlabel ) = bylab ( ii )
		    IF ( ii .le. 2 ) THEN
			xlin ( ii, nlabel ) = xxx ( ii ) 
			ylin ( ii, nlabel ) = yyy ( ii )
		    END IF
		END DO
	    END IF
	END IF
C*
	RETURN
	END
