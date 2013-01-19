	SUBROUTINE G2T_SQUAD ( iret )
C************************************************************************
C* G2T_SQUAD								*
C*									*
C* This subroutine sets quadrant info to the common.  The quad index	*
C* is from the G2T_ZONE.TBL.  For most four quad subzones, the first 	*
C* is NE quad, and the 2nd/3rd/4th quads are SE/SW/NW zones.  The quad	*
C* info is output in the EXCEPTION part of text.			*
C*									*
C* G2T_SQUAD ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = cannot find the subzone	*
C**									*
C* Log:									*
C* T. Lee/SAIC		 1/07	Created					*
C* T. Lee/SAIC		 7/07	Updated for multiple zones		*
C* T. Lee/SAIC		11/07	Updated for TPC zones			*
C* T. Lee/SAIC		12/07	Updated for '_0_' PORTION flag (porflg)	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, numzon
	  found = .false.
	  DO jj = 1, nsubzn ( ii )
C
C*	    Blank for entire zone.
C
	    IF  ( .not. found .and. 
     +	        ( INDEX ( bndtag (ii,jj), '_ENTIRE' ) ) .ne. 0 )  THEN
		found = .true.
		squad ( ii, jj ) = ' '
C
C*	      First quadrant (except of S_OF_NE to avoid NE error).
C
		
	      ELSE IF ( INDEX ( bndtag (ii,jj), '_100_70W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), 'MAINE_N' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), 'MAINE_S' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_EOF120' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_NCNTRL' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_CNTRL' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_68_70W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_31N_E' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_31N_W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_E_68W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_E_70W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_W_70W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_W_1000' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_NESA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_SESA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_SWSA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_CSA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_ESA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_NSA' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_NE' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_SE' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_SW' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_NW' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_E' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_S' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_W' ) .ne. 0 .or.
     +			INDEX ( bndtag (ii,jj), '_N' ) .ne. 0 )  THEN
C
C*		Set 'OVER' flags.
C
		IF ( INDEX ( bndtag (ii,jj), '_0_' ) .ne. 0 )  THEN
		    porflg ( ii, jj ) = .false.
		  ELSE
		    porflg ( ii, jj ) = .true.
		END IF
C
C*		Set ID names.
C
		IF ( INDEX ( bndtag (ii,jj), '_100_70W' ) .ne. 0 )  THEN
		    squad ( ii, jj ) = 'FROM 100NM OF THE COAST TO 70W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), 'MAINE_N' ) .ne. 0 )
     +			    THEN
		    squad ( ii, jj ) = 'NORTH'
		  ELSE IF ( INDEX ( bndtag (ii,jj), 'MAINE_S' ) .ne. 0 )
     +			    THEN
		    squad ( ii, jj ) = 'SOUTH'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_EOF120' )
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST OF 120W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_NCNTRL' )
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NORTH CENTRAL'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_CNTRL' )
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'CENTRAL'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_68_70W' )
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'BETWEEN 68W AND 70W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_31N_E' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_31N_W' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'WEST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_E_68W' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST OF 68W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_E_70W' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST OF 70W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_W_70W' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'WEST OF 70W'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_W_1000' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'WEST OF 100NM OF THE COAST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_NESA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NE'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_SESA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'SE'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_SWSA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'SW'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_CSA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'CENTRAL'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_ESA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_NSA' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NORTH'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_NE' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NE'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_SE' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'SE'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_SW' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'SW'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_NW' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NW'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_E' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'EAST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_W' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'WEST'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_S' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'SOUTH'
		  ELSE IF ( INDEX ( bndtag (ii,jj), '_N' ) 
     +				.ne. 0 )  THEN
		    squad ( ii, jj ) = 'NORTH'
		  ELSE
		    iret = -6
		END IF
	      ELSE
		iret = -6
	    END IF
	  END DO
	END DO
C
	RETURN 
	END
