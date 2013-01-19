	SUBROUTINE POLY_APAD ( iret )
C************************************************************************
C* POLYT_APAD								*
C*									*
C* This subroutine adds one extra grid padding to the warning grids.	*
C*									*
C* POLY_APAD ( iret )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	PARAMETER	( EPS = .001 )
	LOGICAL		fpoly ( igxd, igyd, maxgrp, ncateg )
	LOGICAL		done, bfund, tfund
C-----------------------------------------------------------------------
	iret = 0
C
C*	Initialize warning flags.
C
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		DO jj = 1, igyd
		    DO ii = 1, igxd
			fpoly ( ii, jj, ng, nc ) = .false.
		    END DO
		END DO
	    END DO
	END DO
C
C*	Fill warning flags.
C
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		np = 1
		done = .false.
		DO WHILE ( .not. done )
		    kx = kgridu ( np, ng, nc )
		    ky = kgridv ( np, ng, nc )
		    fpoly ( kx, ky, ng, nc ) = .true.
		    np = np + 1
		    IF ( np .gt. npoint ( ng, nc ) ) done = .true.
		END DO
	    END DO
	END DO
C
C*	Add padding to the warning areas.
C
	DO nc = 1, NCATEG
	    DO ng = 1, ngroup ( nc )
		jb = jmargn ( 1, ng, nc )
		jt = jmargn ( 2, ng, nc )
		np = 0
		DO jj = jb, jt
		    il = imargn ( 1, ng, nc )
		    ir = imargn ( 2, ng, nc )
		    nprow = 0
		    DO ii = il, ir
			IF ( fpoly ( ii, jj, ng, nc ) )  THEN
C
C*			    First point of the row.
C
			    IF ( nprow .eq. 0 )  THEN
				CALL POLY_PAD ( 0, ii, jj, ng, nc, np, 
     +						ier )
				is = ii
				js = jj
C
				CALL POLY_PAD ( 2, ii, jj, ng, nc, np, 
     +						ier )
				CALL POLY_PAD ( 3, ii, jj, ng, nc, np, 
     +						ier )
				CALL POLY_PAD ( 4, ii, jj, ng, nc, np, 
     +						ier )
C
				nprow = nprow + 1
			      ELSE
C
C*				Not the first point in a row.
C
				bfund = .false.
				tfund = .false.
				DO ky = jb, jj - 1
				    IF ( fpoly ( ii, ky, ng, nc ) )
     +					 bfund = .true.
				END DO
C
				DO ky = jj + 1, jt
				    IF ( fpoly ( ii, ky, ng, nc ) )
     +					tfund = .true.
				END DO
C
C*				Mid-point. Add to the polygon and
C*				make it rightmost point for now.
C
				IF ( bfund .and. tfund )  THEN
      				    CALL POLY_PAD ( 0, ii, jj, ng, nc,
     +						    np, ier )
				    is = ii
				    js = jj
C
C*				  Only one point in the column.
C
				  ELSE IF ( .not. bfund .and.
     +					    .not. tfund )  THEN
C
C*				    For top/bottom boundary point, add 
C*				    point below/above it.
C
      				    CALL POLY_PAD ( 0, ii, jj, ng, nc,
     +						    np,ier )
				    is = ii
				    js = jj
				    IF ( jj.eq.1 .or. jj.eq.igyd ) THEN
					IF ( jj .eq. 1 )  THEN
      					  CALL POLY_PAD ( 2, ii, jj, ng,
     +							  nc, np,ier)
				        ELSE
      					  CALL POLY_PAD ( 4, ii, jj, ng,
     +							  nc, np, ier)
					END IF
				      ELSE
C
C*					For a non top/bottom boundary 
C*					point, add a point above AND 
C*					below.
C
      					CALL POLY_PAD ( 2, ii, jj, ng,
     +							nc, np,ier )
      					CALL POLY_PAD ( 4, ii, jj, ng,
     +							nc, np, ier )
				    END IF
				  ELSE
C
C*				    For boundary points in a column,
C*				    add one point to at the top or
C*				    bottom of the column.

      				    CALL POLY_PAD ( 0, ii, jj, ng, nc,
     +						    np, ier )
				    is = ii
				    js = jj
				    IF ( bfund )  THEN
      					CALL POLY_PAD ( 2, ii, jj, ng,
     +							nc, np, ier )
				      ELSE IF ( tfund )  THEN
      					CALL POLY_PAD ( 4, ii, jj, ng,
     +							nc, np, ier )
				    END IF
				END IF
			    END IF
			  ELSE
C
C*			    For a sub-category point, add point(s) if
C*			    it it is next to a point which reaches
C*			    warning categories.
C
			    IF ( ii .gt. 1 .and. ii .lt. igxd )  THEN
				IF ( fpoly ( ii-1, jj, ng, nc ) .or.
     +				   ( fpoly ( ii+1, jj, ng, nc ) ) ) THEN
				    CALL POLY_PAD ( 0, ii, jj, ng, nc,
     +						    np, ier )
				    ncol = 0
				    DO kk = 1, np
					IF (ii.eq.polygi(kk,ng,nc)) THEN
					    ncol = ncol + 1
					END IF
				    END DO

				    IF ( ncol .eq. 1 )  THEN
					IF ( jj .eq. igyd ) THEN
				   	    CALL POLY_PAD ( 0, ii, jj-1,
     +						       ng, nc, np, ier )
					   ELSE 
				    	    CALL POLY_PAD ( 0, ii, jj+1,
     +						       ng, nc, np, ier )
					END IF
				    END IF
				END IF
			    END IF
			END IF
		    END DO
C
C*		    Add padding at the end of the row.
C
		    IF ( is .lt. igxd )  THEN
      		      CALL POLY_PAD ( 1, is, js, ng, nc, np, ier )
		    END IF 
		    numpts ( ng, nc ) = np
		END DO
	    END DO
	END DO
C*
	RETURN
	END
