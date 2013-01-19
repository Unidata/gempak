	SUBROUTINE POLY_MARGIN ( iret )
C************************************************************************
C* POLYT_MARGIN	  							*
C*									*
C* This subroutine gets the outer boundaries of warning areas.		*
C*									*
C* POLY_MARGIN ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	LOGICAL		done
C-----------------------------------------------------------------------
	iret = 0
C
C*	Find the "rectangular" which includes all the grids.		
C
	DO nc = 1, NCATEG
C
	    DO ng = 1, ngroup ( nc )
C
		done = .false.	
		np = 1
		DO WHILE ( .not. done )
		    kx = kgridu ( np, ng, nc )
		    ky = kgridv ( np, ng, nc )
		    IF ( np .eq. 1 )  THEN
			DO nn = 1, 2
			    imargn ( nn, ng, nc ) = kx
			    jmargn ( nn, ng, nc ) = ky
			END DO
		      ELSE
C
C*			Save the extreme values in x-direction.
C
			IF ( kx .lt. imargn ( 1, ng, nc ) ) THEN
			     imargn ( 1, ng, nc) = kx
			  ELSE IF ( kx .gt. imargn ( 2, ng, nc ) ) THEN
			    imargn ( 2, ng, nc) =  kx
			END IF	
C
			IF ( ky .lt. jmargn ( 1, ng, nc ) ) THEN
			     jmargn ( 1, ng, nc ) = ky
			  ELSE IF ( ky .gt. jmargn ( 2, ng, nc ) ) THEN
			    jmargn ( 2, ng, nc) =  ky
			END IF	
		    END IF
		    np = np + 1
		    IF ( np .gt. npoint ( ng, nc ) ) done = .true.
		END DO	
C
	    END DO
	END DO
C*
	RETURN
	END
