	SUBROUTINE GQINTR ( ii, jj, intr, iret )
C************************************************************************
C* GQINTR                                                               *
C*                                                                      *
C* This subroutine determines whether an intersection has been crossed	*
C* when going from (ii-1) to ii along row jj (direction 2).		*
C*                                                                      *
C* GQINTR ( II, JJ, INTR, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      II              INTEGER         I (column) grid location	*
C*      JJ              INTEGER         J (row) grid location		*
C*                                                                      *
C* Output parameters:                                                   *
C*      INTR		LOGICAL		Intersection flag		*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* W.D.Plummer/NCEP     12/02                                           *
C* D.W.Plummer/NCEP     07/03	Chgs for intersection efficiency	*
C* D.W.Plummer/NCEP     09/03	Add parameters as indices to arrays	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'grphgd.cmn'
C
	LOGICAL		found
C------------------------------------------------------------------------
C
	found = .false.
	nn = intptrs(jj,2,STPTR)
	DO WHILE ( nn .lt. intptrs(jj,2,NINTS)+intptrs(jj,2,STPTR) .and. 
     &   	.not. found )
C
	    found = intsct(nn,INT_X) .ge. ii-1 .and. 
     +		    intsct(nn,INT_X) .le. ii  
C
	    nn = nn + 1
C
	END DO
C
	iret = 0
C
	RETURN
	END 
