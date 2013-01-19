	SUBROUTINE GGEDGE ( edge, iret )
C************************************************************************
C* GGEDGE                                                               *
C*                                                                      *
C* This subroutine parse the contents of the edgeopts string and place  *
C* the information in the common /GGEDGE/ for use in G2G program.       *
C*                                                                      *
C* GGEDGE ( EDGE, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*      EDGE            CHARACTER      edgeopts input string      	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC		04/07	Created					*
C* M. Li/SAIC		04/07	parse bvalue, ddist, and spec		*
C* D.W.Plummer/NCEP	05/07	Add GEMPRM.PRM				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
	CHARACTER*(*)	edge	
C
	INTEGER		lens, ier
	REAL		rlst(2)
	CHARACTER       carr (4)*40
C------------------------------------------------------------------------
	iret = 0 
C
   	CALL ST_NULL ( edge, edge, lens, ier ) 	
C
C*	Retrieve edge option flag 'visib'
C
	visib = .false.
        IF (edge(1:1) .eq. 'T' .or. edge(1:1) .eq. 't') visib = .true.
C
C*	Retrieve valueN and ddistN
C	
	bvalue(1) = RMISSD
	bvalue(2) = RMISSD
	ddist(1)   = IMISSD
	ddist(2)   = IMISSD
	spec      = ' '

        CALL ST_CLST ( edge, '/', ' ', 4, carr, num, ier )
	CALL ST_NULL ( carr(2), carr(2), lens, ier )
	IF ( lens .gt. 0 ) THEN 
	    CALL ST_RLST ( carr(2), ';', -1., 2, rlst, inum, ier )
	    bvalue(1) = rlst(1)
	    ddist(1)  = INT ( rlst(2) )
	END IF
C
	CALL ST_NULL ( carr(3), carr(3), lens, ier )
        IF ( lens .gt. 0 ) THEN
            CALL ST_RLST ( carr(3), ';', -1., 2, rlst, inum, ier )
            bvalue(2) = rlst(1)
            ddist(2)  = INT ( rlst(2) )
        END IF
C
	CALL ST_NULL ( carr(4), carr(4), lens, ier )
	spec = carr(4)(1:1)
	CALL ST_UCLC ( spec, spec, ier )
C*
	RETURN
	END
