	SUBROUTINE PC_COMP  ( index, datin, datout, iret )
C************************************************************************
C* PC_COMP								*
C*									*
C* This subroutine computes parameters for a single vertical level	*
C* using the tables stored by PC_METH.  Errors should not occur in	*
C* this subroutine unless it is called out of order.			*
C*									*
C* PC_COMP  ( INDEX, DATIN, DATOUT, IRET )				*
C*									*
C* Input parameters:							*
C*	INDEX		INTEGER		Index to use for conversion	*
C*	DATIN (NPARM)	REAL		Input data for single level	*
C*									*
C* Output parameters:							*
C*	DATOUT (NOUTPM)	REAL		Output data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-22 = invalid index		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 8/90	Add capability to get input in subs	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datin (*), datout (*)
C*
	REAL		dattmp ( MAXTMP )
	LOGICAL		setlev
	INTEGER		locout ( MAXTMP )
C------------------------------------------------------------------------
C*	Check for valid index.
C
	IF  ( ( index .lt. 1 ) .or. ( index .gt. MAXTBL ) .or. 
     +	      ( .not. tabflg (index) ) )  THEN
	    iret = -22
	    RETURN
	ENDIF
C
C*	Check for valid numbers of input and output parameters.
C*	This error should not occur unless this subroutine is called
C*	out of order.
C
	nin  = kinpm ( index )
	nout = koutpm ( index )
	IF  ( ( nin .eq. 0 ) .or. ( nout .eq. 0 ) )  THEN
	    iret = -22
	    RETURN
	  ELSE
	    iret = 0
	ENDIF
C
C*	Move the input data to the temporary array.
C
	DO  i = 1, MAXTMP
	    IF  ( i .le. nin )  THEN
		dattmp (i) = datin (i)
	      ELSE
		dattmp (i) = RMISSD
	    ENDIF
	ENDDO
C
C*	Use PC_FUNC to compute intermediate and output data in temp array.
C
	IF  ( kfunc (index) .gt. 0 )  THEN
	    IF  ( ( index .eq. 1 ) .or. ( index .eq. 3 ) )  THEN
		setlev = .true.
		DO  i = 1, MAXTMP
		    locout (i) = 0
		END DO
		DO  i = 1, nout
		    IF  ( kans ( i, index ) .gt. 0 )  THEN
			DO  j = 1, kfunc ( index )
			    IF  ( koutfn ( j, index ) .eq.
     +				  kans ( i, index ) )  THEN
				locout (j) = i
			    END IF
			END DO
		    END IF
		END DO
	      ELSE
		setlev = .false.
	    END IF
	    CALL PC_FUNC  ( kfunc ( index ), kfuncn ( 1, index ),
     +			    kposno ( 1, 1, index ), MAXPPF,
     +			    koutfn ( 1, index ), setlev, locout,
     +			    dattmp, iret )
	END IF
C
C*	Move data from temporary array to output array.
C*	If not computable, do not change data.
C
	DO  i = 1, nout
	    IF  ( kans ( i, index ) .gt. 0 )   THEN
		datout (i) = dattmp ( kans ( i, index ) )
	    END IF
	ENDDO
C*
	RETURN
	END
