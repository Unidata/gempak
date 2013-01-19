	SUBROUTINE PC_CMDT  ( idx1, idx2, idx3, vlev, ivcord, datain,
     +			      outdat, iret )
C************************************************************************
C* PC_CMDT								*
C*									*
C* This subroutine computes data values at a given vertical level.	*
C* If the vertical level is not in the dataset, the data will be	*
C* interpolated provided the proper interpolation flags are set.	*
C*									*
C* PC_CMDT ( IDX1, IDX2, IDX3, VLEV, IVCORD, DATAIN, OUTDAT, IRET )	*
C*									*
C* Input parameters:							*
C*	IDX1		INTEGER		Index for direct comp		*
C*	IDX2		INTEGER		Index for interm. interp	*
C*	IDX3		INTEGER		Index for interm. to output	*
C*	VLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER		Coordinate type of VLEV		*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	OUTDAT (NOUTPM)	REAL		Computed real data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT not called	*
C*					 -6 = PC_SSTN not called	*
C*					 -7 = PC_DFLV not called	*
C*					-10 = only sfc data ivert=0	*
C*					-13 = interpolation error	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/USRA	 8/89	Enabled last section only for IDX1 = 1	*
C* M. desJardins/GSFC	 7/90	Added layer parameters; add iret = -13	*
C* M. desJardins/GSFC	 3/91	Added interpolation using hght		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* D. Kidwell/NCEP	 5/99	PC_DSET->PC_INIT in prolog; check inton *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
C*
	REAL		dataup (MAXPRM), datadn (MAXPRM),
     +			updata (MAXPRM), dndata (MAXPRM)
	LOGICAL		botup
C*
	DATA		botup, eps / .true., .001 /
C-------------------------------------------------------------------------
C*	Check for input error.
C
	IF  ( ( jcord .eq. 0 ) .and. ( vlev .ne. 0. ) )  THEN
	    iret = -10
	    RETURN
	  ELSE
	    iret = 0
	ENDIF
C
C*	Check for valid coordinate system.
C
	IF  ( ( vlev .ne. 0.0 ) .and.
     +	      ( ( ivcord .lt. 1 ) .or. ( ivcord .gt. 3 ) .or.
     +		( .not. vcomp (ivcord) ) ) )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Set jhght for interpolation with respect to hght.
C
	IF  ( idx2 .eq. 2 )  THEN
	    jhght = jhght2
	  ELSE IF  ( idx2 .eq. 6 )  THEN
	    jhght = jhght6
	  ELSE
	    jhght = 0
	END IF
C
C*	Set variables correctly for surface data
C
	IF  ( vlev .eq. 0. )  THEN
	    CALL PC_FNDL  ( datain, jdsprm, jnumlv, icol, jsfflg,
     +			    vlev, botup, eps, ivcord, level1, level2,
     +			    itype, iret )
C
C*	  Find the required level in the dataset.
C
	  ELSE IF  ( ivcord .eq. jcord )  THEN
	    icol = 1
	    CALL PC_FNDL  ( datain, jdsprm, jnumlv, icol, jsfflg, vlev,
     +			    botup, eps, ivcord, level1, level2, itype,
     +			    iret )
C
C*	  Find level if changing vertical coordinates.
C
	  ELSE
	    IF  ( .not. vtbflg )  CALL PC_MVRT  ( datain, ier )
	    icol = ivcord
	    CALL PC_FNDL  ( vdata, MAXVRT, jnumlv, icol, jsfflg, vlev,
     +			    botup, eps, ivcord, level1, level2, itype,
     +			    iret )
	END IF
C
C*	Get the data for the correct level if found.
C*	If interpolation required, get data for both levels.
C
	IF  ( itype .eq. 1 )  THEN
	    CALL PC_GLEV  ( level1, datain, jdsprm, dataup, ier )
	    index = idx1
	    CALL PC_COMP  ( index, dataup, outdat, ier )
	  ELSE IF  ( ( itype .eq. 2 ) .and. inton )  THEN
C
C*	    Currently we can interpolate if ivcord is pres, z or thta.
C
	    IF  ( ( ivcord .ge. 1 ) .and. ( ivcord .le. 3 ) )  THEN
	        index = idx2
	        CALL PC_GLEV  ( level1, datain, jdsprm, dataup, ier )
	        CALL PC_COMP  ( index, dataup, updata, ier )
	        CALL PC_GLEV  ( level2, datain, jdsprm, datadn, ier )
		CALL PC_COMP  ( index, datadn, dndata, ier )
C
C*	        Do interpolation with respect to p.
C
	        IF  ( ivcord .eq. 1 )  THEN
		    pres = vlev
	          ELSE IF  ( ivcord .eq. 2 )  THEN
		    IF  ( .not. vtbflg )  CALL PC_MVRT ( datain, ier )
		    CALL PC_PVAL  ( vlev, level1, level2, pres, ier )
		  ELSE IF  ( ivcord .eq. 3 )  THEN
		    IF  ( .not. vtbflg )  CALL PC_MVRT  ( datain, ier )
		    CALL PC_PHGT  ( vlev, level1, level2, pres, ier )
	        END IF
		IF  ( ( .not. vcomp (1) ) .and. vcomp (3) .and.
     +		      ( ivcord .eq. 3 ) .and. ( jhght .gt. 0 ) )  THEN
		    CALL PC_INTH  ( vlev, updata, dndata,
     +				    koutpm (index), qint (1,index),
     +				    qang (1,index), jhght, dataup, ier )
		  ELSE
		    CALL PC_INTP  ( pres, updata, dndata,
     +				    koutpm(index), qint (1,index),
     +				    qang (1,index), dataup, ier )
		END IF
	        index = idx3
	        CALL PC_COMP  ( index, dataup, outdat, ier )
	    END IF
C
C*	    Return error if out of bounds.
C
	  ELSE IF ( itype .ne. 2 ) THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Compute integrated parameters if there are any.
C
	IF  ( ( king .ne. 0 ) .and. ( idx1 .eq. 1 ) )  THEN
	    IF  ( ( itype .eq. 1 ) .or. ( itype .eq. 2 ) )  THEN
		CALL PC_CING  ( datain, level1, level2, pres, jdsprm,
     +				outdat, ier )
	      ELSE
		DO  i = 9,12
		  IF  ( qing(i) ) outdat ( kinpos (i) ) = RMISSD
		END DO
	    END IF
	END IF
C
C*	Compute layer parameters if there are any.
C
	IF  ( ( klayr .ne. 0 ) .and. ( idx1 .eq. 1 ) )  THEN
	    IF  ( ( itype .eq. 1 ) .or. ( itype .eq. 2 ) )  THEN
		CALL PC_CLYR  ( datain, level1, level2, pres, vlev,
     +				ivcord, outdat, ier )
	    END IF
	END IF
C*
	RETURN
	END
