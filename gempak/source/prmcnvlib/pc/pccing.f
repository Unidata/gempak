	SUBROUTINE PC_CING  ( datain, level1, level2, pres, nparm, 
     +			      outdta, iret )
C************************************************************************
C* PC_CING								*
C*									*
C* This subroutine computes parameters which must be integrated from	*
C* the surface.  These parameters include: MHGT, DHGT, PWTH, PSYM.	*
C*									*
C* PC_CING  ( DATAIN, LEVEL1, LEVEL2, PRES, NPARM, OUTDTA, IRET )	*
C*									*
C* Input parameters:							*
C*	DATAIN (*)	REAL		Station data			*
C*	LEVEL1		INTEGER		Lower level in data set		*
C*	LEVEL2		INTEGER		Upper level in data set		*
C*	PRES		REAL		Pressure (between lev1,2)	*
C*	NPARM		INTEGER		Unused parameter		*
C*									*
C* Output parameters:							*
C*	OUTDTA (*)	REAL		Data at level			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up for GEMPAK4			*
C* G. Huffman/USRA	 9/89	Insert U, V in indices 7, 8		*
C* M. desJardins/GSFC	 3/91	Eliminate computation below surface	*
C* S. Jacobs/NMC	 6/94	Initialized knt to 0			*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdta (*)
C*
	LOGICAL		twolev
	LOGICAL		intflg (8), angflg (8)
	REAL		data1 (MAXVRT), data2 (MAXVRT), data (MAXVRT)
C
C*	NOTE:  The values of CP and G are divided by 100. in order
C*	       to make the Montgomery Stream Function (PSYM) smaller
C*	       than 10**6.
C
	PARAMETER	( CP = RDGAS / RKAPPA / 100. )
	PARAMETER	( G = GRAVTY / 100. )
	INCLUDE		'ERMISS.FNC'
	DATA		intflg / 8*.true. /, angflg / 8*.false. /
C*
C------------------------------------------------------------------------
	iret = 0
	knt  = 0
C
C*	Check that there are parameters to be computed.
C
	IF  ( king .le. 0 )  RETURN
	IF  ( .not. vtbflg )  CALL PC_MVRT  ( datain, ier )
C
C*	Get minimum and maximum levels and check whether there are two
C*	levels input.
C
	maxl = MAX ( level1, level2 )
	minl = MIN ( level1, level2 )
	IF  ( minl .le. 0 )  THEN
	    twolev = .false.
	  ELSE
	    twolev = .true.
	END IF
C
C*	Put surface level integrated values in table if necessary.
C
	IF  ( kpnt .le. 0 )  THEN
	    CALL PC_SFCZ  ( datain, height, ier )
	    IF  ( qing (9)  ) vdata (9,1)  = height
	    IF  ( qing (10) ) vdata (10,1) = height
	    IF  ( qing (11) ) vdata (11,1) = 0.0
	    IF  ( qing (12) ) THEN
		IF  ( ERMISS ( vdata ( 5, 1 ) ) )  THEN
		    vdata ( 12, 1 ) = RMISSD
		  ELSE
		    vdata ( 12, 1 ) = CP * PR_TMCK ( vdata (5,1) )
     +				      + G * height
		END IF
	    END IF
	    kpnt = 1
	END IF
C
C*	Fill in integrated table to maxl.
C
	IF  ( kpnt .lt. maxl )  THEN
	    hdb = vdata   (  9, kpnt )
	    hmb = vdata   ( 10, kpnt )
	    pwb = vdata   ( 11, kpnt )
	    psb = vdata   ( 12, kpnt )
	    pb  = vdata   (  1, kpnt )
	    tb  = vdata   (  5, kpnt )
	    tkb = PR_TMCK (  tb )
	    tdb = vdata   (  6, kpnt )
	    hb  = vdata   (  3, kpnt )
	    knt = 1
C
	    DO  i = kpnt+1, maxl
	      IF  ( vdata (1,i) .le. vdata (1,1) )  THEN
		pt   = vdata ( 1, i )
		tt   = vdata ( 5, i )
		tkt  = PR_TMCK ( tt )
		tdt  = vdata ( 6, i )
		ht   = vdata ( 3, i )
		delh = ht - hb
		IF  (  qing (9) )  THEN
		    vdata (9,i) = PC_DHGT ( tkb,tkt,pb,pt,hdb,delh )
		  hdb  =  vdata ( 9, i )
		END IF
		IF  ( qing (10) )  THEN
		    vdata (10,i) = PC_MHGT ( tb,tt,tdb,tdt,pb,pt,hmb )
		    hmb  =  vdata ( 10, i )
		END IF
		IF  ( qing (11) )  THEN
		    vdata (11,i) = PC_PWTR (tdb,tdt,pb,pt,pwb)
		    pwb  =  vdata ( 11, i )
		END IF
		IF  ( qing (12) )  THEN
		    vdata (12,i) = PC_PSYM (tkb,tkt,delh,psb)
		    psb  =  vdata ( 12, i )
		END IF
C*
		pb = pt
		tb = tt
		tkb = tkt
		tdb = tdt
		hb  = ht
		knt = i
	       ELSE
		DO  j = 9, 12
		    vdata (j,i) = RMISSD
		END DO
	      END IF
	    END DO
	END IF
	kpnt = knt 
C
C*	Interpolate data to correct level if necessary.
C
	IF  ( twolev )  THEN
	    CALL PC_GLEV  ( level1, vdata, MAXVRT, data1, ier )
	    CALL PC_GLEV  ( level2, vdata, MAXVRT, data2, ier )
	    CALL PC_INTP  ( pres, data2, data1, 8, intflg, angflg, 
     +						data, ier )
	    pt   = data (1)
	    tt   = data (5)
	    tkt  = PR_TMCK ( tt )
	    ht   = data (3)
	    tdt  = data (6)
	    pb   = vdata ( 1, minl )
	    tb   = vdata ( 5, minl )
	    tkb  = PR_TMCK ( tb )
	    hb   = vdata ( 3, minl )
	    delh = ht - hb
	    tdt  = vdata ( 6, minl )
	    hdb  = vdata ( 9, minl )
	    hmb  = vdata ( 10, minl )
	    pwb  = vdata ( 11, minl )
	    psb  = vdata ( 12, minl )
	    IF  ( qing (9) ) 
     +		outdta (kinpos(9)) =PC_DHGT (tkb,tkt,pb,pt,hdb,delh)
	    IF  ( qing (10) )
     +		outdta (kinpos(10))=PC_MHGT (tb,tt,tdb,tdt,pb,pt,hmb)
	    IF  ( qing (11) ) 
     +		outdta (kinpos(11))=PC_PWTR (tdb,tdt,pb,pt,pwb)
	    IF  ( qing (12) ) 
     +		outdta (kinpos(12))=PC_PSYM (tkb,tkt,delh,psb)
	  ELSE
	    DO  i = 9, 12
		IF ( qing (i) ) outdta ( kinpos (i) ) = vdata (i,maxl)
	    END DO
	END IF
C*
	RETURN
	END
