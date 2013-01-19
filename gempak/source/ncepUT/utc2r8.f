	SUBROUTINE UT_C2R8 ( cdata, lens, r8aray, nr8, iret )
C***********************************************************************
C* UT_C2R8						               *
C*								       *
C* Convert character string to real*8 array			       *
C*								       *
C* UT_C2R8 ( cdata, lens, r8aray, nr8, iret )			       *
C*								       *
C* Input parameters:						       *
C*	cdata		character	string to be converted	       *
C*	lens		integer		length of string	       *
C*								       *
C* Output parameters:						       *
C*	r8aray 		real * 8	array to hold character string *
C*	nr8		integer		number of elements in array    *
C*	iret		integer		return code		       *
C*					    = 0 - successful convert   *
C*					    = -1 - null char. string   *
C*								       *
C**                                                                    *
C*	Log:							       *
C*	Kidwell/NCEP	09/95   Original author		               *
C*	Kidwell/NCEP	02/96   Changed name from BJ_ to IF_,          *
C*	          	           added check for lens .lt. 8         *
C*	J.Ator/NP12	07/96	IF_C2R8	-> UT_C2R8		       *
C***********************************************************************
	CHARACTER * ( * ) cdata
	CHARACTER * 8 cval
C*
	REAL * 8 r8aray ( * )
	REAL * 8 rval
C*
	EQUIVALENCE ( cval, rval )
C*----------------------------------------------------------------------
	iret = 0
C
	IF ( lens .gt. 0 ) THEN
	    ib = 1
	    ie = 8
	    IF ( ie .gt. lens ) ie = lens
	    nr8 = 0
	    DO WHILE ( ib .le. lens )
		cval = cdata ( ib:ie )
		nr8 = nr8 + 1
		r8aray ( nr8 ) = rval
		ib = ib + 8
		ie = ie + 8
		IF ( ie .gt. lens ) ie = lens
	    END DO
	ELSE
	    iret = -1
	END IF
C
	RETURN
	END
