#ifdef WITHPYTHON /* Is Python available? */
#include "Python.h"
#endif

#include "da.h"

void da_runpy ( char *pyfile, char *pymeth, int *iret )
/************************************************************************
 * da_runpy								*
 *									*
 * This function runs the given Python method from the given Python	*
 * file. 								*
 * 									*
 * Sample code used in creating this function, and a detailed		*
 * explanation of calling Python from C may be found at:		*
 * https://docs.python.org/2.7/extending/extending.html                 *
 *    #calling-python-functions-from-c                                  *
 * 									*
 * da_runpy ( pyfile, pymeth, iret )					*
 * 									*
 * Input parameters:							*
 *	*pyfile		char	Python file name			*
 *	*pymeth		char	Python method name			*
 * 									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 5/13	Initial coding				*
 ***********************************************************************/
{
#ifdef WITHPYTHON /* Is Python available? */
    int		ii;
    PyObject	*pName, *pModule, *pFunc, *pArgs, *pValue;
    char	*lclchr;
    Py_ssize_t	size;

/*---------------------------------------------------------------------*/

    *iret = 0;


    /* Initialize the Python interface */
    Py_Initialize();

    /* Make a Python object from the script file name */
    pName = PyString_FromString ( pyfile );
    if ( pName == NULL ) {
	PyErr_Print();
	return;
    }

    /* Import the Python file to an object */
    pModule = PyImport_Import ( pName );
    if ( pModule == NULL ) {
	PyErr_Print();
	return;
    }

    /* Deallocate the file name object since it is no longer needed */
    Py_DECREF ( pName );

    if ( pModule != NULL ) {
	/* Get the requested method from the file */
	pFunc = PyObject_GetAttrString ( pModule, pymeth );

	/* Check that the function is usable */
	if ( pFunc && PyCallable_Check ( pFunc ) ) { 

	    /*
	     * Start a new list of arguments for the script.
	     * There is only a single string argument needed.
	     */
	    pArgs = PyTuple_New ( danarg );
	    for ( ii = 0; ii < danarg; ii++ ) {
		pValue = PyString_FromString ( daargs[ii] );
		if ( !pValue ) {
		    Py_DECREF ( pArgs );
		    Py_DECREF ( pModule );
		    *iret = -3;
		    return;
		}
		/* Add the string to the argument list */
		PyTuple_SetItem ( pArgs, ii, pValue );
	    }

	    /* Call the Python script */
	    pValue = PyObject_CallObject ( pFunc, pArgs );
	    Py_DECREF ( pArgs );
	    if ( pValue != NULL ) {
		/* Assign the appropriate output variable */
		switch ( datype ) {
		    case DAINT:
			danumi = PyList_Size(pValue);
			daouti = (int *)malloc(danumi*sizeof(int));
			for ( ii = 0; ii < danumi; ii++ ) {
			    daouti[ii] = (int)PyInt_AsLong(
				    		PyList_GetItem(pValue,ii));
			}
			Py_DECREF ( pValue );
			break;

		    case DAFLOAT:
			danumf = PyList_Size(pValue);
			daoutf = (float *)malloc(danumf*sizeof(float));
			for ( ii = 0; ii < danumf; ii++ ) {
			    daoutf[ii] = (float)PyFloat_AsDouble(
				    		PyList_GetItem(pValue,ii));
			}
			Py_DECREF ( pValue );
			break;

		    /* Must copy the result to the global variable
		     * because after the call to PY_DECREF, the object
		     * and the string are no longer accessible. */
		    case DACHAR:
		    default:
			size = PyString_Size(pValue) + 2;
			daoutc = (char *)malloc(size*sizeof(char));
			lclchr = (char *)malloc(size*sizeof(char));
			lclchr = PyString_AsString ( pValue );
			strcpy ( daoutc, lclchr );
			Py_DECREF ( pValue );
			break;
		}
	    }
	    else {
		Py_DECREF ( pFunc );
		Py_DECREF ( pModule );
		PyErr_Print ();
		*iret = -4;
		return;
	    }
	}
	else {
	    if ( PyErr_Occurred() ) {
		PyErr_Print();
	    }
	    *iret = -2;
	}
	Py_XDECREF ( pFunc );
	Py_DECREF ( pModule );
    }
    else {
	if ( PyErr_Occurred() ) {
	    PyErr_Print();
	}
	*iret = -1;
    }

    /* Close Python */
    /* Do not call Py_Finalize() */
    /* Multiple calls to Intialize and Finalize do not work properly. */

#else /* If Python is not available, return with an error */
    *iret = -1;
#endif

    return;
}
