#ifdef WITHPYTHON /* Is Python available? */
#include "Python.h"
#endif

#include "da.h"

void da_runpy_once ( char *pyfile, char *pymeth, int *iret )
/************************************************************************
 * da_runpy_once                                                        *
 *                                                                      *
 * This function runs the given Python method from the given Python     *
 * file.  It is a modified version of the original da_runpy function,   *
 * adapted to process a full set of stations for a single timestamp. As *
 * such, it is meant to work with a particular Python method -          *
 * StationDataRetriever.gettimestampstations                            *
 *                                                                      *
 * Sample code used in creating this function, and a detailed           *
 * explanation of calling Python from C may be found at:                *
 * http://docs.python.org/2.7/contents.html                             *
 *                                                                      *
 * da_runpy ( pyfile, pymeth, iret )                                    *
 *                                                                      *
 * Input parameters:                                                    *
 *    *pyfile        char    Python file name                           *
 *    *pymeth        char    Python method name                         *
 *                                                                      *
 * Output parameters:                                                   *
 *    *iret        int    Return code                                   *
 **                                                                     *
 * Log:                                                                 *
 * P. Moyer/NCEP  07/11/16  R17968  Initial coding                      *
 ************************************************************************/
{
#ifdef WITHPYTHON /* Is Python available? */
    int        ii, respSize, respCount, argSize, argCount, valuei, *intArray, jj;
    PyObject    *pName, *pModule, *pFunc, *pArgs, *pValue, *pStation;
    char    *lclchr, *paramStr, *station;
    char    db_table[1000];
    char    reportPart[20];
    float   valuef, *floatArray;
    Py_ssize_t    size;
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

            /* clear the current station tree */

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

            /* create db_table identifier for later station parameter inclusion */

            sprintf(db_table, "%s-%s", daargs[0], daargs[1]);

            /* create part/report type identifier for later station parameter inclusion */

            strcpy(reportPart, daargs[5]);

            /* get the parameters string and list size */
            /* create array of param strings for insertion order reference */

            paramStr = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy(paramStr, daargs[4]);
            unsigned int ind;
            argSize = 0;
            for (ind = 0; ind < strlen(paramStr); ind++) {
                if(paramStr[ind] == ',') {
                    argSize++;
                }
            }
            argSize++;

            char** argParms = (char **) malloc( argSize * sizeof(char *));
            char* pch;
            ind = 0;
            pch = strtok (paramStr, ",");
            while (pch != NULL) {
                argParms[ind] = (char *) malloc (STRSIZE * sizeof(char));
                strcpy (argParms[ind], pch);
                pch = strtok (NULL, ",");
                ind++;
            }
            free (paramStr);

            /* Call the Python script */
            pValue = PyObject_CallObject ( pFunc, pArgs );
            Py_DECREF ( pArgs );

            /* get length of total array */
            respSize = PyList_Size(pValue);
            respCount = 0;
            argCount = 0;

            if (pValue != NULL) {
                /* loop through the list of items received until null result*/
                while (respCount < respSize) {
                    /* first item is string - station */
                    pStation = PyList_GetItem(pValue,respCount);
                    int stationSize = PyString_Size(pStation);
                    station = (char *)malloc(stationSize*sizeof(char));
                    lclchr = (char *)malloc(stationSize*sizeof(char));
                    lclchr = PyString_AsString ( pStation );
                    strcpy ( station, lclchr );
                    respCount++;

                    /* next items are the types below into a linked tree */

                    /* storage into a tree.  Items in the argParms array
                     * are in the same sequence as the initial param list, so
                     * they match up for name insertion */

                    /* set interimParamRoot to empty */
                    /* insert parameter, value, and type into param tree */
                    /* if parameter is already there, overwrite */

                    parameterInterimRoot = 0;

                    danumf = argSize;
                    for (argCount = 0 ; argCount < argSize; argCount++){

                        //get the size number.

                        int paramSize = (int)PyInt_AsLong(PyList_GetItem(pValue,respCount));
                        //increment respCount
                        respCount++;

                        if (paramSize == 0) {
                            //if size is 0, perform the below section
                            switch ( datype ) {
                                case DAINT:
                                    valuei = (int)PyInt_AsLong(PyList_GetItem(pValue,respCount));
                                    da_insertParam(argParms[argCount], db_table, (int) datype,
                                        reportPart, 0, valuei, 0, (void**) &parameterInterimRoot);
                                break;
                                case DAFLOAT:
                                    valuef = (float)PyFloat_AsDouble(PyList_GetItem(pValue,respCount));
                                    da_insertParam(argParms[argCount], db_table, (int) datype,
                                        reportPart, valuef, 0, 0, (void**) &parameterInterimRoot);
                                break;
                                case DACHAR:
                                default:
                                    size = PyString_Size(PyList_GetItem(pValue,respCount)) + 2;
                                    lclchr = (char *)malloc(size*sizeof(char));
                                    lclchr = PyString_AsString (PyList_GetItem(pValue,respCount));
                                    da_insertParam(argParms[argCount], db_table, (int) datype,
                                        reportPart, 0, 0, (char *)lclchr, (void**) &parameterInterimRoot);
                                break;
                            }
                            respCount++;
                        }
                        else if (paramSize > 0) {
                            //otherwise, this is an array.
                            //allocate a new array based on data type to that size
                            //use the size from above to control an array loop to read the subsequent
                            //values and assemble the array
                            //increment respCount each time
                            //once completed, perform da_insertParamArray for that data type and array size
                            //do NOT increment respCount after that stage
                            switch ( datype ) {
                            case DAINT:
                                intArray = (int *) malloc ( paramSize * sizeof(int) );
                                for (jj = 0; jj < paramSize; jj++) {
                                    intArray[jj] = (int)PyInt_AsLong(PyList_GetItem(pValue,respCount));
                                    respCount++;
                                }
                                da_insertParamArray(argParms[argCount], db_table, (int) datype, reportPart,
                                    paramSize, 0, intArray, 0, (void**) &parameterInterimRoot);
                            break;
                            case DAFLOAT:
                                floatArray = (float *) malloc ( paramSize * sizeof(float) );
                                for (jj = 0; jj < paramSize; jj++) {
                                    floatArray[jj] = (float)PyFloat_AsDouble(PyList_GetItem(pValue,respCount));
                                    respCount++;
                                }
                                da_insertParamArray(argParms[argCount], db_table, (int) datype, reportPart,
                                    paramSize, floatArray, 0, 0, (void**) &parameterInterimRoot);
                            break;
                            case DACHAR:
                            default:
                                //currently do not have an implementation for arrays of strings (since the individual
                                //elements also also have variable lengths, unlike ints and floats
                            break;
                            }
                        }
                        else {
                            //error code of some type
                        }
                    }

                    /* take station and parameter tree and put into interim station tree */
                    da_insertStation(station, (void *) parameterInterimRoot, (void**) &stationInterimRoot);
                }
                Py_DECREF(pValue);



            }
            else {
                Py_DECREF ( pFunc );
                Py_DECREF ( pModule );
                PyErr_Print ();
                *iret = -4;
                return;
            }

            //deallocate argument array
            for (argCount = 0 ; argCount < argSize; argCount++){
                free(argParms[argCount]);
            }
            free(argParms);
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
