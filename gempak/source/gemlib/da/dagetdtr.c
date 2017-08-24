#include "da.h"


void da_getdtr ( char *station, char *time, int *partnum, int *iflno,
        int *idthdr, float *rdata, int *nval, int *iret )
/************************************************************************
 * da_getdtr                                                            *
 *                                                                      *
 * This function reads real data from a non-GEMPAK data source for the  *
 * given station and time.                                              *
 *                                                                      *
 * da_getdtr ( station, time, idthdr, rdata, nval, iret )               *
 *                                                                      *
 * Input parameters:                                                    *
 *    station     char*       Station ID for request                    *
 *    time        char*       Date/Time for request                     *
 *    partnum     int*        Part number (Fortran counting)            *
 *    iflno       int*        GEMPAK file number                        *
 *                                                                      *
 * Output parameters:                                                   *
 *    idthdr      int*        Data header array                         *
 *    rdata       float*      Array of real data                        *
 *    nval        int*        Length of data array                      *
 *    iret        int*        Return code                               *
 **                                                                     *
 * S. Jacobs/NCEP 6/13        Created                                   *
 * P. Moyer/NCEP  07/11/16    R17968 Added caching of timestamp/station *
 *                                   parameter data in local tree for   *
 *                                   faster retrieval.                  *
 ************************************************************************/
{
    int        ii, jj, gflnum, ier, pindex, scale, ies;
    float    ref;
    char    pyfile[MXFLSZ], pymeth[MXFLSZ];
    char    parms[1000], part[12];
    char    db_table[1000];
    /*---------------------------------------------------------------------*/

    *iret = 0;

    gflnum = *iflno - 1;

    if ( *partnum < 1 ) return;

    /* Set the part number as a string */
    if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTAA") == 0 ) {
        strcpy ( part, "2020" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"PPBB") == 0 ) {
        strcpy ( part, "2021" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTBB") == 0 ) {
        strcpy ( part, "2022" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTCC") == 0 ) {
        strcpy ( part, "2030" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"PPDD") == 0 ) {
        strcpy ( part, "2031" );
    }
    else if ( strcmp(common[gflnum].parts[*partnum-1].name,"TTDD") == 0 ) {
        strcpy ( part, "2032" );
    }
    else {
        strcpy ( part, "0" );
    }

    /* Set the list of parameter names */
    /* Assemble an array of parameter names for existing param check */

    strcpy ( parms, common[gflnum].parts[*partnum-1].parms[0].key );
    jj = 1;
    while ( jj < common[gflnum].parts[*partnum-1].numparms ) {
        strcat ( parms, "," );
        strcat ( parms, common[gflnum].parts[*partnum-1].parms[jj].key );
        jj++;
    }

    char* tokenizedParms = (char*) malloc (STRSIZE * sizeof(char));
    strcpy(tokenizedParms, parms);

    char** argParms = (char **) malloc( jj * sizeof(char *));
    char* pch;
    int ind = 0;
    pch = strtok (tokenizedParms, ",");
    while (pch != NULL) {
        argParms[ind] = (char *) malloc (STRSIZE * sizeof(char));
        strcpy (argParms[ind], pch);
        pch = strtok (NULL, ",");
        ind++;
    }
    free(tokenizedParms);

    /* Set the arguments for input to the request script */
    danarg = 6;
    daargs = (char **) malloc ( danarg * sizeof(char *) );
    for ( ii = 0; ii < danarg; ii++ ) {
        daargs[ii] = (char *) malloc ( STRSIZE * sizeof(char) );
    }
    ii = 0;
    strcpy ( daargs[ii], common[gflnum].dbserver ); ii++;
    strcpy ( daargs[ii], common[gflnum].dbtable ); ii++;
    strcpy ( daargs[ii], station ); ii++;
    strcpy ( daargs[ii], time ); ii++;
    strcpy ( daargs[ii], parms ); ii++;
    strcpy ( daargs[ii], part ); ii++;

    sprintf(db_table, "%s-%s", common[gflnum].dbserver, common[gflnum].dbtable);

    ies = 0;
    timestamp_DN *currentTimestamp = 0;
    da_timeExists(time, &ies, (void*) timestampTreeRoot, (void**) &currentTimestamp);

    if (ies == 0) {
        /* set the interim station root to empty */
        stationInterimRoot = 0;

        /* Get the data for the timestamp */
        /* Run the Python script to make the actual request */
        datype = DAFLOAT;
        strcpy ( pyfile, common[gflnum].parts[*partnum-1].pyfile );
        strcpy ( pymeth, common[gflnum].parts[*partnum-1].pymethhdr );
        da_runpy_once ( pyfile, pymeth, &ier );

        /* Assign 0 to all possible values (StationDataRetriver's
         * 'getheader' just returns 0) */
        danumi = 1;
        //daouti = (int *)malloc(danumi*sizeof(int));
        //idthdr = (int *)malloc(danumi*sizeof(int)); //maybe?
        for ( ii = 0; ii < danumi; ii++ ) {
            idthdr[ii] = 0;
        }
        //free ( daouti );

        /* Take the returned tree of stations from darunpyonce
         * and put it into the tree of timestamps, which creates a new
         * timestamp entry there.
         */

        da_insertTimestamp(time, (void*)stationInterimRoot, (void**)&timestampTreeRoot);

        /* retrieve the newly-stored timestamp */
        da_timeExists(time, &ies, (void*) timestampTreeRoot, (void**) &currentTimestamp);

        /* store the parameter list in argparms into the timestamp record */
        da_insertTimestampParams(currentTimestamp, argParms, ind, datype, db_table, part);

    } else {
        int paramsExist = 1;

        /* check to see if all params are there or not with existing timestamp*/

        da_paramExistsTimestamp(currentTimestamp, argParms, ind, db_table, part, &paramsExist);

        /* if not, get the station root, and use that to run da_runpy_once */
        if (paramsExist == 0) {
            stationInterimRoot = currentTimestamp->stationRoot;

            //which will end up inserting into the existing tree, and the existing
            //timestamp will not be overwritten

            /* Run the Python script to make the actual request */
            datype = DAFLOAT;
            strcpy ( pyfile, common[gflnum].parts[*partnum-1].pyfile );
            strcpy ( pymeth, common[gflnum].parts[*partnum-1].pymethhdr );
            da_runpy_once ( pyfile, pymeth, &ier );

            /* Assign 0 to all possible values (StationDataRetriver's
             * 'getheader' just returns 0) */
            danumi = 1;
            for ( ii = 0; ii < danumi; ii++ ) {
                idthdr[ii] = 0;
            }

            /* store the parameter list in argparms into the timestamp record */
            da_insertTimestampParams(currentTimestamp, argParms, ind, datype, db_table, part);
        }

        /* otherwise, just assign the stationInterimRoot to the current timestamp's */
        stationInterimRoot = currentTimestamp->stationRoot;
    }

    /* Get the data */

    /* check tree for current timestamp entry*/
    /* if found, return station tree for that entry*/
    /* check tree for current station*/

    parameter_DN *returnedParams = 0;

    da_returnStationValueTree(station, (void*) stationInterimRoot, (void**) &returnedParams);

    /* if found, return the proper array (assumed to be reals)*/
    /* pack it for passing back into fortran.*/
    /* if the station was not found, fills the array with -9999 floats */

    if (returnedParams != 0) {
        int resCount = 0;

        //allocate array to temp store param pointers.  Since this is an array of
        //pointers to the parameter_DN objects we don't want to free the referenced objects
        parameter_DN **paramArray = (parameter_DN **)malloc(ind * sizeof(parameter_DN*));
        int greaterThanZero=0;//gets set to 1 if encounters any parameter with size greater than zero
        int maxLength = 0;

        for ( ii = 0; ii < ind; ii++) {

            //first, get params from returned tree, in order.
            //determine lengths
            //if lengths are all 0, then simple flat form
            //if any lengths are > 0, then the flattened array form.
            int exists = 0;
            parameter_DN *currentParam = 0;
            da_paramExistsStation((void *)returnedParams, argParms[ii], db_table, part, &exists, (void**) &currentParam);

            if (exists != 0) {
                paramArray[ii] = currentParam;
                //checking to see if this will be an array,
                //and determining max length
                if ((paramArray[ii]->paramSize) > 0) {
                    greaterThanZero = 1;
                    if ((paramArray[ii]->paramSize) > maxLength) {
                        maxLength = paramArray[ii]->paramSize;
                    }
                }
            }
            else {
                paramArray[ii] = 0; //null pointer
            }
        }

        if (greaterThanZero == 0 ){
            /* extract the values, put them into rdata array */
            for (ii = 0; ii < ind; ii++) {
                if (paramArray[ii] != 0){
                    daret_t dataType = paramArray[ii]->paramType;

                    switch (dataType) {
                    case DAINT:
                        break;
                    case DAFLOAT:
                        if ( ERMISS(paramArray[ii]->paramf) ) {
                            rdata[ii] = paramArray[ii]->paramf;
                        }
                        else {
                            pindex = ii % common[gflnum].parts[*partnum-1].numparms;
                            scale = common[gflnum].parts[*partnum-1].parms[pindex].scale;
                            ref   = common[gflnum].parts[*partnum-1].parms[pindex].offset;
                            rdata[ii] = ( paramArray[ii]->paramf * powf(10.0, (float)scale) ) + ref;
                        }

                        break;
                    case DACHAR:
                    default:
                        break;
                    }
                } else {
                    /*in case parameter requested does not exist in current param tree*/
                    switch (datype) {
                    case DAINT:
                        break;
                    case DAFLOAT:
                        rdata[ii] = -9999;
                        break;
                    case DACHAR:
                    default:
                        break;
                    }
                }
                resCount++;
            }
        } else {
            //assembles a flattened array of values instead
            /* extract the values, put them into rdata array */

            //loop first by position index
            for (jj=0; jj < maxLength; jj++) {
                //then by parameter
                for (ii=0; ii<ind; ii++){
                    //if paramArray == 0
                    if (paramArray[ii] == 0) {
                        //use datype to determine type
                        //then empty -9999 apropos type
                        //increment resCount
                        switch (datype) {
                        case DAINT:
                            break;
                        case DAFLOAT:
                            rdata[resCount] = -9999;
                            break;
                        case DACHAR:
                        default:
                            break;
                        }
                        resCount++;
                    }
                    //otherwise
                    else {
                        //get the da type from the paramArray element
                        daret_t dataType = paramArray[ii]->paramType;
                        //if paramArray count >= current index
                        //then all good for this value
                        //otherwise
                        //insert -9999
                        //increment resCount
                        if(paramArray[ii]->paramSize >= jj) {
                            //for each - confirm that the matching array actually -exists- first.
                            //if so, do the below,
                            //otherwise
                            //retrieve from the singleton variable.
                            switch (dataType) {
                            case DAINT:
                                break;
                            case DAFLOAT:

                                if (paramArray[ii]->paramSize != 0){
                                    if ( ERMISS(paramArray[ii]->paramfArray[jj]) ) {
                                        rdata[resCount] = paramArray[ii]->paramfArray[jj];
                                    }
                                    else {
                                        pindex = resCount % common[gflnum].parts[*partnum-1].numparms;
                                        scale = common[gflnum].parts[*partnum-1].parms[pindex].scale;
                                        ref   = common[gflnum].parts[*partnum-1].parms[pindex].offset;
                                        rdata[resCount] = ( paramArray[ii]->paramfArray[jj] * powf(10.0, (float)scale) ) + ref;
                                    }
                                }
                                else {
                                    if ( ERMISS(paramArray[ii]->paramf) ) {
                                        rdata[resCount] = paramArray[ii]->paramf;
                                    }
                                    else {
                                        pindex = resCount % common[gflnum].parts[*partnum-1].numparms;
                                        scale = common[gflnum].parts[*partnum-1].parms[pindex].scale;
                                        ref   = common[gflnum].parts[*partnum-1].parms[pindex].offset;
                                        rdata[resCount] = ( paramArray[ii]->paramf * powf(10.0, (float)scale) ) + ref;
                                    }
                                }

                                break;
                            case DACHAR:
                            default:
                                break;
                            }
                        }
                        else {
                            switch (dataType) {
                            case DAINT:
                                break;
                            case DAFLOAT:
                                rdata[resCount] = -9999;
                                break;
                            case DACHAR:
                            default:
                                break;
                            }
                        }
                        resCount++;
                    }
                }
            }
        }
        *nval = resCount;

        free(paramArray);
    } else {
        for (ii = 0; ii < common[gflnum].parts[*partnum-1].numparms; ii++) {
            rdata[ii] = -9999;
        }
        *nval = common[gflnum].parts[*partnum-1].numparms;
    }

    /* Free all allocated memory */
    for ( ii = 0 ; ii < danarg; ii++ ) {
        free ( daargs[ii] );
    }
    free ( daargs );
    free ( daoutf );

}
