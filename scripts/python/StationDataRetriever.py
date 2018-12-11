import os
from datetime import datetime
from awips import ThriftClient
from dynamicserialize.dstypes.com.raytheon.uf.common.time import DataTime
from dynamicserialize.dstypes.com.raytheon.uf.common.time import TimeRange
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gempak.request import StationDataRequest


class StationDataRetriever:
    """ Retrieves all data for a requested station and time """

    def __init__(self, server, pluginName, stationId, refTime, parmList, partNumber):
        self.pluginName = pluginName
        self.stationId = stationId
        self.refTime = refTime
        self.parmList = parmList
        self.partNumber = partNumber
        self.host = os.getenv("DEFAULT_HOST", server)
        self.port = os.getenv("DEFAULT_PORT", "9581")
        self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getStationData(self):
        """ Sends ThriftClient request and writes out received files."""
        dtime = datetime.strptime(self.refTime, "%y%m%d/%H%M")
        trange = TimeRange()
        trange.setStart(dtime)
        trange.setEnd(dtime)
        dataTime = DataTime(refTime=dtime, validPeriod=trange)
        req = StationDataRequest()
        req.setPluginName(self.pluginName)
        req.setStationId(self.stationId)
        req.setRefTime(dataTime)
        req.setParmList(self.parmList)
        req.setPartNumber(self.partNumber)
        resp = self.client.sendRequest(req)

        for i, rec in enumerate(resp):
            resp[i] = {
                key.decode() if isinstance(key, bytes) else key:
                val.decode() if isinstance(val, bytes) else val
                for key, val in rec.items()
            }

        return resp


def getstationdata(server, table, stationId, refTime, parmList, partNumber):
    sr = StationDataRetriever(server, table, stationId, refTime, parmList, partNumber)
    lcldict = sr.getStationData()

    rdata = []
    
    for substr in parmList.split(','):
        if substr in lcldict:
            rdata.append(lcldict[substr])
        else:
            rdata.append(-9999.00)

    return rdata


def getleveldata(server, table, stationId, refTime, parmList, partNumber):
    sr = StationDataRetriever(server, table, stationId, refTime, parmList, partNumber)
    lcldict = sr.getStationData()

    numset = [1]
    for substr in parmList.split(','):
        if substr in lcldict:
            pnum = len(lcldict[substr]) - 1
            while pnum >= 0:
                if lcldict[substr][pnum] != -9999.00:
                    break
                pnum = pnum - 1
            numset.append(pnum)

    rdata = []

    for jj in range(max(numset)):
        for substr in parmList.split(','):
            if substr in lcldict:
                if lcldict[substr][jj] == -9999998.0:
                    rdata.append(-9999.0)
                else:
                    rdata.append(lcldict[substr][jj])
            else:
                rdata.append(-9999.0)

    return rdata


def getstationtext(server, table, stationId, refTime, parmList, partNumber):
    sr = StationDataRetriever(server, table, stationId, refTime, parmList, partNumber)
    lcldict = sr.getStationData()

    if parmList in lcldict:
        return lcldict[parmList]
    else:
        return ' '


def getheader(server, table, stationId, refTime, parmList, partNumber):
    idata = []
    idata.append(0)
    return idata


# This is the standard boilerplate that runs this script as a main
if __name__ == '__main__':
    # Run Test
    srv = 'edex-cloud.unidata.ucar.edu'
    key = '-'
    print('OBS - METAR')
    tbl = 'obs'
    stn = 'KLGA'
    time = '130823/1700'
    parm = 'seaLevelPress,temperature,dewpoint,windSpeed,windDir'
    part = '0'
    print(getheader(srv, tbl, stn, time, parm, part))
    print(getstationdata(srv, tbl, stn, time, parm, part))
    parm = 'rawMETAR'
    print(getstationtext(srv, tbl, stn, time, parm, part))
    print('SFCOBS - SYNOP')
    tbl = 'sfcobs'
    stn = '72403'
    time = '130823/1800'
    parm = 'seaLevelPress,temperature,dewpoint,windSpeed,windDir'
    part = '0'
    print(getheader(srv, tbl, stn, time, parm, part))
    print(getstationdata(srv, tbl, stn, time, parm, part))
    parm = 'rawReport'
    print(getstationtext(srv, tbl, stn, time, parm, part))
    print('UAIR')
    tbl = 'bufrua'
    stn = '72469'
    time = '130823/1200'
    parm = 'prMan,htMan,tpMan,tdMan,wdMan,wsMan'
    part = '2020'
    print(getleveldata(srv, tbl, stn, time, parm, part))
    parm = 'prSigT,tpSigT,tdSigT'
    part = '2022'
    print(getleveldata(srv, tbl, stn, time, parm, part))
    parm = 'htSigW,wsSigW,wdSigW'
    part = '2021'
    print(getleveldata(srv, tbl, stn, time, parm, part))
