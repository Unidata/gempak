import os
import sys
from awips import ThriftClient
from dynamicserialize.dstypes.gov.noaa.nws.ncep.common.dataplugin.gempak.request import GetStationsRequest


class StationRetriever:
    """ Retrieves all requested stations """

    def __init__(self, server, pluginName):
        self.pluginName = pluginName
        self.outdir = os.getcwd()
        self.host = os.getenv("DEFAULT_HOST", server)
        self.port = os.getenv("DEFAULT_PORT", "9581")
        self.client = ThriftClient.ThriftClient(self.host, self.port)

    def getStations(self):
        """ Sends ThriftClient request and writes out received files."""
        req = GetStationsRequest()
        req.setPluginName(self.pluginName)
        resp = self.client.sendRequest(req)

        for i, rec in enumerate(resp):
            resp[i] = {
                key.decode() if isinstance(key, bytes) else key:
                    val.decode() if isinstance(val, bytes) else val
                for key, val in rec.items()
            }

        stns = []
        for item in resp:
            stationstr = '{:<8}'.format(item.getStationId())

            if sys.byteorder == 'little':
                stnid = (ord(stationstr[3]) << 24) + (ord(stationstr[2]) << 16) + \
                        (ord(stationstr[1]) << 8) + ord(stationstr[0])
                stnid2 = (ord(stationstr[7]) << 24) + (ord(stationstr[6]) << 16) + \
                         (ord(stationstr[5]) << 8) + ord(stationstr[4])
            else:
                stnid = (ord(stationstr[0]) << 24) + (ord(stationstr[1]) << 16) + \
                        (ord(stationstr[2]) << 8) + ord(stationstr[3])
                stnid2 = (ord(stationstr[4]) << 24) + (ord(stationstr[5]) << 16) + \
                         (ord(stationstr[6]) << 8) + ord(stationstr[7])

            if item.getState() is None:
                stationstr = '    '
            else:
                stationstr = '{:<4}'.format(item.getState())

            if sys.byteorder == 'little':
                state = (ord(stationstr[3]) << 24) + (ord(stationstr[2]) << 16) \
                        + (ord(stationstr[1]) << 8) + ord(stationstr[0])
            else:
                state = (ord(stationstr[0]) << 24) + (ord(stationstr[1]) << 16) \
                        + (ord(stationstr[2]) << 8) + ord(stationstr[3])

            stationstr = '{:<4}'.format(item.getCountry())
            if sys.byteorder == 'little':
                cntry = (ord(stationstr[3]) << 24) + (ord(stationstr[2]) << 16) \
                        + (ord(stationstr[1]) << 8) + ord(stationstr[0])
            else:
                cntry = (ord(stationstr[0]) << 24) + (ord(stationstr[1]) << 16) \
                        + (ord(stationstr[2]) << 8) + ord(stationstr[3])

            stns.append(9999)
            stns.append(stnid)
            stns.append(item.getWmoIndex())
            stns.append(int(item.getLatitude()*100))
            stns.append(int(item.getLongitude()*100))
            stns.append(int(item.getElevation()))
            stns.append(state)
            stns.append(cntry)
            stns.append(stnid2)
            stns.append(0)
        return stns


def getstations(server, table, key, dummy, dummy2):
    sr = StationRetriever(server, table)
    return sr.getStations()


# This is the standard boilerplate that runs this script as a main
if __name__ == '__main__':
    # Run Test
    srv = 'edex-cloud.unidata.ucar.edu'
    key = '-'
    print('OBS - METAR')
    tbl = 'obs'
    print(getstations(srv, tbl, key))
    print('SFCOBS - SYNOP')
    tbl = 'sfcobs'
    print(getstations(srv, tbl, key))
