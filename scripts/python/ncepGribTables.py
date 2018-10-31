#!/usr/bin/env python
# Parse html tables from a given URL and output CSV.
# Note: To install a missing python module foo do "easy_install foo"
#   (or the new way is "pip install foo" but you might have to do 
#    "easy_install pip" first)

from BeautifulSoup import BeautifulSoup
import scrape
import urllib.request, urllib.error, urllib.parse
import html.entities
import re
import sys
import unicodedata


# from http://stackoverflow.com/questions/1197981/convert-html-entities
def asciify2(s):
    matches = re.findall("&#\d+;", s)
    if len(matches) > 0:
        hits = set(matches)
        for hit in hits:
            name = hit[2:-1]
            try:
                entnum = int(name)
                s = s.replace(hit, chr(entnum))
            except ValueError:
                pass

    matches = re.findall("&\w+;", s)
    hits = set(matches)
    amp = "&amp;"
    if amp in hits:
        hits.remove(amp)
    for hit in hits:
        name = hit[1:-1]
        if name in html.entities.name2codepoint:
            s = s.replace(hit, "")
    s = s.replace(amp, "&")
    return s


def opensoup(url):
    request = urllib.request.Request(url)
    request.add_header("User-Agent", "Mozilla/5.0")
    # To mimic a real browser's user-agent string more exactly, if necessary:
    #   Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.1.14)
    #   Gecko/20080418 Ubuntu/7.10 (gutsy) Firefox/2.0.0.14
    pagefile = urllib.request.urlopen(request)
    soup = BeautifulSoup(pagefile)
    pagefile.close()
    return soup


def asciify(s):
    return unicodedata.normalize('NFKD', s).encode('ascii', 'ignore')


# remove extra whitespace, including stripping leading and trailing whitespace.
def condense(s):
    s = re.sub(r"\s+", " ", s, re.DOTALL)
    return s.strip()


def stripurl(s):
    s = re.sub(r"\<span\s+style\s*\=\s*\"display\:none[^\"]*\"[^\>]*\>[^\<]*\<\/span\>", "", s)
    s = re.sub(r"\&\#160\;", " ", s)
    return condense(re.sub(r"\<[^\>]*\>", " ", s))


# this gets rid of tags and condenses whitespace
def striptags(s):
    s = re.sub(r"\<span\s+style\s*\=\s*\"display\:none[^\"]*\"[^\>]*\>[^\<]*\<\/span\>", "", s)
    s = re.sub(r"\&\#160\;", " ", s)
    return condense(s)


def getUrlArgs(parseUrl):
    return re.search('grib2_table4-2-(\d+)-(\d+).shtml', parseUrl).groups()


if len(sys.argv) == 1:
    print("Usage: ", sys.argv[0], " url [n]")
    print("  (where n indicates which html table to parse)")
    exit(1)

url = sys.argv[1]
soup = opensoup(url)
tables = soup.findAll("table")

for table in tables:
    for r in table.findAll('tr'):
        rl = []
        for c in r.findAll(re.compile('td|th')):
            rl.append(striptags(c.renderContents()))
        if len(rl) > 1 and "href" in rl[1]:
            print('! ' + stripurl(rl[1]))
            scrapeUrl = 'http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-2-' + \
                        getUrlArgs(rl[1])[0] + "-" + getUrlArgs(rl[1])[1] + '.shtml'
            scrape.run(scrapeUrl)
