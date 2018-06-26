import csv
import re

from collections import Counter

endnote_record_delim = '\n\n\n\n'

def read_endnote(path):
    # Takes a path to an EndNote extract and returns a list of dicts of certain 
    # fields
    def first(v): return v[0] if v else ''

    en = path.open(encoding='utf8').read()
    en = [r for r in en.split(endnote_record_delim) if r]
    title = [first(re.findall('^%T (.*)$', r, re.M)) for r in en]
    year = [first(re.findall('^%D (.*)$', r, re.M)) for r in en]
    type = [first(re.findall(r'%0 (.*)$', v, re.M)) for v in en]
    cites =  [first(re.findall(r'.*Times Cited: ([0-9]+)$', v, re.M)) for v in en] 
    first_author_surname = [first(re.findall(r'%A (.*)$', v, re.M)) for v in en]
    first_author_surname = [v.split(',')[0] for v in first_author_surname]
    authors = [';'.join(re.findall(r'%A (.*)$', v, re.M)) for v in en]
    journal = [first(re.findall(r'%J (.*)$', v, re.M)) for v in en]
    doi = [first(re.findall(r'%R (.*)$', v, re.M)) for v in en]
    accession = [first(re.findall(r'%M (.*)$', v, re.M)) for v in en]

    # Are any titles duplciated
    duplicated = filter(lambda i: i[1]>1, Counter([t.lower() for t in title]).items())
    if duplicated:
        print('Duplicated titles in EndNote extract')
        for d in duplicated:
            print(d)

    endnote = []
    for v in zip(title, year, type, journal, first_author_surname, authors, 
                 cites, doi, accession, en):
        endnote.append(dict(Title=v[0], Year=v[1], Type=v[2], Journal=v[3], 
                            First_author_surname=v[4], Authors=v[5], Cites=v[6], 
                            DOI=v[7], Accession=v[8], Record=v[9]))
    return endnote

def read_bib(path):
    with path.open(newline='', encoding='utf-8') as file:
        return [r for r in csv.DictReader(file)]

def consistency(en, db):
    # Consistency of EndNote and bib records
    #   First author
    #   Year
    #   DOI
    #   Accession number
    #   Journal

    # DOI is not case-sensitive
    en['DOI'] = en['DOI'].lower()
    db['DOI'] = db['DOI'].lower()

    keys = ( ('Source_ID', 'Source_ID'),
             ('Year_of_publication', 'Year'),
             ('DOI', 'DOI'),
             ('First_author_surname', 'First_author_surname'),
           )
    bad = []
    for a,b in keys:
        if db[a]!=en[b]:
            bad += [a, db[a], en[b]]
    if bad:
        print(db['Source_ID'], 'is not consistent')
        for b in bad:
            print('\t', b)

def match(en, db, extra=[]):
    # Returns a list of dicts of EndNote records in en that are referenced in db.
    # Some Data Sources take data form more than one article - where this 
    # applies, set the extra argumemt to be a list of tuples 
    # [ (Source_ID,article title), ...]

    # Map from lower-case title to record

    db_titles = [v['Source_title'].lower() for v in db]
    db = dict(zip(db_titles, db))

    en_titles = [v['Title'].lower() for v in en]
    en = dict(zip(en_titles, en))

    missing = set(db.keys()).difference(en.keys())
    missing = [db[m] for m in missing]
    missing = sorted(missing, key=lambda r: r['First_author_surname'])

    matched = set(db.keys()).intersection(en.keys())
    for k in matched:
        en[k]['Source_ID'] = db[k]['Source_ID']

    # Check consistency before adding extra references
    for k in matched:
        consistency(en[k], db[k])

    for source_id,title in extra:
        en[title.lower()]['Source_ID'] = source_id
        matched.add(title.lower())

    en = [v for k,v in en.items() if k in matched]
    en = sorted(en, key=lambda r: r['First_author_surname'])

    return en, missing
