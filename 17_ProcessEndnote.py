#!/usr/bin/env python
""" Reads bib and EndNote extracts. Writes an EndNote summary to a csv file.
Writes a filtered EndNote extract, which contains just the references that are
present in our bib file.
"""

import csv
import endnote

from pathlib import Path

endnote_dir = Path('0_data/')
bib_dir = Path('16_ReferencesPreprocess/')
output_dir = Path('17_ProcessEndnote/')

if not output_dir.is_dir():
    output_dir.mkdir()

print('Reading and parsing EndNote extract')
en = endnote.read_endnote(endnote_dir / 'endnote.txt')

print('Reading bib CSV file')
db = endnote.read_bib(bib_dir / 'bib.csv')

# This source has two references
# extra = [ ('DL1_2005__Ferreira', ('Influence of habitat management on the '
#                                   'abundance and diet of wild rabbit '
#                                   '(Oryctolagus cuniculus algirus) '
#                                   'populations in Mediterranean ecosystems')) ]

extra = []
print('Matching bib rows to EndNote references')
processed, missing = endnote.match(en, db, extra)

print('Writing {0} records missing from EndNote'.format(len(missing)))
with (output_dir / 'missing_from_endnote.csv').open('w', newline='', encoding='utf-8') as file:
    w = csv.writer(file)
    w.writerow(('Source_ID', 'Year', 'First_author_surname', 'Source_title'))
    for m in missing:
        w.writerow((m.get('Source_ID'), m.get('Year_of_publication'),
                    m.get('First_author_surname'), m.get('Source_title')))

print('Writing EndNote CSV summary')
# Don't include 'Record' - this is the Raw EndNote record
columns = ['Source_ID','Title','Year','Type','Journal','First_author_surname',
           'Authors','Cites','DOI','Accession']

with (output_dir / 'endnote_summary.csv').open('w', newline='', encoding='utf-8') as file:
    w = csv.writer(file)
    w.writerow(columns)
    for record in processed:
        w.writerow([record[k] for k in columns])

print('Writing filtered EndNote extract')
filtered = [v['Record'] for v in processed]
with (output_dir /'endnote_filtered.txt').open('w',encoding='utf8') as f:
    f.write(endnote.endnote_record_delim.join(filtered))
