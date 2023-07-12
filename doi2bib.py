#!/usr/bin/env python3

import sys
import yaml
import requests
import requests_cache
import re

requests_cache.install_cache(backend='sqlite')

data = yaml.safe_load(sys.stdin)

for key, doi in data['entries'].items():
    bib = requests.get(f"https://dblp.org/doi/{doi}.bib").text
    bib = re.sub('{DBLP.*,', '{' + key + ',', bib)
    for subs in data['subs']:
        bib = re.sub(subs['replace'], subs['with'], bib)
    print(bib)
