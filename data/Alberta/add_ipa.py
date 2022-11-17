import os
import sys
import csv
import eng_to_ipa

if __name__ == '__main__':
    print('change directory')
    os.chdir(os.path.dirname(sys.argv[0]))

    result = []

    for f in sorted(os.listdir()):
        print(f)
        if f.startswith('baby-names-frequency') and f.endswith('.csv'):
            print('read')
            with open(f, 'r') as infile:
                reader = csv.DictReader(
                    infile, ['rank', 'name', 'count', 'sex', 'year'])
                l = list(reader)
                print('add columns')
                for d in l:
                    del d['rank']
                    d['sex'] = 'F' if d['sex'] == 'Girl' else 'M'
                    d['IPA'] = eng_to_ipa.convert(d['name'])
                print('add to result')
                result += l

    print('write')
    with open('../Alberta.csv', 'w') as outfile:
        writer = csv.DictWriter(
            outfile, ['name', 'sex', 'count', 'year', 'IPA'])
        writer.writeheader()
        writer.writerows(result)
