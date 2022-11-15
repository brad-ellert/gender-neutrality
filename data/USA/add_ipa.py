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
        if f.startswith('yob') and f.endswith('.txt'):
            print('read')
            with open(f, 'r') as infile:
                reader = csv.DictReader(infile, ['name', 'sex', 'count'])
                l = list(reader)
                print('add columns')
                for d in l:
                    d['year'] = f[3:7]
                    d['IPA'] = eng_to_ipa.convert(d['name'])
                print('add to result')
                result += l

    print('write')
    with open('../USA.csv', 'w') as outfile:
        writer = csv.DictWriter(
            outfile, ['name', 'sex', 'count', 'year', 'IPA'])
        writer.writeheader()
        writer.writerows(result)
