import logging
import os
from os import walk

PWD = os.path.dirname(os.path.realpath(__file__))

def main():
    paths = []
    for (dirpath, dirnames, filenames) in walk(PWD):
        for filename in filenames:
            name_chunks = filename.split('.')
            fullname = os.path.join(dirpath, filename)
            if (
                not name_chunks or
                name_chunks[-1] not in ['html', 'ipynb'] or
                'Rproj' in dirpath or
                (len(name_chunks) > 2 and name_chunks[-2] == 'nb') or
                '.ipynb_checkpoints' in fullname
                ):
                continue
            paths.append(fullname)

    # Sort them
    paths.sort()

    content = """
        <html>
            <ul>
                {}
            </ul>
        <html>
    """.format('\n'.join(
        ['<li><a href="{}">{}</a></li>'.format(path.replace(PWD, '.'), path.replace(PWD, '')) for path in paths]
    ))

    with open(os.path.join(PWD, 'index.html'), 'w') as f:
        f.write(content)

if __name__ == "__main__":
    main()
