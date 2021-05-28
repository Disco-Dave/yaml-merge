# Yaml Merge
A simple command line tool for combining yaml files.

## How to install
To install `yaml-merge`, clone the repository and run `stack install`. For example
```bash
#!/bin/bash

cd /tmp
git clone https://github.com/Disco-Dave/yaml-merge 
cd yaml-merge
stack install
```

## Motivation and example
The following is a snippet from dotfiles. I built this program to have host specific settings for alacritty.
```bash
#!/bin/bash

path_to_yaml_merge=$(which yaml-merge)

if [ ! -x "$path_to_yaml_merge" ]; then
    >&2 echo "Downloading and installing yaml-merge"

    (
        cd /tmp

        if [ ! -d "yaml-merge" ]; then
            git clone https://github.com/Disco-Dave/yaml-merge 
        fi

        cd yaml-merge
        stack install
    )
fi

host_name_config="$(hostname).yml"

if [ -f "$host_name_config" ]; then
    >&2 echo "Overlaying host specific settings"
    yaml-merge "$host_name_config" base.yml > alacritty.yml
fi
```
