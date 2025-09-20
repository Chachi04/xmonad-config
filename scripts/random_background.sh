#!/bin/bash

wallpapers=()

while IFS= read -r line; do
    name=$(echo "$line" | awk '{print $1}')
    res=$(echo "$line" | grep -oP '[0-9]+x[0-9]+(?=\+)')
    width=$(echo "$res" | cut -d'x' -f1)
    height=$(echo "$res" | cut -d'x' -f2)

    if [[ -z "$res" ]]; then
        echo "No resolution found for ${name}"
        continue  # skip lines without a resolution
    fi

    if (( width < height )); then
        # Portrait mode
        echo "${name} is in portrait"
        wp=$(find /home/chachi/Documents/personal/wallpapers/vertical/ -type f | shuf -n 1)
    else
        # Landscape mode
        echo "${name} is in landscape"
        wp=$(find /home/chachi/Documents/personal/wallpapers/landscape/ -type f | shuf -n 1)
    fi

    wallpapers+=("$wp")
done < <(xrandr --query --verbose | grep ' connected')

feh --bg-fill "${wallpapers[@]}"

