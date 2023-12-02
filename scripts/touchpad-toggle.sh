#!/bin/bash
xinput list-props "ELAN0774:00 04F3:3244 Touchpad" \
    | grep -qE "Device Enabled.*1$" \
    && xinput --set-prop "ELAN0774:00 04F3:3244 Touchpad" "Device Enabled" 0 \
    || xinput --set-prop "ELAN0774:00 04F3:3244 Touchpad" "Device Enabled" 1

