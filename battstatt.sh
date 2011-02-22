#!/bin/bash
while [ 1 ]; do
    acpi -b | cut -d',' -f2
    sleep 10
done
