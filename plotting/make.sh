#!/bin/bash

# Load own functions
R --no-restore-data --save --quiet <density.r

# Service rescource plot
R --quiet --restore --no-save <service_resources.r

# Plot
R --quiet --restore --no-save <resource_densities.r
