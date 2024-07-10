---
title:  The ICOsahedral Nonhydrostatic model (ICON)
event:  ESiWACE3-WarmWorld Summer School on HPC for Climate and Weather Applications
author: Florian Ziemen and Claudia Frauen
lang:   en
---

# What is ICON?

![](img/ICON_logo_black.jpg){.center width=15%}

- ICON stands for ICOsahedral Nonhydrostatic model and is a weather and climate model with atmosphere, ocean and land components
- It's development started more than two decades ago; initially by the German Weather Service (DWD) and the Max Planck Institute for Meterology (MPI-M), later joined by the Karlsruhe Institute for Technology (KIT) and the German Climate Computing Center (DKRZ) and latest the Swiss Center for Climate Systems Modeling (C2SM)
- Since January 2024 it is available under an open source license via https://www.icon-model.org/

# What are the components of ICON?

![](img/csm_ICON_model_9c699406b8.jpg){.center width=35%}
Credit: Max Planck Institute for Meteorology (https://mpimet.mpg.de/en/research/modeling)

# What is ICON used for?

- ICON is used for applications ranging from numerical weather prediction to CMIP-type climate simulations to km-scale climate simulations

# ICON NWP

- Used for operational numerical weather prediction at DWD since 2015
- Other national weather services are using it as well; the latest being Meteo Swiss since June 2024

- DWD setup: 13 km global, 7 km over Europe, 2 km over Germany and surroundings (nested)


# ICON ESM

- Coupled model setup consisting of atmosphere (horizontal resolution 160km, 47 vertical level), ocean (horizontal resolution of 40km, 64 vertical level), land, and optionally ocean biogeochemistry
- Without ocean biogeochemistry it achieved a throughput of ~120 simulated years per day on 120 nodes on the DKRZ system Mistral (predecessor of Levante)
- see also [Jungclaus et al., 2022](https://doi.org/10.1029/2021MS002813)

# ICON at km-scales

- ICON is also one of few models worldwide that can be used for coupled simulations at km-scale allowing to directly simulate smaller scale features like storms or ocean eddies (see also [Hohenegger et al., 2023](https://doi.org/10.5194/gmd-16-779-2023))
- In the NextGEMS project a 30-year simulation was performed with the coupled model with atmosphere (10km horizontal resolution, 90 vertical level), ocean (5km resolution, 72 vertical level) and land components
- It achieved a throughput of ~1 simulated year per day on 472 nodes of Levante at DKRZ

# How is ICON programmed?

- ICON is written mosten in Fortran and consists of more than 1 million lines of code
- It uses a hybrid MPI/OpenMP approach for parallelisation
- The atmosphere and land components have been ported to GPUs using OpenACC directives
- Work is ongoing to port the remaining components

# What hardware can ICON run on?

* ICON is running and regularly tested on a variety of hardware architectures and can in principal run on everything from a laptop to the largest supercomputers in the world
* Standard CPU-systems like Levante (DKRZ)
* GPU-based systems (atmosphere and land) like Levante, Alps (CSCS, Meteo Swiss), LUMI (CSC) and JUWELS Booster (JSC)
* NEC vector (DWD)

# Where does the compute time go?

Assuming same resolutions in atmosphere and ocean:

* 98 % atmosphere and land
* 2 % ocean

Every doubling of the resolution costs a factor of 8 in compute time. 4 times as many grid cells, and half the time step length, as the wind/currents must not pass more than one grid cell per timestep (CFL, the numerics will blow up fast).

nextGEMS R2B8/R2B9: 84% atmosphere, 14% ocean, 2.5% output

# The ICON model grid

![](img/icon_grids.png){.center width=35%}

- Base structure: Icosahedron (20 triangles) covering Earth
- Subdivisions into smaller triangles

- R**X**B**Y** notation 
  - **X** being the number of pieces the edges of the original triangles are divided into 
  - **Y** the number of follow-up Bisections. 

# The ICON MODEL grid

![](img/R2B2.jpg){width=60%}

# The vertical coordinates

![](img/vert-grid.jpg){width=60%}

# The grid cell

![](img/cells.jpg)

# The ICON atmosphere time loop

![](img/icon_time_step.jpg)

# Examples

