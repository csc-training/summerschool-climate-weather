---
title:  Visualization
author: Florian Ziemen / DKRZ
event:  ESiWACE3 and WarmWorld Summer School 2024
lang:   en
---

# Topics today

# What is visualization?
```
    4.779021e-07, 3.000158e-07, 3.943782e-07, 3.010814e-07, 1.824086e-07,
    5.077264e-08, 5.310072e-08, 5.276124e-08, 5.363153e-08, 1.070061e-07,
    6.913928e-08, 1.810711e-07, 9.877869e-08, 1.623726e-08, 4.198973e-08,
    1.642887e-08, 3.282246e-08, 9.109718e-08, 8.140145e-08, 7.397978e-08,
    8.817849e-08, 1.663861e-08, 2.876317e-08, 1.479019e-08, 9.942031e-09,
    2.125612e-08, 1.641772e-08, 2.962805e-08, 2.954731e-08, 3.859089e-08,
    4.320259e-08, 5.651723e-08, 2.374017e-08, 9.255361e-09, 8.066888e-09,
    1.965597e-08, 1.462259e-08, 2.813349e-07, 3.110567e-07, 2.363449e-07,
    2.999532e-07, 2.32825e-07, 2.798415e-07, 2.318629e-07, 2.352825e-07,
    2.309811e-07, 2.338195e-07, 2.171457e-07, 2.598324e-07, 4.017004e-07,
    3.878018e-07, 2.892292e-07, 3.617872e-07, 2.421747e-07, 1.556238e-07,
```

# What is visualization?

![](images/ngc3028_tas.png){width=90%} 

# What is visualization?
![](images/visualization.png){width=100%}


# What are we looking at

* 3D, time varying atmosphere and ocean with many physical / biological properties
* 2D Screen / paper

# Challenges

* Data reduction
* Projection of the sphere onto a flat surface

# Common types of climate visualizations

* Maps
* Sections
* Line plots / scatter plots / ...

# The problem of the peeled orange

* There is no way to simply flatten out a sphere.
* Any projection will distort something.

# Common projections

* Form groups and describe the following projections
  * Plate Carree
  * Mercator
  * Mollweide
  * Robinson
  * Polar stereographic
  
* How are they constructed (if easy), what are they good at? What do they sacrifice?

See [xkcd](https://xkcd.com/977/) for an overview of more projections.

# The issue of too much data

* About 100 variables in a model output dataset
* A 2.5 km resolution simulation has about 320 Mio grid cells per layer.
* Things evolve over time - how do you look at 100 years of simulation?

# What are key variables you would look at? At what frequency / averaging?

# Loading data with xarray and intake catalogs

A minimal version of the global mean surface air temperature plot.

```
import intake
cat = intake.open_catalog("https://data.nextgems-h2020.eu/online.yaml")
cat.ICON.ngc3028.to_dask().tas.mean(dim="cell").plot()
```

![](images/tas_ngc3028_minimal.png){width=30%}


# A minimal map plot

```
import intake
import healpy as hp

cat = intake.open_catalog("https://data.nextgems-h2020.eu/online.yaml")
hp.mollview(cat.ICON.ngc3028.to_dask().tas.isel(time=0), flip='geo', nest=True)
```

![](images/healpix_l0.png){width=40%}

This is a bit coarse. Let's go finer and switch the colormap

# A nicer map plot

```
import intake
import healpy as hp
import matplotlib.pyplot as plt
cat = intake.open_catalog("https://data.nextgems-h2020.eu/online.yaml")
hp.mollview(cat.ICON.ngc3028(zoom=5).to_dask().tas.isel(time=0), flip='geo', nest=True, cmap='inferno')

```

![](images/healpix_l5.png){width=40%}

Much better. :)

# Essential Python Libraries for Data Visualization

* [intake](https://intake.readthedocs.io/en/latest/) and [xarray](https://docs.xarray.dev/en/stable/) - loading data
* [numpy](https://numpy.org/doc/stable/) - efficient number crunching
* [matplotlib](https://matplotlib.org/) & [cartopy](https://scitools.org.uk/cartopy/docs/latest/) - basic plots
* [seaborn](https://seaborn.pydata.org/) - nicer visualizations


# Line plots and friends

* Matplotlib and Seaborn

# Maps and Projections

# Other plots (sections / means)


# Image and data resolution


# A first map plot

# How to render for performance

# Summary



# Filtering and subsetting data
- Zonal and meridional means


# Dask and chunking
- Using Dask for parallel computing
- Caching results

# Exploratory Data Analysis (EDA)
- Summary statistics
- Visualizing distributions
- Identifying patterns and anomalies

# Introduction to Matplotlib
- Basic plotting functions
- Customizing plots (titles, labels, legends)
- Saving and exporting plots

# Creating Line Plots and Time Series Visualizations
- Plotting time series data
- Adding trend lines and error bands
- Customizing time series plots

# Generating Scatter Plots and Correlation Analysis
- Creating scatter plots
- Visualizing correlations
- Adding regression lines and confidence intervals

# Using Seaborn for Statistical Visualizations
- Overview of Seaborn
- Creating advanced statistical plots
- Customizing Seaborn plots

# Interactive Visualizations with Plotly
- Introduction to Plotly
- Creating interactive plots
- Embedding interactive plots in Jupyter Notebooks

# Advanced Geospatial Visualizations with Cartopy
- Overview of Cartopy
- Creating advanced geospatial plots
- Customizing Cartopy maps

# Visualizing Climate Anomalies
- What are climate anomalies?
- Techniques for visualizing anomalies
- Case study: Visualizing temperature anomalies

# Creating Climate Model Comparison Plots
- Comparing outputs from different models
- Visualizing ensemble means and spread
- Creating multi-panel plots for comparison

# Generating Ensemble Plots
- What are ensemble plots?
- Techniques for creating ensemble plots
- Customizing ensemble visualizations

# Animating Climate Data Over Time
- Creating animations with Matplotlib and Plotly
- Techniques for temporal data animation
- Exporting animations as GIFs or videos

# Case Study 1: Visualizing Temperature Changes
- Dataset introduction
- Step-by-step visualization process
- Key insights and interpretations

# Case Study 2: Precipitation Patterns and Trends
- Dataset introduction
- Step-by-step visualization process
- Key insights and interpretations

# Case Study 3: Sea Level Rise Visualization
- Dataset introduction
- Step-by-step visualization process
- Key insights and interpretations

# Best Practices for Effective Data Visualization
- Choosing the right visualization type
- Ensuring clarity and readability
- Using color effectively

# Common Pitfalls and How to Avoid Them
- Avoiding misleading visualizations
- Handling outliers and missing data
- Ensuring reproducibility and transparency

# Optimizing Performance for Large Visualizations
- Techniques for improving performance
- Using efficient data structures
- Parallel processing and optimization tools

# Designing for Clarity and Impact
- Principles of good design
- Tips for creating impactful visualizations
- Examples of effective climate visualizations

# Review of Key Concepts and Techniques
- Summary of main points
- Key takeaways
- Recap of visualization techniques

# Additional Resources and Further Learning
- Recommended books and articles
- Online courses and tutorials
- Useful libraries and tools

# Q&A Session
- Open floor for questions
- Discussion and clarifications
- Sharing additional insights and tips

# Feedback and Next Steps
- Gathering participant feedback
- Discussing next steps and follow-up activities
- Thank you and closing remarks

# Hands-on Exercise 1: Visualizing Your Own Data
- Instructions for the exercise
- Dataset recommendations
- Step-by-step guide

# Hands-on Exercise 2: Customizing Plots and Interactivity
- Instructions for the exercise
- Techniques for customization
- Step-by-step guide

# Jupyter notebooks and *plain* python scripts
* Jupyter keeps the python running.
  * Makes it easy to rapidly prototype.
  * Typically one notebook per analysis.
* Plain python is more helpful for building modular code.
  * Tendency towards building libraries.
  * Scripts that can more easily be used in batch mode.
* [Papermill](https://github.com/nteract/papermill) allows to use notebooks as scripts.
