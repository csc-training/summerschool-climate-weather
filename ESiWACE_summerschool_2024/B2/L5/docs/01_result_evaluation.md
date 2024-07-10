---
title:  ESM result evaluation
author: Florian Ziemen / DKRZ, Victoria Sinclair / University of Helsinki
event:  ESiWACE3 and WarmWorld Summer School 2024
lang:   en
---

#

All models are wrong, but some are useful.

George Box, 1976

# What is our model good at?

* We want to use the model to learn about the climate system.
* We might want to learn about the future, where there are no observations.
* We might want to learn about the past, where observations are scarce.
* Where can we trust the model / where rather not?

# The difficulty of comparing to reality
* There is substantial climate variability, even on longer time scales.
* We don't know for which quantity our reality is on which end of the variability spectrum.
* We don't know for which quantity our simulation is on which end of the variability spectrum.
* We call the path of a simulation through the space of possible weathers the **trajectory**.

# The free-running model
* We don't expect the model to get the weather right, but the climate, i.e. the **statistics** of the weather.
* We can't directly compare to weather observations.
* Climate is changing fast.

What should we do?

# The free-running model
* We don't expect the model to get the weather right, but the climate, i.e. the **statistics** of the weather.
* We can't directly compare to weather observations.
* Climate is changing fast.
* We can verify conservation laws, etc.
* We can analyze the behavior.
* We can compare statistics to observations.
* We can compare statistics to those of other models.

# Topics of this talk

* Analyzing the model itself.
* Direct comparison to reanalysis

# Analyzing the model itself {.section}

# Verifying conservation laws

# Verifying conservation laws
* Conservation of mass
  * Precipitation - evaporation
  * Ocean water and salt
* Conservation of energy
  * TOA radiation imbalance vs imbalance at surface
  * Ocean energy balance
  * Regional budgets are possible but messy regarding advection.
  
# Checking conservation in timeseries plots
[nextgems prefinal ICON](https://swift.dkrz.de/v1/dkrz_b381d76e-63d7-4aeb-96f0-dfd91e102d40/nextgems_prefinal/ngc4008/index.html)


# Analyzing model behavior from animations

Funky features in the model
[ ICON sea ice dynamics bug , see ngc4008_mov_np_conc.mp4](https://owncloud.gwdg.de/index.php/s/jg6fusRH9FO2ojO#/files_mediaviewer/ngc4008_mov_np_conc.mp4)

# Finding strong drift in components in time series plots

[ ICON test run cooling ](https://swift.dkrz.de/v1/dkrz_34406075a1684be9b56a2669a90730f2/nextgems-tmx-tuning/ngc3-tmx001/ngc3-tmx001_atm_mon.html)


# What can we compare to?

# What can we compare to?
* Individual measurements (difficult, c.f. statistics)
* Satellite data (same issue)
* Reanalyses
* Other models and other runs with the same model (but what is *correct*?)

# Comparing to reanalysis {.section}


# Comparing to reanalysis

Let's load some model and ERA5 data

```
import intake
import healpy as hp
import matplotlib.pyplot as plt
from easygems.healpix import attach_coords

hera_cat = intake.open_catalog("https://tcodata.mpimet.mpg.de/internal.yaml")
hera = hera_cat.HERA5.to_dask().pipe(attach_coords)
# Contains modified Copernicus Climate Change Service information 2023. 
# Neither the European Commission nor ECMWF is responsible for any use 
# that may be made of the Copernicus information or data it contains.
# This dataset is on healpix level 7

zoom = 7 
cat = intake.open_catalog("https://data.nextgems-h2020.eu/online.yaml")
icon = cat.ICON.ngc3028(zoom=zoom).to_dask().pipe(attach_coords)

```

# Select the same time slice

```
timeslice = slice("2020-02-01", "2023-01-31")
t_mean_model = icon.sel(time=timeslice).tas.mean(dim="time")
t_mean_obs = hera.sel(time=timeslice)["2t"].mean(dim="time") 
```

# A side-by-side plot

```
params = dict(flip='geo', nest=True, cmap='inferno', min=216, max=304)
hp.mollview(t_mean_model, **params)
plt.title("model")
plt.savefig("images/model_tas.png")
hp.mollview(t_mean_obs, **params)
plt.title("ERA5")
plt.savefig("images/era_tas.png")
```

![](images/model_tas.png){width=40%} ![](images/era_tas.png){width=40%}


# Taking the difference

```
params_diff = dict(cmap="RdBu_r", min=-5, max=5, flip="geo", nest=True)
hp.mollview(t_mean_model-t_mean_obs, **params_diff)
plt.title("Model-ERA5")
```
![](images/t_mean_diff.png)

# Is there a trend?

Get global mean temperature time series
```
icon_tas_ts=cat.ICON.ngc3028.to_dask().tas.mean(dim='cell')
timeslice = slice(icon_tas_ts.time[0],icon_tas_ts.time[-1])
hera_tas_ts=hera['2t'].sel(time=timeslice).mean(dim='cell')
```
# Is there a trend?
And plot them
```
import seaborn as sns
sns.set_theme()
plt.plot(hera_tas_ts.time, hera_tas_ts, label="ERA5")
plt.plot(icon_tas_ts.time, icon_tas_ts, label="ICON")
plt.legend()
```
![](images/tas_ts_comparison.png)



# Comparing precipitation
```
pr_mean_model = icon.sel(time=timeslice)['pr'].mean(dim="time")
pr_mean_obs = hera.sel(time=timeslice)["tp"].mean(dim="time")

params_pr_diff = dict(cmap="BrBG", flip="geo", nest=True)
hp.mollview(pr_mean_model-pr_mean_obs, **params_pr_diff)
plt.title("Model-ERA5")
```
![This looks wrong](images/pr_mean_diff_wrong.png){width=40%}

# What are the units?

```
print ( f'{icon.pr.attrs["units"]=}\n{hera.tp.attrs["units"]=}')

icon.pr.attrs["units"]='kg m-2 s-1'
hera.tp.attrs["units"]='m/d'
```

# New attempt

```
pr_mean_model = icon.sel(time=timeslice)['pr'].mean(dim="time")
pr_mean_obs = hera.sel(time=timeslice)["tp"].mean(dim="time")
# Technically we'd have to use daily means to avoid the error of computing a longer mean
# from 12 equally-weighted monthly means.

params_pr_diff = dict(cmap="BrBG", flip="geo", nest=True)
hp.mollview(pr_mean_model*86400/1000.-pr_mean_obs, **params_pr_diff)
plt.title("precipitation Model-ERA5 (m/d)")
hp.mollview(pr_mean_model*86400/1000, **params_pr_diff, min=-.01, max=.01) # for comparison
plt.title('Model precipitation (m/d)')
```
![Much better](images/pr_mean_diff.png){width=30%} ![For reference](images/pr_mean_model.png){width=30%}

# Wind speed zonal mean section

Problem: The models have different vertical grids. The ICON output is on model levels, while (H)ERA is on pressure levels.


Solution: Vertical interpolation to the pressure levels of HERA.

# Wind speed zonal mean section

Solution: Vertical interpolation to the pressure levels of HERA.

```python
def hacky_linear_interpolation(data, target_grid, target, dim="level_full"):
    from collections.abc import Iterable   
    
    if isinstance(target, Iterable) and len(target) > 1:
        return np.array([hacky_linear_interpolation(data, target_grid, y, dim) for y in target ])
    else:
        level_above = (target_grid > target).argmax(dim=dim)
        level_below = level_above - 1
        value_above = target_grid.isel(**{dim: level_above})
        value_below = target_grid.isel(**{dim: level_below})
        f = (target - value_below) / (value_above - value_below)
        interpolated =  (1-f) * data.isel(**{dim: level_below}) + f * data.isel(**{dim: level_above})
        return np.where(level_above > 0, interpolated, np.nan)
```

# Wind speed zonal mean

```
interpolated = hacky_linear_interpolation(
    icon.ua.sel(time=timeslice).mean(dim='time').compute(), 
    icon.pfull.sel(time=timeslice).mean(dim='time').compute(), 
    target*100)
icon_on_pl = xr.Dataset(
    dict(level=target,
         crs = icon.crs, 
         ua = xr.DataArray(interpolated, 
                           name='ua', 
                           attrs = dict(grid_mapping = 'crs'), 
                           dims=('level','cell')))).pipe(attach_coords)
icon_ua = icon_on_pl.ua.groupby('lat').mean().compute()
hera_ua = hera.u.sel(time=timeslice).mean(dim="time").groupby('lat').mean().compute()
```

# Wind speed zonal mean

```
plt.imshow(icon_ua-hera_ua, aspect='auto', cmap='RdBu', vmin=-10, vmax=10, interpolation='nearest')
plt.colorbar(label="zonal wind speed ICON-ERA (m/s)")
plt.contour(hera_ua, vmin=-40, vmax=40, colors='white')
plt.contour(hera_ua, cmap="RdBu", vmin=-40, vmax=40, linestyles='dashed')
plt.colorbar(label="zonal wind speed in ERA5 (m/s)")
yticks = range (0, len(target), 5)
plt.yticks( yticks, [str(target[x]) for x in yticks] )
plt.ylabel('hPa')
xticks = (range(0, len(hera_ua.lat), len(hera_ua.lat)//6))
plt.xticks(xticks, [f'{hera_ua.lat.values[x]:.0f}' for x in xticks] )
plt.xlabel('deg N')
```
![](images/ua-diff-vs-obs.png){width=20%}


# Climatologies
* The same day / month averaged across many years

Why would we do that?

# Climatologies
* The same day / month averaged across many years
* Allows to study the annual cycle.
* Removes most of the interannual variability.

# The power of SMILE

Comparing a single model initial--condition large ensemble to observations

* Tackles the problem of variability by providing an envelope of runs.
* Standard in weather forecasting.
* Many identical simulations with slightly varying initial conditions
* Ideally, for a long-running ensemble, reality should sometimes be at the upper end, sometimes at the lower end, and most times somewhere within the zoo of simulations. 
* see [Hamill, 2001](https://doi.org/10.1175/1520-0493(2001)129%3C0550:IORHFV%3E2.0.CO;2); [Suarez-Gutierrez, Milinski, Maher, 2021](https://link.springer.com/article/10.1007/s00382-021-05821-w) for details.

# Comparing to other simulations with the same or other models

* Shows outliers that call for more attention.
* Gives an estimate of certainty in simulations.
* Can show agreement because of common biases.
