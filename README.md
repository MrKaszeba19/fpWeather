# fpWeather

**fpweather** – a console application displaying weather information for your city using OpenWeatherMap API

Author: Paul Lipkowski

## Installation

- Required: 
    * FreePascal Compiler `fpc` (version 3.0.4 or newer)
    * an OpenWeather API token (get it [here](https://openweathermap.org/appid) if you don't have one)
- If using Linux, then just compile by executing `compile.sh`
- If using Windows, then compile by executing `compile.bat`
    * The default version of FPC is 3.0.4. If you use the another version of it, then edit the `compile.bat` script and change the setting containing version of FPC (variable `ver`) in order to match your FPC version.
- After compilation you can run `fpweather` to set up the app config file (if it does not exist) 
  
## Usage

- `fpweather` – shows weather of your city using your OpenWeatherMap API token in a compact readable format (*to be implemented*, please use `-j` flag)
- `fpweather [flags]`
    * `-c` or `--config` – Launch config
    * `-h` or `--help` – Display help
    * `-j` or `--json` – Print a raw JSON from OpenWeatherMap
    * `-J` or `--JSON` – Print a more beautified JSON (*to be implemented*; so far it works like `-j`)
    * `-n` or `--no-feed-line` – Don't feed line after the execution of a program 
    * `-u N` or `--units=N` – Change output units (`N = [0, 1, 2, 3]`, see the list of values below)

Available units (`N` values):
- `N = 0` – :united_nations: International SI units (Kelvin and m/s)
- `N = 1` – :eu: Metric units (Celsius and m/s) – to be replaced with Celsius and km/h
- `N = 2` – :us: US Imperial units (Fahrenheit and mph) 
- `N = 3` – :uk: UK Imperial units (Celsius and mph) – so far it works like `N=2`

## More info

- [OpenWeatherMap](https://openweathermap.org/)


