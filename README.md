# fpWeather

**fpweather** – a console application displaying weather information for your city using OpenWeatherMap API

Author: Paul Lipkowski

## Installation

- Required: 
    * FreePascal Compiler `fpc` (version 3.0.4 or newer)
    * an OpenWeather API token (get it [here](https://openweathermap.org/appid))
- If using Linux, then just compile by executing `compile.sh`
- If using Windows, then compile by executing `compile.bat`
    * The default version of FPC is 3.0.4. If you use the another version of it, then edit the `compile.bat` script and change the setting containing version of FPC (variable `ver`) in order to match your FPC version.
- After compilation you can run `fpweather` to set up the app config file (if it does not exist) 
  
## Usage

- `fpweather` – shows weather of your city using your OpenWeatherMap API
- `fpweather [flags]`
    * `-c` or `--config` : Launch config
    * `-h` or `--help` : Display help

## More info

- [OpenWeatherMap](https://openweathermap.org/)


