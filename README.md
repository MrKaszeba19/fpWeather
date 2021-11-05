# fpWeather

**fpweather** – a console application displaying weather information for your city using OpenWeatherMap API

Author: Paul Lipkowski

## Installation

- Required: 
    * FreePascal Compiler `fpc` (version 3.0.4 or newer)
    * an OpenWeather API token (get it [here](https://openweathermap.org/appid) if you don't have one)
- If using Linux, then just compile by executing `compile.sh`
    * Then you can install fpweather to `$PATH` using `installBash.sh`
- If using Windows, then compile by executing `compile.bat`
    * The default version of FPC is 3.0.4. If you use the another version of it, then edit the `compile.bat` script and change the setting containing version of FPC (variable `ver`) in order to match your FPC version.
- After compilation you can run `fpweather` to set up the app config file (if it does not exist) 
  
## Usage

### Overview

- `fpweather` – shows weather of your city using your OpenWeatherMap API token in a compact readable format 
- `fpweather [flags]`
    * `-c` or `--config` – Launch config
    * `-e` or `--emoji` – Display emojis in output (*to be implemented*)
    * `-h` or `--help` – Display help
    * `-j` or `--json` – Print a flat JSON (*to be improved*)
    * `-J` or `--JSON` – Print a beautified JSON (*to be improved*)
    * `-l` or `--location` – Overwrite the city for the particular program execution
    * `-n` or `--no-feed-line` – Don't feed the line after the execution of the program 
    * `-o S` or `--output=S` – Adjust the output data you want to be displayed
    * `--raw-json` – Print a raw JSON from OpenWeatherMap (it returns the result in international SI units only)
    * `-s N` or `--style=N` – Determine the style of the output
    * `-t N` or `--time=N` – Display weather data for the `N`th day after today (*to be implemented*, `N` may be negative, so then it would display weather from `N` days ago)
    * `-T S` or `--token=S` – Overwrite the OpenWeather API token for the particular program execution
    * `-u N` or `--units=N` – Change output units (`N = [0, 1, 2, 3]`, see the list of values below)

**Notes**:
- Cities with names containing spaces must be quoted, e.g. `-l "Tel Aviv"` or `--location="Bnei Brak"`

### Available displayable output data

#### Customized setups for output
Available chars for `S` string used in the `-o`/`--output` flag: (chars may be concatenated)
- `+` – add default settings (equivalent of `S = abcdefg`)
- `a` – add location
- `b` – add date (*to be implemented*)
- `c` – add weather description
- `d` – add current temperature
- `D` – add current temperature and also the lowest and the highest temperature for the day
- `e` – add atmospheric pressure
- `f` – add humidity
- `g` – add wind speed
- `G` – add wind speed and direction
- `h` – add visibility distance (*to be implemented*)

#### Preset output setups
You can use one of the following options for preset output setups:
- `S = default` is an equivalent of `S = abcdefg` or `S = +`, i.e. default display
- `S = basic` is an equivalent of `S = acdef`
- `S = medium` is an equivalent of `S = abcdefgh` or `S = +h`
- `S = high` is an equivalent of `S = abcdefGh` or `S = +Gh`
- `S = full` is an equivalent of `S = abcDefGh` or `S = +DGh`, i.e. full display

### Available line break styles
`N` values for `-s`/`--style` flag:
- `N = 0` – print flat string
- `N = 1` – print flat output, except for location being in a separate line (**default**)
- `N = 2` – same as `N = 1`, but with labels
- `N = 3` – print every value in a separate line (a list of values)
- `N = 4` – print every value in a separate line, and all its subvalues as well

### Available locales 
`N` values for `-u`/`--units` flag:
- `N = 0` – :united_nations: International SI system (Kelvin, m/s and hPa)
- `N = 1` – :eu: Metric units (Celsius, km/h and hPa; m/s on JSON output)
- `N = 2` – :us: US units (Fahrenheit, mph and psi; hPa on JSON output) 
- `N = 3` – :uk: UK units (Celsius, mph and mb) – so far it works like `N=1` on JSON output

## Examples

- `fpweather` shows the current weather of your city using your OpenWeatherMap API token in a compact readable format 
- `fpweather -c` launches config
- `fpweather -l "Tel Aviv"` shows the current weather in Tel Aviv, IL.
- `fpweather -u 3` displays weather info of your city using the British locale (Celsius, mph, millibars) 
- `fpweather -s 4 -u 1 -l Skopje` displays weather of Skopje in a bulleted list using Metric units
- `fpweather -s 4 -u 1 -o +DG -l Bydgoszcz` displays weather of Bydgoszcz in a bulleted list using Metric units and adds both wind direction and min/max temperature
- `fpweather --style=3 --units=1 --output=+DG --location=Bydgoszcz` does the same as above
- `fpweather -s 0 -u 1 -o D -l Gdynia` displays the full temperature info of Gdynia in a flat string using Metric units
- `fpweather -s 4 -u 1 -o +DGh -l Gdynia` displays the full info of Gdynia in a flat string using Metric units
- `fpweather -s 4 -u 1 -o full -l Gdynia` does the same as above

## More info

- [OpenWeatherMap](https://openweathermap.org/)


