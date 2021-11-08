unit VisualHelper;

{$mode objfpc}{$H+}

interface

function getWindDirection16(angle : Integer) : String;

implementation

// haze 🌫️
// rain 🌧️
// moderate rain 🌧️
// light snow 🌩️
// drizzle ❄️❄️ 
// snow ❄️
// light snow 🌨️
// scattered clouds ⛅
// broken clouds ⛅
// overcast clouds ☁️
// light rain 🌧️
// rain ⛆
// hail ⛆
// storm 🌩️
// clear sky ☀️
// hot 🌡️
// cold 🥶
// ice 🧊

function isInInterval(x : Integer; a1, a2 : Extended) : Boolean;
begin
    Result := (x >= a1) and (x <= a2);
end;

function getWindDirection16(angle : Integer) : String;
begin
         if isInInterval(angle, -11.25,  11.25) then Result := 'N'
    else if isInInterval(angle,  11.25,  33.75) then Result := 'NNE'
    else if isInInterval(angle,  33.75,  56.25) then Result := 'NE'
    else if isInInterval(angle,  56.25,  78.75) then Result := 'ENE'
    else if isInInterval(angle,  78.75, 101.25) then Result := 'E'
    else if isInInterval(angle, 101.25, 123.75) then Result := 'ESE'
    else if isInInterval(angle, 123.75, 146.25) then Result := 'SE'
    else if isInInterval(angle, 146.25, 168.75) then Result := 'SSE'
    else if isInInterval(angle, 168.75, 191.25) then Result := 'S'
    else if isInInterval(angle, 191.25, 213.75) then Result := 'SSW'
    else if isInInterval(angle, 213.75, 236.25) then Result := 'SW'
    else if isInInterval(angle, 236.25, 258.75) then Result := 'WSW'
    else if isInInterval(angle, 258.75, 281.25) then Result := 'W'
    else if isInInterval(angle, 281.25, 303.75) then Result := 'WNW'
    else if isInInterval(angle, 303.75, 326.25) then Result := 'NW'
    else if isInInterval(angle, 326.25, 348.75) then Result := 'NNW'
    else Result := 'N';
end;

end.
