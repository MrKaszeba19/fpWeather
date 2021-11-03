unit WeatherEngine;

{$mode objfpc}{$H+}

interface

uses 
    fpHTTPClient, fpJSON, JSONParser,
    AppConfig;

function getRequest(addr : String) : String;
function printFormattedJSON(str : String) : String;
function printJSON(str : String) : String;
function printRaw(str : String) : String;
function printInfo(str : String; loc : Locale; dsp : DisplayOptions) : String;

implementation

uses SysUtils;

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

function getRequest(addr : String) : String;
begin
    Result := '{}';
    With TFPHttpClient.Create(Nil) do
        try
            Result := Get(addr);
        finally
            Free;
        end;
end;

function printFormattedJSON(str : String) : String;
begin
    Result := GetJSON(getRequest(str)).FormatJSON;
    //Result := GetJSON(getRequest(str)).AsJSON;
end;

function printJSON(str : String) : String;
begin
    Result := GetJSON(getRequest(str)).AsJSON;
end;

function printRaw(str : String) : String;
begin
    Result := getRequest(str);
end;

function printInfo(str : String; loc : Locale; dsp : DisplayOptions) : String;
var
    jData : TJSONData;
    //jObject : TJSONObject;
    s        : String;
    location : String;
    desc     : String;
    degrees  : Extended;
    pressure : Integer;
    humidity : Integer;
    windspeed : Extended;
    windangle : Integer;
begin
    //Result := 'A more beautified look of the weather info is coming soon! ^_^';
    jData := GetJSON(getRequest(str));
    //location := jData.GetPath('name').AsString;
    //desc := jData.GetPath('weather[0]').GetPath('description').AsString;
    //degrees := jData.GetPath('main').GetPath('temp').AsFloat;
    //pressure := jData.GetPath('main').GetPath('pressure').AsInteger;
    //humidity := jData.GetPath('main').GetPath('humidity').AsInteger;
    //windspeed := jData.GetPath('wind').GetPath('speed').AsFloat;
    //windangle := jData.GetPath('wind').GetPath('deg').AsInteger;
    //Result := 'Now in '+location+': '+#13#10+desc+', '
    //    +FloatToStr(degrees)+loc.DegreesUnit+', '
    //    +Format('%.2f', [(pressure)/loc.PressureCoef])+loc.PressureUnit+', '
    //    +IntToStr(humidity)+'% humid, '
    //    +Format('%.2f', [(windspeed)*loc.SpeedCoef])+loc.SpeedUnit+' '+getWindDirection16(windangle)+' wind.';
    s := '';
    if (dsp.IsLocation) then 
    begin
        location := jData.GetPath('name').AsString;
        s := s + 'Weather in ' + location + ': ' + dsp.TtlSep;
    end;
    if (dsp.IsDescription) then
    begin
        desc := jData.GetPath('weather[0]').GetPath('description').AsString;
        s := s + dsp.OptBullet
               + desc 
               + ', ' + dsp.OptSep;
    end;
    if (dsp.IsTemperature) then
    begin
        degrees := jData.GetPath('main').GetPath('temp').AsFloat;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'temp     ';
        s := s + FloatToStr(degrees)+loc.DegreesUnit
               + ', ';
        if (dsp.IsTempMax) then
        begin
            s := s + dsp.SubSep + dsp.SubBullet;
            if (dsp.SepSetting = 3) then s := s + 'min    ';
            if (dsp.SepSetting = 2) then s := s + 'min ';
            degrees := jData.GetPath('main').GetPath('temp_min').AsFloat;
            s := s + FloatToStr(degrees)+loc.DegreesUnit
               + ', ' + dsp.SubSep;
            s := s + dsp.SubBullet;
            if (dsp.SepSetting = 3) then s := s + 'max    ';
            if (dsp.SepSetting = 2) then s := s + 'max ';
            degrees := jData.GetPath('main').GetPath('temp_max').AsFloat;
            s := s + FloatToStr(degrees)+loc.DegreesUnit
               + ', ';
        end;
        s := s + dsp.OptSep;
    end;
    if (dsp.IsPressure) then
    begin
        pressure := jData.GetPath('main').GetPath('pressure').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'pressure ';
        s := s + Format('%.2f', [(pressure)/loc.PressureCoef])+loc.PressureUnit
               + ', ' + dsp.OptSep;
    end;
    if (dsp.IsHumidity) then
    begin
        humidity := jData.GetPath('main').GetPath('humidity').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'humidity ';
        s := s + IntToStr(humidity)+'%'
               + ', ' + dsp.OptSep;
    end;
    if (dsp.IsWindSpeed) then
    begin
        windspeed := jData.GetPath('wind').GetPath('speed').AsFloat;
        windangle := jData.GetPath('wind').GetPath('deg').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'wind     ';
        s := s + Format('%.2f', [(windspeed)*loc.SpeedCoef])+loc.SpeedUnit;
        if (dsp.IsWindAngle) 
            then s := s + ' '+getWindDirection16(windangle);
        s := s + ', ' + dsp.OptSep;
    end;
    if (dsp.SepSetting > 1) 
        then s := LeftStr(s, Length(s)-3) + '.'
        else if (dsp.SepSetting = 1) 
            then s := LeftStr(s, Length(s)-2) + '.'
            else s := LeftStr(s, Length(s)-2);
    
    Result := s;
end;

end.
