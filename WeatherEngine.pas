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

function checkInternet() : Boolean;

implementation

uses SysUtils, DateParser, VisualHelper;

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
    date     : Integer;
    desc     : String;
    degrees  : Extended;
    pressure : Integer;
    humidity : Integer;
    windspeed : Extended;
    windangle : Integer;
    visibility : Integer;
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
        s := s + 'Weather in ' + location;
        if (dsp.isDate) then 
        begin
            date := jData.GetPath('dt').AsInteger;
            //s := s + ' on ' + getDateFromTimestamp(date);
            s := s + ' on ' + getDateFromTimestamp(date, loc.DateFormat);
        end;
        s := s + ': ' + dsp.TtlSep;
    end else if (dsp.IsDate) then
    begin
        date := jData.GetPath('dt').AsInteger;
        s := getDateFromTimestamp(date, loc.DateFormat) + ': ' + dsp.TtlSep;
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
        if (dsp.SepSetting > 2) then s := s + 'temp       '
        else if (dsp.SepSetting = 2) then s := s + 'temp ';
        s := s + FloatToStr(degrees)+loc.DegreesUnit
               + ', ';
        if (dsp.IsTempMax) then
        begin
            s := s + dsp.SubSep + dsp.SubBullet;
            if (dsp.SepSetting = 4) then s := s + 'min      '
            else if (dsp.SepSetting in [2, 3]) then s := s + 'min ';
            degrees := jData.GetPath('main').GetPath('temp_min').AsFloat;
            s := s + FloatToStr(degrees)+loc.DegreesUnit
               + ', ' + dsp.SubSep;
            s := s + dsp.SubBullet;
            if (dsp.SepSetting = 4) then s := s + 'max      '
            else if (dsp.SepSetting in [2, 3]) then s := s + 'max ';
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
        if (dsp.SepSetting > 1) then s := s + 'pressure   ';
        s := s + Format('%.2f', [(pressure)/loc.PressureCoef])+loc.PressureUnit
               + ', ' + dsp.OptSep;
    end;
    if (dsp.IsHumidity) then
    begin
        humidity := jData.GetPath('main').GetPath('humidity').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'humidity   ';
        s := s + IntToStr(humidity)+'%'
               + ', ' + dsp.OptSep;
    end;
    if (dsp.IsWindSpeed) then
    begin
        windspeed := jData.GetPath('wind').GetPath('speed').AsFloat;
        windangle := jData.GetPath('wind').GetPath('deg').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 2) then s := s + 'wind       '
        else if (dsp.SepSetting = 2) then s := s + 'wind ';
        s := s + Format('%.2f', [(windspeed)*loc.SpeedCoef])+loc.SpeedUnit;
        if (dsp.IsWindAngle) 
            then s := s + ' '+getWindDirection16(windangle);
        s := s + ', ' + dsp.OptSep;
    end;
    if (dsp.IsVisibility) then
    begin
        visibility := jData.GetPath('visibility').AsInteger;
        s := s + dsp.OptBullet;
        if (dsp.SepSetting > 1) then s := s + 'visibility ';
        if ((visibility * loc.DistanceCoef) >= loc.DistanceRatio)
            then s := s + Format('%.2f', [(visibility)*loc.DistanceCoef/loc.DistanceRatio]) + loc.DistanceUnit2
                        + ', ' + dsp.OptSep
            else s := s + Format('%.2f', [(visibility)*loc.DistanceCoef]) + loc.DistanceUnit
                        + ', ' + dsp.OptSep;
    end;
    if (dsp.SepSetting > 2) 
        then s := LeftStr(s, Length(s)-3) + '.'
        else if (dsp.SepSetting in [1, 2]) 
            then s := LeftStr(s, Length(s)-2) + '.'
            else s := LeftStr(s, Length(s)-2);
    
    Result := s;
end;

function checkInternet() : Boolean;
begin
    Result := False;
    try
        if (getRequest('http://www.google.com') <> '') then Result := True;
    except
        on E: Exception do
        begin
            Result := False;
        end;
    end;
end;

end.
