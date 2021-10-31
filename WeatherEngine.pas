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
function printInfo(str : String; loc : Locale) : String;

implementation

uses SysUtils;

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

function printInfo(str : String; loc : Locale) : String;
var
    jData : TJSONData;
    //jObject : TJSONObject;
    location : String;
    desc     : String;
    degrees  : Extended;
    pressure : Integer;
    humidity : Integer;
    windspeed : Extended;
begin
    //Result := 'A more beautified look of the weather info is coming soon! ^_^';
    jData := GetJSON(getRequest(str));
    location := jData.GetPath('name').AsString;
    desc := jData.GetPath('weather[0]').GetPath('description').AsString;
    degrees := jData.GetPath('main').GetPath('temp').AsFloat;
    pressure := jData.GetPath('main').GetPath('pressure').AsInteger;
    humidity := jData.GetPath('main').GetPath('humidity').AsInteger;
    windspeed := jData.GetPath('wind').GetPath('speed').AsFloat;
    Result := 'Now in '+location+': '+desc+', '
        +FloatToStr(degrees)+loc.DegreesUnit+', '
        +Format('%.2f', [(pressure)/loc.PressureCoef])+loc.PressureUnit+', '
        +'humidity '+IntToStr(humidity)+'%, '
        +'wind speed '+Format('%.2f', [(windspeed)*loc.SpeedCoef])+loc.SpeedUnit+'.';
end;

end.
