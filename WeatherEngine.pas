unit WeatherEngine;

{$mode objfpc}{$H+}

interface

uses fpHTTPClient, fpJSON, JSONParser;

function getRequest(addr : String) : String;
function printFormattedJSON(str : String) : String;
function printJSON(str : String) : String;
function printRaw(str : String) : String;
function printInfo(str : String) : String;

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

function printInfo(str : String) : String;
var
    jData : TJSONData;
    //jObject : TJSONObject;
    location : String;
    desc     : String;
    degrees  : Extended;
    pressure : Integer;
    humidity : Integer;
begin
    //jObject := TJSONObject(GetJSON(getRequest(str)));
    //location := jObject.Get('name');
    //desc := jObject.Get('weather[0]').Get('description');
    jData := GetJSON(getRequest(str));
    location := jData.GetPath('name').AsString;
    desc := jData.GetPath('weather[0]').GetPath('description').AsString;
    degrees := jData.GetPath('main').GetPath('temp').AsFloat;
    pressure := jData.GetPath('main').GetPath('pressure').AsInteger;
    humidity := jData.GetPath('main').GetPath('humidity').AsInteger;
    //Result := 'A more beautified look of the weather info is coming soon! ^_^';
    Result := 'Now in '+location+': '+desc+', '
        +FloatToStr(degrees)+' degrees, '
        +IntToStr(pressure)+' mb, '
        +IntToStr(humidity)+'% humid.';
end;

end.
