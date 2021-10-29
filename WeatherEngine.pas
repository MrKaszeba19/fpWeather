unit WeatherEngine;

{$mode objfpc}{$H+}

interface

uses fpHTTPClient, fpJSON, JSONParser;

function getRequest(addr : String) : String;
function printJSON(str : String) : String;
function printFormattedJSON(str : String) : String;

implementation

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
end;

function printJSON(str : String) : String;
begin
    Result := getRequest(str);
end;

end.
