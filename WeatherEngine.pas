unit WeatherEngine;

{$mode objfpc}{$H+}

interface

uses fpHTTPClient, fpJSON, JSONParser;

function getRequest(addr : String) : String;

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

end.
