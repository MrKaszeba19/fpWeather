unit AppConfig;

{$mode objfpc}{$H+}

interface

uses SysUtils;

procedure doConfig(var tok : String; var loc : String);
function configExists() : Boolean;

implementation

procedure setUp(tok : String; loc : String);
var
    dir   : String;
    isdir : Boolean;
    fl    : Text;
begin
    isdir := false;
    dir := GetAppConfigDir(false)+'/';
    if not DirectoryExists(dir) then
    begin
        if not CreateDir(dir) then
        begin
            writeln('Failed to set up a directory for the config file.');
            isDir := false;  
        end else begin
            isDir := true;
        end;
    end else begin
        isDir := true;
    end;
    if (isDir) then
    begin
        assignfile(fl, dir+'cfg');
        rewrite(fl);
        writeln(fl, tok);
        writeln(fl, loc);
        closefile(fl);
    end;
end;

procedure doConfig(var tok : String; var loc : String);
begin
    writeln('fpWeather Config');
    write('Type your OpenWeatherApp API token: ');
    readln(tok);
    write('Type your city (in English):        ');
    readln(loc);
    setUp(tok, loc);
end;

function configExists() : Boolean;
begin
    Result := FileExists(GetAppConfigDir(false)+'/cfg');
end;

end.
