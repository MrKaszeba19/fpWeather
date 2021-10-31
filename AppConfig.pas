unit AppConfig;

{$mode objfpc}{$H+}

interface

uses SysUtils;

{$IFDEF WINDOWS}
const CATSLASH = '\';
{$ELSE}
const CATSLASH = '/';
{$ENDIF}


type Locale = record
    Name         : String;
    Param        : String;
    DegreesUnit  : String;
    PressureUnit : String;
    PressureCoef : Extended; 
    SpeedUnit    : String;
    SpeedCoef    : Extended;
end;

procedure raiserror(Const msg : string);  
procedure setConfig(var tok : String; var loc : String; var units : Integer);
procedure getConfig(var tok : String; var loc : String; var units : Integer);
function configExists() : Boolean;
function getUnits(x : Integer) : String;
function getLocale(x : Integer) : Locale;

implementation

procedure raiserror(Const msg : string);  
begin  
  raise exception.create(Msg) at  
  get_caller_addr(get_frame),  
  get_caller_frame(get_frame);  
end; 

// https://stackoverflow.com/questions/65016942/freepascal-how-can-i-copy-a-file-from-one-location-and-paste-it-in-another
function CopyFile(const SrcFileName, DstFileName: AnsiString): Boolean;
var
    Src, Dst: File;
    Buf: array of Byte;
    ReadBytes: Int64;
begin
    Assign(Src, SrcFileName);
    {$PUSH}{$I-}
    Reset(Src, 1);
    {$POP}
    if IOResult <> 0 then
        Exit(False);

    Assign(Dst, DstFileName);
    {$PUSH}{$I-}
    Rewrite(Dst, 1);
    {$POP}
    if IOResult <> 0 then begin
        Close(Src);
        Exit(False);
    end;

    SetLength(Buf, 64 * 1024 * 1024);
    while not Eof(Src) do begin
        {$PUSH}{$I-}
        BlockRead(Src, Buf[0], Length(Buf), ReadBytes);
        {$POP}
        if IOResult <> 0 then begin
            Close(Src);
            Close(Dst);
            Exit(False);
        end;
        {$PUSH}{$I-}
        BlockWrite(Dst, Buf[0], ReadBytes);
        {$POP}
        if IOResult <> 0 then begin
            Close(Src);
            Close(Dst);
            Exit(False);
        end;
    end;
    Close(Src);
    Close(Dst);
    Exit(True);
end;

// locales

function LocaleSI() : Locale;
begin
    Result.Name         := 'SI International';
    Result.Param        := 'standard';
    Result.DegreesUnit  := ' K';
    Result.PressureUnit := ' hPa';
    Result.PressureCoef := 1.0; 
    Result.SpeedUnit    := ' m/s';
    Result.SpeedCoef    := 1.0;
end;

function LocaleEU() : Locale;
begin
    Result.Name         := 'EU Metric';
    Result.Param        := 'metric';
    Result.DegreesUnit  := '°C';
    Result.PressureUnit := ' hPa';
    Result.PressureCoef := 1.0; 
    Result.SpeedUnit    := ' km/h';
    Result.SpeedCoef    := 3.6;
end;

function LocaleUS() : Locale;
begin
    Result.Name         := 'US Imperial';
    Result.Param        := 'imperial';
    Result.DegreesUnit  := '°F';
    Result.PressureUnit := ' psi';
    Result.PressureCoef := 68.9475728; 
    Result.SpeedUnit    := ' mph';
    Result.SpeedCoef    := 1.0;
end;

function LocaleUK() : Locale;
begin
    Result.Name         := 'UK Imperial';
    Result.Param        := 'metric';
    Result.DegreesUnit  := '°C';
    Result.PressureUnit := ' mb';
    Result.PressureCoef := 1.0; 
    Result.SpeedUnit    := ' mph';
    Result.SpeedCoef    := 2.23693629;
end;


//

procedure setUp(tok : String; loc : String; var units : Integer);
var
    dir   : String;
    isdir : Boolean;
    fl    : Text;
    ok    : Boolean;
begin
    isdir := false;
    dir := GetAppConfigDir(false)+CATSLASH;
    if not DirectoryExists(dir) then
    begin
        if not CreateDir(dir) then
        begin
            raiserror('Error: Failed to set up a directory for the config file.');
            isDir := false;  
        end else begin
            isDir := true;
        end;
    end else begin
        isDir := true;
    end;
    if (isDir) then
    begin
        ok := CopyFile(dir+'cfg', dir+'cfg.bak');
        assignfile(fl, dir+'cfg');
        rewrite(fl);
        writeln(fl, tok);
        writeln(fl, loc);
        writeln(fl, units);
        closefile(fl);
    end;
end;

function getUnits(x : Integer) : String;
begin
    case x of
        0 : Result := 'standard';
        1 : Result := 'metric';
        2 : Result := 'imperial';
        3 : Result := 'metric';
        else Result := 'standard';
    end;
end;

function getLocale(x : Integer) : Locale;
begin
    case x of
        0 : Result := LocaleSI();
        1 : Result := LocaleEU();
        2 : Result := LocaleUS();
        3 : Result := LocaleUK();
        else Result := LocaleSI();
    end;
end;

procedure setConfig(var tok : String; var loc : String; var units : Integer);
var
    guess : Integer;
begin
    writeln('fpWeather Config');
    write('Type your OpenWeatherApp API token: ');
    readln(tok);
    write('Type your city (in English):        ');
    readln(loc);
    repeat
        writeln('Preferable locale:                 ');
        writeln('    0 - SI (Kelvin, m/s, YMD)');
        //writeln('    1 - Metric (Celsius, km/h)');
        writeln('    1 - Metric (Celsius, m/s, DMY)');
        writeln('    2 - US Imperial (Fahrenheit, mph, MDY)');
        writeln('    3 - UK Imperial (Celsius, mph, DMY)');
        write('Your choice: ');
        readln(guess);
    until guess in [0, 1, 2, 3];
    Units := guess;
    setUp(tok, loc, units);
end;

function configExists() : Boolean;
begin
    Result := FileExists(GetAppConfigDir(false)+CATSLASH+'cfg');
end;

procedure getConfig(var tok : String; var loc : String; var units : Integer);
var
    dir : String;
    fl  : Text;
begin
    if (configExists) then
    begin
        dir := GetAppConfigDir(false)+CATSLASH;
        try
            assignfile(fl, dir+'cfg');
            reset(fl);
            readln(fl, tok);
            readln(fl, loc);
            readln(fl, units);
            closefile(fl);  
        except
            on E: Exception do
            begin
                raiserror('Error: Config file is corrupted.');  
            end;
        end;
    end else begin
        raiserror('Error: Config file not found.');
    end;
end;

end.
