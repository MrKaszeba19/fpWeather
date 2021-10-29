unit AppConfig;

{$mode objfpc}{$H+}

interface

uses SysUtils;

procedure raiserror(Const msg : string);  
procedure setConfig(var tok : String; var loc : String; var units : String);
procedure getConfig(var tok : String; var loc : String; var units : String);
function configExists() : Boolean;
function getUnits(x : Integer) : String;

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

procedure setUp(tok : String; loc : String; var units : String);
var
    dir   : String;
    isdir : Boolean;
    fl    : Text;
    ok    : Boolean;
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
        else Result := 'standard';
    end;
end;

procedure setConfig(var tok : String; var loc : String; var units : String);
var
    guess : Integer;
begin
    writeln('fpWeather Config');
    write('Type your OpenWeatherApp API token: ');
    readln(tok);
    write('Type your city (in English):        ');
    readln(loc);
    repeat
        writeln('Preferable units:                 ');
        writeln('    0 - Standard (Kelvin, m/s)');
        //writeln('    1 - Metric (Celsius, km/h)');
        writeln('    1 - Metric (Celsius, m/s)');
        writeln('    2 - Imperial (Fahrenheit, mph)');
        write('Your choice: ');
        readln(guess);
    until guess in [0, 1, 2];
    Units := getUnits(guess);
    setUp(tok, loc, units);
end;

function configExists() : Boolean;
begin
    Result := FileExists(GetAppConfigDir(false)+'/cfg');
end;

procedure getConfig(var tok : String; var loc : String; var units : String);
var
    dir : String;
    fl  : Text;
begin
    if (configExists) then
    begin
        dir := GetAppConfigDir(false)+'/';
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
                raiserror('Config file is corrupted!');  
            end;
        end;
    end else begin
        raiserror('Config file not found!');
    end;
end;

end.
