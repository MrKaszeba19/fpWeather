program main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

uses SysUtils, Classes,
    {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    unix,
    {$ENDIF} 
    {$IFDEF MSWINDOWS}
    Process,
    {$ENDIF}
    CustApp, AppConfig, WeatherEngine;



type
    MyApp = class(TCustomApplication)
    private
        Token    : String;
        City     : String;    
        Units    : Integer;   
        ToJSON   : Integer;
        FeedLine : Boolean;
        URL      : String;
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
end;


procedure MyApp.DoRun;
begin
    // flags for non-weather things
    if HasOption('h', 'help') then begin
        WriteHelp();
        Halt();
    end;

    if HasOption('c', 'config') then begin
        SetConfig(Token, City, Units);
        Halt();
    end;

    
    // check if config exists
    if not (ConfigExists()) then
    begin
        try
            writeln('Note: No config file found. I''m creating new one.');
            SetConfig(Token, City, Units); 
        except
            on E: Exception do
            begin
                writeln(E.toString());      
            end;
        end;
    end;

    // get config
    //ToJSON   := HasOption('j', 'json');
    if HasOption('J', 'JSON') 
        then ToJSON := 2
        else if HasOption('j', 'json') 
            then ToJSON := 1
            else ToJSON := 0;
    FeedLine := not HasOption('n', 'no-feed-line');
    try
        GetConfig(Token, City, Units);
    except
        on E: Exception do
        begin
            writeln(E.toString());
            Terminate; 
        end;
    end;

    // change config to suit flags
    if HasOption('u', 'units') then begin
        try
            if StrToInt(getOptionValue('u', 'units')) in [0, 1, 2, 3] then
            begin
                Units := StrToInt(getOptionValue('u', 'units'));
            end else begin
                writeln('Note: wrong unit flag value. I''m not changing the units then.');
            end;
        except
            on E: Exception do
            begin
                writeln('Note: wrong unit flag value. I''m not changing the units then.');
                writeln(E.toString());
            end;
        end;
    end;

    // print weather info
    try
        URL := 'http://api.openweathermap.org/data/2.5/weather?q='+City+'&appid='+Token+'&units='+getUnits(Units)+'';
        if (toJSON > 0) then
        begin
            write(printJSON(URL));
            if FeedLine then writeln();
        end else begin
            write(printInfo(URL));
            if FeedLine then writeln();
        end;
    except
        on E: Exception do
        begin
            writeln(E.toString());
        end;
    end;

    // terminate
    Terminate; 
end;

constructor MyApp.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    //StopOnException:=True;
end;

destructor MyApp.Destroy;
begin
    inherited Destroy;
end;

procedure MyApp.WriteHelp;
begin
    writeln('Usage: '+Title+' ''COMMAND'' flags');
    writeln('Available flags: ');
    writeln('  (no flags)           : Show weather using your default configuration');
    writeln('                         (if it does not exist, then create one)');
    writeln('  -c  , --config       : Set up the config file');
    writeln('  -h  , --help         : Display help');
    writeln('  -j  , --json         : Print a raw JSON from OpenWeatherApp');
    //writeln('  -J  , --JSON         : Print a human-readable JSON');
    writeln('  -n  , --no-feed-line : Do not feed line after program execution');
    writeln('  -u 0, --units=0      : Display weather in Kelvins and m/s');
    writeln('     1,        =1      : Display weather in Celsius and m/s');
    writeln('     1,        =1      : Display weather in Celsius and km/h');
    writeln('     2,        =2      : Display weather in Celsius and mph');
    writeln('     3,        =3      : Display weather in Fahrenheit and mph');
end;

var App : MyApp;

//{$R *.res}

begin
    App := MyApp.Create(nil);
    App.Title := 'fpweather';
    App.Run;
    App.Free;
    //{$IFDEF MSWINDOWS}
    //Sleep(500);
    //{$ENDIF}
    //readln();
end.
