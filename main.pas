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
        Locale   : Locale; 
        Settings : AppSettings;
        Display  : DisplayOptions; 
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
        SetConfig(Settings);
        Halt();
    end;

    
    // check if config exists
    if not (ConfigExists()) then
    begin
        try
            writeln('Note: No config file found. I''m creating new one.');
            SetConfig(Settings);
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
        then ToJSON := 3
        else if HasOption('j', 'json') 
            then ToJSON := 2
            else if HasOption('raw-json') 
                then ToJSON := 1
                else ToJSON := 0;
    FeedLine := not HasOption('n', 'no-feed-line');
    try
        GetConfig(Settings);
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
                Settings.Units := StrToInt(getOptionValue('u', 'units'));
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
    if HasOption('l', 'location') then begin
        try
            if getOptionValue('l', 'location') <> '' then
            begin
                Settings.City := getOptionValue('l', 'location');
            end else begin
                writeln('Note: empty city name. I''m not changing the city then.');
            end;
        except
            on E: Exception do
            begin
                writeln('Note: wrong city name. I''m not changing the city then.');
                writeln(E.toString());
            end;
        end;
    end;
    if HasOption('o', 'output') then begin
        try
            Display := adjustDisplay(getOptionValue('o', 'output'));
        except
            on E: Exception do
            begin
                writeln('Note: wrong token name. I''m not changing the token then.');
                writeln(E.toString());
            end;
        end;
    end else begin
        Display := adjustDisplay(Settings.Output);
    end;
    if HasOption('s', 'style') then begin
        try
            if StrToInt(getOptionValue('s', 'style')) in [0, 1, 2, 3] then
            begin
                setSeparators(Display, StrToInt(getOptionValue('s', 'style')));
            end else begin
                writeln('Note: wrong style flag value. I''m not changing the styles then.');
            end;
        except
            on E: Exception do
            begin
                writeln('Note: wrong style flag value. I''m not changing the styles then.');
                writeln(E.toString());
            end;
        end;
    end else begin
        try
            setSeparators(Display, Settings.Style);
        except
            on E: Exception do
            begin
                writeln('Note: wrong style flag value. I''m not changing the styles then.');
                writeln(E.toString());
            end;
        end;
    end;
    if HasOption('T', 'token') then begin
        try
            if getOptionValue('T', 'token') <> '' then
            begin
                Settings.Token := getOptionValue('T', 'token');
            end else begin
                writeln('Note: empty token name. Using your token from the config file.');
            end;
        except
            on E: Exception do
            begin
                writeln('Note: wrong token name. I''m not changing the token then.');
                writeln(E.toString());
            end;
        end;
    end;

    // print weather info
    Locale := GetLocale(Settings.Units);
    if (toJSON = 1) then Locale.Param := 'standard';
    try
        URL := 'http://api.openweathermap.org/data/2.5/weather?q='+Settings.City+'&appid='+Settings.Token+'&units='+Locale.Param+'';
        if (toJSON >= 3) then
        begin
            write(printFormattedJSON(URL));
            if FeedLine then writeln();
        end else if (toJSON = 2) then 
        begin 
            write(printJSON(URL));
            if FeedLine then writeln();
        end else if (toJSON = 1) then 
        begin 
            write(printRaw(URL));
            if FeedLine then writeln();
        end else begin
            write(printInfo(URL, Locale, Display));
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
    writeln('Usage: '+Title+' [flags]');
    writeln('Available flags: ');
    writeln('  (no flags)           : Show weather using your default configuration');
    writeln('                         (if it does not exist, then create one)');
    writeln('  -c  , --config       : Set up the config file');
    writeln('  -h  , --help         : Display help');
    writeln('  -j  , --json         : Print a flat JSON');
    writeln('  -J  , --JSON         : Print a human-readable JSON');
    writeln('  -l S, --location=S   : Show weather in a specified city');
    writeln('  -n  , --no-feed-line : Do not feed line after program execution');
    writeln('        --raw-json     : Get a raw JSON from OpenWeatherMap (in standard units)');
    writeln('  -o S, --output=S     : Determine which data entries may be displayed using S');
    writeln('                         (If S="full", then display all data)');
    writeln('  -s 0, --style=0      : Flat output');
    writeln('     1,        =1      : Flat output with location in separate line (default)');
    writeln('     2,        =2      : Output as a list of values with flat subvalues');
    writeln('     3,        =3      : Output as a list of values with a list of subvalues');
    writeln('  -T S, --token=S      : Show weather using a specified OpenWeatherMap API token');
    writeln('  -u 0, --units=0      : Use international SI units: Kelvin, m/s, hPa, km');
    writeln('     1,        =1      : Use metric units:           Celsius, km/h, hPa, km');
    writeln('     2,        =2      : Use US Imperial units:      Fahrenheit, mph, psi, mi');
    writeln('     3,        =3      : Use UK Imperial units:      Celsius, mph, mb, mi');
    //writeln('     4,        =4      : Use EU Aviation units:      Celsius, kts, hPa, nm');
    //writeln('     5,        =5      : Use US Aviation units:      Fahrenheit, kts, psi, nm');
    writeln();
    writeln('See more at https://github.com/RooiGevaar19/fpWeather');
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
