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
        Token : String;
        City  : String;    
        Units : String;   
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
end;


procedure MyApp.DoRun;
begin
    // flags
    if HasOption('h', 'help') then begin
        WriteHelp();
        Halt();
    end;

    if HasOption('c', 'config') then begin
        SetConfig(Token, City, Units);
        Halt();
    end;

    
    // code

    if not (ConfigExists()) then
    begin
        writeln('Note: No config file found. I''m creating new one.');
        SetConfig(Token, City, Units);
    end;

    GetConfig(Token, City, Units);

    if HasOption('u', 'units') then begin
        try
            if StrToInt(getOptionValue('u', 'units')) in [0, 1, 2] then
            begin
                Units := getUnits(StrToInt(getOptionValue('u', 'units')));
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

    try
        //writeln(GetRequest('http://api.openweathermap.org/data/2.5/weather?q='+City+'&appid='+Token+'&units='+Units+''));
        writeln(printJSON('http://api.openweathermap.org/data/2.5/weather?q='+City+'&appid='+Token+'&units='+Units+''));
    except
        on E: Exception do
        begin
            writeln(E.toString());
        end;
    end;


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
    writeln('  -c,   --config  : Set up the config file');
    writeln('  -h,   --help    : Display help');
    writeln('  -u 0, --units=0 : Display weather in Kelvins and m/s');
    writeln('     1,        =1 : Display weather in Celsius and km/h');
    writeln('     2,        =2 : Display weather in Fahrenheit and mph');
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
