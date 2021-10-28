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
    CustApp, AppConfig;



type
    MyApp = class(TCustomApplication)
    private
        Token : String;
        City  : String;       
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
        DoConfig(Token, City);
        Halt();
    end;
    
    // code

    if not (ConfigExists()) then
    begin
        writeln('Note: No config file found. Creating new one.');
        DoConfig(Token, City);
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
