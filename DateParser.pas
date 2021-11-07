unit DateParser;

{$mode objfpc}{$H+}

interface

function getDateFromTimestamp(date : LongInt) : String;
function getDateFromTimestamp(date : LongInt; format : String) : String;

implementation

uses SysUtils, DateUtils;

function getDateFromTimestamp(date : LongInt) : String;
begin
    Result := DateTimeToStr(UnixToDateTime(date));
end;

function getDateFromTimestamp(date : LongInt; format : String) : String;
begin
    Result := FormatDateTime(format, UnixToDateTime(date));
    if (format = 'mm/dd/yyyy') then Result := StringReplace(Result, '-', '/', [rfReplaceAll]);
end;

end.
