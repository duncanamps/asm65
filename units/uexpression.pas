unit uexpression;

{$mode objfpc}{$H+}

interface

//
// Variable types are stored in a string in the format:
//
//    & letter data
//
// For example:
//
//    &132         1 byte unsigned value of 32 decimal
//    &sDuncan     String value "Duncan"
//    &b0          Boolean value of false
//
// The letter codes:
//
//    1 Unsigned 1 byte value
//    2 Unsigned 2 byte value
//    3 Unsigned 4 byte value
//    4 Signed 1 byte value
//    5 Signed 2 byte value
//    6 Signed 3 byte value
//    7 Unresolved integer value (assumed)
//    b Boolean value (0 or 1)
//    s String value
//

uses
  Classes, SysUtils;

// Forward declarations

function StringFromVariable(const _v: string): string;
function VariableFromHexLiteral(const _s: string): string;
function VariableFromOctLiteral(const _s: string): string;
function VariableFromString(const _s: string): string;
function VariableFromStringQuoted(const _s: string): string;

implementation

function StringFromVariable(const _v: string): string;
begin
  if Length(_v) < 2 then
    raise Exception.Create('Variable error (' + _v + ')');
  if Copy(_v,1,2) <> '&s' then
    raise Exception.Create('Variable is not a string (' + _v + ')');
  Result := Copy(_v,3,32767);
end;

function StripQuotes(const _s: string): string;
begin
  Result := _s;
  if Length(Result) < 2 then
    raise Exception.Create('Trying to strip quotes from string which is too short ' + _s);
  if (_s[1] <> Chr(34)) or
     (_s[Length(_s)] <> Chr(34)) then
    raise Exception.Create('Trying to strip quotes which are not present ' + _s);
  Result := Copy(_s,2,Length(_s)-2);
end;

function VariableFromHexLiteral(const _s: string): string;
var decval: int64;
begin
  decval := StrToInt(_s);
  Result := '&1';
  if decval > 255 then
    Result := '&2';
  if decval > 65535 then
    Result := '&3';
  Result := Result + IntToStr(decval);
end;

function VariableFromOctLiteral(const _s: string): string;
var decval: int64;
    i:      integer;
begin
  decval := 0;
  for i := 1 to Length(_s) do
    decval := decval * 8 + (Ord(_s[i])-Ord('0'));
  Result := '&1';
  if decval > 255 then
    Result := '&2';
  if decval > 65535 then
    Result := '&3';
  Result := Result + IntToStr(decval);
end;

function VariableFromString(const _s: string): string;
begin
  Result := '&s' + _s;
end;

function VariableFromStringQuoted(const _s: string): string;
begin
  Result := VariableFromString(StripQuotes(_s));
end;

end.

