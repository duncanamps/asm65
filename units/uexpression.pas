unit uexpression;

{
    ASM65 - Cross Assembler for 6502 processor
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Forward declarations

function StripQuotes(const _s: string): string;
function StripQuotesAndEscaped(const _s: string): string;
function VariableFromBinLiteral(const _s: string): string;
function VariableFromHexLiteral(const _s: string): string;
function VariableFromOctLiteral(const _s: string): string;

implementation

uses
  strutils;


function StripQuotes(const _s: string): string;
var l: integer;
begin
  Result := _s;
  l := Length(_s);
  if (l > 0) and (LeftStr(_s,1) = CHR(34)) then
    begin
      if l < 2 then
        raise Exception.Create('Trying to strip quotes from string which is too short ' + _s);
      if (LeftStr(_s,1) <> Chr(34)) or
         (RightStr(_s,1) <> Chr(34)) then
        raise Exception.Create('Trying to strip quotes which are not present ' + _s);
      Result := Copy(_s,2,Length(_s)-2);
    end;
end;

function StripQuotesAndEscaped(const _s: string): string;
begin
  Result := StripQuotes(_s);
  Result := StringReplace(Result,'\"','"',[rfReplaceAll]);
  Result := StringReplace(Result,'\\','\',[rfReplaceAll]);
  Result := StringReplace(Result,'\t',#9, [rfReplaceAll]);
  Result := StringReplace(Result,'\n',#10,[rfReplaceAll]);
  Result := StringReplace(Result,'\r',#13,[rfReplaceAll]);
end;

function VariableFromBinLiteral(const _s: string): string;
var decval: int64;
    i:      integer;
begin
  decval := 0;
  if Length(_s) < 2 then
    raise Exception.Create('BinLiteral is too short');
  for i := 3 to Length(_s) do
    decval := decval * 2 + (Ord(_s[i])-Ord('0'));
  Result := IntToStr(decval);
end;

function VariableFromHexLiteral(const _s: string): string;
var decval: int64;
begin
  decval := StrToInt(_s);
  Result := IntToStr(decval);
end;

function VariableFromOctLiteral(const _s: string): string;
var decval: int64;
    i:      integer;
begin
  decval := 0;
  for i := 1 to Length(_s) do
    decval := decval * 8 + (Ord(_s[i])-Ord('0'));
  Result := IntToStr(decval);
end;

end.

