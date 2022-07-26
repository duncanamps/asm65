unit umacro;

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
  Classes, SysUtils, fgl;

type
  TMacroObject = class(TObject)
    public
      FName: string;
      FList: TStringList;
      constructor Create;
      destructor Destroy; override;
  end;

  TMacroList = class(specialize TFPGObjectList<TMacroObject>)
    private
      FLocalCount: integer;
    public
      function  Add(const name: string; slist: TStringList): integer;
      function  IndexOf(const s: string): integer;
      procedure Init;
      function  LocalPrefix: string;
  end;

implementation

{ TMacroObject }

constructor TMacroObject.Create;
begin
  FList := TStringList.Create;
end;

destructor TMacroObject.Destroy;
begin
  if Assigned(FList) then
    FList.Free;
  inherited Destroy;
end;

{ TMacroList }

function TMacroList.Add(const name: string; slist: TStringList): integer;
var obj: TMacroObject;
begin
  obj := TMacroObject.Create;
  obj.FName := name;
  obj.FList := slist;;
  Result := inherited Add(obj);
end;

function TMacroList.IndexOf(const s: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].FName = s then
      Exit(i);
end;

procedure TMacroList.Init;
begin
  FLocalCount := 0;
end;

function TMacroList.LocalPrefix: string;
begin
  Result := Format('@L%4.4X',[FLocalCount]);
  Inc(FLocalCount);
end;

end.

