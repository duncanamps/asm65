unit umacro;

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

