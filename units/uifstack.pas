unit uifstack;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, deployment_parser_module, deployment_parser_types;

type

TIfStackEntry = record
  Index:     integer;
  Succeeded: boolean;
  ElseMode:  boolean;
  class operator = (ise1, ise2: TIfStackEntry): boolean;
end;

TIfStack = class(specialize TFPGList<TIfStackEntry>)
  private
    FParser:    TLCGParser;
    function  GetAllowed: boolean;
    function  GetTop: TIfStackEntry;
  public
    constructor Create(_parser: TLCGParser);
    procedure ElseSwap;
    procedure Pop;
    procedure Push(_succeeded: boolean);
    property Allowed: boolean read GetAllowed;
    property Top:     TIfStackEntry  read GetTop;
end;


implementation

{ TIfStackEntry }

class operator TIfStackEntry.= (ise1, ise2: TIfStackEntry): boolean;
begin
  Result := (ise1.Index = ise2.Index);
end;

{ TIfStack }

constructor TIfStack.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
end;

function TIfStack.GetAllowed: boolean;
var i:       integer;
begin
  if Count = 0 then
    Exit(True); // Not in any kind of If construct
  Result := True;
  for i := 0 to Count-1 do
    if Items[i].Succeeded = Items[i].ElseMode then
      Result := False;
end;

procedure TIfStack.ElseSwap;
var entry: TIfStackEntry;
begin
  if Count = 0 then
    FParser.Monitor(ltError,'.ELSE without corresponding .IF / .IFDEF / .IFNDEF');
  entry := Top;
  if entry.ElseMode then
    FParser.Monitor(ltError,'.ELSE has already been specified for this block');
  entry.ElseMode := True;
  Items[Count-1] := entry;
end;

function TIfStack.GetTop: TIfStackEntry;
begin
  if Count = 0 then
    FParser.Monitor(ltInternal,'Attempting to take the top of an empty if stack');
  Result := Items[Count-1];
end;

procedure TIfStack.Pop;
begin
  if Count = 0 then
    FParser.Monitor(ltError,'.ENDIF without corresponding .IF / .IFDEF / .IFNDEF');
  Delete(Count-1);
end;

procedure TIfStack.Push(_succeeded: boolean);
var entry: TIfStackEntry;
begin
  entry.Index     := Count;
  entry.Succeeded := _succeeded;
  entry.ElseMode  := False;
  Add(entry);
end;

end.

