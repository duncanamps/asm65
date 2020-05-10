unit usymbol;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

//------------------------------------------------------------------------------
//
//  usymbol.pas
//
//  Handle symbol table operations
//
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, fgl, uutility;

const
  DEFAULT_HASH_SIZE = 1000;        // Initial size for hash table
  HASH_MARGIN = 4;                 // Hash table is > 4 times size of sym table
  HASH_MULTIPLIER = 8;             // Hash table multiplies by 8 on resize


type

  TSymbolType = (sytNone,sytBoolean,sytUnsigned1,sytUnsigned2,sytUnsigned4,
                 sytSigned1,sytSigned2,sytSigned4,sytUnresolved);

  TSymbolEntry = record
    SymName:     string;      // Symbol name
    SymType:     TSymbolType; // Symbol type (see above)
    SymPass:     integer;     // Pass defined in (1 or 2)
    SymResolved: boolean;     // True if the value has been resolved
    SymValue:    int64;       // Value
    class operator = (se1, se2: TSymbolEntry): boolean;
  end;

  TSymbolTable = class (specialize TFPGList<TSymbolEntry>)
    private
      FHashTable: array of integer;
      FHashSize:  integer;
      procedure AddHash(_name: string; _recno: integer);
      function  CalcHash(_name: string): integer;
      procedure CheckHashSize;
      procedure ClearHash;
    public
      constructor Create;
      destructor Destroy; override;
      function Define(_pass: integer; const _name: string; const _variable: string): string;
      function IndexOf(_key: string): integer;
      function Exists(_name: string): boolean;
      function Variable(_pass: integer; _name: string): string;
  end;


implementation

{ Class operators }

class operator TSymbolEntry.= (se1, se2: TSymbolEntry): boolean;
begin
  Result := (se1.SymName = se2.SymName);
end;

{ Utility routines }

function SymTypeFromVariable(const _variable: string): TSymbolType;
begin
  Result := sytNone;
  if (Length(_variable) < 2) or (_variable[1] <> '&') then
    raise Exception.Create('Variable error (' + _variable + ')');
  case _variable[2] of
    '1': Result := sytUnsigned1;
    '2': Result := sytUnsigned2;
    '3': Result := sytUnsigned4;
    '4': Result := sytSigned1;
    '5': Result := sytSigned2;
    '6': Result := sytSigned4;
    '7': Result := sytUnresolved;
    'b': Result := sytBoolean;
  end;
end;

function VariableFromSymType(_symtype: TSymbolType; _value: int64): string;
begin
  case _symtype of
    sytNone:       Result := '&7';
    sytBoolean:    Result := '&b';
    sytUnsigned1:  Result := '&1';
    sytUnsigned2:  Result := '&2';
    sytUnsigned4:  Result := '&3';
    sytSigned1:    Result := '&4';
    sytSigned2:    Result := '&5';
    sytSigned4:    Result := '&6';
    sytUnresolved: Result := '&7';
  end;
  Result := Result + IntToStr(_value);
end;

function ValueFromVariable(const _variable: string): int64;
begin
  Result := 0;
  if (Length(_variable) < 2) or (_variable[1] <> '&') then
    raise Exception.Create('Variable error (' + _variable + ')');
  Result := StrToInt(Copy(_variable,3,32767));
end;

{ TSymbolTable }

constructor TSymbolTable.Create;
begin
  inherited Create;
  FHashSize := NextPrime(DEFAULT_HASH_SIZE);
  SetLength(FHashTable,FHashSize);
  ClearHash;
end;

destructor TSymbolTable.Destroy;
begin
  inherited Destroy;
end;

procedure TSymbolTable.AddHash(_name: string; _recno: integer);
var index: integer;
    done:  boolean;
begin
  index := CalcHash(_name);
  done := False;
  while not done do
    if FHashTable[index] < 0 then
      begin
        FHashTable[index] := _recno;
        done := True;
      end
    else
      begin
        Inc(index);
        if index >= FHashSize then
          index := 0;
      end;
end;

function TSymbolTable.CalcHash(_name: string): integer;
var hashval: int64;
    i:       integer;
begin
  _name := UpperCase(_name);
  hashval := 0;
  for i := 1 to Length(_name) do
    begin
      hashval := hashval * 257;
      hashval := hashval + Ord(_name[i]);
      hashval := hashval xor (hashval shr 32);
      hashval := hashval and $7fffffff;
    end;
  Result := hashval mod FHashSize;
end;

procedure TSymbolTable.CheckHashSize;
var i:integer;
begin
  if (Count * HASH_MARGIN) > FHashSize then
    begin // Englarge the hash table
      FHashSize := NextPrime(FHashSize * HASH_MULTIPLIER);
      SetLength(FHashTable,FHashSize);
      ClearHash;
      // Now re-hash everything
      for i := 0 to Count-1 do
        AddHash(Items[i].SymName,i);
    end;
end;

procedure TSymbolTable.ClearHash;
var i: integer;
begin
  for i := 0 to FHashSize-1 do
    FHashTable[i] := -1;
end;

function TSymbolTable.Define(_pass: integer; const _name: string; const _variable: string): string;
var entry: TSymbolEntry;
begin
  Result := '';
  case _pass of
    1:  begin
          if Exists(_name) then
            Result := 'Symbol ' + _name + ' already exists'
          else
            begin
              entry.SymName     := UpperCase(_name);
              entry.SymType     := SymTypeFromVariable(_variable);
              entry.SymPass     := _pass;
              entry.SymResolved := entry.SymType <> sytUnresolved;
              entry.SymValue    := ValueFromVariable(_variable);
              AddHash(_name,Add(entry));
              CheckHashSize;
            end;
        end;
  end;
end;

function TSymbolTable.Exists(_name: string): boolean;
begin
  Result := IndexOf(_name) >= 0;
end;

function TSymbolTable.IndexOf(_key: string): integer;
var index: integer;
    done:  boolean;
begin
  _key := UpperCase(_key);
  Result := -1;
  index := CalcHash(_key);
  done := False;
  while not done do
    begin
      if FHashTable[index] < 0 then
        done := True
      else if Items[FHashTable[index]].SymName = _key then
        begin
          done := True;
          Result := FHashTable[index];
        end
      else
        begin
          Inc(index);
          if index >= FHashSize then
            index := 0;
        end;
    end;
end;

function TSymbolTable.Variable(_pass: integer; _name: string): string;
var i: integer;
begin
  _name := UpperCase(_name);
  i := IndexOf(_name);
  case _pass of
    1:  begin
          if i < 0 then
            Result := '&70'
          else
            Result := VariableFromSymType(Items[i].SymType,Items[i].SymValue);
        end;
    2:  begin
          if i < 0 then
            raise Exception.Create('Symbol ' + _name + ' not found');
          Result := VariableFromSymType(Items[i].SymType,Items[i].SymValue);
        end;
  end;
end;

end.

