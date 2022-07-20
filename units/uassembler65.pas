{$WARN 5024 off : Parameter "$1" not used}
unit uassembler65;

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
  Classes, SysUtils, deployment_parser_module_12, deployment_parser_types_12,
  ufilestack, usymbol, uasmglobals, uoutput, uifstack, umacro, udebuglist;


type

  TAssembler65 = class(TLCGParser)
    private
      FAddr:           UINT16;
      FAddrMode:       TAddrMode;
      FAssemblyEnd:    TDateTime;
      FAssemblyStart:  TDateTime;
      FBytesFromLine:  integer;
      FBytesTotal:     integer;
      FCmdDefines:     TStringList;
      FCmdIncludes:    TStringList;
      FDebugList:      TDebugList;
      FDefiningMacro:  boolean;
      FForceList:      boolean;
      FFilenameSrc:    string;
      FFileStack:      TFileStack;
      FIfStack:        TIfStack;
      FIncludeNext:    string;
      FLineCount:      integer;
      FList:           boolean;
      FListNext:       boolean;
      FListing:        TStringList;
      FLocalPrefix:    string;
      FMacroCapture:   TStringList;
      FMacroName:      string;
      FMacroNestLevel: integer;
      FMacroList:      TMacroList;
      FOutput:         TOutput;
      FOutputArr:      TOutputSequence;
      FOpCode:         TOpCode;
      FOrg:            UINT16;
      FPass:           integer;
      FProcArray:      array of TLCGParserProc;
      FProcessMacro:   string;
      FProcessParms:   string;
      FStreamLog:      TFileStream;
      FSymbols:        TSymbolTable;
      FVerbose:        boolean;
      function  ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDD(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDefmacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirNolist(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpA(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbs(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbsX(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpAbsY(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpImmed(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpImpl(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndX(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpIndY(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
      function  ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
      procedure FilesClose;
      procedure FilesOpen;
      function  GetSource: string;
      procedure InitLine;
      procedure InitPass;
      procedure InitStart;
      procedure OutputDebugLine(const _asmline: string);
      procedure OutputListingLine(const _asmline: string);
      procedure ProcessFile(const _fn: string; _listing: boolean = True);
      procedure ProcessFileInner;
      function  ProcessingAllowed: boolean;
      procedure ProcessMacroExpansion;
      procedure ProcessLine(const _line: string);
      procedure RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
      procedure RegisterProcs;
      procedure SetFilenameSrc(const _fn: string);
      procedure SetOnMonitor(_monitor: TLCGMonitorProc);
      procedure WriteMapFile;
    public
      FilenameDbg:    string;
      FilenameHex:    string;
      FilenameLst:    string;
      FilenameLog:    string;
      FilenameMap:    string;
      FilenameObj:    string;
      constructor Create;
      destructor Destroy; override;
      procedure Assemble;
      procedure AssemblePass(_pass: integer);
      procedure Monitor(LogType: TLCGLogType; const Message: string); override;
      procedure Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const); override;
      function  Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
      property CmdDefines:  TStringList     read FCmdDefines;
      property CmdIncludes: TStringList     read FCmdIncludes;
      property FilenameSrc: string          read FFilenameSrc write SetFilenameSrc;
      property OnMonitor:   TLCGMonitorProc read FOnMonitor   write SetOnMonitor;
      property Source:      string          read GetSource;
      property TabSize:     integer         read FTabSize     write FTabSize;
  end;


implementation

uses
  uexpression, strutils, fileinfo;

const CodeTable: array[TOpCode,TAddrMode] of integer = (
          (109,125,121,-1,-1,113,105,-1,97,101,117,-1,-1),
          (45,61,57,-1,-1,49,41,-1,33,37,53,-1,-1),
          (14,30,-1,-1,-1,-1,-1,-1,-1,6,22,-1,10),
          (-1,-1,-1,-1,-1,-1,-1,144,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,176,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,240,-1,-1,-1,-1,-1),
          (44,-1,-1,-1,-1,-1,-1,-1,-1,36,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,208,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,16,-1,-1,-1,-1,-1),
          (-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,80,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,112,-1,-1,-1,-1,-1),
          (-1,-1,-1,24,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,216,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,88,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,184,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (205,221,217,-1,-1,209,201,-1,193,197,213,-1,-1),
          (236,-1,-1,-1,-1,-1,224,-1,-1,228,-1,-1,-1),
          (204,-1,-1,-1,-1,-1,192,-1,-1,196,-1,-1,-1),
          (206,222,-1,-1,-1,-1,-1,-1,-1,198,214,-1,-1),
          (-1,-1,-1,202,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,136,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (77,93,89,-1,-1,81,73,-1,65,69,85,-1,-1),
          (238,254,-1,-1,-1,-1,-1,-1,-1,230,246,-1,-1),
          (-1,-1,-1,232,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,200,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (76,-1,-1,-1,108,-1,-1,-1,-1,-1,-1,-1,-1),
          (32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (173,189,185,-1,-1,177,169,-1,161,165,181,-1,-1),
          (174,-1,190,-1,-1,-1,162,-1,-1,166,-1,182,-1),
          (172,188,-1,-1,-1,-1,160,-1,-1,164,180,-1,-1),
          (78,94,-1,-1,-1,-1,-1,-1,-1,70,86,-1,74),
          (-1,-1,-1,234,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (13,29,25,-1,-1,17,9,-1,1,5,21,-1,-1),
          (-1,-1,-1,72,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,104,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,40,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (46,62,-1,-1,-1,-1,-1,-1,-1,38,54,-1,42),
          (110,126,-1,-1,-1,-1,-1,-1,-1,102,118,-1,106),
          (-1,-1,-1,64,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (237,253,249,-1,-1,241,233,-1,225,229,245,-1,-1),
          (-1,-1,-1,56,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,248,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,120,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (141,157,153,-1,-1,145,-1,-1,129,133,149,-1,-1),
          (142,-1,-1,-1,-1,-1,-1,-1,-1,134,-1,150,-1),
          (140,-1,-1,-1,-1,-1,-1,-1,-1,132,148,-1,-1),
          (-1,-1,-1,170,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,168,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,186,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,138,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,154,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,152,-1,-1,-1,-1,-1,-1,-1,-1,-1),
          (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1));

  InstructionSizes: array[TAddrMode] of integer =
    (
      3,       // ADM_ABS
      3,       // ADM_ABSX
      3,       // ADM_ABSY
      1,       // ADM_IMPL
      3,       // ADM_IND
      2,       // ADM_INDY
      2,       // ADM_LIT
      2,       // ADM_REL
      2,       // ADM_XIND
      2,       // ADM_ZPG
      2,       // ADM_ZPGX
      2,       // ADM_ZPGY
      1        // ADM_ACC
    );


{ TAssembler65 }

constructor TAssembler65.Create;
begin
  inherited Create;
  LoadFromResource('ASM65');
  SetLength(FProcArray,Rules);
  FSymbols := TSymbolTable.Create;
  FFileStack := TFileStack.Create(Self);
  FIfStack   := TIfStack.Create(Self);
  FOutput := TOutput.Create;
  FDebugList := TDebugList.Create;
  FListing := TStringList.Create;
  FMacroList := TMacroList.Create;
  FCmdDefines := TStringList.Create;
  FCmdIncludes := TStringList.Create;
  FPass := 0;
  FTabSize := 4;
  FVerbose := False;
  FOnReduce := @Reduce;
  RegisterProcs;
end;

destructor TAssembler65.Destroy;
begin
  FCmdIncludes.Free;
  FCmdDefines.Free;
  FMacroList.Free;
  FListing.Free;
  FDebugList.Free;
  FOutput.Free;
  FIfStack.Free;
  FFileStack.Free;
  FSymbols.Free;
  inherited Destroy;
end;

function TAssembler65.ActBinLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromBinLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActCharLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Ord(_parser.ParserStack[_parser.ParserSP-1].Buf[2]));
end;

function TAssembler65.ActCompEQ(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) =
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActCompGE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActCompGT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) >
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActCompLE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <=
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActCompLT(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
  result.Buf := '1'
else
  result.Buf := '0';
end;

function TAssembler65.ActCompNE(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <>
     StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActCopy1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler65.ActDecLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActDirByte(_parser: TLCGParser): TLCGParserStackEntry;
var bval: integer;
    bcount: integer;
    i:      integer;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  bval   := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  bcount := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if bcount < 1 then
    Monitor(ltError,'Byte count must be greater than zero');
  if (bval > 255) or (bval < -128) then
    Monitor(ltError,'Byte value must be in the range -128 to 255');
  FBytesFromLine := bcount;
  SetLength(FOutputArr,bcount);
  for i := 0 to bcount-1 do
    FOutputArr[i] := bval and $00FF;
end;

function TAssembler65.ActDirDB(_parser: TLCGParser): TLCGParserStackEntry;
var sl: TStringList;
    i:   integer;
    bval: integer;
begin
  if not ProcessingAllowed then
    Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := _parser.ParserStack[_parser.ParserSP-1].Buf;
    FBytesFromLine := sl.Count;
    SetLength(FOutputArr,FBytesFromLine);
    for i := 0 to sl.Count-1 do
      begin
        bval := StrToInt(sl[i]);
        if (bval > 255) or (bval < -128) then
          Monitor(ltError,'Byte value %d not in range',[bval]);
        FOutputArr[i] := bval and $00FF;
      end;
    FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  finally
    sl.Free;
  end;
  Result.Buf := '';
end;

function TAssembler65.ActDirDD(_parser: TLCGParser): TLCGParserStackEntry;
var sl: TStringList;
    i:   integer;
    bval: int64;
begin
  if not ProcessingAllowed then
    Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := _parser.ParserStack[_parser.ParserSP-1].Buf;
    FBytesFromLine := sl.Count * 4;
    SetLength(FOutputArr,FBytesFromLine);
    for i := 0 to sl.Count-1 do
      begin
        bval := StrToInt(sl[i]);
        if (bval > HIGH(uint32)) or (bval < LOW(int32)) then
          Monitor(ltError,'Byte value %d not in range',[bval]);
        FOutputArr[i*4+0] := bval and $000000FF;
        FOutputArr[i*4+1] := (bval shr 8) and $000000FF;
        FOutputArr[i*4+2] := (bval shr 16) and $000000FF;
        FOutputArr[i*4+3] := (bval shr 24) and $000000FF;
      end;
    FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  finally
    sl.Free;
  end;
  Result.Buf := '';
end;

function TAssembler65.ActDirDefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler65.ActDirDefineExpr(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,False,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler65.ActDirDefineString(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,True,symbolname,expression);
  if message <> '' then
    Monitor(ltError,message);
end;
function TAssembler65.ActDirDefmacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not FIfStack.Allowed then
    exit;
  if FDefiningMacro then
    Monitor(ltError,'.DEFMACRO cannot be nested');
  FDefiningMacro := True;
  FMacroName := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  if FMacroList.IndexOf(FMacroName) >= 0 then
    Monitor(ltError,'Macro %s is already defined',[FMacroName]);
  FMacroCapture := TStringList.Create;
  Monitor(ltWarAndPeace,'Defining macro %s',[FMacroName]);
end;

function TAssembler65.ActDirDS(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s);
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    FOutputArr[i-1] := Ord(s[i]);
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
end;

function TAssembler65.ActDirDSH(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s);
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    if i = Length(s) then
      FOutputArr[i-1] := Ord(s[i]) or $80
    else
      FOutputArr[i-1] := Ord(s[i]);
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
end;

function TAssembler65.ActDirDSZ(_parser: TLCGParser): TLCGParserStackEntry;
var i:   integer;
    s:   string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  s := _parser.ParserStack[_parser.ParserSP-1].Buf;
  FBytesFromLine := Length(s) + 1;
  SetLength(FOutputArr,FBytesFromLine);
  for i := 1 to Length(s) do
    FOutputArr[i-1] := Ord(s[i]);
  FOutputArr[FBytesFromLine-1] := 0;
  FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
end;

function TAssembler65.ActDirDW(_parser: TLCGParser): TLCGParserStackEntry;
var sl: TStringList;
    i:   integer;
    bval: integer;
begin
  if not ProcessingAllowed then
    Exit;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.DelimitedText := _parser.ParserStack[_parser.ParserSP-1].Buf;
    FBytesFromLine := sl.Count*2;
    SetLength(FOutputArr,FBytesFromLine);
    for i := 0 to sl.Count-1 do
      begin
        bval := StrToInt(sl[i]);
        if (bval > 65535) or (bval < -32768) then
          Monitor(ltError,'Word value %d not in range',[bval]);
        FOutputArr[i*2+0] := bval and $00FF;
        FOutputArr[i*2+1] := (bval shr 8) and $00FF;
      end;
    FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
  finally
    sl.Free;
  end;
  Result.Buf := '';
end;

function TAssembler65.ActDirElse(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.ElseSwap;
  Result.Buf := '';
end;

function TAssembler65.ActDirEndif(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FIfStack.Pop;
  Result.Buf := '';
end;

function TAssembler65.ActDirEndm(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not FIfStack.Allowed then
    exit;
  FMacroList.Add(FMacroName,FMacroCapture);
  FDefiningMacro := False;
  FMacroName := '';
  FMacroCapture := nil;
  Monitor(ltWarAndPeace,'End of defining macro %s',[FMacroName]);
end;

function TAssembler65.ActDirError(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltError,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActDirIf(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  succeeded := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0;
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler65.ActDirIfdef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler65.ActDirIfndef(_parser: TLCGParser): TLCGParserStackEntry;
var succeeded: boolean;
begin
  FSymbols.SetUsed(_parser.ParserStack[_parser.ParserSP-1].Buf);
  succeeded := not FSymbols.ExistsInPass(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf);
  FIfStack.Push(succeeded);
  Result.Buf := '';
end;

function TAssembler65.ActDirInclude(_parser: TLCGParser): TLCGParserStackEntry;
var parentfile: string;
    myfile:     string;
    newfile:    string;
    saveddir:   string;
    i:          integer;
  // Check if a file combo exists, return true if it does
  function CheckFile(folder: string; fn: string): boolean;
  begin
    SetCurrentDir(folder);
    try
      newfile := ExpandFilename(myfile);
    finally
      SetCurrentDir(saveddir);
    end;
    Monitor(ltDebug,'Searching for include file %s in folder %s',[myfile,folder]);
    if FileExists(newfile) then
      begin
        Monitor(ltDebug,'Found include file %s in folder %s',[myfile,folder]);
        CheckFile := True;
        FIncludeNext := newfile;
      end
    else
      CheckFile := False;
  end;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  // Rules for processing the include file
  // The file will first be processed relative folders in the following order
  // 1. The parent file which called the include
  // 2. The folder of the initial source file (could be same as 1.)
  // 3. The current directory used to start the software (could also be same as 1.)
  // 4. The folders specified in the --include directive
  // 5.  :
  // 6.  :
  saveddir := GetCurrentDir;
  parentfile := FFileStack.Filename;
  myfile := _parser.ParserStack[_parser.ParserSP-1].Buf;
  if CheckFile(ExtractFilePath(parentfile),myfile) then
    Exit;
  for i := 0 to CmdIncludes.Count-1 do
    if CheckFile(CmdIncludes[i],myfile) then
      Exit;
  Monitor(ltError,'Include file %s could not be found',[myfile]);
end;

function TAssembler65.ActDirIncludeList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FForceList := True;
  Result := ActDirInclude(_parser);
end;

function TAssembler65.ActDirList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FList := True;
  FListNext := True;
end;

function TAssembler65.ActDirMacro(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FProcessParms := _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler65.ActDirMacroNoexpr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FProcessMacro := UpperCase(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FProcessParms := '';
end;

function TAssembler65.ActDirMessage(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltInfo,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActDirNoList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FListNext := False;
end;

function TAssembler65.ActDirOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  FOrg := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActDirSet(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-3].Buf;
  expression := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Define(FPass,False,symbolname,expression,true);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler65.ActDirUndefine(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    message:    string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  message := FSymbols.Undefine(symbolname);
  if message <> '' then
    Monitor(ltError,message);
end;

function TAssembler65.ActDirWarning(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  Monitor(ltWarning,_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActExprAdd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) +
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) and
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprBracket(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-2].Buf;
end;

function TAssembler65.ActExprDiv(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) div
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprList(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                ',' +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler65.ActExprMinus(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(-StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprMod(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) mod
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprMul(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) *
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) xor
                         $FFFF);
end;

function TAssembler65.ActExprOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) or
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprShl(_parser: TLCGParser): TLCGParserStackEntry;
var val1,val2: uint32;
begin
  val1 := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  val2 := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  val1 := val1 shl val2;
  Result.Buf := IntToStr(val1 and $FFFF);
end;

function TAssembler65.ActExprShr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) shr
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprSub(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) -
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActExprXor(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) xor
                         StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf));
end;

function TAssembler65.ActFuncAsc(_parser: TLCGParser): TLCGParserStackEntry;
var s: string;
begin
  s := _parser.ParserStack[_parser.ParserSP-2].Buf;
  if Length(s) < 1 then
    Monitor(ltError,'No argument provided to ASC() function');
  Result.Buf := IntToStr(Ord(s[1]));
end;

function TAssembler65.ActFuncHigh(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) shr 8);
end;

function TAssembler65.ActFuncIif(_parser: TLCGParser): TLCGParserStackEntry;
var expr:     integer;
    trueval:  integer;
    falseval: integer;
    resval:   integer;
begin
  expr     := StrToInt(_parser.ParserStack[_parser.ParserSP-6].Buf);
  trueval  := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  falseval := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if expr <> 0 then
    resval := trueval
  else
    resval := falseval;
  Result.Buf := IntToStr(resval);
end;

function TAssembler65.ActFuncLow(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf) and $00FF);
end;

function TAssembler65.ActFuncPos(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Pos(_parser.ParserStack[_parser.ParserSP-4].Buf,
                             _parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler65.ActFuncValue(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler65.ActHexLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromHexLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActIgnore(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '';
end;

function TAssembler65.ActInstruction(_parser: TLCGParser): TLCGParserStackEntry;
var opstr: string;
    i:     TOpCode;
    iaddr: integer;
    found: boolean;
    instr: byte;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  // We have the operand type and address in FAddrMode / FAddr, find opcode
  opstr := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
  found := False;
  for i in TOpCode do
    if OpCodeText[i] = opstr then
      begin
        found := True;
        FOpCode := i;
        break;
      end;
  if not found then
    Monitor(ltError,'Opcode %s not known',[opstr]);
  // There are 13 address modes, we will have identified from a range of 9 so
  // far. Need to expand out to the missing four if possible (zpg / zpg,x /
  // zpg,y / rel)
  if (FAddrMode = ADM_ABS) and (FAddr < $100) and (FOpCode <> OPC_JMP) then
    FAddrMode := ADM_ZPG;
  if (FAddrMode = ADM_ABSX) and (FAddr < $100) then
    FAddrMode := ADM_ZPGX;
  if (FAddrMode = ADM_ABSY) and (FAddr < $100) and (FOpCode in [OPC_STX,OPC_LDX]) then
    FAddrMode := ADM_ZPGY;
  if (FOpCode in [OPC_BCC,OPC_BCS,OPC_BEQ,OPC_BMI,OPC_BNE,OPC_BPL,OPC_BVC,OPC_BVS]) then
    begin
      FAddrMode := ADM_REL;
      iaddr := FAddr - (FOrg+2);
      if (iaddr < -128) or (iaddr > 127)  then
        Monitor(ltError,'Range error on relative instruction');
      FAddr := iaddr and $00FF;
    end;
  // Check that we have a valid instruction
  if CodeTable[FOpCode][FAddrMode] < 0 then
    Monitor(ltError,'Illegal opcode and operand combination %s / %s',[OpcodeText[FOpCode],AddrModeText[FAddrMode]]);
  instr := CodeTable[FOpCode][FAddrMode];
  // @@@@@ Code here to write out instruction data
  if ProcessingAllowed then
    begin
      FBytesFromLine := InstructionSizes[FAddrMode];
      SetLength(FOutputArr,FBytesFromLine);
      FOutputArr[0] := instr;
      if FBytesFromLine > 1 then FOutputArr[1] := FAddr and $00FF;
      if FBytesFromLine > 2 then FOutputArr[2] := FAddr shr 8;
      FOutput.Write(FOutputArr,FOrg,FBytesFromLine);
    end;
end;

function TAssembler65.ActLabel(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-1].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler65.ActLabelC(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := _parser.ParserStack[_parser.ParserSP-2].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler65.ActLabelLocal(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := FLocalPrefix + _parser.ParserStack[_parser.ParserSP-1].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler65.ActLabelLocalC(_parser: TLCGParser): TLCGParserStackEntry;
var symbolname: string;
    expression: string;
    msg:        string;
begin
  Result.Buf := '';
  if not ProcessingAllowed then
    Exit;
  symbolname := FLocalPrefix + _parser.ParserStack[_parser.ParserSP-2].Buf;
  expression := IntToStr(FOrg);
  msg := FSymbols.Define(FPass,False,symbolname,expression);
  if msg <> '' then
    Monitor(ltError,msg);
end;

function TAssembler65.ActLogAnd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) and
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActLogNot(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) = 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActLogOr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if (StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf) <> 0) or
     (StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf) <> 0) then
    result.Buf := '1'
  else
    result.Buf := '0';
end;

function TAssembler65.ActMacroPlaceholder(_parser: TLCGParser): TLCGParserStackEntry;
begin
  // We will only end up here if a macro parameter was not expanded
  if ProcessingAllowed then
    Monitor(ltError,'@ macro placeholder used without a corresponding parameter');
  Result.Buf := IntToStr(FOrg);
end;

function TAssembler65.ActOctLiteral(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := VariableFromOctLiteral(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActSetOpA(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := 0;
  FAddrMode := ADM_ACC;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpAbs(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FAddrMode := ADM_ABS;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpAbsX(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_ABSX;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpAbsY(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_ABSY;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpImmed(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-1].Buf);
  FAddrMode := ADM_LIT;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpImpl(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := 0;
  FAddrMode := ADM_IMPL;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpIndX(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  FAddrMode := ADM_XIND;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpIndY(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-3].Buf);
  FAddrMode := ADM_INDY;
  Result.Buf := '';
end;

function TAssembler65.ActSetOpInd(_parser: TLCGParser): TLCGParserStackEntry;
begin
  FAddr := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  FAddrMode := ADM_IND;
  Result.Buf := '';
end;

function TAssembler65.ActStrBuild(_parser: TLCGParser): TLCGParserStackEntry;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result.Buf := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function TAssembler65.ActStrCat(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := _parser.ParserStack[_parser.ParserSP-3].Buf +
                _parser.ParserStack[_parser.ParserSP-1].Buf;
end;

function TAssembler65.ActStrChr(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Chr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler65.ActStrDate(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('yyyy-mm-dd',FAssemblyStart);
end;

function TAssembler65.ActStrHex1(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := Format('%X',[StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf)]);
end;

function TAssembler65.ActStrHex2(_parser: TLCGParser): TLCGParserStackEntry;
var fmt: string;
    digits: integer;
begin
  digits := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if (digits < 1) or (digits > 4) then
      Monitor(ltError,'Range for number of hex digits is 1 to 4');
  fmt := Format('%%%d.%dX',[digits,digits]);
  Result.Buf := Format(fmt,[StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf)]);
end;

function TAssembler65.ActStringConstant(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := StripQuotesAndEscaped(_parser.ParserStack[_parser.ParserSP-1].Buf);
end;

function TAssembler65.ActStringSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,'',FIfStack);
end;

function TAssembler65.ActStrLeft(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for LEFT() cannot use a negative number');
  Result.Buf := LeftStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler65.ActStrLower(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := LowerCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler65.ActStrMid(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
    start: integer;
begin
  start := StrToInt(_parser.ParserStack[_parser.ParserSP-4].Buf);
  len   := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if start < 0 then
    Monitor(ltError,'Start for MID() cannot use a negative number');
  if len < 0 then
    Monitor(ltError,'Length for MID() cannot use a negative number');
  Result.Buf := Copy(_parser.ParserStack[_parser.ParserSP-6].Buf,start,len);
end;

function TAssembler65.ActStrRight(_parser: TLCGParser): TLCGParserStackEntry;
var len: integer;
begin
  len := StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf);
  if len < 0 then
    Monitor(ltError,'Length for RIGHT() cannot use a negative number');
  Result.Buf := RightStr(_parser.ParserStack[_parser.ParserSP-4].Buf,len);
end;

function TAssembler65.ActStrString(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(StrToInt(_parser.ParserStack[_parser.ParserSP-2].Buf));
end;

function TAssembler65.ActStrTime(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FormatDateTime('hh:nn:ss',FAssemblyStart);
end;

function TAssembler65.ActStrUpper(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := UpperCase(_parser.ParserStack[_parser.ParserSP-2].Buf);
end;

function TAssembler65.ActValueOrg(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(FOrg);
end;

function TAssembler65.ActValueLocal(_parser: TLCGParser): TLCGParserStackEntry;
begin
  if not ProcessingAllowed then
    Result.Buf := IntToStr(FOrg)
  else
    Result.Buf := FSymbols.Variable(FPass,FLocalPrefix+_parser.ParserStack[_parser.ParserSP-1].Buf,IntToStr(FOrg),FIfStack);
end;

function TAssembler65.ActValueSymbol(_parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := FSymbols.Variable(FPass,_parser.ParserStack[_parser.ParserSP-1].Buf,IntToStr(FOrg),FIfStack);
end;

procedure TAssembler65.Assemble;
var elapsed: double;
begin
  FilesOpen;
  FAssemblyStart := Now;
  Monitor(ltVerbose,'Assembly started %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyStart)]);
  InitStart;
  try
    AssemblePass(1);
    AssemblePass(2);
    WriteMapFile;
    if FilenameDbg <> '' then
      FDebugList.SaveToFile(FilenameDbg);
    if FilenameLst <> '' then
      FListing.SaveToFile(FilenameLst);
    if FBytesTotal > 0 then
      begin
//      FOutput.SaveDebug(FilenameDbg);
        FOutput.SaveHex(FilenameHex);
        FOutput.SaveObject(FilenameObj);
      end;
  finally
    Monitor(ltInfo,'%d lines assembled, %d bytes generated',[FLineCount, FBytesTotal]);
    FAssemblyEnd := Now;
    Monitor(ltVerbose,'Assembly ended %s',[FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',FAssemblyEnd)]);
    Monitor(ltVerbose,'Total assembly time %.3f seconds',[(FAssemblyEnd-FAssemblyStart)*86400.0]);
    FilesClose;
  end;
end;

procedure TAssembler65.AssemblePass(_pass: integer);
begin
  FPass := _pass;
  Monitor(ltInfo,'ASSEMBLER PASS %d',[_pass]);
  InitPass;
  ProcessFile(FFilenameSrc);
  if FDefiningMacro then
    Monitor(ltError,'Pass terminated unexpectedly in the middle of a .MACRO block');
end;

procedure TAssembler65.FilesClose;
begin
  if Assigned(FStreamLog) then
    begin
      Monitor(ltDebug,'Closing %s',[FilenameLog]);
      FreeAndNil(FStreamLog);
    end;
end;

procedure TAssembler65.FilesOpen;
begin
  try
    Monitor(ltDebug,'Opening %s',[FilenameLog]);
    if FilenameLog <> '' then
      FStreamLog := TFileStream.Create(FilenameLog,fmCreate,fmShareDenyWrite);
  except
    Monitor(ltError,'Unable to create log file %s',[FilenameLog]);
  end;
end;

function TAssembler65.GetSource: string;
begin
  Result := '';
  if FFileStack.Count > 0 then
    Result := FFileStack.Filename;
end;

procedure TAssembler65.InitLine;
begin
  FAddrMode := ADM_IMPL;
  FOpCode := OPC_NOP;
  FBytesFromLine := 0;
end;

procedure TAssembler65.InitPass;
var i: integer;
    symbolstring: string;
    symbolname:   string;
    symbolval:    string;
    symbolvaln:   integer;
    symindex:     integer;
    symentry:     TSymbolEntry;
    eqpos:        integer;
    message:      string;
begin
  FBytesTotal := 0;
  FDefiningMacro := False;
  FForceList := False;
  FLineCount := 0;
  FList := True;
  FListNext := True;
  FMacroList.Clear;
  FMacroList.Init;
  FMacroNestLevel := 0;
  FOrg := $0200;
  FOutput.Clear;
  // Now add the predefined symbols if present on the command line
  for i := 0 to CmdDefines.Count-1 do
    begin
      symbolstring := CmdDefines[i];
      eqpos := Pos('=',symbolstring);
      if eqpos = 0 then
        message := FSymbols.Define(FPass,symbolstring)
      else
        begin // Symbol AND defined value
          symbolname := Copy(symbolstring,1,eqpos-1);
          symbolval  := Copy(symbolstring,eqpos+1,9999);
          try
            symbolvaln := StrToInt(symbolval);
            message := FSymbols.Define(FPass,symbolname,symbolvaln);
          except
            message := FSymbols.Define(FPass,symbolname,symbolval);
          end;
        end;
      if message <> '' then
        Monitor(ltError,message);
    end;
end;

procedure TAssembler65.InitStart;
begin
  Monitor(ltWarAndPeace,'Defines = %s',[CmdDefines.DelimitedText]);
  Monitor(ltWarAndPeace,'Includes = %s',[CmdIncludes.DelimitedText]);
  Monitor(ltWarAndPeace,'D65 filename = %s',[FilenameDbg]);
  Monitor(ltWarAndPeace,'HEX filename = %s',[FilenameHex]);
  Monitor(ltWarAndPeace,'LST filename = %s',[FilenameLst]);
  Monitor(ltWarAndPeace,'LOG filename = %s',[FilenameLog]);
  Monitor(ltWarAndPeace,'MAP filename = %s',[FilenameMap]);
  Monitor(ltWarAndPeace,'O65 filename = %s',[FilenameObj]);
  Monitor(ltWarAndPeace,'tab value = %d',[TabSize]);
end;

procedure TAssembler65.Monitor(LogType: TLCGLogType; const Message: string);
var s: string;
    prefix: string;
    lineinfo: string;
    line,column: integer;
    msg: string;
    raisedin: string;
    i: integer;
begin
  if LogType > FLogLevel then
    Exit;
  // Sort out prefix
  case LogType of
    ltInternal:    prefix := 'INTERNAL';
    ltError:       prefix := '   ERROR';
    ltWarning:     prefix := ' WARNING';
    ltInfo,
    ltVerbose,
    ltWarAndPeace: prefix := '    INFO';
    ltDebug:       prefix := '   DEBUG';
    otherwise      prefix := '????????';
  end;
  line := 0;
  column := 0;
  if FFileStack.Count > 0 then
    line := FFileStack.InputLine;
  if LogType <= ltWarning then
    column := FInputColumnSave;
  lineinfo := '';
  if line > 0 then
    begin
      if column > 0 then
        lineinfo := Format('[%d:%d] ',[line,column])
      else
        lineinfo := Format('[%d] ',[line]);
    end;
  raisedin := Source;
  msg := Format('%s: %s%s',[prefix,lineinfo,message]);
  if (LogType in [ltInternal,ltError,ltWarning]) and (raisedin <> '') then
    msg := msg + ', raised in ' + raisedin;
  if FLogLevel >= ltVerbose then
    for i := FFileStack.Count-2 downto 0 do
      msg := msg + #13 + #10 + '          > ' + FFileStack[i].Filename;

  if Assigned(FStreamLog)  then
    begin
      s := msg + #13 + #10;
      FStreamLog.Write(s[1],Length(s));
    end;
  inherited Monitor(LogType,msg);
end;

procedure TAssembler65.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TAssembler65.OutputDebugLine(const _asmline: string);
var addr:     uint16;
begin
  if (FPass = 1) then
    Exit;
  if FBytesFromLine > 0 then
    addr := FOrg
  else
    addr := $FFFF;
  FDebugList.Add(TDebugEntry.Create(addr,FBytesFromLine,FOutputArr,_asmline));
end;

procedure TAssembler65.OutputListingLine(const _asmline: string);
const MAX_BYTES_PER_ROW = 4;
      HEX_WIDTH = MAX_BYTES_PER_ROW * 3 + 7;
      MAX_MACRO_INDENTS = 3;
{$DEFINE SHORTEN_OVERSPILL}
var byteindex: integer;
    bytesleft: integer;
    bcount:    integer;
    i:         integer;
    s:         string;
begin
  if (FPass = 1) or
     (not FList) or
     (not FFileStack.IsListing) then
    Exit;
  if FBytesFromLine = 0 then
    FListing.Add(StringOfChar(' ',HEX_WIDTH+MAX_MACRO_INDENTS) + _asmline)
  else
    begin
      byteindex := 0;
      bytesleft := FBytesFromLine;
      bcount := bytesleft;
      if bcount > MAX_BYTES_PER_ROW then
        bcount := MAX_BYTES_PER_ROW;
      bytesleft := bytesleft - bcount;
      s := Format('%4.4X:',[byteindex+FOrg]);
      for i := 0 to bcount-1 do
        s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
{$IFDEF SHORTEN_OVERSPILL}
      if bytesleft > 0 then
        s := s + '+';
{$ENDIF}
      s := PadRight(s,HEX_WIDTH);
      if FMacroNestLevel > MAX_MACRO_INDENTS then
        s := s + '>' + StringOfChar('.',MAX_MACRO_INDENTS-2) + '>'
      else if FMacroNestLevel = 0 then
        s := s + Space(MAX_MACRO_INDENTS)
      else
        begin
          s := s + StringOfChar('>',FMacroNestLevel);
          s := s + StringOfChar(' ',MAX_MACRO_INDENTS-FMacroNestLevel);
        end;
      s := s + _asmline;
      FListing.Add(s);
      byteindex := byteindex + bcount;
{$IFNDEF SHORTEN_OVERSPILL}
      // Do any overspill lines if required
      while bytesleft > 0 do
        begin
          bcount := bytesleft;
          if bcount > MAX_BYTES_PER_ROW then
            bcount := MAX_BYTES_PER_ROW;
          bytesleft := bytesleft - bcount;
          s := Format('%4.4X:',[byteindex+FOrg]);
          for i := 0 to bcount-1 do
            s := s + Format(' %2.2X',[FOutputArr[i+byteindex]]);
          FListing.Add(s);
          byteindex := byteindex + bcount;
        end;
{$ENDIF}
    end;
end;

procedure TAssembler65.ProcessFile(const _fn: string; _listing: boolean);
begin
  Monitor(ltWarAndPeace,'Processing file %s',[_fn]);
  FFileStack.Push(_fn,_listing);
  try
    ProcessFileInner;
  finally
    FFileStack.Pop();
  end;
end;

procedure TAssembler65.ProcessFileInner;
var asmline: string;
    incl:    string;
    list:    boolean;
    anew:    integer;
begin
  while not FFileStack.EOF do
    begin
      InitLine;
      asmline := FFileStack.GetLine;
      if LogLevel >= ltDebug then
        Monitor(ltDebug,'> ' + asmline);
      Inc(FLineCount);
      // Check for macro definition
      if FDefiningMacro then
        FMacroCapture.Add(asmline);
      // Assemble the line here
      if asmline <> '' then
        begin
          ProcessLine(asmline);
          // Add to debug
          OutputDebugLine(asmline);
          // Add to listing
          OutputListingLine(asmline);
          // Bump org
          anew := FOrg;
          anew := anew + FBytesFromLine;
          FOrg := anew and $FFFF;
          if (anew > $FFFF) and (FPass = 2) then
            Monitor(ltWarning,'ORG has wrapped back round to zero');
          FBytesTotal := FBytesTotal + FBytesFromLine;
          // Check to see if listing flag has changed
          FList := FListNext;
          // Check to see if there is a macro to expand
          if FProcessMacro <> '' then
            ProcessMacroExpansion;
          // Check to see if we created a new include file to use
          if FIncludeNext <> '' then
            begin
              incl := FIncludeNext;
              list := FForceList;
              FIncludeNext := '';
              FForceList := False;
              ProcessFile(incl,list); // Recursive!!!
            end;
        end
      else
        OutputListingLine(asmline);
    end;
end;

procedure TAssembler65.ProcessMacroExpansion;
var parms:      TStringList;
    index:   integer;
    sl:      TStringList;
    savedprefix: string;
begin
  // Insert the macro definition into the current "stream"
  index := FMacroList.IndexOf(FProcessMacro);
  if index < 0 then
    Monitor(ltError,'Macro %s is not defined',[FProcessMacro]);
  parms := TStringList.Create;
  Inc(FMacroNestLevel);
  try
    savedprefix := FLocalPrefix;
    FLocalPrefix := FMacroList.LocalPrefix;
    parms.Delimiter := ',';
    parms.DelimitedText := FProcessParms;
    sl := FMacroList.Items[index].FList;
    FFileStack.PushMacro(FProcessMacro,sl,parms);
    FProcessMacro := '';
    FProcessParms := '';
    ProcessFileInner;
    FFileStack.Pop;
    FLocalPrefix := savedprefix;
  finally
    parms.Free;
    Dec(FMacroNestLevel);
  end;
end;

function TAssembler65.ProcessingAllowed: boolean;
begin
  Result := FIfStack.Allowed and (not FDefiningMacro);
end;

procedure TAssembler65.ProcessLine(const _line: string);
var strm: TStringStream;
begin
  if (Length(_line) > 0) and (_line[1] = '*') then
    Exit; // Comment line
  strm := TStringStream.Create(_line);
  try
    Parse(strm);
  finally
    strm.Free;
  end;
end;

function TAssembler65.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  Result.Buf := '';
  if Assigned(FProcArray[RuleIndex]) then
    Result := FProcArray[RuleIndex](Parser)
  else
    Monitor(ltInternal,'Code not defined for rule no. %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TAssembler65.RegisterProc(const _procname: string; _proc: TLCGParserProc; _procs: TStringArray);
var i: integer;
    done_one: boolean;
begin
  done_one := False;
  for i := 0 to Rules-1 do
    if _procs[i] = _procname then
      begin
        FProcArray[i] := _proc;
        done_one := True;
      end;
  if not done_one then
    Monitor(ltInternal,'Could not find procedure %s in grammar',[_procname]);
end;

procedure TAssembler65.RegisterProcs;
var _procs: TStringArray;
begin
  _procs := RuleProcs;
  RegisterProc('ActBinLiteral',     @ActBinLiteral, _procs);
  RegisterProc('ActCharLiteral',    @ActCharLiteral, _procs);
  RegisterProc('ActCopy1',          @ActCopy1, _procs);
  RegisterProc('ActCompEQ',         @ActCompEQ, _procs);
  RegisterProc('ActCompGE',         @ActCompGE, _procs);
  RegisterProc('ActCompGT',         @ActCompGT, _procs);
  RegisterProc('ActCompLE',         @ActCompLE, _procs);
  RegisterProc('ActCompLT',         @ActCompLT, _procs);
  RegisterProc('ActCompNE',         @ActCompNE, _procs);
  RegisterProc('ActDecLiteral',     @ActDecLiteral, _procs);
  RegisterProc('ActDirByte',        @ActDirByte, _procs);
  RegisterProc('ActDirDB',          @ActDirDB, _procs);
  RegisterProc('ActDirDD',          @ActDirDD, _procs);
  RegisterProc('ActDirDefine',      @ActDirDefine, _procs);
  RegisterProc('ActDirDefineExpr',  @ActDirDefineExpr, _procs);
  RegisterProc('ActDirDefineString',@ActDirDefineString, _procs);
  RegisterProc('ActDirDefmacro',    @ActDirDefmacro, _procs);
  RegisterProc('ActDirDS',          @ActDirDS, _procs);
  RegisterProc('ActDirDSH',         @ActDirDSH, _procs);
  RegisterProc('ActDirDSZ',         @ActDirDSZ, _procs);
  RegisterProc('ActDirDW',          @ActDirDW, _procs);
  RegisterProc('ActDirElse',        @ActDirElse, _procs);
  RegisterProc('ActDirEndif',       @ActDirEndif, _procs);
  RegisterProc('ActDirEndm',        @ActDirEndm, _procs);
  RegisterProc('ActDirError',       @ActDirError, _procs);
  RegisterProc('ActDirIf',          @ActDirIf, _procs);
  RegisterProc('ActDirIfdef',       @ActDirIfdef, _procs);
  RegisterProc('ActDirIfndef',      @ActDirIfndef, _procs);
  RegisterProc('ActDirInclude',     @ActDirInclude, _procs);
  RegisterProc('ActDirIncludeList', @ActDirIncludeList, _procs);
  RegisterProc('ActDirList',        @ActDirList, _procs);
  RegisterProc('ActDirMacro',       @ActDirMacro, _procs);
  RegisterProc('ActDirMacroNoexpr', @ActDirMacroNoexpr, _procs);
  RegisterProc('ActDirMessage',     @ActDirMessage, _procs);
  RegisterProc('ActDirNolist',      @ActDirNolist, _procs);
  RegisterProc('ActDirOrg',         @ActDirOrg, _procs);
  RegisterProc('ActDirSet',         @ActDirSet, _procs);
  RegisterProc('ActDirUndefine',    @ActDirUndefine, _procs);
  RegisterProc('ActDirWarning',     @ActDirWarning, _procs);
  RegisterProc('ActExprAdd',        @ActExprAdd, _procs);
  RegisterProc('ActExprAnd',        @ActExprAnd, _procs);
  RegisterProc('ActExprBracket',    @ActExprBracket, _procs);
  RegisterProc('ActExprDiv',        @ActExprDiv, _procs);
  RegisterProc('ActExprList',       @ActExprList, _procs);
  RegisterProc('ActExprMinus',      @ActExprMinus, _procs);
  RegisterProc('ActExprMod',        @ActExprMod, _procs);
  RegisterProc('ActExprMul',        @ActExprMul, _procs);
  RegisterProc('ActExprNot',        @ActExprNot, _procs);
  RegisterProc('ActExprOr',         @ActExprOr, _procs);
  RegisterProc('ActExprShl',        @ActExprShl, _procs);
  RegisterProc('ActExprShr',        @ActExprShr, _procs);
  RegisterProc('ActExprSub',        @ActExprSub, _procs);
  RegisterProc('ActExprXor',        @ActExprXor, _procs);
  RegisterProc('ActFuncAsc',        @ActFuncAsc, _procs);
  RegisterProc('ActFuncHigh',       @ActFuncHigh, _procs);
  RegisterProc('ActFuncIif',        @ActFuncIif, _procs);
  RegisterProc('ActFuncLow',        @ActFuncLow, _procs);
  RegisterProc('ActFuncPos',        @ActFuncPos, _procs);
  RegisterProc('ActFuncValue',      @ActFuncValue, _procs);
  RegisterProc('ActHexLiteral',     @ActHexLiteral, _procs);
  RegisterProc('ActIgnore',         @ActIgnore, _procs);
  RegisterProc('ActInstruction',    @ActInstruction, _procs);
  RegisterProc('ActLabel',          @ActLabel, _procs);
  RegisterProc('ActLabelC',         @ActLabelC, _procs);
  RegisterProc('ActLabelLocal',     @ActLabelLocal, _procs);
  RegisterProc('ActLabelLocalC',    @ActLabelLocalC, _procs);
  RegisterProc('ActLogAnd',         @ActLogAnd, _procs);
  RegisterProc('ActLogNot',         @ActLogNot, _procs);
  RegisterProc('ActLogOr',          @ActLogOr, _procs);
  RegisterProc('ActMacroPlaceholder', @ActMacroPlaceholder, _procs);
  RegisterProc('ActOctLiteral',     @ActOctLiteral, _procs);
  RegisterProc('ActSetOpA',         @ActSetOpA, _procs);
  RegisterProc('ActSetOpAbs',       @ActSetOpAbs, _procs);
  RegisterProc('ActSetOpAbsX',      @ActSetOpAbsX, _procs);
  RegisterProc('ActSetOpAbsY',      @ActSetOpAbsY, _procs);
  RegisterProc('ActSetOpImmed',     @ActSetOpImmed, _procs);
  RegisterProc('ActSetOpImpl',      @ActSetOpImpl, _procs);
  RegisterProc('ActSetOpIndX',      @ActSetOpIndX, _procs);
  RegisterProc('ActSetOpIndY',      @ActSetOpIndY, _procs);
  RegisterProc('ActSetOpInd',       @ActSetOpInd, _procs);
  RegisterProc('ActStrBuild',       @ActStrBuild, _procs);
  RegisterProc('ActStrCat',         @ActStrCat, _procs);
  RegisterProc('ActStrChr',         @ActStrChr, _procs);
  RegisterProc('ActStrDate',        @ActStrDate, _procs);
  RegisterProc('ActStrHex1',        @ActStrHex1, _procs);
  RegisterProc('ActStrHex2',        @ActStrHex2, _procs);
  RegisterProc('ActStringConstant', @ActStringConstant, _procs);
  RegisterProc('ActStringSymbol',   @ActStringSymbol, _procs);
  RegisterProc('ActStrLeft',        @ActStrLeft, _procs);
  RegisterProc('ActStrLower',       @ActStrLower, _procs);
  RegisterProc('ActStrMid',         @ActStrMid, _procs);
  RegisterProc('ActStrRight',       @ActStrRight, _procs);
  RegisterProc('ActStrString',      @ActStrString, _procs);
  RegisterProc('ActStrTime',        @ActStrTime, _procs);
  RegisterProc('ActStrUpper',       @ActStrUpper, _procs);
  RegisterProc('ActValueLocal',     @ActValueLocal, _procs);
  RegisterProc('ActValueOrg',       @ActValueOrg, _procs);
  RegisterProc('ActValueSymbol',    @ActValueSymbol, _procs);
end;

procedure TAssembler65.SetFilenameSrc(const _fn: string);
begin
  FFilenameSrc := _fn;
  if FilenameObj  = '' then FilenameObj  := ChangeFileExt(_fn,'.o65');
end;

procedure TAssembler65.SetOnMonitor(_monitor: TLCGMonitorProc);
begin
  FOnMonitor := _monitor;
  if Assigned(FFileStack) then
    FFileStack.OnMonitor := _monitor;
end;

procedure TAssembler65.WriteMapFile;
var sl: TStringList;
begin
  if FilenameMap = '' then
    Exit;
  sl := TStringList.Create;
  try
    FSymbols.SortByName;
    sl.Add('SYMBOLS BY NAME');
    sl.Add('');
    FSymbols.Dump(sl);
    FSymbols.SortByAddr;
    sl.Add('');
    sl.Add('');
    sl.Add('');
    sl.Add('SYMBOLS BY VALUE');
    sl.Add('');
    FSymbols.Dump(sl);
    sl.SaveToFile(FilenameMap);
  finally
    sl.Free;
  end;
end;

end.

