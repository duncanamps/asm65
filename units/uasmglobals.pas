unit uasmglobals;

//
// Global definitions for the assembler
//

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const
  DEFAULT_LISTING_MARGIN_LEFT       = 0;
  DEFAULT_LISTING_MARGIN_TOP        = 2;
  DEFAULT_LISTING_PAGE_LENGTH       = 40;
  DEFAULT_LISTING_PAGE_WIDTH        = 120;
  INCLUDE_FILE_DELIMITER            = ',';
  HEX_FILE_BYTES_PER_LINE           = 16;
  MAX_BYTES_PER_CODE_RECORD         = 256;
  MAX_DIGITS_LINENUMBER             = 5;
  MAX_DIGITS_PAGENUMBER             = 4;
  MAX_HEX_BYTES_IN_LISTING          = 8;
  MAX_NESTED_FILES                  = 16;
  MAX_SOURCE_BYTES_PER_LISTING_LINE = 4;
  TEMP_FILE_DIVIDER                 = '?END?';
  // Derived constants
  HEXBUF_LENGTH                     = ((3*MAX_HEX_BYTES_IN_LISTING) + 7);


type
  TOpcode  = (OPC_ADC,
              OPC_AND,
              OPC_ASL,
              OPC_BCC,
              OPC_BCS,
              OPC_BEQ,
              OPC_BIT,
              OPC_BMI,
              OPC_BNE,
              OPC_BPL,
              OPC_BRK,
              OPC_BVC,
              OPC_BVS,
              OPC_CLC,
              OPC_CLD,
              OPC_CLI,
              OPC_CLV,
              OPC_CMP,
              OPC_CPX,
              OPC_CPY,
              OPC_DEC,
              OPC_DEX,
              OPC_DEY,
              OPC_EOR,
              OPC_INC,
              OPC_INX,
              OPC_INY,
              OPC_JMP,
              OPC_JSR,
              OPC_LDA,
              OPC_LDX,
              OPC_LDY,
              OPC_LSR,
              OPC_NOP,
              OPC_ORA,
              OPC_PHA,
              OPC_PHP,
              OPC_PLA,
              OPC_PLP,
              OPC_ROL,
              OPC_ROR,
              OPC_RTI,
              OPC_RTS,
              OPC_SBC,
              OPC_SEC,
              OPC_SED,
              OPC_SEI,
              OPC_STA,
              OPC_STX,
              OPC_STY,
              OPC_TAX,
              OPC_TAY,
              OPC_TSX,
              OPC_TXA,
              OPC_TXS,
              OPC_TYA,
              OPC_ERR);

  TAddrMode  = (ADM_ABS,
                ADM_ABSX,
                ADM_ABSY,
                ADM_IMPL,
                ADM_IND,
                ADM_INDY,
                ADM_LIT,
                ADM_REL,
                ADM_XIND,
                ADM_ZPG,
                ADM_ZPGX,
                ADM_ZPGY,
                ADM_ACC);



  { Superceded by TSourceLine including TCodeRecord
  TAsmListingRecord = record
    original_filename:  string;      // Shortened version of name, e.g. test_file.asm
    current_filename:   string;      // Long version of name, e.g. C:\temp\myfiles\test_file.asm
    linenumber:         integer;     // Listing line number to be displayed
    code_record:        TCodeRecord; // The output code record
    sourceline:         string;      // The original source line
    warning:            string;      // Any warning message to add
  end;
  }

  TCodeRecord = record
    start_address: word;
    data_length:   integer;
    code_segment:  array [0..MAX_BYTES_PER_CODE_RECORD-1] of byte;
  end;

  TSourceLine = record
    Filename:   string;
    LineNumber: integer;
    OutputCode: boolean;
    OutputList: boolean;
    SourceLine: string;
    CodeRecord: TCodeRecord;
  end;


const
  OpCodeText: array[TOpcode] of string =
    ('ADC','AND','ASL','BCC','BCS','BEQ','BIT','BMI',
     'BNE','BPL','BRK','BVC','BVS','CLC','CLD','CLI',
     'CLV','CMP','CPX','CPY','DEC','DEX','DEY','EOR',
     'INC','INX','INY','JMP','JSR','LDA','LDX','LDY',
     'LSR','NOP','ORA','PHA','PHP','PLA','PLP','ROL',
     'ROR','RTI','RTS','SBC','SEC','SED','SEI','STA',
     'STX','STY','TAX','TAY','TSX','TXA','TXS','TYA',
     '');

  AddrModeText: array [TAddrMode] of string =
    ('ABSOLUTE',
     'ABSOLUTE,X',
     'ABSOLUTE,Y',
     'IMPLIED',
     '[INDIRECT]',
     '[INDIRECT,Y',
     '#LITERAL',
     'RELATIVE',
     '[INDIRECT,X]',
     'ZEROPAGE',
     'ZEROPAGE,X',
     'ZEROPAGE,Y',
     'ACCUMULATOR');


function BinToDecStr(_s: string): string;
function BooleanToYN(_b: boolean): string;
function ExpandTabs(_src: string; _tabsize: integer): string;
function NCSPos(_a, _b: string): integer;
function OctToDecStr(_s: string): string;
procedure UnderlinedText(_sl: TStringList; _text: string; _blank_after: boolean = True; _underline_char: char = '-');

implementation

function BinToDecStr(_s: string): string;
var i: integer;
    v: integer;
begin
  v := 0;
  for i := 3 to Length(_s) do // Start at 3 to skip the '0b' at the start
    begin
      v := v * 2;
      v := v + Ord(_s[i]) - Ord('0');
    end;
  result := IntToStr(v);
end;

function BooleanToYN(_b: boolean): string;
begin
  if _b then
    BooleanToYN := 'Y'
  else
    BooleanToYN := 'N';
end;

function ExpandTabs(_src: string; _tabsize: integer): string;
var i: integer;
begin
  result := '';
  for i := 1 to Length(_src) do
    if _src[i] = #9 then
      repeat  // Expand to boundary
        result := result + ' ';
      until (Length(result) mod _tabsize) = 0
    else
      result := result + _src[i];
  ExpandTabs := result;
end;

function NCSPos(_a, _b: string): integer;
begin
  NCSPos := Pos(UpperCase(_a),UpperCase(_b));
end;

function OctToDecStr(_s: string): string;
var i: integer;
    v: integer;
begin
  v := 0;
  for i := 2 to Length(_s) do // Start at 2 to skip the '0' at the start
    begin
      v := v * 8;
      v := v + Ord(_s[i]) - Ord('0');
    end;
  result := IntToStr(v);
end;

procedure UnderlinedText(_sl: TStringList; _text: string; _blank_after: boolean = True; _underline_char: char = '-');
begin
  _sl.Add(_text);
  _sl.Add(StringOfChar(_underline_char,Length(_text)));
  if _blank_after then
    _sl.Add('');
end;


end.

