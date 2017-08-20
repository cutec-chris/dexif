unit dUtils;

{$IFDEF FPC}
 {$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}fgl,{$ENDIF}
  dGlobal;

{$IFDEF FPC}
type
  TInt64List = specialize TFPGList<int64>;
{$ELSE}
type
  TInt64List = class(TList)
  private
    function GetItem(AIndex: Integer): Int64;
    procedure SetItem(AIndex: Integer; AValue: Int64);
  public
    destructor Destroy; override;
    function Add(AValue: Int64): Integer;
    procedure Clear; override;
    property Items[AIndex: Integer]: Int64 read GetItem write SetItem; default;
  end;

  function NtoBE(const AValue: Word): Word; overload;
  function NtoBE(const AValue: DWord): DWord; overload;

  function BEtoN(const AValue: Word): Word; overload;
  function BEtoN(const AValue: DWord): DWord; overload;
{$ENDIF}

function CvtRational(InStr: AnsiString): double;
function CvtTime(InStr: AnsiString): String;
function FmtRational(ANum, ADenom: Integer): String;

function GetByte(var AStream: TStream): byte;
function GetWord(var AStream: TStream): word;
function GetCardinal(var AStream: TStream): Cardinal;

function siif(const ACond: boolean; const s1: AnsiString;
  const s2: AnsiString=''): AnsiString;

function StrBefore(s, ATarget: Ansistring): AnsiString;
function StrAfter(s, ATarget: Ansistring): AnsiString;
function StrNth(s: Ansistring; ADelim: Ansistring; n: integer): AnsiString;
function StrCount(s: Ansistring; ADelim: Ansistring): integer;

function aPick(AInfo: AnsiString; AItem:integer; ADecodeStr: AnsiString): AnsiString;
function DecodeField(DecodeStr, idx: AnsiString): AnsiString;

// These formatting functions can be used elsewhere
function DefIntFmt(inInt: Integer): String;
function DefRealFmt(inReal: Double): String;
function DefFracFmt(inNum, inDenom: Integer): String;

//  Formatting callbacks
Function GpsPosn(instr: AnsiString): String;
Function GpsAltitude(instr: AnsiString): String;
Function GenCompConfig(instr: AnsiString): String;
Function ExposCallBack(instr: AnsiString): String;
Function FlashCallBack(instr: AnsiString): String;
Function SSpeedCallBack(instr: AnsiString): String;
Function xpTranslate(instr: AnsiString): String;


implementation

{$IFNDEF FPC}
//------------------------------------------------------------------------------
//  Helper class: TInt64List - a list for 64-bit integers
//------------------------------------------------------------------------------
type
  TInt64 = record Value: Int64; end;
  PInt64 = ^TInt64;

destructor TInt64List.Destroy;
begin
  Clear;
  inherited;
end;

procedure TInt64List.Clear;
var
  i: Integer;
  P: PInt64;
begin
  for i:=0 to Count-1 do begin
    P := inherited Items[i];
    Dispose(P);
  end;
  inherited Clear;
end;

function TInt64List.Add(AValue: Int64): Integer;
var
  P: PInt64;
begin
  New(P);
  P^.Value := AValue;
  Result := inherited Add(P);
end;

function TInt64List.GetItem(AIndex: Integer): Int64;
begin
  Result := PInt64(inherited Items[AIndex])^.Value;
end;

procedure TInt64List.SetItem(AIndex: Integer; AValue: Int64);
var
  p: PInt64;
begin
  p := inherited Items[AIndex];
  p^.Value := AValue;
end;

function SwapEndian(const AValue: Word): Word; overload;
begin
  Result := Word((AValue shr 8) or (AValue shl 8));
end;

function SwapEndian(const AValue: DWord): DWord; overload;
  begin
    Result := ((AValue shl 8) and $FF00FF00) or ((AValue shr 8) and $00FF00FF);
    Result := (Result shl 16) or (Result shr 16);
  end;

function BEtoN(const AValue: Word): Word;
begin
  Result := SwapEndian(AValue);
end;

function BEtoN(const AValue: DWord): DWord;
begin
  Result := SwapEndian(AValue);
end;

function NtoBE(const AValue: Word): Word;
begin
  Result := SwapEndian(AValue);
end;

function NtoBE(const AValue: DWord): DWord;
begin
  Result := SwapEndian(AValue);
end;

{$ENDIF}

function CvtRational(InStr: AnsiString): double;
var
  s: String;
  p: Integer;
  intVal, num, denom: Integer;
begin
  Result := 0.0;
  s := trim(string(InStr));
  p := pos(' ', s);
  if p > 0 then
  begin
    if not TryStrToInt(copy(s, 1, p-1), intVal) then
      exit;
    s := copy(s, p+1, length(s));
  end
  else
    intVal := 0;
  p := pos('/', s);
  if not TryStrToInt(copy(s, 1, p-1), num) then
    exit;
  if not TryStrToInt(copy(s, p+1, MaxInt), denom) then
    exit;
  if denom = 0 then
    exit;
  Result := intVal + num/denom;
end;

function CvtTime(InStr: AnsiString): String;
var
  p, len: integer;
  s: ansistring;
  tHours, tMin, tSec: double;
begin
  Result := InStr;                    // if error return input string
  len := Length(dExifDataSep);
  p := pos(dExifDataSep, InStr);
  s := copy(InStr, 1, p-1);           // get first rational number
  tHours := CvtRational(s);           // bottom of lens speed range
  InStr := copy(InStr, p+len-1, MaxInt);
  p := pos(dExifDataSep, InStr);
  s := copy(InStr, 1, p-1);           // get second irrational number
  tMin := CvtRational(s);             // minutes
  InStr := copy(InStr, p+1, MaxInt);
  tSec := CvtRational(InStr);         // seconds
  Result := Format('%.0f:%.0f:%.0f', [tHours, tMin, tSec]);
end;

function DefIntFmt(inInt: Integer): String;
begin
  Result := IntToStr(inInt);
end;

function DefRealFmt(inReal: Double): String;
begin
  result := FloatToStr(inReal);
end;

function defFracFmt(inNum, inDenom: Integer): String;
begin
  result := Format('%d/%d', [inNum, inDenom]);
 // result := fmtRational(inNum, inDenom);
 //
 // It turns out this is not a good idea generally
 // because some std. calculation use rational
 // representations internally
end;

function GCD(a, b: integer): integer;
begin
  try
    if (b mod a) = 0 then
      Result := a
    else
      Result := GCD(b, a mod b);
  except
    result := 1
  end;
end;

function FmtRational(ANum, ADenom: integer): String;
var
  gcdVal, intPart, fracPart, newNum, newDenom: integer;
  outStr: String;
begin
  // first, find the values
  gcdVal := GCD(ANum, ADenom);
  newNum := ANum div gcdVal;    // reduce the numerator
  newDenom := ADenom div gcdVal;    //  reduce the denominator
  intPart := newNum div newDenom;
  fracPart := newNum mod newDenom;

  // now format the string
  outStr := '';
  if intPart <> 0 then
    outStr := IntToStr(intPart) + ' ';
  if fracPart <> 0 then
    outStr := outStr + IntToStr(fracPart) + '/' + IntToStr(newDenom);
  result := trim(outstr);      // trim cleans up extra space
end;

{ A simple Delphi-7 compatible way of reading a byte from a stream }
function GetByte(var AStream: TStream): byte;
var
  a: byte;
begin
  AStream.Read(a, 1);
  Result := a;
end;

{ A simple Delphi-7 compatible way of reading two bytes from a stream }
function GetWord(var AStream: TStream): word;
var
  w: word;
begin
  AStream.Read(w, 2);
  Result := w;
end;

{ A simple Delphi-7 compatible way of reading four bytes from a stream }
function GetCardinal(var AStream: TStream): Cardinal;
var
  c: cardinal;
begin
  AStream.Read(c, 4);
  Result := c;
end;

{ Formatting callbacks }

Function GpsPosn(InStr: AnsiString): String;
var
  p, sl: integer;
  s: ansistring;
  gDegree, gMin, gSec: double;
begin
  sl := length(DexifDataSep);
  result := instr;                     // if error return input string
  p := Pos(dExifDataSep,instr);
  s := copy(InStr, 1, p-1);            // get first irrational number
  gDegree := CvtRational(s);           // degrees
  InStr := copy(InStr, p+sl-1, 64);
  p := Pos(DexifDataSep,instr);
  s := copy(InStr, 1, p-1);            // get second irrational number
  gMin := CvtRational(s);              // minutes
  InStr := copy(InStr, p+sl-1, 64);
  gSec := CvtRational(InStr);          // seconds
  if gSec = 0 then       // camera encoded as decimal minutes
  begin
    gSec := ((gMin - trunc(gMin))*100);  // seconds as a fraction of degrees
    gSec := gSec * 0.6;                // convert to seconds
    gMin := trunc(gMin);               // minutes is whole portion
  end;
  // Ok we'll send the result back as Degrees with
  // Decimal Minutes.  Alternatively send back as Degree
  // Minutes, Seconds or Decimal Degrees.
  case GpsFormat of
    gf_DD:
      Result := Format('%1.4f Decimal Degrees',[gDegree + ((gMin + (gSec/60))/60)]);
    gf_DM:
      Result := Format('%0.0f Degrees %1.2f Minutes',[gDegree, gMin + (gsec/60)]);
    gf_DMS:
      Result := Format('%0.0f Degrees %0.0f Minutes %0.0f Seconds', [gDegree, gMin, gSec]);
  end;
end;

function GpsAltitude(InStr: Ansistring): String;
var
  gAltitude: double;
begin
  Result := InStr;                        // if error return input string
  gAltitude := CvtRational(InStr);        // meters/multiplier, e.g.. 110/10
  Result := Format('%1.2f m', [gAltitude]);
end;

function GenCompConfig(InStr: AnsiString): String;
var
  i, ti: Integer;
begin
  Result := '';
  for i := 1+1 to 4+1 do  // skip first char...
  begin
    ti := integer(InStr[i]);
    case ti of
      1: Result := Result + 'Y';
      2: Result := Result + 'Cb';
      3: Result := Result + 'Cr';
      4: Result := Result + 'R';
      5: Result := Result + 'G';
      6: Result := Result + 'B';
    end;
  end;
end;

function FlashCallBack(InStr: AnsiString): String;
var
  tmp: integer;
begin
  tmp := StrToInt(InStr);
  Result :=          siif(tmp and  1 =  1, 'On', 'Off');             // bit0
  Result := Result + siif(tmp and  6 =  2, ', UNKNOWN');             // bit1
  Result := Result + siif(tmp and  6 =  4, ', no strobe return');    // bit2
  Result := Result + siif(tmp and  6 =  6, ', strobe return');       // bit1+2
  Result := Result + siif(tmp and 24 =  8, ', forced');              // bit3
  Result := Result + siif(tmp and 24 = 16, ', surpressed');          // bit4
  Result := Result + siif(tmp and 24 = 24, ', auto mode');           // bit3+4
  Result := Result + siif(tmp and 32 = 32, ', no flash function');   // bit5
  Result := Result + siif(tmp and 64 = 64, ', red-eye reduction');   // bit6
end;

function ExposCallBack(InStr: AnsiString): String;
var
  expoTime: double;
begin
  expoTime := StrToFloat(string(InStr));
  Result := Format('%4.4f sec', [expoTime]);
  if expoTime <= 0.5 then
    Result := Result + Format(' (1/%d)',[round(1/expoTime)]);
// corrected by M. Schwaiger - adding ".5" is senseless when using "round"!
end;

function SSpeedCallBack(InStr: Ansistring): String;
var
  expoTime: double;
begin
  expoTime := CvtRational(instr);
  expoTime := 1.0 / exp(expoTime*ln(2));
  Result := Format('%4.4f sec',[expoTime]);
  if expoTime <= 0.5 then
    Result := Result + Format(' (1/%d)', [round(1/ExpoTime)]);
end;

function xpTranslate(InStr: AnsiString): String;
var
  i: integer;
  ch: AnsiChar;
begin
  Result := '';
  for i := 1 to StrCount(InStr, ',') do
    if odd(i) then
    begin
       ch := AnsiChar(StrToInt(StrNth(Instr, ',', i)));
       if ch <> #0 then
         Result := Result + ch;
    end;
end;

// Careful : this function's arguments are always evaluated which may have
// unintended side-effects
// (thanks to Jan Derk for pointing this out)
function siif(const ACond: boolean; const s1: AnsiString; const s2: AnsiString=''): AnsiString;
begin
  if ACond then
    Result := s1
  else
    Result := s2;
end;

function StrBefore(s, ATarget: Ansistring): Ansistring;
var
  p: integer;
begin
  p := Pos(ATarget, s);
  if p = 0 then
    Result := s
  else
    Result := Copy(s, 1, p-1)
end;

function StrAfter(s, ATarget: Ansistring): Ansistring;
var
  p: integer;
begin
  p := Pos(ATarget, s);
  if p = 0 then
  begin
    if ATarget = '' then
      Result := s
    else
      Result := '';
  end
  else
    Result := Copy(s, p + Length(ATarget), Length(s) - Length(ATarget) - p + 1)
end;

function StrNth(s: AnsiString; ADelim: AnsiString; n: integer): AnsiString;
var
  i: integer;
begin
  for i := 2 to n do
    s := strAfter(s, ADelim);
  Result := strBefore(s, ADelim);
end;

function StrCount(s: AnsiString; ADelim: AnsiString): Integer;
var
  i: Integer;
begin
  i := 0;
  while Pos(ADelim, s) <> 0 do
  begin
    s := StrAfter(s, ADelim);
    inc(i);
  end;
  Result := i;
end;

function aPick(AInfo: AnsiString; AItem:integer;
  ADecodeStr: AnsiString): AnsiString;
var
  s, r: AnsiString;
begin
  try
    s := StrNth(AInfo, ',', AItem+1);
    r := DecodeField(ADecodeStr, s);
  except
    r := '0';
  end;
  Result := r;
end;


function DecodeField(DecodeStr, idx: AnsiString): AnsiString;
var
  stPos: integer;
  ts: AnsiString;
begin
  Result := '';
  idx := dExifDecodeSep + AnsiString(Trim(String(idx))) + ':';   // ease parsing
  decodeStr := dExifDecodeSep + DecodeStr + dExifDecodeSep;
  stPos := Pos(idx, DecodeStr);
  if stPos > 0 then
  begin
    ts := copy(DecodeStr, stPos+Length(idx), Length(decodeStr));
    Result := Copy(ts, 1, Pos(dExifDecodeSep, ts) - 1);
  end;
end;


end.
