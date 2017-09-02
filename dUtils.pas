unit dUtils;

{$IFDEF FPC}
 {$mode ObjFPC}{$H+}
{$ENDIF}

{$I dExif.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}fgl,
  {$ELSE}
  {$IFNDEF dExifNoJpeg}Graphics, jpeg,{$ENDIF}
  {$ENDIF}
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

function NtoLE(const AValue: Word): Word; overload;
function NtoLE(const AValue: DWord): DWord; overload;

function LEtoN(const AValue: Word): Word; overload;
function LEtoN(const AValue: DWord): DWord; overload;

{$ENDIF}

function CvtRational(InStr: AnsiString): double;
function CvtTime(InStr: AnsiString): String;
function FmtRational(ANum, ADenom: Integer): String;
function DoubleToRational(AValue: Double): TExifRational;

function FindTextIndexInCode(AText, ACode: String): Integer;
function Float2Str(AValue: Double; ADecs: Integer = 2): String;

function GetByte(var AStream: TStream): byte;
function GetWord(var AStream: TStream): word;
function GetCardinal(var AStream: TStream): Cardinal;

function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
function StrToGPS(s: String): Extended;

procedure InitTagEntry(out ATagEntry: TTagEntry);
function InsertSpaces(InStr: String): String;

function JPEGImageSize(AStream: TStream; out AWidth, AHeight: Integer): Boolean;
procedure JPEGScaleImage(ASrcStream, ADestStream: TStream;
  ADestSize: Integer = DEFAULT_THUMBNAIL_SIZE);

function MakeHex(s: String): String;
function MakePrintable(s: String): String;

function siif(const ACond: boolean; const s1: String;
  const s2: String=''): String;

function StrBefore(s, ATarget: String): String;
function StrAfter(s, ATarget: String): String;
function StrNth(s, ADelim: String; n: integer): String;
function StrCount(s, ADelim: String): integer;

function aPick(AInfo: String; AItem:integer; ADecodeStr: String): String;
function DecodeField(DecodeStr, idx: String): String;

// These formatting functions can be used elsewhere
function DefIntFmt(inInt: Integer): String;
function DefRealFmt(inReal: Double): String;
function DefFracFmt(inNum, inDenom: Integer): String;

//  Formatting callbacks
function GpsPosn(instr: AnsiString): String;
function GpsAltitude(instr: AnsiString): String;
function GenCompConfig(instr: AnsiString): String;
function ExposCallBack(instr: AnsiString): String;
function FlashCallBack(instr: AnsiString): String;
function SSpeedCallBack(instr: AnsiString): String;
function xpTranslate(instr: AnsiString): String;



implementation

uses
{$IFDEF FPC}
  fpreadjpeg, fpwritejpeg, fpimage, fpcanvas, fpimgcanv,
{$ENDIF}
  Math;

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
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function BEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function NtoBE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;

function NtoBE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := AValue;
  {$ELSE}
  Result := SwapEndian(AValue);
  {$ENDIF}
end;


function LEtoN(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function LEtoN(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function NtoLE(const AValue: Word): Word;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
end;

function NtoLE(const AValue: DWord): DWord;
begin
  {$IFDEF ENDIAN_BIG}
  Result := SwapEndian(AValue);
  {$ELSE}
  Result := AValue;
  {$ENDIF}
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
  result := FloatToStr(inReal, PointSeparator);
end;

function DefFracFmt(inNum, inDenom: Integer): String;
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

procedure DoubleToRationalHelper(AValue: Double; out ANum, ADenom: Integer; Eps: Double);
var
  i: Integer;
  f: Double;
begin
  i := trunc(AValue);
  f := frac(AValue);
  if (1.0 - f) < EPS then begin  //  e.g. 2.999999999999 should be 3
    inc(i);
    f := 0.0;
  end else
  if f < EPS then                // e.g. 3.00000000001 should be 3
    f := 0.0;

  // AValue effectively is an integer
  if f = 0.0 then begin
    ANum := i;
    ADenom := 1;
  end else begin
    ANum := round(AValue / EPS);
    ADenom := round(1.0 / EPS);
  end;
end;

function DoubleToRational(AValue: Double): TExifRational;
const
  EPS = 1E-6;
var
  num, denom: Integer;
  gcdval: Integer;
begin
  if AValue = 0 then begin
    Result.Numerator := 0;
    Result.Denominator := 1;
  end else begin
    if (abs(AValue) > 1) then
      DoubleToRationalHelper(abs(AValue), num, denom, EPS)
    else
      DoubleToRationalHelper(abs(1.0/AValue), denom, num, EPS);
    gcdVal := GCD(num, denom);
    Result.Numerator := num div gcdVal;
    Result.Denominator := denom div gcdVal;
    if AValue < 0 then
      Result.Numerator := -Result.Numerator;
  end;
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

{ Converts a GPS coordinate (extended data type) to a string }
function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
const
  {$IFDEF FPC}
  DEG_SYMBOL: string = '°';
  {$ELSE}
  DEG_SYMBOL: ansistring = #176;  // Delphi 7 wants the degree symbol in ANSI
  {$ENDIF}
  RefStr: array[TGpsCoordType] of String[2] = ('NS', 'EW');
var
  idegs, imins: Integer;
  floatval: Extended;
  sgn: String;
  s: String;
begin
  if IsNaN(ACoord) then begin
    Result := '';
    exit;
  end;
  sgn := RefStr[ACoordType][1 + ord(ACoord < 0)];
  ACoord := abs(ACoord);
  case AGpsFormat of
    gf_DD, gf_DD_Short :
      begin
        s := Float2Str(floatVal, ADecs);
        case AGpsFormat of
          gf_DD:
            Result := Format('%s degrees', [s]);
          gf_DD_Short:
            Result := Format('%s%s', [s, DEG_SYMBOL]);
        end;
      end;
    gf_DM, gf_DM_Short:
      begin
        idegs := trunc(ACoord);
        floatVal := frac(ACoord) * 60;
        s := Float2Str(floatVal, ADecs);
        case AGpsFormat of
          gf_DM:
            Result := Format('%d degrees %s minutes', [idegs, s]);
          gf_DM_Short:
            Result := Format('%d%s %s''', [idegs, DEG_SYMBOL, s]);
        end;
      end;
    gf_DMS, gf_DMS_Short:
      begin
        idegs := trunc(ACoord);
        imins := trunc(frac(ACoord)*60);
        floatVal := frac(frac(ACoord)*60)*60;  // seconds
        s := Float2Str(floatVal, ADecs);
        case AGpsFormat of
          gf_DMS:
            Result := Format('%d degrees %d minutes %s seconds', [idegs, imins, s]);
          gf_DMS_Short:
            Result := Format('%d%s %d'' %s"', [idegs, DEG_SYMBOL, imins, s]);
        end;
      end;
  end;

  Result := Result + ' ' + sgn;
end;

{ Converts a string to a GPS extended number. The input string s must be
  formatted as  dd° mm' ss[.zzz]" E|W. Decimal places of seconds are optional.
  E|W means: either E or W. }
function StrToGPS(s: String): Extended;
var
  ds, ms, ss: String;
  i: Integer;
  tmp: String;
  degs, mins, secs: Extended;
  res: Integer;
  scannedPart: Integer;  // 0=degrees, 1=minutes, 2=seconds
  isFloat: Array[-1..2] of Boolean;
  sgn: Integer;
begin
  if s = '' then begin
    Result := NaN;
    exit;
  end;
  i := 1;
  tmp := '';
  scannedPart := 0;
  isFloat[0] := false;
  isFloat[1] := false;
  isFloat[2] := false;
  degs := 0;
  mins := 0;
  secs := 0;
  sgn := +1;
  while i <= Length(s) do begin
    case s[i] of
      '0'..'9':
        tmp := tmp + s[i];
      '.', ',':
        begin
          tmp := tmp + '.';
          isFloat[scannedPart] := true;
        end;
      ' ':
        if scannedPart = 0 then begin   // in degrees par
          val(tmp, degs, res);
          if res > 0 then
            raise Exception.Create('No numeric data in gps coordinate.');
          tmp := '';
          scannedPart := 1;
        end;
      '''':
        if not isFloat[0] then begin // ignore minutes and seconds if degrees are floats
          val(tmp, mins, res);
          if res > 0 then
            raise Exception.Create('No numeric data in gps coordinate.');
          tmp := '';
          scannedPart := 2;
        end;
      '"':
        // ignore seconds of degrees or minutes are floating point values
        if not (isFloat[0] or isFloat[1]) then begin
          val(tmp, secs, res);
          if res > 0 then
            raise Exception.Create('No numerica data in gps coordinate.');
          tmp := '';
          scannedPart := -1;
        end;
      'W', 'w', 'S', 's':
        sgn := -1;
    end;
    inc(i);
  end;
  Result := (degs + mins/60 + secs/3600) * sgn;
end;

{ Extracts the width and height of a JPEG image from its data without loading
  it into a TJpegImage.
  Returns false if the stream does not contain a jpeg image. }
function JPEGImageSize(AStream: TStream; out AWidth, AHeight: Integer): Boolean;
type
  TJPGHeader = array[0..1] of Byte; //FFD8 = StartOfImage (SOI)
  TJPGRecord = packed record
    Marker: Byte;
    RecType: Byte;
    RecSize: Word;
  end;
var
  n: integer;
  hdr: TJPGHeader;
  rec: TJPGRecord;
  p: Int64;
  savedPos: Int64;
begin
  Result := false;

  AWidth := 0;
  AHeight := 0;

  savedPos := AStream.Position;
  try
    // Check for SOI (start of image) record
    n := AStream.Read(hdr{%H-}, SizeOf(hdr));
    if (n < SizeOf(hdr)) or (hdr[0] <> $FF) or (hdr[1] <> $D8) then
      exit;

    rec.Marker := $FF;
    while (AStream.Position < AStream.Size) and (rec.Marker = $FF) do begin
      if AStream.Read(rec, SizeOf(rec)) < SizeOf(rec) then
        exit;
      rec.RecSize := BEToN(rec.RecSize);
      p := AStream.Position - 2;
      case rec.RecType of
        $C0..$C3:
          if (rec.RecSize >= 4) then // Start of frame markers
          begin
            AStream.Seek(1, soFromCurrent);  // Skip "bits per sample"
            AHeight := BEToN(GetWord(AStream));
            AWidth := BEToN(GetWord(AStream));
            Result := true;
            exit;
          end;
        $D9:  // end of image;
          break;
      end;
      AStream.Position := p + rec.RecSize;
    end;
  finally
    AStream.Position := savedPos;
  end;
end;

procedure JPEGScaleImage(ASrcStream, ADestStream: TStream;
  ADestSize: Integer = DEFAULT_THUMBNAIL_SIZE);
{$IFDEF FPC}
var
  srcImage, destImage: TFPCustomImage;
  destCanvas: TFPImageCanvas;
  reader: TFPCustomImageReader;
  writer: TFPCustomImageWriter;
  w, h: Integer;
  f: Double;
begin
  srcImage := TFPMemoryImage.Create(10, 10);
  reader := TFPReaderJPEG.Create;
  srcImage.LoadFromStream(ASrcStream, reader);
  reader.Free;

  w := srcImage.Width;
  h := srcImage.Height;
  if w > h then f := ADestSize / w else f := ADestSize / h;

  destImage := TFPMemoryImage.Create(round(w*f), round(h*f));
  destCanvas := TFPImageCanvas.Create(destImage);
  destCanvas.StretchDraw(0, 0, destImage.Width, destImage.Height, srcImage);

  writer := TFPWriterJPEG.Create;
  destImage.SaveToStream(ADestStream, writer);
  writer.Free;
end;
{$ELSE}
{$IFNDEF dExifNoJpeg}
var
  jpeg: TJPegImage;
  bmp: TBitmap;
  w, h: Integer;
  f: Double;
begin
  jpeg := TJpegImage.Create;
  try
    jpeg.LoadfromStream(ASrcStream);
    w := jpeg.Width;
    h := jpeg.Height;
    if w > h then f := ADestSize / w else f := ADestSize / h;
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Width := round(w * f);
    bmp.Height := round(h * f);
    bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), jpeg);
    jpeg.Free;
    jpeg := TJpegImage.Create;
    jpeg.Assign(bmp);
    jpeg.SaveToStream(ADestStream);
  finally
    jpeg.Free;
    bmp.Free;
  end;
end;
{$ELSE}
begin
  // CreateThumb will not work in delphi if dExifNoJpeg is defined.
end;
{$ENDIF}
{$ENDIF}


{ Formatting callbacks }

Function GpsPosn(InStr: AnsiString): String;
const
  {$IFDEF FPC}
  DEGREES: string = '°';
  {$ELSE}
  DEGREES: ansistring = #176;
  {$ENDIF}
var
  p, sl: integer;
  s: ansistring;
  gDegree, gMin, gSec: double;
begin
  sl := length(dExifDataSep);
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
      Result := Format('%1.4f Decimal Degrees', [gDegree + (gMin + gSec/60)/60]);
    gf_DD_Short:
      Result := Format('%1.4f%s', [gDegree + (gmin + gSec/60)/60, DEGREES]);
    gf_DM:
      Result := Format('%0.0f Degrees %1.2f Minutes',[gDegree, gMin + gsec/60]);
    gf_DM_Short:
      Result := Format('%0.0f%s %1.2f''', [gDegree, DEGREES, gMin +  gsec/60]);
    gf_DMS:
      Result := Format('%0.0f Degrees %0.0f Minutes %0.0f Seconds', [gDegree, gMin, gSec]);
    gf_DMS_Short:
      Result := Format('%0.0f%s %0.0f'' %0.0f"', [gDegree, DEGREES, gMin, gSec]);
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

{ ACode combines, separated by a comme, a number and a texts (both separated by
  a colon). Searches the specified text and returns the number before the colon}
function FindTextIndexInCode(AText, ACode: String): Integer;
var
  i: Integer;
  indexStr: String;
  codeStr: String;
  inIndex: Boolean;
  len: INteger;
begin
  Result := -1;
  i := 1;
  codeStr := '';
  indexStr := '';
  inIndex := true;
  len := Length(ACode);
  while (i <= len) do begin
    if inIndex then begin
      while (i <= len) and (ACode[i] <> ':') do begin
        indexStr := indexStr + ACode[i];
        inc(i);
      end;
      if indexStr = codeStr then begin
        Result := StrToInt(indexStr);
        exit;
      end;
      inIndex := false;
      codeStr := '';
    end else begin
      while (i <= len) and ((ACode[i] <> ',') or (i = Length(ACode))) do begin
        codeStr := codeStr + ACode[i];
        inc(i);
      end;
      if SameText(AText, codeStr) then begin
        Result := StrToInt(indexStr);
        exit;
      end;
      inIndex := true;
      indexStr := '';
      codeStr := '';
    end;
    inc(i);
  end;
end;

function Float2Str(AValue: Double; ADecs: Integer = 2): String;
begin
  str(AValue:0:ADecs, Result);
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

procedure InitTagEntry(out ATagEntry: TTagEntry);
begin
  with ATagEntry do begin
    TID := 0;          // TagTableID - EXIF use
    TType := 0;        // tag type
    ICode := 0;        // iptc code
    Tag := 0;          // primary key
    Name := '';        // searchable
    Count := 1;        // elements of TType
    Desc := '';        // translatable
    Code := '';        // decode capability
    Data := '';        // display value
    Raw := '';         // unprocessed value
    FormatS := '';     // Format string
    Size := 0;         // used by ITPC module
    CallBack := nil;   // formatting string
    parentID := 0;     // msta - used for exif-parent-child-structure
  end;
end;

function InsertSpaces(InStr: String): String;
var
  i: integer;
  ch: char;
  lastUC: boolean;
begin
  lastUC := true;
  Result := InStr[1]; //Copy(InStr, 1, 1);
  for i := 2 to Length(InStr) do
  begin
    ch := InStr[i];
    if (ch >= 'A') and (ch <= 'Z') then
    begin
      if lastUC then
        Result := Result + ch
      else
        Result := Result + ' ' + ch;
      lastUc := true;
    end
    else
    begin
      lastUC := false;
      Result := Result + ch;
    end;
  end;
end;

function MakeHex(s: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Min(Length(s), 16) do
    Result := Result + IntToHex(ord(s[i]), 2) + ' ';
  if Length(s) > 16 then
    Result := Result + '...';
end;

function MakePrintable(s: String): String;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Min(Length(s), 50) do
    if not (ord(s[i]) in [32..255]) then
      Result := Result + '.'
    else
      Result := Result + s[i];
end;

// Careful : this function's arguments are always evaluated which may have
// unintended side-effects
// (thanks to Jan Derk for pointing this out)
function siif(const ACond: boolean; const s1: String; const s2: String=''): String;
begin
  if ACond then
    Result := s1
  else
    Result := s2;
end;

function StrBefore(s, ATarget: String): String;
var
  p: integer;
begin
  p := Pos(ATarget, s);
  if p = 0 then
    Result := s
  else
    Result := Copy(s, 1, p-1)
end;

function StrAfter(s, ATarget: String): String;
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

function StrNth(s, ADelim: String; n: integer): String;
var
  i: integer;
begin
  for i := 2 to n do
    s := strAfter(s, ADelim);
  Result := strBefore(s, ADelim);
end;

function StrCount(s, ADelim: String): Integer;
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

function aPick(AInfo: String; AItem:integer; ADecodeStr: String): String;
var
  s: String;
begin
  try
    s := StrNth(AInfo, ',', AItem+1);
    Result := DecodeField(ADecodeStr, s);
  except
    Result := '0';
  end;
end;


function DecodeField(DecodeStr, idx: String): String;
var
  stPos: integer;
  ts: String;
begin
  Result := '';
  idx := dExifDecodeSep + Trim(idx) + ':';   // ease parsing
  decodeStr := dExifDecodeSep + DecodeStr + dExifDecodeSep;
  stPos := Pos(idx, DecodeStr);
  if stPos > 0 then
  begin
    ts := copy(DecodeStr, stPos + Length(idx), Length(decodeStr));
    Result := Copy(ts, 1, Pos(dExifDecodeSep, ts) - 1);
  end;
end;

end.

