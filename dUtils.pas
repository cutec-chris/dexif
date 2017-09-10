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

function CvtRational(AText: String): double;
function CvtTime(AText: String): String;
function FmtRational(ANum, ADenom: Integer): String;
function DoubleToRational(AValue: Double): TExifRational;

function FindTextIndexInCode(AText, ACode: String): Integer;
//function Float2Str(AValue: Double; ADecs: Integer = 2): String;

function GetByte(var AStream: TStream): byte;
function GetWord(var AStream: TStream): word;
function GetCardinal(var AStream: TStream): Cardinal;

procedure ExtractGPSPosition(InStr: String; out ADeg, AMin, ASec: Double);
function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
function StrToGPS(s: String): Extended;

procedure InitTagEntry(out ATagEntry: TTagEntry);
function InsertSpaces(ACamelCaseText: String): String;

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

// These formatting functions can be used everywhere
function DefIntFmt(AValue: Integer): String;
function DefRealFmt(AValue: Double): String;
function DefFracFmt(ANum, ADenom: Integer): String;

//  Formatting callbacks
function GpsPosn(instr: String): String;
function GpsAltitude(instr: String): String;
function GpsVersionID(AText: String): String;
function CompCfgCallBack(AText: String): String;
function ExposCallBack(instr: String): String;
function FlashCallBack(instr: String): String;
function SSpeedCallBack(instr: String): String;
function xpTranslate(instr: String): String;
function VersionCallback(AText: String): String;


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

function CvtRational(AText: String): double;
var
  s: String;
  p: Integer;
  intVal, num, denom: Integer;
begin
  Result := 0.0;
  s := trim(AText);
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
  if p > 0 then begin
    if not TryStrToInt(copy(s, 1, p-1), num) then
      exit;
    if not TryStrToInt(copy(s, p+1, MaxInt), denom) then
      exit;
    if denom = 0 then
      exit;
    Result := intVal + num/denom;
  end else
  if TryStrToFloat(s, Result, dExifFmtSettings) then
    Result := Result + intVal;
end;

function CvtTime(AText: String): String;
var
  p, len: integer;
  s: ansistring;
  tHours, tMin, tSec: double;
begin
  Result := AText;                    // if error return input string
  len := Length(dExifDataSep);
  p := pos(dExifDataSep, AText);
  s := copy(AText, 1, p-1);           // get first rational number
  tHours := CvtRational(s);           // bottom of lens speed range
  AText := copy(AText, p+len-1, MaxInt);
  p := pos(dExifDataSep, AText);
  s := copy(AText, 1, p-1);           // get second irrational number
  tMin := CvtRational(s);             // minutes
  AText := copy(AText, p+1, MaxInt);
  tSec := CvtRational(AText);         // seconds
  Result := Format('%.0f:%.0f:%.0f', [tHours, tMin, tSec]);
end;

function DefIntFmt(AValue: Integer): String;
begin
  Result := IntToStr(AValue);
end;

function DefRealFmt(AValue: Double): String;
begin
  Result := Format('%.g', [AValue], dExifFmtSettings);
//  result := FloatToStr(AValue, dExifFmtSettings);
end;

function DefFracFmt(ANum, ADenom: Integer): String;
begin
  if ANum = 0 then
    Result := '0'
  else
  if ADenom = 1 then
    Result := IntToStr(ANum)
  else
  if ANum = ADenom then
    Result := '1'
  else
    Result := Format('%d/%d', [ANum, ADenom]);
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

{ dEXIF exports GPS coordinates as "d degrees m minutes s seconds" }
procedure ExtractGPSPosition(InStr: String; out ADeg, AMin, ASec: Double);
const
   NUMERIC_CHARS = ['0'..'9', '.', ',', '-', '+'];
var
  p, p0: PChar;
  n: Integer;
  s: String;
  res: Integer;
begin
  ADeg := NaN;
  AMin := NaN;
  ASec := NaN;

  if InStr = '' then
    exit;

  // skip leading non-numeric characters
  p := @InStr[1];
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract first value: degrees
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, ADeg, res);

  // skip non-numeric characters between degrees and minutes
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract second value: minutes
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, AMin, res);

  // skip non-numeric characters between minutes and seconds
  while (p <> nil) and not (p^ in NUMERIC_CHARS) do
    inc(p);

  // extract third value: seconds
  p0 := p;
  n := 0;
  while (p <> nil) and (p^ in NUMERIC_CHARS) do begin
    if p^ = ',' then p^ := '.';
    inc(p);
    inc(n);
  end;
  SetLength(s, n);
  Move(p0^, s[1], n*SizeOf(Char));
  val(s, ASec, res);
end;

{ Converts a GPS coordinate (extended data type) to a string }
function GPSToStr(ACoord: Extended; ACoordType: TGpsCoordType;
  AGpsFormat: TGpsFormat = gf_DMS_Short; ADecs: Integer = 0): String;
const
  {$IFDEF FPC}
  DEG_SYMBOL: string = '°';
  {$ELSE}
  DEG_SYMBOL: ansistring = #176;
  // Delphi 7 wants the degree symbol in ANSI, newer versions will convert
  // it to a widechar automatically.
  {$ENDIF}
  RefStr: array[TGpsCoordType] of String[2] = ('NS', 'EW');
var
  idegs, imins: Integer;
  floatval: Extended;
  sgn: String;
begin
  if IsNaN(ACoord) then begin
    Result := '';
    exit;
  end;
  sgn := RefStr[ACoordType][1 + ord(ACoord < 0)];
  ACoord := abs(ACoord);
  case AGpsFormat of
    gf_DD, gf_DD_Short :
      case AGpsFormat of
        gf_DD:
          Result := Format('%.*f degrees', [ADecs, ACoord], dExifFmtSettings);
        gf_DD_Short:
          Result := Format('%.*f%s', [ADecs, ACoord, DEG_SYMBOL], dExifFmtSettings);
      end;
    gf_DM, gf_DM_Short:
      begin
        idegs := trunc(ACoord);
        floatVal := frac(ACoord) * 60;
        case AGpsFormat of
          gf_DM:
            Result := Format('%d degrees %.*f minutes',
              [idegs, ADecs, floatVal], dExifFmtSettings);
          gf_DM_Short:
            Result := Format('%d%s %.*f''',
              [idegs, DEG_SYMBOL, ADecs, floatVal], dExifFmtSettings);
        end;
      end;
    gf_DMS, gf_DMS_Short:
      begin
        idegs := trunc(ACoord);
        imins := trunc(frac(ACoord)*60);
        floatVal := frac(frac(ACoord)*60)*60;  // seconds
        case AGpsFormat of
          gf_DMS:
            Result := Format('%d degrees %d minutes %.*f seconds',
              [idegs, imins, ADecs, floatVal], dExifFmtSettings);
          gf_DMS_Short:
            Result := Format('%d%s %d'' %.*f"',
              [idegs, DEG_SYMBOL, imins, ADecs, floatVal], dExifFmtSettings);
        end;
      end;
  end;

  Result := Result + ' ' + sgn;
end;

{ Converts a string to a GPS extended number. The input string s must be
  formatted as  dd° mm' ss[.zzz]" E|W. Decimal places of seconds are optional.
  Instead of seconds, the string can also contain a fractional part for minutes,
  e.g. dd° m.mmmmmm', or for degress: d.ddddd°
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

Function GpsPosn(InStr: String): String;
const
  {$IFDEF FPC}
  DEGREES: string = '°';
  {$ELSE}
  DEGREES: ansistring = #176;
  {$ENDIF}
var
  p, sl: integer;
  s: string;
  gDegree, gMin, gSec: double;
begin
  sl := length(dExifDataSep);
  Result := instr;                     // if error return input string
  p := Pos(dExifDataSep, instr);
  s := copy(InStr, 1, p-1);            // get first irrational number
  gDegree := CvtRational(s);           // degrees
  InStr := copy(InStr, p+sl, 64);
  p := Pos(dExifDataSep, instr);
  s := copy(InStr, 1, p-1);            // get second irrational number
  gMin := CvtRational(s);              // minutes
  InStr := copy(InStr, p+sl, 64);
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
      Result := Format('%1.4f Decimal Degrees', [gDegree + (gMin + gSec/60)/60], dExifFmtSettings);
    gf_DD_Short:
      Result := Format('%1.4f%s', [gDegree + (gmin + gSec/60)/60, DEGREES], dExifFmtSettings);
    gf_DM:
      Result := Format('%0.0f Degrees %1.2f Minutes',[gDegree, gMin + gsec/60], dExifFmtSettings);
    gf_DM_Short:
      Result := Format('%0.0f%s %1.2f''', [gDegree, DEGREES, gMin +  gsec/60], dExifFmtSettings);
    gf_DMS:
      Result := Format('%0.0f Degrees %0.0f Minutes %0.2f Seconds', [gDegree, gMin, gSec], dExifFmtSettings);
    gf_DMS_Short:
      Result := Format('%0.0f%s %0.0f'' %0.2f"', [gDegree, DEGREES, gMin, gSec], dExifFmtSettings);
  end;
end;

function GpsAltitude(InStr: string): String;
var
  gAltitude: double;
begin
  Result := InStr;                        // if error return input string
  gAltitude := CvtRational(InStr);        // meters/multiplier, e.g.. 110/10
  Result := Format('%1.2f m', [gAltitude]);
end;

function GpsVersionID(AText: String): String;
var
  i: Integer;
  sep: Char;
begin
  Result := '';
  sep := ',';
  for i:=1 to Length(dExifDataSep) do
    if dExifDataSep[i] <> ' ' then begin
      sep := char(dExifDataSep[i]);
      break;
    end;

  for i:=1 to Length(AText) do begin
    if AText[i] = sep then
      Result := Result + '.'
    else if AText[i] <> ' ' then
      Result := Result + AText[i];
  end;
end;

function CompCfgCallback(AText: String): String;
var
  i, ti: Integer;
begin
  Result := '';
  for i := 1 to 4 do
    if i <= Length(AText) then begin
      ti := integer(AText[i]);
      case ti of
//        0: Result := Result + '-';
        1: Result := Result + 'Y';
        2: Result := Result + 'Cb';
        3: Result := Result + 'Cr';
        4: Result := Result + 'R';
        5: Result := Result + 'G';
        6: Result := Result + 'B';
      end;
    end;
end;

function FlashCallBack(InStr: String): String;
var
  tmp: integer;
begin
  if (Instr = '') or (InStr = '0') then begin
    Result := 'Unknown';
    exit;
  end;

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

function ExposCallBack(InStr: String): String;
var
  expoTime: double;
begin
  expoTime := StrToFloat(InStr);
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
               {
function Float2Str(AValue: Double; ADecs: Integer = 2): String;
begin
  Result := Format('%.*f', [ADecs, AValue], PointSeparator);
end;
                }
function SSpeedCallBack(InStr: String): String;
var
  expoTime: double;
begin
  expoTime := CvtRational(instr);
  expoTime := 1.0 / exp(expoTime*ln(2));
  Result := Format('%4.4f sec', [expoTime]);
  if expoTime <= 0.5 then
    Result := Result + Format(' (1/%d)', [round(1/ExpoTime)]);
end;

function xpTranslate(InStr: String): String;
var
  i: integer;
  ch: Char;
begin
  Result := '';
  for i := 1 to StrCount(InStr, ',') do
    if odd(i) then
    begin
       ch := char(StrToInt(StrNth(Instr, ',', i)));
       if ch <> #0 then
         Result := Result + ch;
    end;
end;

function VersionCallback(AText: String): String;
begin
  Result := AText;
end;

procedure InitTagEntry(out ATagEntry: TTagEntry);
begin
  with ATagEntry do begin
    TID := 0;          // TagTableID - EXIF use
    TType := 0;        // tag type
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

{ Inserts spaces into a camel-case text, i.e. 'ShutterSpeed' --> 'Shutter Speed'}
function InsertSpaces(ACamelCaseText: String): String;
var
  i: integer;
  ch: char;
  lastUC: boolean;
begin
  if Length(ACamelCaseText) < 3 then begin
    Result := ACamelCaseText;
    exit;
  end;
  lastUC := true;
  Result := ACamelCaseText[1];
  for i := 2 to Length(ACamelCaseText) do
  begin
    ch := ACamelCaseText[i];
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
    s := StrAfter(s, ADelim);
  Result := StrBefore(s, ADelim);
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

