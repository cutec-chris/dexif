unit tstwritereadexif;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif FPC}

{$i dExifTest.inc}

// If ERASE_TESTIMAGE is active then the test images are deleted after the test.
// Deactivate this define for debugging purposes.
{$DEFINE ERASE_TESTIMAGE}

// If TEST_FILE_INTEGRITY is activated then the written image file is opened
// by a TJpegImage to make sure that it is a valid jpeg.
// Slows down the test!
{$DEFINE TEST_FILE_INTEGRITY}

interface

uses
  Classes
  {$ifdef FPC}
     , Graphics, SysUtils, FileUtil, fpcunit, testutils, testregistry
  {$else}
     , Windows, Graphics, SysUtils, jpeg, TestFrameWork
  {$endif}
     , DateUtils, dMetadata, dEXIF;

const
  // Picture with EXIF data taken from CANON camera }
  co_SrcPic = './testpictures/original/img_9438.jpg';
  co_DestPic = './testpictures/ReadWriteTest.jpg';

type
  { TTstWriteReadFile_dEXIF }

  TTstWriteReadFile_dEXIF= class(TTestCase)
  {$ifdef FPC}
    protected
  {$else}
    public
  {$endif}
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    FSourceFilename: String;
    FDestFileName: String;
    procedure GenericTest(ATestID: Integer);
  published
    procedure Test_DateTimeOriginal;
    procedure Test_DateTimeDigitized;
    procedure Test_DateTimeModify;
    procedure Test_CommentSegment;
    procedure Test_CommentSegment_UTF8;
    procedure Test_Artist;
    procedure Test_Artist_Umlaut;
    procedure Test_ExifComment_ASCII;
    procedure Test_ExifComment_UNICODE;
    procedure Test_ImageDescription;
    procedure Test_CameraMake;
    procedure Test_CameraMake_TooLong;
    procedure Test_CameraModel;
    procedure Test_Copyright;
    procedure Test_GPSLatitude_DMS;
    procedure Test_GPSLongitude_DMS;
    procedure Test_GPSLatitude_DM;
    procedure Test_GPSLongitude_DM;
  end;

{$ifndef FPC}
function scandatetime(const pattern:string;const s:string;const fmt:TFormatSettings;startpos:integer=1) : tdatetime;  overload;

function scandatetime(const pattern:string;const s:string;startpos:integer=1) : tdatetime; overload;

{$endif}
implementation

uses
  Math, dGlobal, dUtils;

type
  TWriteReadParam = record
    Tag: String;
    Value: String;      // all results will be converted to strings
    Decimals: Integer;  // needed for conversion of floats to string
  end;

const
  // !!! INCREMENT WHEN ADDING TESTS !!!
  TestCount = 18;

{$ifdef FPC}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
{$endif}

const
  {$IFDEF FPC}
  DEG_SYMBOL: string = '°';
  {$ELSE}
  DEG_SYMBOL: ansistring = #176;
  // Delphi 7 wants the degree symbol in ANSI, newer versions will convert
  // it to a widechar automatically.
{$ENDIF}

var
// !!! ADD NEW TESTS HERE !!!
  TestParams: Array[0..TestCount-1] of TWriteReadParam = (
{0}  (Tag:'DateTimeOriginal';   Value:'2017-01-01 12:00:00'),
     (Tag:'DateTimeDigitized';  Value:'2017-01-01 12:00:00'),
     (Tag:'DateTimeModify';     Value:'2017-01-01 12:00:00'),
     (Tag:'Comment section';    Value:'This is a comment.'),
     (Tag:'Comment section';    Value:'This is a comment - äöüß.'),
{5}  (Tag:'Artist';             Value:'Ansel Adams'),
     (Tag:'Artist';             Value:'Hansi Müller'),   // Arist with Umlaut
     (Tag:'ExifComment';        Value:'This is a comment'),
     (Tag:'ExifComment';        Value:'äöü αβγ'),
     (Tag:'ImageDescription';   Value:'My image'),
{10} (Tag:'CameraMake';         Value:'Kodak'),   // same length as text in file (Canon)  --> pass
     (Tag:'CameraMake';         Value:'Minolta'), // longer than text in file --> fail
     (Tag:'CameraModel';        Value:'My Super Camera'),
     (Tag:'Copyright';          Value:'(c) Team'),
     (Tag:'GPSLatitude';        Value:'20%s 30'' 40.123" N'),
     (Tag:'GPSLongitude';       Value:'10%s 20'' 30.456" W'),
     (Tag:'GPSLatitude';        Value:'22%s 31.1234567'' S'),
     (Tag:'GPSLongitude';       Value:'11%s 21.3456788'' E')
  );


{ TTstWriteReadFile_dEXIF }

procedure TTstWriteReadFile_dEXIF.SetUp;
{$ifndef FPC}
  function CopyFile(f1,f2:string):boolean;
  begin
    Result:=  {$ifndef DELPHI7}Winapi.{$endif}Windows.CopyFile(PChar(f1),PChar(f2),true);
  end;
{$endif}
begin
  FSourceFileName := co_SrcPic;
  FDestFileName := co_DestPic;
  if not FileExists(co_DestPic) then
    if FileExists(co_SrcPic) then
      CopyFile(co_SrcPic, co_DestPic);
end;

procedure TTstWriteReadFile_dEXIF.TearDown;
begin
  // nothing to do here
  {$IFDEF ERASE_TESTIMAGE}
    if FileExists(FDestFileName) then
      sysutils.DeleteFile(FDestFileName);
  {$ENDIF}
end;


{ Standard test }

procedure TTstWriteReadFile_dEXIF.GenericTest(ATestID: Integer);
var
  srcDUT: TImgData;
  destDUT: TImgData;
  currVal: string;
  expVal: string;
  oldVal: string;
  msg: String;
  imgStream: TMemoryStream;
  jpeg: TJpegImage;
  bmp: TBitmap;

  function ReadTagValue(ATestID: Integer; DUT: TImgData): string;
  var
    f: Extended;
  begin
    Result := '';
    // !!!!!  ADD NEW TESTS HERE !!!!!!
    case ATestID of
      0: Result := FormatDateTime(ISO_DATETIME_FORMAT, DUT.EXIfObj.DateTimeOriginal);
      1: Result := FormatDateTime(ISO_DATETIME_FORMAT, DUT.EXIfObj.DateTimeDigitized);
      2: Result := FormatDateTime(ISO_DATETIME_FORMAT, DUT.EXIfObj.DateTimeModified);
      3: Result := DUT.Comment;
      4: Result := DUT.Comment;
      5: Result := DUT.ExifObj.Artist;
      6: Result := DUT.ExifObj.Artist;
      7: Result := DUT.ExifObj.ExifComment;     // Ascii
      8: Result := DUT.ExifObj.ExifComment;     // Unicode
      9: Result := DUT.ExifObj.ImageDescription;
     10: Result := DUT.ExifObj.CameraMake;
     11: Result := DUT.ExifObj.CameraMake;
     12: Result := DUT.ExifObj.CameraModel;
     13: Result := DUT.ExifObj.TagValueAsString['Copyright'];
     14: begin
           f := DUT.ExifObj.GPSLatitude;
           Result := GpsToStr(f, ctLatitude, gf_DMS_Short, 3);
         end;
     15: begin
           f := DUT.ExifObj.GPSLongitude;
           Result := GpsToStr(f, ctLongitude, gf_DMS_Short, 3);
         end;
     16: begin
           f := DUT.ExifObj.GPSLatitude;
           Result := GpsToStr(f, ctLatitude, gf_DM_Short, 7);
         end;
     17: begin
           f := DUT.ExifObj.GPSLongitude;
           Result := GpsToStr(f, ctLongitude, gf_DM_Short, 7);
         end;
    end;
  end;

  procedure WriteTagValue(ATestID: Integer; DUT: TImgData);
  var
    strValue: String;
  begin
    strValue := TestParams[ATestID].Value;
    // !!!!!  ADD NEW TESTS HERE !!!!!!
    case ATestID of
      0: DUT.ExifObj.DatetimeOriginal := ScanDatetime(ISO_DATETIME_FORMAT, strValue);
      1: DUT.ExifObj.DatetimeDigitized := ScanDatetime(ISO_DATETIME_FORMAT, strValue);
      2: DUT.ExifObj.DatetimeModified := ScanDatetime(ISO_DATETIME_FORMAT, strValue);
      3: DUT.Comment := strValue;
      4: DUT.Comment := strValue;
      5: DUT.ExifObj.Artist := strValue;
      6: DUT.ExifObj.Artist := strValue;
      7: DUT.ExifObj.ExifComment := strValue;    // Ascii
      8: DUT.ExifObj.ExifComment := strValue;    // Unicode
      9: DUT.ExifObj.ImageDescription := strValue;
     10: DUT.ExifObj.CameraMake := strValue;
     11: DUT.ExifObj.CameraMake := strValue;
     12: DUT.ExifObj.CameraModel := strValue;
     13: DUT.ExifObj.TagValueAsString['Copyright'] := strValue;
     14: DUT.ExifObj.GpsLatitude := StrToGps(strValue);
     15: DUT.ExifObj.GPSLongitude := StrToGps(strValue);
     16: DUT.ExifObj.GpsLatitude := StrToGps(strValue);
     17: DUT.ExifObj.GPSLongitude := StrToGps(strValue);
    end;
  end;

begin
  if ATestID >= TestCount then
    fail('Unknown test ID');

  srcDUT := TImgData.Create;
  try
    srcDUT.ProcessFile(FSourceFileName);
    CheckTrue(srcDUT.HasEXIF,'TImgData cannot detect EXIF in file: '+FSourceFilename);

    // -------------------------------------------------------------------------
    // First test
    // -------------------------------------------------------------------------
    // Read the requested tag. If the tag is present, then save the file
    // unchanged, and make sure that the tag has not changed.
    // -------------------------------------------------------------------------
    oldVal := ReadTagValue(ATestID, srcDUT);
    if oldVal <> '' then begin

      imgStream := TMemoryStream.Create;
      try
        imgStream.LoadFromFile(FSourceFilename);
        srcDUT.WriteEXIFJpeg(imgStream, FDestFileName);
      finally
        imgStream.Free;
      end;

      //srcDUT.WriteExifJpeg(FDestFileName, FSourceFilename);

      destDUT := TImgData.Create;
      try
        destDUT.ProcessFile(FDestFileName);
        CheckTrue(destDUT.HasEXIF,'TImgData cannot detect EXIF in file: ' + FDestFileName);
        currVal := ReadTagValue(ATestID, destDUT);
        msg := 'Unintended change of tag ' + TestParams[ATestID].Tag;
        CheckEquals(oldval, currVal, msg);
      finally
        destDUT.Free;
      end;
    end;

    // -------------------------------------------------------------------------
    // Second test
    // -------------------------------------------------------------------------
    // Change the requested tag according to the value in TestParams
    // Read the parameter without saving and check whether it really has
    // changed as intended.
    // -------------------------------------------------------------------------
    WriteTagValue(ATestID, srcDUT);
    currVal := ReadTagValue(ATestID, srcDUT);

    msg := TestParams[ATestID].Tag + ' creation mismatch.';
    CheckEquals(TestParams[ATestID].Value, currVal, msg);

    // -------------------------------------------------------------------------
    // Third test
    // -------------------------------------------------------------------------
    // Save the modified file, read back and check wether the tag value
    // matches the written value
    // -------------------------------------------------------------------------

    srcDUT.WriteExifJpegTo(FDestFileName);
    (*
    imgStream := TMemoryStream.Create;
    try
      imgStream.LoadFromFile(FSourceFilename);
      srcDUT.WriteEXIFJpeg(imgStream, FDestFileName);
    finally
      imgStream.Free;
    end;
      *)
//    srcDUT.WriteExifJpeg(FDestFileName, FSourceFilename);

    destDUT := TImgData.Create;
    try
      destDUT.ProcessFile(FDestFileName);
      CheckTrue(destDUT.HasEXIF,'TImgData cannot detect EXIF in file: ' + FDestFilename);
      currVal := ReadTagValue(ATestID, destDUT);
      msg := TestParams[ATestID].Tag + ' readback mismatch.';
      CheckEquals(TestParams[ATestID].Value, currVal, msg);
    finally
      destDUT.Free;
    end;

    {$IFDEF TEST_FILE_INTEGRITY}
    // -------------------------------------------------------------------------
    // Forth test
    // -------------------------------------------------------------------------
    // Open the written file into a TJpegImage. It must open without an error.
    //--------------------------------------------------------------------------
    jpeg := TJpegImage.Create;
    bmp := TBitmap.Create;
    try
      try
        jpeg.LoadfromFile(FDestFileName);
        bmp.Assign(jpeg);
      except
        fail('Incorrectly written file "' + FDestFileName + '"');
      end;
    finally
      bmp.Free;
      jpeg.Free;
    end;
    {$ENDIF}

  finally
    srcDUT.Free;
  end;
end;

{ DateTime Original }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeOriginal;
begin
  GenericTest(0);
end;

{ DateTime Digitized }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeDigitized;
begin
  GenericTest(1);
end;

{ DateTime Modify }
procedure TTstWriteReadFile_dEXIF.Test_DateTimeModify;
begin
  GenericTest(2);
end;

{ Comment in COM segment }
procedure TTstWriteReadFile_dEXIF.Test_CommentSegment;
begin
  GenericTest(3);
end;

{ Comment in COM segment }
procedure TTstWriteReadFile_dEXIF.Test_CommentSegment_UTF8;
begin
  GenericTest(4);
end;

{ Artist }
procedure TTstWriteReadFile_dEXIF.Test_Artist;
begin
  GenericTest(5);
end;

{ Artist with Umlaut}
procedure TTstWriteReadFile_dEXIF.Test_Artist_Umlaut;
begin
  GenericTest(6);
end;

{ UserComment in EXIF segment - ASCII }
procedure TTstWriteReadFile_dEXIF.Test_ExifComment_ASCII;
begin
  GenericTest(7);
end;

{ UserComment in EXIF - UNICODE }
procedure TTstWriteReadFile_dEXIF.Test_ExifComment_UNICODE;
begin
  GenericTest(8);
end;

{ Image description }
procedure TTstWriteReadFile_dEXIF.Test_ImageDescription;
begin
  GenericTest(9);
end;

{ Camera make }
procedure TTstWriteReadFile_dEXIF.Test_CameraMake;
begin
  GenericTest(10);
end;

procedure TTstWriteReadFile_dEXIF.Test_CameraMake_TooLong;
begin
  GenericTest(11);
end;

{ Camera model }
procedure TTstWriteReadFile_dEXIF.Test_CameraModel;
begin
  GenericTest(12);
end;

{ Copyright }
procedure TTstWriteReadFile_dEXIF.Test_Copyright;
begin
  GenericTest(13);
end;

{ GPS Latitude - DMS format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLatitude_DMS;
const
  idx = 14;
begin
  if pos('%s', TestParams[idx].Value) > 0 then
    TestParams[idx].Value := Format(TestParams[idx].Value, [DEG_SYMBOL]);
  GenericTest(idx);
end;

{ GPS Longitude - DMS format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLongitude_DMS;
const
  idx = 15;
begin
  if pos('%s', TestParams[idx].Value) > 0 then
    TestParams[idx].Value := Format(TestParams[idx].Value, [DEG_SYMBOL]);
  GenericTest(idx);
end;

{ GPS Latitude - DM format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLatitude_DM;
const
  idx = 16;
begin
  if pos('%s', TestParams[idx].Value) > 0 then
    TestParams[idx].Value := Format(TestParams[idx].Value, [DEG_SYMBOL]);
  GenericTest(idx);
end;

{ GPS Longitude - DM format }
procedure TTstWriteReadFile_dEXIF.Test_GPSLongitude_DM;
const
  idx = 17;
begin
  if pos('%s', TestParams[idx].Value) > 0 then
    TestParams[idx].Value := Format(TestParams[idx].Value, [DEG_SYMBOL]);
  GenericTest(idx);
end;

{$ifndef FPC}
//*****************************************************************
// Copied from Freepascal - Start
//*****************************************************************
function scandatetime(const pattern:string;const s:string;const fmt:TFormatSettings;startpos:integer=1) : tdatetime;

var len ,ind  : integer;
    yy,mm,dd  : integer;
    timeval   : TDateTime;
    activequote: char;

procedure raiseexception(const s:string);

begin
  raise EConvertError.Create(s);
end;



procedure intscandate(ptrn:pchar;plen:integer;poffs:integer);
// poffs is the offset to

var
    pind : integer;

function findimatch(const mnts:array of string;p:pchar):integer;
var i : integer;
begin
  result:=-1;
  i:=0;
  while (i<=high(mnts)) and (result=-1) do
    begin
      if AnsiStrLIComp(p,@mnts[i][1],length(mnts[i]))=0 then
        result:=i;
      inc(i);
    end;
end;

procedure arraymatcherror;
const
  SNoArrayMatch                 = 'Can''t match any allowed value at pattern position %d, string position %d.';

begin
  raiseexception(format(SNoArrayMatch,[pind+1,ind]))
end;

function findmatch(const mnts : array of string;const s:string):integer;

begin
  result:=findimatch(mnts,@s[ind]);
  if result=-1 then
    arraymatcherror
  else
    begin
      inc(ind,length(mnts[result])+1);
      inc(pind,length(mnts[result])+1);
      inc(result); // was 0 based.
    end;
end;

var
    pivot,
    i     : integer;

function scanfixedint(maxv:integer):integer;
const
  SPatternCharMismatch          = 'Pattern mismatch char "%s" at position %d.';
var c : char;
    oi:integer;
begin
  result:=0;
  oi:=ind;
  c:=ptrn[pind];
  while (pind<plen) and (ptrn[pind]=c) do inc(pind);
  while (maxv>0) and (ind<=len) and (s[ind] IN ['0'..'9']) do
    begin
      result:=result*10+ord(s[ind])-48;
      inc(ind);
      dec(maxv);
    end;
  if oi=ind then
    raiseexception(format(SPatternCharMismatch,[c,oi]));
end;

procedure matchchar(c:char);
const
  SNoCharMatch                  = 'Mismatch char "%s" <> "%s" at pattern position %d, string position %d.';
begin
  if (ind>len) or (s[ind]<>c) then
    raiseexception(format(SNoCharMatch,[s[ind],c,pind+poffs+1,ind]));
  inc(pind);
  inc(ind);
end;

function scanpatlen:integer;
var c : char;
    lind : Integer;
begin
  result:=pind;
  lind:=pind;
  c:=ptrn[lind];

  while (lind<=plen) and (ptrn[lind]=c) do
      inc(lind);
  result:=lind-result;
end;

procedure matchpattern(const lptr:string);

var len:integer;
begin
  len:=length(lptr);
  if len>0 then
    intscandate(@lptr[1],len,pind+poffs);
end;

const
  SHHMMError                    = 'mm in a sequence hh:mm is interpreted as minutes. No longer versions allowed! (Position : %d).' ;
  SFullpattern                  = 'Couldn''t match entire pattern string. Input too short at pattern position %d.';

const
  AMPMformatting : array[0..2] of string =('am/pm','a/p','ampm');

const whitespace  = [' ',#13,#10];
      hrfactor    = 1/(24);
      minfactor   = 1/(24*60);
      secfactor   = 1/(24*60*60);
      mssecfactor = 1/(24*60*60*1000);

var lasttoken,lch : char;

begin
  pind:=0;     lasttoken:=' ';
  while (ind<=len) and (pind<plen) do
     begin
       lch:=upcase(ptrn[pind]);
       if activequote=#0 then
          begin
            if (lch='M') and (lasttoken='H') then
              begin
                i:=scanpatlen;
                if i>2 then
                  raiseexception(format(Shhmmerror,[poffs+pind+1]));
                timeval:=timeval+scanfixedint(2)* minfactor;
              end
            else
            case lch of
               'H':  timeval:=timeval+scanfixedint(2)* hrfactor;
               'D':  begin
                       i:=scanpatlen;
                       case i of
                          1,2 : dd:=scanfixedint(2);
                          3   : dd:=findmatch(fmt.shortDayNames,s);
                          4   : dd:=findmatch(fmt.longDayNames,s);
                          5   : matchpattern(fmt.shortdateformat);
                          6   : matchpattern(fmt.longdateformat);
                         end;
                     end;
               'N':  timeval:=timeval+scanfixedint(2)* minfactor;
               'S':  timeval:=timeval+scanfixedint(2)* secfactor;
               'Z':  timeval:=timeval+scanfixedint(3)* mssecfactor;
               'Y':  begin
                       i:=scanpatlen;
                       yy:=scanfixedint(i);
                       if i<=2 then
                         begin
                           pivot:=YearOf(now)-fmt.TwoDigitYearCenturyWindow;
                           inc(yy, pivot div 100 * 100);
                           if (fmt.TwoDigitYearCenturyWindow > 0) and (yy < pivot) then
                              inc(yy, 100);
                         end;
                      end;
               'M': begin
                       i:=scanpatlen;
                       case i of
                          1,2: mm:=scanfixedint(2);
                          3:   mm:=findmatch(fmt.ShortMonthNames,s);
                          4:   mm:=findmatch(fmt.LongMonthNames,s);
                          end;
                    end;
               'T' : begin
                       i:=scanpatlen;
                       case i of
                       1: matchpattern(fmt.shortdateformat);
                       2: matchpattern(fmt.longtimeformat);
                       end;
                     end;
               'A' : begin
                            i:=findimatch(AMPMformatting,@ptrn[pind]);
                            case i of
                              0: begin
                                   i:=findimatch(['AM','PM'],@s[ind]);
                                   case i of
                                     0: ;
                                     1: timeval:=timeval+12*hrfactor;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[0]));
                                   inc(ind,2);
                                 end;
                              1: begin
                                    case upcase(s[ind]) of
                                     'A' : ;
                                     'P' : timeval:=timeval+12*hrfactor;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[1]));
                                   inc(ind);
                                 end;
                               2: begin
                                    i:=findimatch([fmt.timeamstring,fmt.timepmstring],@s[ind]);
                                    case i of
                                     0: inc(ind,length(fmt.timeamstring));
                                     1: begin
                                          timeval:=timeval+12*hrfactor;
                                          inc(ind,length(fmt.timepmstring));
                                        end;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[2]));
                                   inc(pind,2);
                                   inc(ind,2);
                                 end;
                            else  // no AM/PM match. Assume 'a' is simply a char
                                matchchar(ptrn[pind]);
                             end;
                         end;
               '/' : matchchar(fmt.dateSeparator);
               ':' : begin
                       matchchar(fmt.TimeSeparator);
                       lch:=lasttoken;
                     end;
               #39,'"' : begin
                           activequote:=lch;
                           inc(pind);
                         end;
               'C' : begin
                       intscandate(@fmt.shortdateformat[1],length(fmt.ShortDateFormat),pind+poffs);
                       intscandate(@fmt.longtimeformat[1],length(fmt.longtimeformat),pind+poffs);
                       inc(pind);
                     end;
               '?' : begin
                       inc(pind);
                       inc(ind);
                     end;
               #9  : begin
                       while (ind<=len) and (s[ind] in whitespace) do
                         inc(ind);
                       inc(pind);
                     end;
               else
                 matchchar(ptrn[pind]);
             end; {case}
             lasttoken:=lch;
            end
          else
            begin
              if activequote=lch then
                begin
                  activequote:=#0;
                  inc(pind);
                end
              else
                matchchar(ptrn[pind]);
            end;
     end;
   if (pind<plen) and (plen>0) and (ptrn[plen-1]<>#9) then  // allow omission of trailing whitespace
     RaiseException(format(SFullpattern,[poffs+pind+1]));
end;

var plen:integer;

begin
  activequote:=#0;
  yy:=0; mm:=0; dd:=0;
  timeval:=0.0;
  len:=length(s); ind:=startpos;
  plen:=length(pattern);
  intscandate(@pattern[1],plen,0);
  result:=timeval;
  if (yy>0) and (mm>0) and (dd>0) then
     result:=result+encodedate(yy,mm,dd);
end;

var
{$ifndef DELPHI7}
  DefaultFormatSettings : TFormatSettings = (
    CurrencyString: '$';
    CurrencyFormat: 1;
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');

    ThousandSeparator: ',';
    DecimalSeparator: '.';
    TwoDigitYearCenturyWindow: 50;
    NegCurrFormat: 5;
  );
{$else}
  DefaultFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun', 
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );
{$endif}

function scandatetime(const pattern:string;const s:string;startpos:integer=1) : tdatetime; overload;

begin
 result:=scandatetime(pattern,s,defaultformatsettings,startpos);
end;
//*****************************************************************
// Copied from Freepascal - End
//*****************************************************************
{$endif}

initialization
  {$ifndef FPC}TestFramework.{$endif}RegisterTest(TTstWriteReadFile_dEXIF{$ifndef FPC}.Suite{$endif});

end.

