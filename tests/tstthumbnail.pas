unit tstThumbnail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testutils, testregistry,
  dMetadata, dEXIF;

type
  TTstThumbnail_dEXIF= class(TTestCase)
  protected
    FTestcase: Integer;
    FWorkfileName: String;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure InternalCreateThumbnail;
    procedure InternalReadThumbnailCompressionTag;
    procedure InternalRemoveThumbnail;
    procedure InternalReplaceThumbnail;
    procedure InternalSaveThumbnail;
    procedure InternalTestThumbnailBuffer;
  end;

  { Test case for a little endian file }
  TTstThumbnail_dEXIF_LE = class(TTstThumbnail_dEXIF)
  public
    constructor Create; override;
  published
    procedure Test_ReadCompressionTag;
    procedure Test_ThumbnailBuffer;
    procedure Test_RemoveThumbnail;
    procedure Test_SaveThumbnail;
    procedure Test_CreateThumbnail;
    procedure Test_ReplaceThumbnail;
  end;

  { Test case for a big endian file }
  TTstThumbnail_dEXIF_BE = class(TTstThumbnail_dEXIF)
  public
    constructor Create; override;
  published
    procedure Test_ReadCompressionTag;
    procedure Test_ThumbnailBuffer;
    procedure Test_RemoveThumbnail;
    procedure Test_SaveThumbnail;
    procedure Test_CreateThumbnail;
    procedure Test_ReplaceThumbnail;
  end;

implementation

uses
  FileUtil, Variants, crc,
  dUtils;

type
  TThumbTestParams = record
    Filename: string;
    BigEndian: Boolean;
    HasThumbnail: boolean;
    ThumbWidth: Integer;       // -1 --> ignore in test
    ThumbHeight: Integer;
    ThumbSize: Integer;
    Compression: Integer;
  end;

const
  SRC_DIR: String = './testpictures/original/';
  WORK_DIR: String = './testpictures/';
  THUMB_IMAGE = 'thumbnail.jpg';

  NUM_TESTCASES = 2;

  TestParams: Array[0..NUM_TESTCASES-1] of TThumbTestParams = (
{0} ( Filename: 'ExThLE_Nikon.jpg'; BigEndian: false; HasThumbnail: true;
      ThumbWidth: 160; ThumbHeight: 120; ThumbSize: 4303; Compression: 6),
{1} ( Filename: 'ExThBE_Nokia.jpg'; BigEndian: true; HasThumbnail: true;
      ThumbWidth: 160; ThumbHeight: 213; ThumbSize: 12025; Compression: 6)
    );

procedure CreateGreenJpg(AFilename: String; AWidth, AHeight: Integer);
const
  co_BMPWidht = 400;
  co_BMPHeigh = 250;
var
  bmp : TBitmap;
  jpg : TJPEGImage;
begin
  // Bitmap in green
  bmp:=TBitmap.Create;
  jpg:= TJPEGImage.Create;
  try
    bmp.PixelFormat:=pf32bit;
    bmp.SetSize(AWidth, AHeight);
    bmp.Canvas.Brush.Color:=clgreen;
    bmp.Canvas.FillRect(0, 0, AWidth, AHeight);
    jpg.Assign(bmp);
    jpg.SaveToFile(AFilename);
  finally
    bmp.Free;
    jpg.free;
  end;
end;

procedure TTstThumbnail_dEXIF.Setup;
var
  fn: String;
begin
  fn := TestParams[FTestCase].Filename;
  if not FileExists(SRC_DIR + fn) then
    raise Exception.Create('Thumbnail test: Source picture "' + SRC_DIR+fn + '" not found.');

  FWorkFilename := WORK_DIR + fn;
  if FileExists(FWorkFilename) then
    DeleteFile(FWorkFilename);
  CopyFile(SRC_DIR + fn, FWorkFileName);
end;

procedure TTstThumbnail_dEXIF.TearDown;
begin
  if FileExists(FWorkfileName) then
    DeleteFile(FWorkfilename);
end;


constructor TTstThumbnail_dEXIF_LE.Create;
begin
  inherited;
  FTestcase := 0;
end;

constructor TTstThumbnail_dEXIF_BE.Create;
begin
  inherited;
  FTestcase := 1;
end;


{------------------------------------------------------------------------------}
{                        Read the tag "Compression"                            }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalReadThumbnailCompressionTag;
var
  DUT: TImgData;
  v: variant;
  c: integer;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFileName);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkfilename + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');
    if not TestParams[FTestcase].HasThumbnail then
      exit;

    v := DUT.ExifObj.ThumbTagValue['Compression'];
    if VarIsNull(v) then c := -1 else c := v;
    CheckEquals(TestParams[FTestcase].Compression, c,
      'Thumbnail compression tag mismatch in "' + FWorkFilename + '"');
  finally
    DUT.Free;
  end;
end;

procedure TTstThumbnail_dEXIF_LE.Test_ReadCompressionTag;
begin
  InternalReadThumbnailCompressionTag;
end;

procedure TTstThumbnail_dEXIF_BE.Test_ReadCompressionTag;
begin
  InternalReadThumbnailCompressionTag;
end;


{------------------------------------------------------------------------------}
{                             Test of thumbnail buffer                         }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalTestThumbnailBuffer;
var
  DUT: TImgData;
  len: Integer;
  w, h: Integer;
  ms: TMemoryStream;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFileName);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkfilename + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');
    if not TestParams[FTestcase].HasThumbnail then
      exit;

    // Check the size of the buffer
    len := Length(DUT.ExifObj.ThumbnailBuffer);
    CheckEquals(Testparams[FTestcase].ThumbSize, len, 'Thumbnail buffer size mismatch');

    // The buffer is a valid jpeg file. Writing it to a memorystream allows
    // to determine the width and height of the jpeg image and check the
    // validitiy of the jpeg structure
    ms := TMemorystream.Create;
    try
      ms.WriteBuffer(DUT.ExifObj.ThumbnailBuffer[0], Length(DUT.ExifObj.ThumbnailBuffer));
      ms.Position := 0;
      CheckTrue(JPEGImageSize(ms, w, h), 'The thumbnail buffer in "' + FWorkfilename + '" does not contain a valid jpeg');
      CheckEquals(TestParams[FTestcase].ThumbWidth, w, 'Thumbnail width mismatch in "' + FWorkfilename + '"');
      CheckEquals(TestParams[FTestcase].ThumbHeight, h, 'Thumbnail height mismatch in "' + FWorkfilename + '"');
    finally
      ms.Free;
    end;
  finally
    DUT.Free;
  end;
end;

procedure TTstThumbnail_dEXIF_LE.Test_ThumbnailBuffer;
begin
  InternalTestThumbnailBuffer;
end;

procedure TTstThumbnail_dEXIF_BE.Test_ThumbnailBuffer;
begin
  InternalTestThumbnailBuffer;
end;


{------------------------------------------------------------------------------}
{                               Save thumbnail                                 }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalSaveThumbnail;
var
  DUT: TImgData;
  w, h: Integer;
  s: Int64;
  fn: String;
  fs: TFileStream;
  ms: TMemorystream;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFilename);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkfilename + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');
    if not TestParams[FTestcase].HasThumbnail then
      exit;

    fn := ChangeFileExt(FWorkFilename, '') + '_thumb.jpg';
    fs := TFileStream.Create(fn, fmCreate);
    try
      DUT.ExifObj.SaveThumbnailToStream(fs);
    finally
      fs.Free;
    end;
    CheckTrue(FileExists(fn), 'Thumbnail file "' + fn + '" not found.');
  finally
    DUT.Free;
  end;

  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(fn);
    s := ms.Size;
    ms.Position := 0;
    CheckTRUE(JpegImageSize(ms, w, h), 'The thumbnail file "' + fn + '" is not a valid JPEG');
  finally
    ms.Free;
  end;
  CheckEquals(TestParams[FTestcase].ThumbSize, s, 'Thumbnail file size mismatch');
  CheckEquals(TestParams[FTestcase].ThumbWidth, w, 'Thumbnail width mismatch');
  CheckEquals(TestParams[FTestcase].ThumbHeight, h, 'Thumbnail height mismatch');
end;

procedure TTstThumbnail_dEXIF_LE.Test_SaveThumbnail;
begin
  InternalSaveThumbnail;
end;

procedure TTstThumbnail_dEXIF_BE.Test_SaveThumbnail;
begin
  InternalSaveThumbnail;
end;


{------------------------------------------------------------------------------}
{                               Remove thumbnail                               }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalRemoveThumbnail;
var
  DUT: TImgData;
begin
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFileName);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkFileName + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');
    if not TestParams[FTestcase].HasThumbnail then
      exit;

    DUT.ExifObj.RemoveThumbnail;
    DUT.WriteEXIFJpegTo(FWorkFilename);
  finally
    DUT.Free;
  end;

  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFilename);
    CheckTRUE(DUT.HasExif, 'No EXIF written to file "' + FWorkFilename + '"');
    CheckFALSE(DUT.HasThumbnail, 'Written file "' + FWorkfilename + '" still has a thumbnail - it should not.');
  finally
    DUT.Free;
  end;
end;

procedure TTstThumbnail_dEXIF_LE.Test_RemoveThumbnail;
begin
  InternalRemoveThumbnail;
end;

procedure TTstThumbnail_dEXIF_BE.Test_RemoveThumbnail;
begin
  InternalRemoveThumbnail;
end;


{------------------------------------------------------------------------------}
{                              Create thumbnail                                }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalCreateThumbnail;
const
  THUMB_SIZE = 250;
var
  DUT: TImgData;
  w, h: Integer;
  s: Int64;
  fn: String;
  ms: TMemoryStream;
begin
  { We create a new thumbnail from the image itself }
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFilename);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkfilename + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');

    // Create the thumbnail, 250 pixels wide or high (whichever is larger)
    DUT.ExifObj.CreateThumbnail(THUMB_SIZE);

    // Write image to file with "_modified" appended
    fn := ChangeFileExt(FWorkFilename, '') + '_modified.jpg';
    DUT.WriteEXIFJpegTo(fn);
    CheckTrue(FileExists(fn), 'Modified image file "' + fn + '" not found.');
  finally
    DUT.Free;
  end;

  // Now we load the image again and save the thumbnail to a separate file
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(fn);
    CheckTRUE(DUT.HasExif, 'No EXIF in modified file "' + fn + '"');
    CheckTRUE(DUT.HasThumbnail,
      'The modified Image "' + fn + '" has no thumbnail although it was added in the previous step');

    // Isolate thumbnail and save to file
    fn := ChangeFileExt(FWorkfileName, '') + '_thumb.jpg';
    ms := TMemoryStream.Create;
    try
      DUT.ExifObj.SaveThumbnailToStream(ms);
      ms.SaveToFile(fn);
      CheckTrue(FileExists(fn), 'Thumbnail file "' + fn + '" not found.');
      s := ms.Size;
      ms.Position := 0;
      CheckTRUE(JPEGImageSize(ms, w, h), 'No valid jpeg in thumbnail file "' + fn + '"');
    finally
      ms.Free;
    end;
  finally
    DUT.Free;
  end;

  if w > h then
    CheckEquals(THUMB_SIZE, w, 'Thumbnail width mismatch');
  if h > w then
    CheckEquals(THUMB_SIZE, h, 'Thumbnail height mismatch');
end;

procedure TTstThumbnail_dEXIF_LE.Test_CreateThumbnail;
begin
  InternalCreateThumbnail;
end;

procedure TTstThumbnail_dEXIF_BE.Test_CreateThumbnail;
begin
  InternalCreateThumbnail;
end;


{------------------------------------------------------------------------------}
{                            Replace thumbnail                                 }
{------------------------------------------------------------------------------}

procedure TTstThumbnail_dEXIF.InternalReplaceThumbnail;
const
  THUMB_WIDTH = 200;
  THUMB_HEIGHT = 300;
var
  DUT: TImgData;
  w, h: Integer;
  fn: String;
  fs: TFileStream;
  ms: TMemorystream;
  currCRC, expCRC, n: Cardinal;
  currSize, expSize: Int64;
begin
  { Create a jpeg image to be used as a thumbnail }
  fn := ExtractFilepath(FWorkfileName) + THUMB_IMAGE;
  CreateGreenJpg(fn, THUMB_WIDTH, THUMB_HEIGHT);

  { Load this jpeg as a new thumbnail into the test image }
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(FWorkFilename);
    CheckTRUE(DUT.HasExif, 'No EXIF in source file "' + FWorkfilename + '"');
    CheckEquals(TestParams[FTestcase].HasThumbnail, DUT.HasThumbnail,
      'Mismatch in detection of thumbnail of "' + FWorkFileName + '"');

    // Load the thumbnail image.
    ms := TMemoryStream.Create;
    try
      ms.LoadFromFile(fn);
      ms.Position := 0;
      expSize := ms.Size;
      n := crc32(0, nil, 0);
      expCRC := crc32(n, ms.Memory, ms.Size);
      ms.Position := 0;
      DUT.ExifObj.LoadThumbnailFromStream(ms);
    finally
      ms.Free;
    end;

    // Write image to file with "_modified" appended
    fn := ChangeFileExt(FWorkFilename, '') + '_modified.jpg';
    DUT.WriteExifJpegTo(fn);
    CheckTrue(FileExists(fn), 'Modified image file "' + fn + '" not found.');
  finally
    DUT.Free;
  end;

  // Now we load the image again and save the thumbnail to a separate file
  DUT := TImgData.Create;
  try
    DUT.ProcessFile(fn);
    CheckTRUE(DUT.HasExif, 'No EXIF in modified file "' + fn + '"');
    CheckTRUE(DUT.HasThumbnail,
      'The modified Image "' + fn + '" has no thumbnail although it was added in the previous step');

    // Isolate thumbnail and save to file
    fn := ChangeFileExt(FWorkfileName, '') + '_thumb.jpg';
    ms := TMemoryStream.Create;
    try
      DUT.ExifObj.SaveThumbnailToStream(ms);
      CheckTrue(FileExists(fn), 'Thumbnail file "' + fn + '" not found.');
      currSize := ms.Size;
      n := crc32(0, nil, 0);
      currCRC := crc32(n, ms.Memory, ms.Size);
      ms.Position := 0;
      CheckTRUE(JPEGImageSize(ms, w, h), 'No valid jpeg in thumbnail file "' + fn + '"');
    finally
      ms.Free;
    end;
  finally
    DUT.Free;
  end;

  CheckEquals(expSize, currSize, 'Thumbnail file size mismatch');
  CheckEquals(THUMB_WIDTH, w, 'Thumbnail width mismatch');
  CheckEquals(THUMB_HEIGHT, h, 'Thumbnail height mismatch');
  CheckEquals(expCRC, currCRC, 'CRC mismatch');
end;

procedure TTstThumbnail_dEXIF_LE.Test_ReplaceThumbnail;
begin
  InternalReplaceThumbnail;
end;

procedure TTstThumbnail_dEXIF_BE.Test_ReplaceThumbnail;
begin
  InternalReplaceThumbnail;
end;


initialization
  RegisterTest(TTstThumbnail_dEXIF_LE);
  RegisterTest(TTstThumbnail_dEXIF_BE);


end.

