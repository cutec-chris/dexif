{ Writer for EXIF data

  Writes the TIFF part of the APP0 segment.
  In a JPEG image, the header of the APP0 segment must have been written before.
}

unit dexifwrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  dglobal, dEXIF;

type
  TInt64List = specialize TFPGList<int64>;

  TExifWriter = class(TBasicMetadataWriter)
  private
    FBigEndian: Boolean;
    FTiffHeaderPosition: Int64;
    FExifSegmentStartPos: Int64;

  protected
    function CalcOffsetFromTiffHeader(APosition: Int64): DWord;
    function FixEndian16(AValue: word): Word;
    function FixEndian32(AValue: DWord): DWord;

    procedure WriteIFD(AStream: TStream; ASubIFDList: TInt64List;
      var ATotalCount: Integer; ADirectoryID: Integer);
    procedure WriteSubIFDs(AStream: TStream; ASubIFDList: TInt64List;
      var ATotalCount: Integer);
    procedure WriteTag(AStream, AValueStream: TStream; ADataStartOffset: Int64;
      ATag: TTagEntry);
    procedure WriteTiffHeader(AStream: TStream);

  public
    constructor Create(AImgData: TImgData); override;
    procedure WriteExifHeader(AStream: TStream);
    procedure WriteToStream(AStream: TStream); override;

    property BigEndian: Boolean read FBigEndian write FBigEndian;
  end;

  EExifWriter = class(Exception);

implementation

uses
  Math;

const
  OFFSET_TO_DATAVALUE = 2 * SizeOf(Word) + SizeOf(DWord);

//------------------------------------------------------------------------------
//  Constructor of the Exif writer
//------------------------------------------------------------------------------
constructor TExifWriter.Create(AImgData: TImgData);
begin
  inherited;
  FExifSegmentStartPos := -1;
end;

//------------------------------------------------------------------------------
// Calculates the difference of the specified stream position to the position
// where the TIFF header starts.
//------------------------------------------------------------------------------
function TExifWriter.CalcOffsetFromTiffHeader(APosition: Int64): DWord;
begin
  if APosition > FTiffHeaderPosition then
    Result := DWord(APosition - FTiffHeaderPosition)
  else
    raise EExifWriter.Create('Incorrect stream position');
end;

//------------------------------------------------------------------------------
// Converts a 2-byte integer to BigEndian format if required
//------------------------------------------------------------------------------
function TExifWriter.FixEndian16(AValue: Word): Word;
begin
  if FBigEndian then
    Result := NtoBE(AValue)
  else
    Result := AValue;
end;

//------------------------------------------------------------------------------
// Converts a 4-byte integer to BigEndian format if required
//------------------------------------------------------------------------------
function TExifWriter.FixEndian32(AValue: DWord): DWord;
begin
  if FBigEndian then
    Result := NtoBE(AValue)
  else
    Result := AValue;
end;


//------------------------------------------------------------------------------
//  Writes the Exif header needed by JPEG files.
//  Call WriteToStream immediately afterwards
//------------------------------------------------------------------------------
procedure TExifWriter.WriteExifHeader(AStream: TStream);
const
  SEGMENT_MARKER = #$FF#$E1;
  EXIF_SIGNATURE = 'Exif'#0#0;
var
  header: String;
begin
  header := SEGMENT_MARKER + #$0 + #$0 + EXIF_SIGNATURE;
  // The two zero bytes are the size of the entire Exif segiment, They will be
  // replaced when the segment is completely written. For this, we store the
  // offset to the begin of the EXIF segment in FExifSegmentStartPos.
  FExifSegmentStartPos := AStream.Position;
  AStream.WriteBuffer(header[1], Length(header));
end;

//------------------------------------------------------------------------------
// Writes all IFD record belonging to the same directory
// -----------------------------------------------------------------------------
procedure TExifWriter.WriteIFD(AStream: TStream; ASubIFDList: TInt64List;
  var ATotalCount: Integer; ADirectoryID: Integer);
var
  valueStream: TMemoryStream;
  i: Integer;
  tag: TTagEntry;
  offs: DWord;
  count: Integer;
  dataStartOffset: Int64;
  startPos: Int64;
  sizeOfTagPart: DWord;
  offsetToNextIFDPos: DWord;
begin
  case ADirectoryID of
    TAG_EXIF_OFFSET    : ADirectoryID := 1;
    TAG_GPS_OFFSET     : ADirectoryID := 3;
    TAG_INTEROP_OFFSET : ADirectoryID := 2;
  end;

  valueStream := TMemoryStream.Create;
  try
    // Count IDF records in this directory
    count := 0;
    for i:=0 to FImgData.ExifObj.FITagCount - 1 do begin
      tag := FImgData.ExifObj.FITagArray[i];
      if tag.ParentID = ADirectoryID then
        inc(count);
    end;

    // No records in this directory? Nothing to do...
    if count = 0 then
      exit;

    // The IFD begins at the current stream position...
    startPos := AStream.Position;
    // ... and knowing the size of tag part of the subdirectory we can
    // calculate where the data part of the subdirectory will begin
    // This is needed in terms of offset from the Tiff header.
    sizeOfTagPart := SizeOf(Word) +  // count of tags in IFD, as 16-bit integer
      count * SizeOf(TIFDRecord) +   // each tag occupies a TIFDRecord
      SizeOf(DWord);                 // offset to next IFD, as 32-bit integer.
    dataStartOffset := startPos + sizeOfTagPart - FTiffHeaderPosition; // + 4;

    // Write record count as 16-bit integer
    AStream.WriteWord(FixEndian16(count));

    // Now write all the records in this directory
    for i:=0 to FImgData.ExifObj.FITagCount-1 do begin
      tag := FImgData.ExifObj.FITagArray[i];
      if tag.ParentID = ADirectoryID then begin
        // Some tags will link to subdirectories. The offset to the start of
        // a subdirectory must be specified in the DataValue field of the
        // written ifd record. Since it is not clear at this moment where the
        // subdirectory will begin we store the offset to the ifd record in
        // ASubIFDlist for later correction.
        if (tag.Tag = TAG_EXIF_OFFSET) or
           (tag.Tag = TAG_GPS_OFFSET) or
           (tag.Tag = TAG_INTEROP_OFFSET) or
           (tag.Tag = TAG_SUBIFD_OFFSET)
        then
          ASubIFDList.Add(AStream.Position);

        // Now write the tag
        WriteTag(AStream, valueStream, datastartOffset, tag);
        inc(ATotalCount);
      end;
    end;

    // Store the current stream position. We will have to write the offset
    // to the next IFD here.
    offsetToNextIFDPos := AStream.Position;
    AStream.WriteDWord(0);

    // copy the value stream to the end of the tag stream (AStream)
    valueStream.Seek(0, soFromBeginning);
    AStream.Seek(0, soFromEnd);
    AStream.CopyFrom(valueStream, valueStream.Size);

    // Now that the entire IFD has been written we must complete the skipped
    // field for the offset to the next IFD (calculated from TIFF header start).
    // All tags written?
    inc(ATotalCount, count);
    if ATotalCount = FImgData.ExifObj.FITagCount then
      // yes: The field for the offset to the next IFD will get a zero.
      offs := 0
    else
      // no: The next IFD will follow immediately, i.e. the value is determined
      // by the current stream position;
      offs := CalcOffsetFromTiffHeader(AStream.Position);

    // Move to the position remembered above
    AStream.Position := offsetToNextIFDPos;
    // ... and write the offset value
    AStream.WriteDWord(FixEndian32(offs));

    // Rewind the stream to its end
    AStream.Seek(0, soFromEnd);

  finally
    valueStream.Free;
  end;
end;

//------------------------------------------------------------------------------
// The integer list ASubIFDList contains all the stream positions (in AStream)
// where tags begin which link to a subdirectory.
// WriteSubIFDs will read back the TagID of the subdirectory, write the tags
// of the subdirectory and write the position where the subdirectory starts
// to the tag's DataValue field in AStream.
//------------------------------------------------------------------------------
procedure TExifWriter.WriteSubIFDs(AStream: TStream; ASubIFDList: TInt64List;
  var ATotalCount: Integer);
var
  subIFDStartPos: Int64;
  tagPos: Int64;
  i: Integer;
  id: DWord;
  rec: TIFDRecord = (TagID:0; DataType:0; DataSize:0; DataValue:0);
  offs: DWord;
begin
  i := 0;
  while i < ASubIFDList.Count do begin
    // The current stream position is where the subdirectory tags will be
    // begin. It must be written to the subdirectory tag's DataValue field.
    subIFDStartPos := AStream.Position;

    // Extract the ID of the tag linking to the first subdirectory in the list
    // from the already written stream. Use the offset stored in ASubIFDList
    // to find it.
    tagPos := ASubIFDList[0];
    AStream.Position := tagPos;

    // Read the tag's IFD record
    AStream.ReadBuffer(rec, SizeOf(rec));

    // Get the TagID of the subdirectory
    if FBigEndian then id := BEToN(rec.TagID) else id := rec.TagID;
    // Write the correct subdirectory start position to the IFD record
    offs := CalcOffsetFromTiffHeader(subIFDStartPos);
    rec.DataValue := FixEndian32(offs);

    // Write the IFD record back to the stream
    AStream.Position := tagPos;
    AStream.WriteBuffer(rec, SizeOf(rec));

    // Now return the stream to the end (i.e. where the subdirectory should be)
    // and write the tags of the subdirectory.
    AStream.Seek(0, soFromEnd);
    WriteIFD(AStream, ASubIFDList, ATotalCount, id);

    // Delete the current SubIFDList entry because it already has been handled.
    ASubIFDList.Delete(0);
  end;
end;

//------------------------------------------------------------------------------
// Writes a tag and all its related elements to the stream as an IFDRecord.
//
// AStream: stream to which the tag is written
// AValueStream: Since the data of tags being longer than 4 bytes are written
//   after the tag part of the streasm, but AStream has not seen all tags yet
//   we temporarily write the data part into a separate "value stream".
// ADataStartOffset: Indiates the offset of the first data bytes in the
//   value stream once it has been appended to the output stream (AStream).
//   It is measureed from the beginning of the TIFF header.
// ATag: Tag entry to be written
//------------------------------------------------------------------------------
procedure TExifWriter.WriteTag(AStream, AValueStream: TStream;
  ADataStartOffset: Int64; ATag: TTagEntry);
var
  rec: TIFDRecord;
  rat: TExifRational;
  s: rawbytestring;
begin
  rec.TagID := FixEndian16(ATag.Tag);
  rec.DataType := FixEndian16(ATag.TType);
  if ATag.TType = FMT_STRING then
  begin
    // A null byte must be added to the tag text.
    s := ATag.Raw + #0;
    rec.DataSize := FixEndian32(Length(s));
    rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
    AValueStream.WriteBuffer(s[1], Length(s));
  end else
  if ATag.TType = FMT_BINARY then begin
    rec.DataSize := FixEndian32(Length(ATag.Raw));
    rec.DataValue := ADataStartOffset + AValueStream.Position;
    AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
  end else
  if BYTES_PER_FORMAT[ATag.TType] > 4 then begin
    // If the value requires mote than 4 bytes the data bytes are written to
    // the ValueStream, and the DataValue field gets the offset to the begin
    // of data, counted from the start of the TIFF header. Since the stream
    // with all the IDFRecords is not complete at this moment we store the
    // offsets to these fields in the OffsetList for correction later.
    // For this reason, we do not take care of endianness here as well.
    rec.DataSize := FixEndian32(BYTES_PER_FORMAT[ATag.TType]);
    rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
    case ATag.TType of
      FMT_URATIONAL, FMT_SRATIONAL:
        begin
          rat := PExifRational(@ATag.Raw[1])^;
          rat.Numerator := FixEndian32(rat.Numerator);
          rat.Denominator := FixEndian32(rat.Denominator);
          AValueStream.WriteBuffer(rat, SizeOf(TExifRational));
        end;
      FMT_DOUBLE:
        begin
          AValueStream.WriteBuffer(ATag.Raw[1], SizeOf(Double));
        end;
    end;
  end else
  begin
    // If the size of the data field is not more than 4 bytes
    // then the data value is written to the rec.DataValue field directly.
    rec.DataSize := FixEndian32(BYTES_PER_FORMAT[ATag.TType]);
    case ATag.TType of
      FMT_BYTE, FMT_SBYTE:
        rec.DataValue := byte(ATag.Raw[1]);
      FMT_USHORT, FMT_SSHORT:
        rec.DataValue := FixEndian32(PWord(@ATag.Raw[1])^);
      FMT_ULONG, FMT_SLONG:
        rec.DataValue := FixEndian32(PDWord(@ATag.Raw[1])^);
      FMT_SINGLE:
        Move(ATag.Raw[1], rec.DataValue, SizeOf(Single));
    end;
  end;

  // Write out
  AStream.Write(rec, SizeOf(Rec));
end;

procedure TExifWriter.WriteTiffHeader(AStream: TStream);
const
  LITTLE_ENDIAN_BOM: array[0..1] of AnsiChar = ('I', 'I');
  BIG_ENDIAN_BOM: array[0..1] of AnsiChar = ('M', 'M');
var
  header: TTiffHeader;
begin
  if FBigEndian then
    header.BOM := BIG_ENDIAN_BOM
  else
    header.BOM := LITTLE_ENDIAN_BOM;
  header.Signature := 42;  // magic number
  header.IFDOffset := 0;   // we'll write IDF0 immediately afterwards
  header.IFDOffset := FixEndian32(8);

  // Write out
  AStream.WriteBuffer(header, SizeOf(header));
end;

procedure TExifWriter.WriteToStream(AStream: TStream);
var
  subIFDList: TInt64List;
  totalcount: Integer;
  i: Integer;
  segmentSize: Word;
  startPos: Int64;
begin
  subIFDList := TInt64List.Create;
  try
    totalCount := 0;
    FTiffHeaderPosition := AStream.Position;
    WriteTiffHeader(AStream);

    // Write IFD0
    WriteIFD(AStream, subIFDList, totalCount, 0);

    // Write IFD1
    //WriteIFD(AStream, subIFDList, totalCount, 1);

    // Write special subIFDs collected in subIFDList
    WriteSubIFDs(AStream, subIFDList, totalCount);

    if FExifSegmentStartPos > -1 then begin
      // If WriteToStream is called within a JPEG structure we must update the
      // size of the EXIF segment.
      startPos := FExifSegmentStartPos + SizeOf(word);
      segmentSize := AStream.Position - startPos;
      AStream.Position := startPos;
      AStream.WriteWord(BEToN(segmentSize));
      AStream.Seek(0, soFromEnd);
    end;

  finally
    subIFDList.Free;
  end;
end;

end.

