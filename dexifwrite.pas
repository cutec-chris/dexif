{ Writer for EXIF data

  Writes the TIFF part of the APP0 segment.
  In a JPEG image, the header of the APP0 segment must have been written before.
}

unit dexifwrite;

{$IFDEF FPC}
 {$mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  dGlobal, dUtils, dMetadata, dEXIF;

type
  TExifWriter = class(TBasicMetadataWriter)
  private
    FBigEndian: Boolean;
    FTiffHeaderPosition: Int64;
    FExifSegmentStartPos: Int64;
    FHasThumbnail: Boolean;

  protected
    function CalcOffsetFromTiffHeader(APosition: Int64): DWord;
    function FixEndian16(AValue: Word): Word;
    function FixEndian32(AValue: DWord): DWord;

    procedure WriteIFD(AStream: TStream; ASubIFDList: TInt64List;
      ADirectoryID: TTagID; AHardwareSpecific: Boolean);
    procedure WriteSubIFDs(AStream: TStream; ASubIFDList: TInt64List;
      AHardwareSpecific: Boolean);
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
  dTags;

//------------------------------------------------------------------------------
//  Constructor of the Exif writer
//------------------------------------------------------------------------------
constructor TExifWriter.Create(AImgData: TImgData);
begin
  inherited;
  FExifSegmentStartPos := -1;
  FHasThumbnail := FImgData.HasExif and FImgData.HasThumbnail and
    (FImgData.ExifObj.ThumbTagCount > 0);
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
    Result := NtoLE(AValue);
end;

//------------------------------------------------------------------------------
// Converts a 4-byte integer to BigEndian format if required
//------------------------------------------------------------------------------
function TExifWriter.FixEndian32(AValue: DWord): DWord;
begin
  if FBigEndian then
    Result := NtoBE(AValue)
  else
    Result := NtoLE(AValue);
end;

//------------------------------------------------------------------------------
//  Writes the Exif header needed by JPEG files.
//  Call WriteToStream immediately afterwards
//------------------------------------------------------------------------------
procedure TExifWriter.WriteExifHeader(AStream: TStream);
const
  SEGMENT_MARKER: array[0..1] of byte = ($FF, $E1);
  EXIF_SIGNATURE: array[0..5] of ansichar = ('E', 'x', 'i', 'f', #0, #0);
  SIZE: Word = 0;
begin
  FExifSegmentStartPos := AStream.Position;
  AStream.WriteBuffer(SEGMENT_MARKER[0], 2);
  // Next two zero bytes are the size of the entire Exif segiment, they will be
  // replaced when the segment is completely written. For this, we store the
  // offset to the begin of the EXIF segment in FExifSegmentStartPos.
  AStream.WriteBuffer(SIZE, 2);
  AStream.WriteBuffer(EXIF_SIGNATURE[0], 6);
end;

//------------------------------------------------------------------------------
// Writes all IFD record belonging to the same directory
// -----------------------------------------------------------------------------
procedure TExifWriter.WriteIFD(AStream: TStream; ASubIFDList: TInt64List;
  ADirectoryID: TTagID; AHardwareSpecific: Boolean);
var
  valueStream: TMemoryStream;
  i: Integer;
  offs: DWord;
  count: Integer;
  dataStartOffset: Int64;
  thumbStartOffset: Int64;
  startPos: Int64;
  sizeOfTagPart: DWord;
  offsetToIFD1: Int64;
  offsetToThumb: Int64;
  w: Word;
  dw: DWord;
  tag: TTagEntry;
  tagArray: TTagArray;
  tagCount: Integer;
  b: TBytes;
begin
  if ADirectoryID = 1 then
    tagCount := FImgData.ExifObj.ThumbTagCount
  else
    tagCount := FImgData.ExifObj.TagCount;

  valueStream := TMemoryStream.Create;
  try
    // Count IFD records in this directory
    count := 0;
    for i:=0 to tagCount - 1 do begin
      if ADirectoryID = 1 then
        tag := FImgData.ExifObj.ThumbTagByIndex[i] else
        tag := FImgData.ExifObj.TagByIndex[i];
      if (tag.ParentID = ADirectoryID) then begin
        if AHardwareSpecific and (tag.TID = 1) or
           ((not AHardwareSpecific) and (tag.TID = 0))
        then
          inc(count);
      end;
    end;

    // The IFD begins at the current stream position...
    startPos := AStream.Position;
    // ... and, knowing the size of tag part of the subdirectory, we can
    // calculate where the data part of the subdirectory will begin
    // This is needed as offset from the begin of the Tiff header.
    sizeOfTagPart := SizeOf(Word) +  // count of tags in IFD, as 16-bit integer
      count * SizeOf(TIFDRecord) +   // each tag occupies a TIFDRecord
      SizeOf(DWord);                 // offset to next IFD, as 32-bit integer.
    dataStartOffset := startPos + sizeOfTagPart - FTiffHeaderPosition;

    if (ADirectoryID = 1) and FImgData.ExifObj.HasThumbnail then begin
      // In case of IFD1 (ADirectoryID = 1) the thumbnail will be written
      // immediately after all tags of IFD1. This offset position must be noted
      // in the tag
      thumbStartOffset := dataStartOffset;
      dataStartOffset := dataStartOffset + Length(FImgData.ExifObj.ThumbnailBuffer);
    end;

    // Write record count as 16-bit integer
    w := FixEndian16(count);
    AStream.WriteBuffer(w, SizeOf(w));

    // Now write all the records in this directory
    if count > 0 then begin
      for i:=0 to tagCount-1 do begin
        if ADirectoryID = 1then
          tag := FImgData.ExifObj.ThumbTagByIndex[i] else
          tag := FImgData.ExifObj.TagByIndex[i];
        if AHardwareSpecific and (tag.TID <> 1) then continue;
        if (not AHardwareSpecific) and (tag.TID = 1) then continue;

        if tag.ParentID = ADirectoryID then begin
          // Some tags will link to subdirectories. The offset to the start of
          // a subdirectory must be specified in the DataValue field of the
          // written ifd record. Since it is not clear at this moment where the
          // subdirectory will begin we store the offset to the ifd record in
          // ASubIFDlist for later correction.
          if TagLinksToSubIFD(tag.Tag) then
            ASubIFDList.Add(AStream.Position);

          // If the tag contains the offset to the thumb image we write the
          // correct value into the tag - it is not known earlier.
          if tag.Tag = TAG_THUMBSTARTOFFSET then begin
            dw := FixEndian32(thumbStartOffset);
            Move(dw, tag.Raw[1], 4);
          end;

          // Now write the tag
          WriteTag(AStream, valueStream, datastartOffset, tag);
        end;
        //inc(tag);
      end;
    end;

    // The last entry of the directory is the offset to the next IFD, or 0
    // if not other IFD follows at the same level. This affects only IFD0
    // where IFD1 can follow if an embedded thumbnail image exists.
    if (ADirectoryID = 0) and FHasThumbnail then begin
      offsetToIFD1 := AStream.Position + SizeOf(DWord) + valuestream.Size;
      offs := CalcOffsetFromTiffHeader(offsetToIFD1);
    end else
      offs := 0;
    dw := FixEndian32(offs);
    AStream.WriteBuffer(dw, SizeOf(dw));

    // Write the thumbnail
    if ADirectoryID = 1 then begin
      b := FImgData.ExifObj.ThumbnailBuffer;
      AStream.Write(b[0], Length(b));
    end;

    // Copy the valuestream to the end of the tag stream (AStream)
    valueStream.Seek(0, soFromBeginning);
    AStream.CopyFrom(valueStream, valueStream.Size);

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
  AHardwareSpecific: Boolean);
var
  subIFDStartPos: Int64;
  tagPos: Int64;
  i: Integer;
  id: TTagID;
  rec: TIFDRecord;
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

    // Get the TagID of the subdirectory (note: this might be written as big-endian)
    // The TagID is needed when calling WriteIFD
    if FBigEndian then id := BEToN(rec.TagID) else id := rec.TagID;

    // Write the correct subdirectory start position to the IFD record
    offs := CalcOffsetFromTiffHeader(subIFDStartPos);
    rec.DataValue := FixEndian32(offs);

    // Write the IFD record back to the stream. Don't forget to return to
    // where the tag starts!
    AStream.Position := tagPos;
    AStream.WriteBuffer(rec, SizeOf(rec));

    // Now return the stream to the end (i.e. where the subdirectory should be)
    // and write the tags of the subdirectory.
    AStream.Seek(0, soFromEnd);
    WriteIFD(AStream, ASubIFDList, id, AHardwareSpecific);

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
  s: ansistring;
  n: DWord;
begin
  rec.TagID := FixEndian16(ATag.Tag);
  rec.DataType := FixEndian16(ATag.TType);
  if ATag.TType = FMT_STRING then
  begin
    s := ATag.Raw;
    if s[Length(s)] <> #0 then s := s + #0;
    rec.DataCount := FixEndian32(Length(s));
    if Length(s) <= 4 then begin
      n := 0;
      Move(s[1], n, Length(s));
      rec.DataValue := n;  // tag.Raw is already has the endianness needed  //FixEndian32(n);
    end else begin
      rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
      AValueStream.WriteBuffer(s[1], Length(s));
    end;
  end else
  if ATag.TType = FMT_BINARY then begin
    rec.DataCount := FixEndian32(Length(ATag.Raw));
    if Length(ATag.Raw) <= 4 then begin
      n := 0;
      Move(ATag.Raw[1], n, Length(ATag.Raw));
      rec.DataValue := n;  // tag.Raw is already has the endianness needed  //FixEndian32(n);
//      rec.DataValue := FixEndian32(n);
    end else begin
      rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
      AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
    end;
  end else
  if BYTES_PER_FORMAT[ATag.TType] > 4 then begin
    // If the value requires mote than 4 bytes the data bytes are written to
    // the ValueStream, and the DataValue field gets the offset to the begin
    // of data, counted from the start of the TIFF header. Since the stream
    // with all the IDFRecords is not complete at this moment we store the
    // offsets to these fields in the OffsetList for correction later.
    // For this reason, we do not take care of endianness here as well.
    rec.DataCount := FixEndian32(Length(ATag.Raw) div BYTES_PER_FORMAT[ATag.TType]);
    rec.DataValue := FixEndian32(DWord(ADataStartOffset + AValueStream.Position));
    case ATag.TType of
      FMT_URATIONAL, FMT_SRATIONAL:
        begin
          AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
          {
          // Note: ATag.Raw already has the correct endianness!
          rat := PExifRational(@ATag.Raw[1])^;
//          rat.Numerator := FixEndian32(rat.Numerator);
//          rat.Denominator := FixEndian32(rat.Denominator);
          rat.Numerator := rat.Numerator;
          rat.Denominator := rat.Denominator;
          AValueStream.WriteBuffer(rat, SizeOf(TExifRational));
          }
        end;
      FMT_DOUBLE:
        begin
          AValueStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
        end;
    end;
  end else
  begin
    // If the size of the data field is not larger than 4 bytes
    // then the data value is written to the rec.DataValue field directly.
    // Note: ATag.Raw already has the correct endianness
    rec.DataCount := FixEndian32(Length(ATag.Raw) div BYTES_PER_FORMAT[ATag.TType]);
    rec.DataValue := 0;
    Move(ATag.Raw[1], rec.DataValue, Length(ATag.Raw));
    {
    rec.DataValue :
    case ATag.TType of
      FMT_BYTE, FMT_SBYTE:
        rec.DataValue := byte(ATag.Raw[1]);
      FMT_USHORT, FMT_SSHORT:
        rec.DataValue := PWord(@ATag.Raw[1])^;
        //rec.DataValue := FixEndian32(PWord(@ATag.Raw[1])^);
      FMT_ULONG, FMT_SLONG:
        rec.DataValue := PDWord(@ATag.Raw[1])^;
        //rec.DataValue := FixEndian32(PDWord(@ATag.Raw[1])^);
      FMT_SINGLE:
        Move(ATag.Raw[1], rec.DataValue, SizeOf(Single));
    end;
    }
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
    Move(BIG_ENDIAN_BOM[0], header.BOM[0], 2)
  else
    Move(LITTLE_ENDIAN_BOM[0], header.BOM[0], 2);
  header.Signature := 42;  // magic number
  header.IFDOffset := 0;   // we'll write IDF0 immediately afterwards
  header.IFDOffset := FixEndian32(8);

  // Write out
  AStream.WriteBuffer(header, SizeOf(header));
end;

procedure TExifWriter.WriteToStream(AStream: TStream);
var
  subIFDList: TInt64List;
begin
  subIFDList := TInt64List.Create;
  try
    // Tiff header
    FTiffHeaderPosition := AStream.Position;
    WriteTiffHeader(AStream);

    // Write IFD0
    WriteIFD(AStream, subIFDList, 0, false);

    // Write IFD1
    if FHasThumbnail then
      WriteIFD(AStream, subIFDList, 1, false);

    // Write special subIFDs collected in subIFDList
    WriteSubIFDs(AStream, subIFDList, false);

    // If WriteToStream is called within a JPEG structure we must update the
    // size of the EXIF segment.
    UpdateSegmentSize(AStream, FExifSegmentStartPos);

  finally
    subIFDList.Free;
  end;
end;

end.

