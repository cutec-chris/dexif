unit dIptcWrite;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  dGlobal, dUtils, dMetadata, dIPTC;

type
  { Writer for the IPTC data (Adobe image resource blocks)
    NOTE: Data must be written in Big-Endian format. }

  { TIPTCWriter }

  TIPTCWriter = class(TBasicMetadataWriter)
  private
    FIPTCSegmentStartPos: Int64;
  protected
    procedure WriteEndOfDataResourceBlock(AStream: TStream);
    procedure WriteImageResourceBlockHeader(AStream: TStream; AResourceID: Integer;
      AResourceName: String); //; ABuffer: Pointer; ABufferSize: DWord);
    procedure WriteIPTCImageResourceBlock(AStream: TStream);
    procedure WriteTag(AStream: TStream; ATag: TTagEntry);
  public
    constructor Create(AImgData: TImgData); override;
    procedure WriteIPTCHeader(AStream: TStream);
    procedure WriteToStream(AStream: TStream); override;
  end;

  EIPTCWriter = class(Exception);

implementation

type
  // http://search.cpan.org/dist/Image-MetaData-JPEG/lib/Image/MetaData/JPEG/Structures.pod#Structure_of_an_IPTC_data_block
  TIptcTag = packed record
    TagMarker: Byte;     // must be $1C
    RecordNumber: Byte;  // this is the number before the colon in the tag listing
    DatasetNumber: Byte; // this is the number after the colon in the tag listing ("Tag")
    Size: Word;          // Size of data if < 32768, otherwise size of datalength element
    // SizeOfDatasize: word --> if Size of data > 32767
    // Data: variable
  end;


constructor TIPTCWriter.Create(AImgData: TImgData);
begin
  inherited;
  FIPTCSegmentStartPos := -1;
end;

procedure TIptcWriter.WriteEndOfDataResourceBlock(AStream: TStream);
begin
  WriteImageResourceBlockHeader(AStream, $0B04, ''); //, nil, 0);
end;

//------------------------------------------------------------------------------
//  Writes the IPTC header needed by JPEG files (Segment APP13 header)
//  Call WriteToStream immediately afterwards
//------------------------------------------------------------------------------
procedure TIPTCWriter.WriteIPTCHeader(AStream: TStream);
const
  SEGMENT_MARKER: array[0..1] of byte = ($FF, $ED);
  IPTC_SIGNATURE: ansistring = 'Photoshop 3.0'#0;
  SIZE: Word = 0;
begin
  FIPTCSegmentStartPos := AStream.Position;
  AStream.WriteBuffer(SEGMENT_MARKER[0], 2);
  // Next two zero bytes are the size of the entire IPTC segiment, they will be
  // replaced when the segment is completely written. For this, we store the
  // offset to the begin of the IPTC segment in FIPTCSegmentStartPos.
  AStream.WriteBuffer(SIZE, 2);
  AStream.WriteBuffer(IPTC_SIGNATURE[1], Length(IPTC_SIGNATURE));
end;

procedure TIPTCWriter.WriteIPTCImageResourceBlock(AStream: TStream);
var
  i: Integer;
  tag: TTagEntry;
begin
  WriteImageResourceBlockHeader(AStream, $0404, '');

  for i := 0 to FImgData.IptcObj.Count-1 do begin
    tag := FImgData.IptcObj.ITagArray[i];
    WriteTag(AStream, tag);
  end;
end;
(*
var
  buf: ansistring;  // to do: replace by TBytes or similar, but no string!
begin
  buf := FImgData.IPTCobj.IPTCArrayToBuffer;
  WriteImageResourceBlock(AStream, $0404, '', @buf[1], Length(buf));
end;
*)

{ Adobe image resource block:
  Length    Description
  --------- -----------
  4         Signature: '8BIM'
  2         Unique identifier for the resource. Image resource IDs contains a
            list of resource IDs used by Photoshop.
  Variable  Name: Pascal string, padded to make the size even (a null name
            consists of two bytes of 0)
  4         Actual size of resource data that follows
  Variable  The resource data, described in the sections on the individual
            resource types. It is padded to make the size even }
procedure TIPTCWriter.WriteImageResourceBlockHeader(AStream: TStream;
  AResourceID: Integer; AResourceName: String);
//  ABuffer: Pointer; ABufferSize: DWord);
const
  RESOURCE_MARKER: ansistring = '8BIM';
var
  b: Byte;
  w: Word;
  dw: DWord;
begin
  // Resource marker: 8BIM
  AStream.WriteBuffer(RESOURCE_MARKER[1], Length(RESOURCE_MARKER));

  // Resource ID
  w := AResourceID;
  w := NtoBE(w);
  AStream.WriteBuffer(w, SizeOf(w));

  // Resource name
  if Length(AResourceName) = 0 then begin
    w := 0;
    AStream.WriteBuffer(w, 2);
  end else
  begin
    dw := Length(AResourceName);
    if dw > 255 then
      raise Exception.Createfmt('Image resource name "%s" too long.', [AResourceName]);
    if not odd(dw) then begin
      inc(dw);
      AResourceName := AResourceName + #0;
    end;
    b := dw;
    AStream.WriteBuffer(b, 1);
    AStream.WriteBuffer(AResourceName[1], b);
  end;
                 (*
  // Resource data
  if ABuffer <> nil then begin
    if odd(ABufferSize) then begin
      // Pad with zero to get even byte count
      dw := NtoBE(ABufferSize + 1);
      AStream.WriteBuffer(dw, SizeOf(dw));
      AStream.WriteBuffer(ABuffer^, ABufferSize);
      b := 0;
      AStream.WriteBuffer(b, 1);
    end else begin
      dw := NtoBE(ABufferSize);
      AStream.WriteBuffer(dw, SizeOf(dw));
      AStream.WriteBuffer(ABuffer^, ABufferSize);
    end;
  end;             *)

end;

procedure TIptcWriter.WriteTag(AStream: TStream; ATag: TTagEntry);
const
  TAG_MARKER = $1C;
var
  iptcTag: TIptcTag;
  w: Word;
  dw: DWord;
begin
  iptcTag.TagMarker := TAG_MARKER;
  iptcTag.RecordNumber := (ATag.Tag and $FF00) shr 8;
  iptctag.DatasetNumber := (ATag.Tag and $00FF);
  case ATag.TType of
    FMT_USHORT:
      begin
        iptcTag.Size := 2;
        AStream.WriteBuffer(iptcTag, SizeOf(iptcTag));
        AStream.WriteBuffer(ATag.Raw[1], 2);
      end;
    FMT_STRING:
      begin
        if odd(Length(ATag.Raw)) then
          ATag.Raw := ATag.Raw + #0;
        w := Length(ATag.Raw);
        if w < 32768 then begin
          iptcTag.Size := NtoBE(w);
          AStream.WriteBuffer(iptcTag, SizeOf(iptcTag));
          AStream.WriteBuffer(ATag.Raw[1], w);
        end else
        if w < 65536 then begin
          // Size is 2, but we must set highest bit to mark tag as being extended.
          {$IFDEF ENDIAN_BIG}
          iptcTag.Size := $8002;    // 80 is written first --> 8 is highest bit.
          {$ELSE}
          iptcTag.Size := $0082;    // 82 is written first --> 8 is highest bit.
          {$ENDIF}
          AStream.WriteBuffer(iptcTag, Sizeof(iptcTag));
          w := NtoBE(w);
          AStream.WriteBuffer(w, SizeOf(w));
          AStream.WriteBuffer(ATag.Raw[1], w);
        end else begin
          // Size is 4, but we must set highest bit to mark tag as being extended.
          {$IFDEF ENDIAN_BIG}
          iptcTag.Size := $8004;    // 80 is written first --> 8 is highest bit.
          {$ELSE}
          iptcTag.Size := $0084;    // 84 is written first --> 8 is highest bit.
          {$ENDIF}
          AStream.WriteBuffer(iptcTag, SizeOf(iptcTag));
          dw := NtoBE(w);
          AStream.WriteBuffer(dw, SizeOf(dw));
          AStream.WriteBuffer(ATag.Raw[1], Length(ATag.Raw));
        end;
      end;
    else
      // I've never seen other tag types than USHORT and STRING...
      raise Exception.Create('Tag type not supported.');
  end;
end;

procedure TIptcWriter.WriteToStream(AStream: TStream);
begin
  // Write the IPTC tags
  WriteIptcImageResourceBlock(AStream);

  // wp: is this really needed?
  // Write the end-of-data marker
//  WriteEndOfDataResourceBlock(AStream);

  // If WriteToStream is called within a JPEG structure we must update the
  // size of the IPTC segment.
  UpdateSegmentSize(AStream, FIptcSegmentStartPos);
end;

end.

