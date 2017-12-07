unit dTags;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  dGlobal;

const
  // Tags with offsets to subdirectories (subIFD)
  TAG_EXIF_OFFSET        = $8769;
  TAG_GPS_OFFSET         = $8825;
  TAG_INTEROP_OFFSET     = $A005;
  TAG_SUBIFD_OFFSET      = $014A;
  // The following tag ids with more offsets are taken from
  // https://sno.phy.queensu.ca/~phil/exiftool/TagNames/EXIF.html
  TAG_GLOBALPARAMS_OFFSET= $0190;
  TAG_KODAK_OFFSET       = $8290;
  // there are some more...

  TAG_INTEROPVERSION     = $0002;
  TAG_IMAGEWIDTH         = $0100;
  TAG_IMAGELENGTH        = $0101;
  TAG_BITSPERSAMPLE      = $0102;
  TAG_COMPRESSION        = $0103;
  TAG_PHOTOMETRICINTERPRETATION = $0106;
  TAG_IMAGEDESCRIPTION   = $010E;
  TAG_MAKE               = $010F;
  TAG_MODEL              = $0110;
  TAG_DATETIME_MODIFY    = $0132;
  TAG_ARTIST             = $013B;
  TAG_WHITEPOINT         = $013E;

  TAG_THUMBSTARTOFFSET   = $0201;
  TAG_THUMBSIZE          = $0202;

  TAG_COPYRIGHT          = $8298;
  TAG_EXPOSURETIME       = $829A;
  TAG_FNUMBER            = $829D;
  TAG_TIMEZONEOFFSET     = $882A;

  TAG_EXIFVERSION        = $9000;
  TAG_DATETIME_ORIGINAL  = $9003;
  TAG_DATETIME_DIGITIZED = $9004;
  TAG_SHUTTERSPEED       = $9201;
  TAG_APERTURE           = $9202;
  TAG_BRIGHTNESSVALUE    = $9203;
  TAG_EXPOSUREBIASVALUE  = $9204;
  TAG_MAXAPERTUREVALUE   = $9205;
  TAG_SUBJECT_DISTANCE   = $9206;
  TAG_LIGHT_SOURCE       = $9208;
  TAG_FLASH              = $9209;
  TAG_FOCALLENGTH        = $920A;
  TAG_FLASH_ENERGY       = $920B;
  TAG_NOISE              = $920D;
  TAG_MAKERNOTE          = $927C;
  TAG_USERCOMMENT        = $9286;
  TAG_XP_TITLE           = $9C9B;
  TAG_XP_COMMENT         = $9C9C;
  TAG_XP_AUTHOR          = $9C9D;
  TAG_XP_KEYWORDS        = $9C9E;
  TAG_XP_SUBJECT         = $9C9F;

  TAG_FLASHPIXVERSION    = $A000;
  TAG_EXIF_IMAGEWIDTH    = $A002;
  TAG_EXIF_IMAGELENGTH   = $A003;
  TAG_FOCALPLANEXRES     = $A20E;
  TAG_FOCALPLANEYRES     = $A20F;
  TAG_FOCALPLANEUNITS    = $A210;
  TAG_FOCALLENGTH35MM    = $A405;


//------------------------------------------------------------------------------
// SubIFD Tags
//
// These are tags which link to subdirecties. Their IDs are stored in an
// internal array (SubIFDtags). }
//------------------------------------------------------------------------------

// Check whether a tag links to a subdirectory
function TagLinksToSubIFD(ATagID: TTagID): Boolean;

// Extend the list of subifd tags  -- USE AT YOUR OWN RISK
procedure RegisterSubIFDTag(ATagID: TTagID);


implementation

{ Tags which link to subdirectories (SubIFD) }
var
  SubIFDTags: Array of TTagID;

function TagLinksToSubIFD(ATagID: TTagID): Boolean;
var
  i: Integer;
begin
  for i:=0 to High(SubIFDTags) do
    if SubIFDTags[i] = ATagID then begin
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure RegisterSubIFDTag(ATagID: TTagID);
var
  n: Integer;
begin
  // Ignore if new tag is already registered;
  if TagLinksToSubIFD(ATagID) then
    exit;

  n := Length(SubIFDTags);
  SetLength(SubIFDTags, n + 1);
  SubIFDTags[n] := ATagID;
end;


initialization
  SetLength(SubIFDTags, 3);
  SubIFDTags[0] := TAG_EXIF_OFFSET;
  SubIFDTags[1] := TAG_INTEROP_OFFSET;
  SubIFDTags[2] := TAG_GPS_OFFSET;

finalization
  SubIFDTags := nil;

end.

