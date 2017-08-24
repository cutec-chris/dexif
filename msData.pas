unit msData;

////////////////////////////////////////////////////////////////////////////////
// msData.pas - Copyright 2001-2004, Gerry McGuire
//--------------------------------------------------------------------------
// msData - maker specific data as encoded in exif structures
// for use with Dexif module (Delphi EXIF).
//
//   Gerry McGuire, March - April 7, 2001 - Initial Beta Release - 0.8
//   Gerry McGuire, September 3, 2001 - Second Beta Release - 0.9
//   Gerry McGuire, June 1, 2003 - First Release - 1.0
//
//--------------------------------------------------------------------------
//
// Add a new TTagEntry and "if..else" in ReadMsData to extend for
// other makers/models.  This can be modified to read values instead
// from an external file to permit end-user tailoring of data.
//
// Results from these table scans shows up in msTraceStr field of
// tImageInfo structure (declared in dExif.pas)
//
//  Information here was derived from the following document:
//
//      www.butaman.ne.jp/~tsuruzoh/Computer/Digicams/exif-e.html
//
//--------------------------------------------------------------------------

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

{$I dExif.inc}

interface


uses
  Sysutils, math,
  dglobal, dEXIF, dIPTC;


type

     TmsInfo = class // manufactorer specific
        isTiff:boolean;
        IMGparent:tImageInfo;
        makerOffset:integer;
        gblUCMaker:ansistring ;
        function ReadMSData(var DR:TImageInfo):boolean;
        constructor Create(tiffFlag:boolean; p:tImageInfo);
     end;

     ////////////////////////////////
     //  More complex fields can be formatted with a
     //  callback function.  Declare them here and insert
     //  the code in the implemenetation section.
     Function NikonLens(InStr: AnsiString): String;
     Function NikonColorMode(InStr: AnsiString): String;
     Function CanonExp1(InStr: AnsiString): String;
     Function CanonExp2(InStr: AnsiString): String;
     Function CanonCustom1(InStr: AnsiString): String;

const
     Nikon1Table : array [0..10] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $02;    Name:'FamilyID';    Desc:'FamilyID'),
      (TID:0;TType:0;ICode: 2;Tag: $03;    Name:'Quality';     Desc:'Quality'; Code:'1:Vga Basic,2:Vga Normal,'+
          '3:Vga Fine,4:SXGA Basic,5:SXGA Normal,6:SXGA Fine'+
          '10:2 Mpixel Basic,11:2 Mpixel Normal,12:2 Mpixel Fine'),
      (TID:0;TType:0;ICode: 2;Tag: $04;    Name:'ColorMode';   Desc:'ColorMode'; Code:'1:Color,2:Monochrome'),
      (TID:0;TType:0;ICode: 2;Tag: $05;    Name:'ImageAdjustment';  Desc:'ImageAdjustment'; Code:'0:Normal,1:Bright+,'+
          '2:Bright-,3:Contrast+,4:Contrast-'),
      (TID:0;TType:0;ICode: 2;Tag: $06;    Name:'ISOSpeed';    Desc:'ISOSpeed'; Code:'0:ISO80,2:ISO160,4:ISO320,'+
          '5:ISO100'),
      (TID:0;TType:0;ICode: 2;Tag: $07;    Name:'WhiteBalance'; Desc:'WhiteBalance'; Code:'0:Auto,1:Preset,2:Daylight,'+
          '3:Incandescense,4:Fluorescence,5:Cloudy,6:SpeedLight'),
      (TID:0;TType:0;ICode: 2;Tag: $08;    Name:'Focus';       Desc:'Focus'),
      (TID:0;TType:0;ICode: 2;Tag: $09;    Name:'Skip';        Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0A;    Name:'DigitalZoom'; Desc:'DigitalZoom'),
      (TID:0;TType:0;ICode: 2;Tag: $0B;    Name:'Converter';   Desc:'Converter'; Code:'0:Not used,1:Used'),
      (TID:0;TType:0;ICode: 2;Tag: $0F00;  Name:'Skip';        Desc:'Skip'));

     Nikon2Table : array [0..27] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $01;    Name:'FamilyID';            Desc:'Family ID'),
      (TID:0;TType:0;ICode: 2;Tag: $02;    Name:'ISOSpeed';            Desc:'ISO Speed'),
      (TID:0;TType:0;ICode: 2;Tag: $03;    Name:'ColorMode';           Desc:'Color Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $04;    Name:'Quality';             Desc:'Quality'),
      (TID:0;TType:0;ICode: 2;Tag: $05;    Name:'WhiteBalance';        Desc:'White Balance'),
      (TID:0;TType:0;ICode: 2;Tag: $06;    Name:'ImageSharpening';     Desc:'Image Sharpening'),
      (TID:0;TType:0;ICode: 2;Tag: $07;    Name:'FocusMode';           Desc:'Focus Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $08;    Name:'FlashSetting';        Desc:'Flash Setting'),
      (TID:0;TType:0;ICode: 2;Tag: $09;    Name:'AutoFlashMode';       Desc:'Auto Flash Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $0A;    Name:'Skip';                Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0B;    Name:'WhiteBiasValue';      Desc:'White Bias Value'),
      (TID:0;TType:0;ICode: 2;Tag: $0F;    Name:'DigitalZoom';         Desc:'Digital Zoom'),
      (TID:0;TType:0;ICode: 2;Tag: $10;    Name:'Skip';                Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $11;    Name:'Skip';                Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $80;    Name:'ImageAdjustment';     Desc:'Image Adjustment'),
      (TID:0;TType:0;ICode: 2;Tag: $81;    Name:'ImageAdjustment';     Desc:'Image Adjustment'),
      (TID:0;TType:0;ICode: 2;Tag: $82;    Name:'Adapter';             Desc:'Adapter'),
      (TID:0;TType:0;ICode: 2;Tag: $84;    Name:'LensInformation';     Desc:'Lens information'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:0;CallBack:NikonLens),
      (TID:0;TType:0;ICode: 2;Tag: $85;    Name:'ManualFocusDistance'; Desc:'Manual Focus Distance'),
      (TID:0;TType:0;ICode: 2;Tag: $86;    Name:'DigitalZoom';         Desc:'Digital Zoom'),
      (TID:0;TType:0;ICode: 2;Tag: $88;    Name:'FocusArea';           Desc:'Focus Area'),
      (TID:0;TType:0;ICode: 2;Tag: $89;    Name:'Mode';                Desc:'Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $8D;    Name:'ColorMode';           Desc:'Color Mode'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:0;CallBack:NikonColorMode),
      (TID:0;TType:0;ICode: 2;Tag: $8F;    Name:'SceneMode';           Desc:'Scene Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $92;    Name:'HueAdjustment';       Desc:'Hue Adjustment'), // ??
      (TID:0;TType:0;ICode: 2;Tag: $94;    Name:'Saturation';          Desc:'Saturation'),
      (TID:0;TType:0;ICode: 2;Tag: $95;    Name:'NoiseReduction';      Desc:'Noise Reduction'),
      (TID:0;TType:0;ICode: 2;Tag: $0F00;  Name:'Skip';                Desc:'Skip')) ;

     Olympus1Table : array [0..32] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $0200;    Name:'SpecialMode';      Desc:'SpecialMode'),
      (TID:0;TType:0;ICode: 2;Tag: $0201;    Name:'JpegQual';         Desc:'JpegQual'; Code:'1:SQ,2:HQ,3:SHQ,4:Raw'),
      (TID:0;TType:0;ICode: 2;Tag: $0202;    Name:'Macro';            Desc:'Macro';    Code:'0=Normal,1:Macro;'),
      (TID:0;TType:0;ICode: 2;Tag: $0203;    Name:'Skip';             Desc:'Skip'              ),
      (TID:0;TType:0;ICode: 2;Tag: $0204;    Name:'DigiZoom';         Desc:'Digital Zoom Ratio'),
      (TID:0;TType:0;ICode: 2;Tag: $0205;    Name:'Skip';             Desc:'Skip'              ),
      (TID:0;TType:0;ICode: 2;Tag: $0206;    Name:'Skip';             Desc:'Skip'              ),
      (TID:0;TType:0;ICode: 2;Tag: $0207;    Name:'Firmware';         Desc:'Firmware'          ),
      (TID:0;TType:0;ICode: 2;Tag: $0208;    Name:'PictInfo';         Desc:'Picture Info'      ),
      (TID:0;TType:0;ICode: 2;Tag: $0209;    Name:'CameraID';         Desc:'Camera ID'         ),
      (TID:0;TType:0;ICode: 2;Tag: $0F00;    Name:'Skip';             Desc:'Skip'              ),
      (TID:0;TType:0;ICode: 2;Tag: $1004;    Name:'FlashMode';        Desc:'Flash Mode'        ),
      (TID:0;TType:0;ICode: 2;Tag: $1006;    Name:'Bracket'  ;        Desc:'Bracket'           ),
      (TID:0;TType:0;ICode: 2;Tag: $100B;    Name:'FocusMode';        Desc:'Focus Mode'        ),
      (TID:0;TType:0;ICode: 2;Tag: $100C;    Name:'FocusDistance';    Desc:'Focus Distance'    ),
      (TID:0;TType:0;ICode: 2;Tag: $100D;    Name:'Zoom';             Desc:'Zoom'              ),
      (TID:0;TType:0;ICode: 2;Tag: $100E;    Name:'MacroFocus';       Desc:'Macro Focus'       ),
      (TID:0;TType:0;ICode: 2;Tag: $100F;    Name:'Sharpness';        Desc:'Sharpness'         ),
      (TID:0;TType:0;ICode: 2;Tag: $1011;    Name:'ColorMatrix';      Desc:'Color Matrix'      ),
      (TID:0;TType:0;ICode: 2;Tag: $1012;    Name:'BlackLevel';       Desc:'Black Level'       ),
      (TID:0;TType:0;ICode: 2;Tag: $1015;    Name:'WhiteBalance';     Desc:'White Balance'     ),
      (TID:0;TType:0;ICode: 2;Tag: $1017;    Name:'RedBias';          Desc:'Red Bias'          ),
      (TID:0;TType:0;ICode: 2;Tag: $1018;    Name:'BlueBias';         Desc:'Blue Bias'         ),
      (TID:0;TType:0;ICode: 2;Tag: $101A;    Name:'SerialNumber';     Desc:'SerialNumber'      ),
      (TID:0;TType:0;ICode: 2;Tag: $1023;    Name:'FlashBias';        Desc:'Flash Bias'        ),
      (TID:0;TType:0;ICode: 2;Tag: $1029;    Name:'Contrast';         Desc:'Contrast'          ),
      (TID:0;TType:0;ICode: 2;Tag: $102A;    Name:'SharpnessFactor';  Desc:'Sharpness Factor'  ),
      (TID:0;TType:0;ICode: 2;Tag: $102B;    Name:'ColorControl';     Desc:'Color Control'     ),
      (TID:0;TType:0;ICode: 2;Tag: $102C;    Name:'ValidBits';        Desc:'Valid Bits'        ),
      (TID:0;TType:0;ICode: 2;Tag: $102D;    Name:'Coring';           Desc:'Coring Filter'     ),
      (TID:0;TType:0;ICode: 2;Tag: $102E;    Name:'FinalWidth';       Desc:'Final Width'       ),
      (TID:0;TType:0;ICode: 2;Tag: $102F;    Name:'FinalHeight';      Desc:'Final Height'      ),
      (TID:0;TType:0;ICode: 2;Tag: $1034;    Name:'CompressionRatio'; Desc:'Compression Ratio' )
     );

     Casio1Table : array [0..25] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $01;   Name:'RecordingMode';  Desc:'RecordingMode'; Code:'1:Single Shutter,2:Panorama,'+
          '3:Night Scene,4:Portrait,5:Landscape'),
      (TID:0;TType:0;ICode: 2;Tag: $02;   Name:'Quality'     ;  Desc:'Quality'      ; Code:'1:Economy,2:Normal,3:Fine'),
      (TID:0;TType:0;ICode: 2;Tag: $03;   Name:'FocusingMode';  Desc:'FocusingMode' ; Code:'2:Macro,3:Auto Focus,'+
          '4:Manual Focus,5:Infinity'),
      (TID:0;TType:0;ICode: 2;Tag: $04;   Name:'FlashMode';  Desc:'FlashMode'    ; Code:'1:Auto,2:On,3:Off,'+
          '4:Red Eye Reduction'),
      (TID:0;TType:0;ICode: 2;Tag: $05;   Name:'FlashIntensity';  Desc:'FlashIntensity'; Code:'11:Weak,13:Normal,15:Strong'),
      (TID:0;TType:0;ICode: 2;Tag: $06;   Name:'ObjectDistance';  Desc:'ObjectDistance'),
      (TID:0;TType:0;ICode: 2;Tag: $07;   Name:'WhiteBalance'  ;  Desc:'WhiteBalance'; Code:'1:Auto,2:Tungsten,'+
          '3:Daylight,4:Fluorescent,5:Shade,129:Manual'),
      (TID:0;TType:0;ICode: 2;Tag: $08;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $09;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0A;   Name:'DigitalZoom'; Desc:'DigitalZoom'; Code:'65536:Off,65537:2X Digital Zoom'),
      (TID:0;TType:0;ICode: 2;Tag: $0B;   Name:'Sharpness';   Desc:'Sharpness'; Code:'0:Normal,1:Soft,2:Hard'),
      (TID:0;TType:0;ICode: 2;Tag: $0C;   Name:'Contrast';    Desc:'Contrast'; Code:'0:Normal,1:Low,2:High'),
      (TID:0;TType:0;ICode: 2;Tag: $0D;   Name:'Saturation';  Desc:'Saturation'; Code:'0:Normal,1:Low,2:High'),
      (TID:0;TType:0;ICode: 2;Tag: $0E;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0F;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $10;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $11;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $12;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $13;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $14;   Name:'CCDSensitivity';  Desc:'CCDSensitivity'; Code:'64:Normal,125:+1.0,250:+2.0,'+
          '244:+3.0,80:Normal,100:High'),
      (TID:0;TType:0;ICode: 2;Tag: $15;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $16;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $17;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $18;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $19;   Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $1A;   Name:'Skip';  Desc:'Skip'));

     Casio2Table : array [0..44] of TTagEntry =
     (
      (TID:0;TType:0;ICode: 2;Tag: $02;   Name:'ThumbnailDimensions';  Desc:'Thumbnail Dimensions'),
      (TID:0;TType:0;ICode: 2;Tag: $03;   Name:'ThumbnailSize'; Desc:'Thumbnail Size'),
      (TID:0;TType:0;ICode: 2;Tag: $04;   Name:'ThumbnailOffset'; Desc:'Thumbnail OffSet'),
      (TID:0;TType:0;ICode: 2;Tag: $08;   Name:'Quality'; Desc:'Quality'; Code:'1:Fine,2:Super Fine'),

      (TID:0;TType:0;ICode: 2;Tag: $09;   Name:'ImageSize'; Desc:'ImageSize';
        Code:'0:640 x 480,4:1600 x 1200,5:2048 x 1536,20:2288 x 1712,'+
             '21:2592 x 1944,22:2304 x 1728,36:3008 x 2008'),
      (TID:0;TType:0;ICode: 2;Tag: $0D;   Name:'FocusMode'; Desc:'FocusMode'; Code:'0:Normal,1:Macro'),
      (TID:0;TType:0;ICode: 2;Tag: $14;   Name:'IsoSensitivity'; Desc:'IsoSensitivity';
        Code:'3:50,4:64,6:100,9:200'),
      (TID:0;TType:0;ICode: 2;Tag: $19;   Name:'WhiteBalance'; Desc:'WhiteBalance';
        Code:'0:Auto,1:Daylight,2:Shade,3:Tungsten,4:Fluorescent,5:Manual'),
      (TID:0;TType:0;ICode: 2;Tag: $1D;   Name:'FocalLength'; Desc:'Focal Length (.1 mm)'),

      (TID:0;TType:0;ICode: 2;Tag: $1F;    Name:'Saturation'; Desc:'Saturation'; Code:'0:-1,1:Normal,2:+1'),
      (TID:0;TType:0;ICode: 2;Tag: $20;    Name:'Contrast'; Desc:'Contrast'; Code:'0:-1,1:Normal,2:+1'),
      (TID:0;TType:0;ICode: 2;Tag: $21;    Name:'Sharpness'; Desc:'Sharpness'; Code:'0:-1,1:Normal,2:+1'),
      (TID:0;TType:0;ICode: 2;Tag: $E00;   Name:'PIM'; Desc:'	Print Image Matching Info'),
//      (TID:0;TType:0;ICode: 2;Tag: $2000;  Name:'CasioPreviewThumbnail'; Desc:'Casio Preview Thumbnail'),
      (TID:0;TType:0;ICode: 2;Tag: $2000;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2001;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2002;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2003;  Name:'Skip'; Desc:'Skip'),

      (TID:0;TType:0;ICode: 2;Tag: $2011;  Name:'WhiteBalanceBias'; Desc:'White Balance Bias'),
      (TID:0;TType:0;ICode: 2;Tag: $2013;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2012;  Name:'WhiteBalance'; Desc:'White Balance'; Code:'0:Manual,1:Auto,4:Flash,12:Flash'),
      (TID:0;TType:0;ICode: 2;Tag: $2021;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2022;  Name:'ObjectDistance'; Desc:'Object Distance (mm)'),
      (TID:0;TType:0;ICode: 2;Tag: $2023;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2031;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2032;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2033;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $2034;  Name:'FlashDistance'; Desc:'Flash Distance'),
      (TID:0;TType:0;ICode: 2;Tag: $3000;  Name:'RecordMode'; Desc:'Record Mode'; Code:'2:Normal'),

      (TID:0;TType:0;ICode: 2;Tag: $3001;   Name:'SelfTimer'; Desc:'Self Timer'; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $3002;   Name:'Quality'; Desc:'Quality'; Code:'1:Economy,2:Normal,3:Fine'),
      (TID:0;TType:0;ICode: 2;Tag: $3003;   Name:'FocusMode'; Desc:'Focus Mode'; Code:'1:Fixed,3:Auto Focus,6:Multi-Area Auto Focus'),
      (TID:0;TType:0;ICode: 2;Tag: $3005;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $3006;   Name:'TimeZone'; Desc:'Time Zone'),
      (TID:0;TType:0;ICode: 2;Tag: $3007;   Name:'BestshotMode'; Desc:'Bestshot Mode'; Code:'0:Off,1:On'),

      (TID:0;TType:0;ICode: 2;Tag: $3011;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $3012;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $3013;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $3014;   Name:'CCDSensitivity'; Desc:'CCD Sensitivity'),
      (TID:0;TType:0;ICode: 2;Tag: $3015;   Name:'ColorMode'; Desc:'Color Mode'; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $3016;   Name:'Enhancement'; Desc:'Enhancement'; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $3017;   Name:'Filter'; Desc:'Filter'; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $3018;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $3019;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $301A;  Name:'Skip'; Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $301B;  Name:'Skip'; Desc:'Skip')

);

     Fuji1Table : array [0..17] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $0000;    Name:'Version';    Desc:'Version'),
      (TID:0;TType:0;ICode: 2;Tag: $1000;    Name:'Quality';    Desc:'Quality'     ; Code:''),
      (TID:0;TType:0;ICode: 2;Tag: $1001;    Name:'Sharpness';  Desc:'Sharpness'   ; Code:'1:Soft,2:Soft,3:Normal,'+
            '4:Hard,5:Hard'),
      (TID:0;TType:0;ICode: 2;Tag: $1002;    Name:'WhiteBalance';  Desc:'WhiteBalance'; Code:'0:Auto,256:Daylight,'+
            '512:Cloudy,768:DaylightColor-fluorescence,'+
            '769:DaywhiteColor-fluorescence,770:White-fluorescence,'+
            '1024:Incandenscense,3840:Custom white balance.'),
      (TID:0;TType:0;ICode: 2;Tag: $1003;    Name:'Color'    ;  Desc:'Color'    ; Code:'0:Normal,256:High,512:Low'),
      (TID:0;TType:0;ICode: 2;Tag: $1004;    Name:'Tone'     ;  Desc:'Tone'     ; Code:'0:Normal,256:High,512:Low'),
      (TID:0;TType:0;ICode: 2;Tag: $1010;    Name:'FlashMode';  Desc:'FlashMode'; Code:'0:Auto,1:On,2:Off,'+
            '3:Red-eye reduction'),
      (TID:0;TType:0;ICode: 2;Tag: $1011;    Name:'FlashStrength';  Desc:'FlashStrength'; Code:''),
      (TID:0;TType:0;ICode: 2;Tag: $1020;    Name:'Macro'        ;  Desc:'Macro'        ; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $1021;    Name:'Focusmode'    ;  Desc:'Focusmode'    ; Code:'0:Auto Focus,1:Manual Focus'),
      (TID:0;TType:0;ICode: 2;Tag: $1030;    Name:'SlowSync'     ;  Desc:'SlowSync'     ; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $1031;    Name:'PictureMode'  ;  Desc:'PictureMode'  ; Code:'0:Auto,1:Portrait scene,'+
            '2:Landscape scene,4:Sports scene,5:Night scene,6:Program AE,'+
            '256:Aperture prior AE,512:Shutter prior AE,768:Manual exposure'),
      (TID:0;TType:0;ICode: 2;Tag: $1032;    Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $1100;    Name:'ContTake/Bracket';  Desc:'ContTake/Bracket'; Code:'0:Off,1:On'),
      (TID:0;TType:0;ICode: 2;Tag: $1200;    Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $1300;    Name:'BlurWarning';  Desc:'BlurWarning'     ; Code:'0:No blur warning,'+
            '1:Blur warning'),
      (TID:0;TType:0;ICode: 2;Tag: $1301;    Name:'FocusWarning';  Desc:'FocusWarning'    ; Code:'0:Auto Focus good,'+
            '1:Out of focus'),
      (TID:0;TType:0;ICode: 2;Tag: $1302;    Name:'AEWarning';  Desc:'AEWarning'       ; Code:'0:AE good,1:Over exposure')) ;

     Canon1Table : array [0..15] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $00;    Name:'Skip';   Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $01;    Name:'ExposureInfo1';    Desc:'ExposureInfo1'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:0;CallBack:CanonExp1),
      (TID:0;TType:0;ICode: 2;Tag: $02;    Name:'Skip';   Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $03;    Name:'Skip';   Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $04;    Name:'ExposureInfo2';    Desc:'ExposureInfo2'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:0;CallBack:CanonExp2),
      (TID:0;TType:0;ICode: 2;Tag: $06;    Name:'ImageType';        Desc:'ImageType'),
      (TID:0;TType:0;ICode: 2;Tag: $07;    Name:'FirmwareVersion';  Desc:'FirmwareVersion'),
      (TID:0;TType:0;ICode: 2;Tag: $08;    Name:'ImageNumber';      Desc:'ImageNumber'),
      (TID:0;TType:0;ICode: 2;Tag: $09;    Name:'OwnerName';        Desc:'OwnerName'),
      (TID:0;TType:0;ICode: 2;Tag: $0A;    Name:'Skip';   Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0B;    Name:'Skip';   Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0C;    Name:'CameraSerialNumber';  Desc:'CameraSerialNumber'),
      (TID:0;TType:0;ICode: 2;Tag: $0D;    Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0E;    Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0F;    Name:'CustomFunctions';  Desc:'CustomFunctions'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:0;CallBack:CanonCustom1),
      (TID:0;TType:0;ICode: 2;Tag: $10;    Name:'Skip';   Desc:'Skip'));

     Epson1Table : array [0..11] of TTagEntry =           //For Epson pc850Z     Lucas P.
     ((TID:0;TType:0;ICode: 2;Tag: $0200;    Name:'Special Mode';  Desc:'Special Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $0201;    Name:'JpegQuality';   Desc:'JpegQuality'),
      (TID:0;TType:0;ICode: 2;Tag: $0202;    Name:'Macro';     Desc:'Macro'),
      (TID:0;TType:0;ICode: 2;Tag: $0203;    Name:'Skip';      Desc:'Skip'),     // ??
      (TID:0;TType:0;ICode: 2;Tag: $0204;    Name:'DigiZoom';  Desc:'DigiZoom'),
      (TID:0;TType:0;ICode: 2;Tag: $0209;    Name:'CameraID';  Desc:'CameraID'),
      (TID:0;TType:0;ICode: 2;Tag: $020a;    Name:'Comments';  Desc:'Comments'),
      (TID:0;TType:0;ICode: 2;Tag: $020b;    Name:'Width';     Desc:'Width'),
      (TID:0;TType:0;ICode: 2;Tag: $020c;    Name:'Height';    Desc:'Height'),
      (TID:0;TType:0;ICode: 2;Tag: $020d;    Name:'SoftRelease';  Desc:'SoftRelease'),
      (TID:0;TType:0;ICode: 2;Tag: $0300;    Name:'??';        Desc:'??'),       // ??
      (TID:0;TType:0;ICode: 2;Tag: $0f00;    Name:'skip';      Desc:'skip'));

     Sanyo1Table : array [0..5] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $0200;    Name:'Special Mode';  Desc:'Special Mode'),
      (TID:0;TType:0;ICode: 2;Tag: $0201;    Name:'JpegQuality';  Desc:'JpegQuality'),
      (TID:0;TType:0;ICode: 2;Tag: $0202;    Name:'Macro';  Desc:'Macro'),
      (TID:0;TType:0;ICode: 2;Tag: $0203;    Name:'Skip';  Desc:'Skip'),
      (TID:0;TType:0;ICode: 2;Tag: $0204;    Name:'DigiZoom';  Desc:'DigiZoom'),
      (TID:0;TType:0;ICode: 2;Tag: $0F00;    Name:'DataDump';  Desc:'DataDump' ));

     MinoltaTable : array [0..1] of TTagEntry =
     ((TID:0;TType:0;ICode: 2;Tag: $00;      Name:'ModelID';  Desc:'ModelID'),
      (TID:0;TType:0;ICode: 2;Tag: $0E00;    Name:'PIMdata';  Desc:'PIMdata')) ;


implementation

uses
  dUtils;
  
////////////////
// Callback Functions - one per field
//
//  Ok, Ok, usually you'd have a parser do the
//  work but hey - this is just a simple example
Function NikonColorMode(InStr: AnsiString): String;
begin
  InStr := Copy(InStr, 2, 5);
  Result := InStr;
  if InStr = 'MODE1' then
    Result := 'Mode1 (sRGB)'
  else
  if InStr = 'MODE2' then
    Result := 'Mode 2 (Adobe RGB)'
  else
  if InStr = 'MODE3' then
    Result := 'Mode 3 (sRGB): higher saturation'
end;

function NikonLens(InStr: AnsiString): String;
var
  i,sl: Integer;
  tb: Ansistring;
  MaxSp,MinSp,MaxFL,MinFL:double;
begin
  Result := InStr;                     // if error return input string
  sl := Length(dExifDataSep);
  i := Pos(dExifDataSep, InStr);
  tb := Copy(InStr, 1, i-1);           // get first irrational number
  MinFL := CvtRational(tb);            // bottom of lens speed range
  InStr := Copy(InStr, i+sl-1, 64);
  i := Pos(dExifDataSep, InStr);
  tb := Copy(InStr, 1, i-1);           // get second irrational number
  MaxFL := CvtRational(tb);            // top of lens speed range
  InStr := Copy(InStr, i+sl-1, 64);
  i := Pos(dExifDataSep, instr);
  tb := copy(InStr, 1, i-1);           // get third irrational number
  MinSp := CvtRational(tb);            // minimum focal length
  InStr := Copy(InStr, i+sl-1, 64);
  MaxSp := CvtRational(InStr);         // maximum focal length
  Result := Format('%0.1f-%0.1f mm  F%0.1f-F%0.1f', [MinFl, MaxFl, MinSp, MaxSp]);
end;

type
  strArray = array of ansistring;

function CustBld(fname:ansistring; item:integer; decodeStr:ansistring): ansistring;
var
  valStr:ansistring;
begin
  valStr := DecodeField(decodeStr, AnsiString(inttostr(item)));
  if trim(string(valStr)) <> '' then
  begin
    curTagArray.AddMSTag(fname,valStr, FMT_STRING);
    result := crlf + fname + dExifDelim + valStr;
  end
  else
    result := '';
end;

Function CustNth(instr, fname:ansistring; item:integer): ansistring;
var
  valStr:ansistring;
begin
  valStr := StrNth(instr,DexifDecodeSep,item);
  if trim(string(valStr)) <> '' then
  begin
    curTagArray.AddMSTag(fname,valStr, FMT_STRING);
    result := crlf + fname + dExifDelim + valStr;
  end
  else
    result := '';
end;

Function CustAPick(InStr, fname: AnsiString; AItem:integer; ADecodeStr:String): String;
var
  valStr: String;
begin
  valStr := aPick(InStr, AItem, ADecodeStr);
  if Trim(valStr) <> '' then
  begin
    curTagArray.AddMSTag(fname, valStr, FMT_STRING);
    Result := crlf + fname + dExifDelim + valStr;
  end
  else
    result := '';
end;

const
  CanonGen     : string = '65535:Low,0:Normal,1:High';
  CanonMacro   : string = '1:Macro,2:Normal';
  CanonCompress: string = '0:SuperFine,1:Fine,2:Normal,3:Basic,5:SuperFine';
  CanonFlash   : string = '0:Not fired,1:Auto,2:On,3:Red-eye,4:Slow sync,'+
                          '5:Auto+red-eye,6:On+red eye,16:External flash';
  CanonDrive   : string = '0:Single,1:Continuous';
  CanonFocus   : string = '0:One-Shot,1:AI Servo,2:AI Focus,3:MF,4:Single,'+
                          '5:Continuous,6:MF';
  CanonSize    : string = '0:Large,1:Medium,2:Small,4:5MPixel,5:2 MPixel,6:1.5 MPixel';
  CanonEasy    : string = '0:Full Auto,1:Manual,2:Landscape,3:Fast Shutter,'+
                          '4:Slow Shutter,5:Night,6:B&W,7:Sepia,8:Portrait,9:Sports,'+
                          '10:Macro/Close-Up,11:Pan Focus';
  CanonISO     : string = '0:Not used,15:auto,16:50,17:100,18:200,19:400';
  CanonMeter   : string = '3:Evaluative,4:Partial,5:Center-weighted';
  CanonAF      : string = '12288:None (MF),12289:Auto-selected,12290:Right,'+
                          '12291:Center,12292:Left';
  CanonExpose  : string = '0:Easy shooting,1:Program,2:Tv-priority,'+
                          '3:Av-priority,4:Manual,5:A-DEP';
  CanonFocus2  : string = '0:Single,1:Continuous';

function CanonExp1(instr: AnsiString): String;
var
  s: String;
begin
  rawDefered := true;
//  s := instr;
  s := '';
  s := s + CustAPick(instr, 'Macro mode'      , 1, CanonMacro);
  s := s + CustNth(  instr, 'Self Timer'      , 2);
  s := s + CustAPick(instr, 'Compression Rate', 3, CanonCompress);
  s := s + CustAPick(instr, 'Flash Mode'      , 4, CanonFlash);
  s := s + CustAPick(instr, 'Drive Mode'      , 5, CanonDrive);
  s := s + CustAPick(instr, 'Focus Mode'      , 7, CanonFocus);
  s := s + CustAPick(instr, 'Image Size'      ,10, CanonSize);
  s := s + CustAPick(instr, 'Easy Shoot'      ,11, CanonEasy);
  s := s + CustAPick(instr, 'Contrast'        ,13, CanonGen);
  s := s + CustAPick(instr, 'Saturation'      ,14, CanonGen);
  s := s + CustAPick(instr, 'Sharpness'       ,15, CanonGen);
  s := s + CustAPick(instr, 'CCD ISO'         ,16, CanonISO);
  s := s + CustAPick(instr, 'Metering Mode'   ,17, CanonGen);
  s := s + CustAPick(instr, 'AF Point'        ,19, CanonGen);
  s := s + CustAPick(instr, 'Exposure Mode'   ,20, CanonGen);
  s := s + CustNth(  instr, 'Long focal'      ,24);
  s := s + CustNth(  instr, 'Short focal'     ,25);
  s := s + CustNth(  instr, 'Focal Units'     ,26);
  s := s + CustNth(  instr, 'Flash Details'   ,29);
  s := s + CustAPick(instr, 'Focus Mode'      ,32, CanonGen);
  result := s;
end;

const
  CanonWhite: String = '0:Auto,1:Sunny,2:Cloudy,3:Tungsten,4:Flourescent,'+
                       '5:Flash,6:Custom';
  CanonBias: String  = '65472:-2 EV,65484:-1.67 EV,65488:-1.50 EV,65492:-1.33 EV,'+
                       '65504:-1 EV,65516:-0.67 EV,65520:-0.50 EV,65524:-0.33 EV,'+
                       '0:0 EV,12:0.33 EV,16:0.50 EV,20:0.67 EV,'+
                       '32:1 EV,44:1.33 EV,48:1.50 EV,52:1.67 EV,'+
                       '64:2 EV';

function CanonExp2(InStr: AnsiString): String;
var
  s: ansistring;
begin
  rawDefered := true;
//  s := instr;
  s := '';
  s := s+ CustAPick(instr,'White balance'   , 7,CanonWhite);
  s := s+ CustNth(  instr,'Sequence Number' , 9);
  s := s+ CustNth(  instr,'OpticalZoom Step',11);
  s := s+ CustNth(  instr,'AF point'        ,14);
  s := s+ CustAPick(instr,'Flash bias'      ,15,CanonBias);
  s := s+ CustNth(  instr,'Distance'        ,19);
  result := s;
end;

Const
  CanonOpt1    : ansistring = '0:Disable,1:Enable';
  CanonOpt2    : ansistring = '0:Enable,1:Disable';
  CanonNR      : ansistring = '0: Off,1: On';
  CanonYR      : ansistring = '0: On,1: Off';
  CanonAEBtn   : ansistring = '0:AF/AE lock,1:AE lock/AF,2:AF/AF lock,3:AE+release/AE+AF';
  CanonExpLevel: ansistring = '0:1/2 stop,1:1/3 stop';
  CanonAFassist: ansistring = '0:On (auto),1:Off';
  CanonAvSpeed : ansistring = '0:Automatic,1: 1/200 (fixed)';
  CanonAEB     : ansistring = '0:0, -, + / Enabled1: 0, -, + / Disabled,'+
                              '2: -, 0, + / Enabled,3: -, 0, + / Disabled';
  CanonSCS     : ansistring = '0:1st-curtain sync,1: 2nd-curtain sync';
  CanonAFBtn   : ansistring = '0:AF stop,1:Operate AF,2:Lock AE and start timer';
  CanonMenu    : ansistring = '0:Top,1:Previous (volatile),2:Previous';
  CanonSetBtn  : ansistring = '0:Not assigned,1:Change quality,2:Change ISO speed,'+
                              '3:Select parameters';

Function CanonCustom1(InStr: AnsiString): String;
var
  fn, s, r: String;
  fnct, data, i, j: Integer;
begin
  s := '';
  rawDefered := true;
  for i := 1 to StrCount(InStr, ',') do
  begin
    try
      fn := StrNth(InStr, ',', i);
      j  := StrToInt(fn);
      fnct := j div 256;  // upper 8 bits
      data := j mod 256;  // Lower 8 bits
      case fnct of
         1: r := CustBld('Noise Reduction', data, CanonNR);
         2: r := CustBld('Shutter AE Lock Button', data, CanonAEBtn);
         3: r := CustBld('Mirror Lockup', data, CanonOpt1);
         4: r := CustBld('Exposure Level', data, CanonExpLevel);
         5: r := CustBld('AF Assist', data, CanonAFassist);
         6: r := CustBld('AV Shutter Speed', data, CanonAvSpeed);
         7: r := CustBld('AEB Sequence', data, CanonAEB);
         8: r := CustBld('Shutter Sync', data, CanonSCS);
         9: r := CustBld('Lens AF Button', data, CanonAFBtn);
        10: r := CustBld('Fill Flash Reduction', data, CanonOpt2);
        11: r := CustBld('Menu Return', data, CanonMenu);
        12: r := CustBld('Set Button', data, CanonSetBtn);
        13: r := CustBld('Sensor Cleaning', data, CanonOpt1);
        14: r := CustBld('Superimposed Display', data, CanonYR);
        15: r := CustBld('Shutter Release w/o CF Card', data, CanonOpt2);
      else
        continue;  // unknown value;
      end
    except
    end;
    s := s + r;
  end;
  result := s;
end;


////////////////
// The key to the following function is the call into
// dEXIF's parser: ProcessHWSpecific
//
// The arguments are:
//       MakerNote: a Delphi string containing the maker note data
//        TagTable: TTagEntry containing tag entries
//  Initial offset: Offset to directory
//     Data offset: subtracted from the data offset for long data
//                  elements - typically contains the offset of the
//                  makernote field from the start of the file
//
constructor TmsInfo.Create(tiffFlag: boolean; p: TImageInfo);
begin
  inherited Create;  // Initialize inherited parts
  isTiff := tiffFlag;
  IMGparent := p;
  makerOffset := p.MakerOffset;
end;

function TmsInfo.ReadMSData(var DR:TImageInfo):boolean;
var
  UCMaker, tmp, tmp2: ansistring;
  MMode: boolean;
  x: integer;
begin
  UCMaker := Copy(AnsiString(AnsiUpperCase(DR.CameraMake)),1,5);
  gblUCMaker := '';
  curTagArray := IMGparent;
  result := true;
  if isTiff then
    MakerOffset := MakerOffset+16;
  with DR do
  begin
    if (UCMaker = 'NIKON') then
    begin
      tmp := copy(MakerNote,1,5);
      x := max(0,Pos(' ', imgParent.CameraModel));
      tmp2 := imgParent.CameraModel[x+1];
      if (imgParent.exifVersion > '0210') or
          ((imgParent.exifVersion = '') and
          (tmp2 = 'D') and isTiff) then
        ProcessHWSpecific(MakerNote,Nikon2Table,18,9)
      else
        if (tmp = 'Nikon')
          then ProcessHWSpecific(MakerNote,Nikon1Table,8,MakerOffset)
          else ProcessHWSpecific(MakerNote,Nikon2Table,0,MakerOffset-8);
    end
    else if (UCMaker = 'OLYMP') then
    begin
      ProcessHWSpecific(MakerNote,Olympus1Table,8,MakerOffset,9)
    end
    else if (UCMaker = 'CASIO') then
    begin
      if Pos('QVC', MakerNote) <> 1 then // newer style: unknown format
        ProcessHWSpecific(MakerNote,Casio1Table,0,MakerOffset-8)
      else
      begin
        ProcessHWSpecific(MakerNote,Casio2Table,6,MakerOffset-2)
      end;
    end
    else if (UCMaker = 'FUJIF') then
    begin
      MMode := MotorolaOrder;           //  Fuji uses motorola format for exif
      MotorolaOrder := false;           //  but not for MakerNote!!
      ProcessHWSpecific(MakerNote,Fuji1Table,12,12+1);
      MotorolaOrder := MMode;
    end
    else if (UCMaker = 'CANON') then
    begin
      ProcessHWSpecific(MakerNote,Canon1Table,0,MakerOffset-8)
    end
    else if (UCMaker = 'SEIKO') then
    begin
      ProcessHWSpecific(MakerNote,Epson1Table,8,MakerOffset)
    end
    else if (UCMaker = 'SANYO') then
    begin
      ProcessHWSpecific(MakerNote,Sanyo1Table,8,MakerOffset)
    end
    else if (UCMaker = 'MINOL') then
    begin
      ProcessHWSpecific(MakerNote,MinoltaTable,0,MakerOffset-8)
    end
    else
      result := false;   // not a recognized maker
  end;
  if result then
    gblUCMaker := DR.CameraMake;   //only if there's a match
end;

end.
