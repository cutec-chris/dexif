
{$IFDEF FPC}
  {------------------------------------------------------------------
    Defines for Lanzarus and FPC
  ------------------------------------------------------------------}
  ////// { Activate this define if an FPC version of at least 3.0 is used. }
  { This define signals that an FPC version >= 3.0 is used }
  {$DEFINE FPC3+}
  {$I fpc.inc}

  // Don't use jpeg units
  {$DEFINE dExifNoJpeg}

{$ELSE}
  {------------------------------------------------------------------
   Defines for Delphi
  ------------------------------------------------------------------}
  {$UNDEF FPC3+}

  { Activate this define if a library other than Delphi's jpeg is
    used for reading of jpeg files.
    Is active by default for Lazarus/FPC }
  {.$DEFINE dEXIFNoJpeg}

  { Activate the define ENDIAN_BIG if working on a Big-Endian machine }
  {.$DEFINE ENDIAN_BIG}

{$ENDIF}

