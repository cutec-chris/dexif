// Common include for all tests
{$ifdef FPC}
  // {$mode objfpc}{$H+}
{$else}
 	{$ifdef VER140}
		{$define DELPHI6} // Delphi 6
		{$define DELPHI6OROLDER}
	{$endif}
	{$ifdef VER150}
		{$define DELPHI7}
		{$define ISDELPHI7ANDUP} // Delphi 7 or newer
	{$ENDIF}
	{$ifdef VER160}
		{$define DELPHI8}
		{$define ISDELPHI8ANDUP} // Delphi 8 or newer
	{$ENDIF}
    {$if CompilerVersion >= 17}
      {$define ISDELPHI2005ANDUP} // Delphi 2005 or newer
    {$ifend}
    {$if CompilerVersion >= 18}
      {$define ISDELPHI2006ANDUP} // Delphi 2006 or newer
    {$ifend}
    {$ifdef VER180}
      {$define ISDELPHI20062007} // to circumvent some specific bugs
    {$endif}
    {$ifdef VER185}
      {$define ISDELPHI20062007}
    {$endif}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // Delphi 2007 or newer
    {$ifend}
    {$if CompilerVersion = 20}
      {$define ISDELPHI2009} // Delphi 2009 
    {$ifend}
    {$if CompilerVersion >= 21.0}
      // Delphi 2010/XE
      {$define ISDELPHI2010}
    {$ifend}
    {$if CompilerVersion >= 22.0}
      {$define ISDELPHIXE}
    {$ifend}
    {$if CompilerVersion >= 23.0}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef ISDELPHIXE2}VCL.Graphics{$else}Graphics{$endif}
      {$define ISDELPHIXE2}
    {$ifend}
    {$if CompilerVersion >= 24.0}
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0}
      {$define ISDELPHIXE4}
    {$ifend}
    {$if CompilerVersion >= 26.0}
      {$define ISDELPHIXE5}
    {$ifend}
    {$if CompilerVersion >= 27.0}
      {$define ISDELPHIXE6}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
    {$ifend}
    {$if CompilerVersion >= 29.0}
      {$define ISDELPHIXE8}
    {$ifend}
    {$if CompilerVersion >= 30.0}
      {$define ISDELPHI10}
    {$ifend}
    {$if CompilerVersion >= 31.0}
      {$define ISDELPHI101}
    {$ifend}
    {$if CompilerVersion >= 32.0}
      {$define ISDELPHI102}
    {$ifend}

{$ENDIF FPC}

