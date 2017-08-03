{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dexif_package;

interface

uses
  dIPTC, msData, dEXIF, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('dexif_package', @Register);
end.
