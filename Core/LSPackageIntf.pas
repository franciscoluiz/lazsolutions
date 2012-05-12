(*
  LazSolutions, Package intf unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSPackageIntf;

{$I lazsolutions.inc}

interface

uses
  PackageIntf, SysUtils;

{ Find a package in IDE. }
function LSFindIDEPackage(const APkgName: string): TIDEPackage;
{ Get package version. }
function LSGetPackageVersion(const APkgName: string): TPkgVersion;

implementation

function LSFindIDEPackage(const APkgName: string): TIDEPackage;
var
  I: Integer;
begin
  for I := 0 to Pred(PackageEditingInterface.GetPackageCount) do
  begin
    Result := PackageEditingInterface.GetPackages(I);
    if SameText(Result.Name, APkgName) then
      Break;
  end;
end;

function LSGetPackageVersion(const APkgName: string): TPkgVersion;
begin
  Result := LSFindIDEPackage(APkgName).Version;
end;

end.

