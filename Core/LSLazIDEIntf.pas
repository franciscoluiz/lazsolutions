(*
  LazSolutions, Lazarus IDE intf unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSLazIDEIntf;

{$I lazsolutions.inc}

interface

uses
  LSConsts, LazIDEIntf, FileUtil;

{ Get path of config of experts. }
function LSGetExpertsConfigPath: string;
{ Get filename of config of experts. }
function LSGetExpertsConfigFileName: string;

implementation

function LSGetExpertsConfigPath: string;
begin
  Result := AppendPathDelim(
{$IFDEF LSUSELAZIDEINTFPRIMARYCONFIGPATH}
    LazarusIDE.GetPrimaryConfigPath
{$ELSE}
    GetAppConfigDirUTF8(False)
{$ENDIF});
end;

function LSGetExpertsConfigFileName: string;
begin
  Result := LSGetExpertsConfigPath + CLSExpertsConfigFile;
end;

end.

