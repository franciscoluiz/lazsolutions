(*
  LazSolutions, Registry all experts unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSExpertsReg;

{$I lazsolutions.inc}

interface

uses
  LSMenuIntf, LResources;

procedure Register;

implementation

procedure Register;
begin
  LSCreateIDEMenus;
end;

initialization
  {$I LSExpertsReg.lrs}

end.

