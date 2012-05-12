(*
  LazSolutions, Grids columns manager unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSGridsColumnsManagerFrm;

{$I lazsolutions.inc}

interface

uses
  Forms, CheckLst, ExtCtrls, StdCtrls;

type

  { TLSGridsColumnsManagerForm }

  TLSGridsColumnsManagerForm = class(TForm)
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    ColumnsCheckListBox: TCheckListBox;
    OKButton: TButton;
  end;

implementation

{$R *.lfm}

end.

