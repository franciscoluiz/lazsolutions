(*
  LazSolutions, Exception dialog unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSExceptionDlg;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, ActnList, Buttons;

type

  { TLSExceptionDialog }

  TLSExceptionDialog = class(TForm)
    CopyAction: TAction;
    CloseBitBtn: TBitBtn;
    CopyBitBtn: TBitBtn;
    EmailAction: TAction;
    EmailBitBtn: TBitBtn;
    MessageMemo: TMemo;
    TerminateAction: TAction;
    MainActionList: TActionList;
    BottomPanel: TPanel;
    MessagePanel: TPanel;
    TerminateBitBtn: TBitBtn;
  end;

implementation

{$R *.lfm}

end.

