(*
  LazSolutions, HTTP user/password config. dialog unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSHTTPSendUPCfgDlg;

{$I lazsolutions.inc}

interface

uses
  Forms, StdCtrls, ExtCtrls;

type

  { TLSHTTPSendUPConfigDialog }

  TLSHTTPSendUPConfigDialog = class(TForm)
    HostLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
    AuthenticationGroupBox: TGroupBox;
    UserLabel: TLabel;
    PasswordLabel: TLabel;
    BottomPanel: TPanel;
  end;

implementation

{$R *.lfm}

end.

