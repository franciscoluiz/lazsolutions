(*
  LazSolutions, Proxy config. dialog unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSHTTPSendProxyCfgDlg;

{$I lazsolutions.inc}

interface

uses
  Forms, ExtCtrls, StdCtrls;

type

  { TLSHTTPSendProxyConfigDialog }

  TLSHTTPSendProxyConfigDialog = class(TForm)
    ProxyGroupBox: TGroupBox;
    AuthenticationGroupBox: TGroupBox;
    HostEdit: TEdit;
    HostLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    ClientPanel: TPanel;
    BottomPanel: TPanel;
    PasswordEdit: TEdit;
    PasswordLabel: TLabel;
    PortEdit: TEdit;
    PortLabel: TLabel;
    UserEdit: TEdit;
    UserLabel: TLabel;
  end;

implementation

{$R *.lfm}

end.

