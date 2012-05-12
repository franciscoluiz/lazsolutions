(*
  LazSolutions, HTTP tool Form
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSHTTPToolFrm;

{$I lazsolutions.inc}

interface

uses
  LSConsts, LSHTTPSend, LSDialogs, LSJSONPropStorage, LSLazIDEIntf, HTTPSend,
  SysUtils, Forms, Controls, Menus, ComCtrls, ExtCtrls, StdCtrls, Classes;

type

  { TLSHTTPToolForm }

  TLSHTTPToolForm = class(TForm)
    HTTPPPMethodComboBox: TComboBox;
    HTTPGDMethodComboBox: TComboBox;
    HTTPGDResponseLabel: TLabel;
    HTTPPPContentTypeRadioGroup: TRadioGroup;
    HTTPPPCookiesLabel: TLabel;
    HTTPPPCookiesMemo: TMemo;
    HTTPPPDataLabel: TLabel;
    HTTPPPDataMemo: TMemo;
    HTTPPPExecuteButton: TButton;
    HTTPPPHeadersLabel: TLabel;
    HTTPPPHeadersMemo: TMemo;
    HTTPPPSplitter1: TSplitter;
    HTTPGDExecuteButton: TButton;
    HTTPGDURLEdit: TEdit;
    HTTPGDURLLabel: TLabel;
    ConfigLSJSONPropStorage: TLSJSONPropStorage;
    HTTPPPSplitter2: TSplitter;
    HTTPPPSplitter3: TSplitter;
    HTTPPPURLDataLabel: TLabel;
    HTTPGDCookiesLabel: TLabel;
    HTTPGDHeadersLabel: TLabel;
    HTTPPPURLDataMemo: TMemo;
    HTTPPPURLEdit: TEdit;
    HTTPPPURLLabel: TLabel;
    HTTPPPURLPanel: TPanel;
    HTTPGDMethodLabel: TLabel;
    HTTPPPMethodLabel: TLabel;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    MainPageControl: TPageControl;
    HTTPGDURLPanel: TPanel;
    HTTPGDHeadersMemo: TMemo;
    HTTPGDResponseMemo: TMemo;
    HTTPGDCookiesMemo: TMemo;
    HTTPMenuItem: TMenuItem;
    HTTPGDMenuItem: TMenuItem;
    HTTPGDSaveHeadersMenuItem: TMenuItem;
    HTTPGDSaveCookiesMenuItem: TMenuItem;
    HTTPGDLoadCookiesMenuItem: TMenuItem;
    HTTPGDSaveResponseMenuItem: TMenuItem;
    HTTPGDSaveDocumentMenuItem: TMenuItem;
    HTTPPPMenuItem: TMenuItem;
    HTTPPPLoadCookiesMenuItem: TMenuItem;
    HTTPPPSaveHeadersMenuItem: TMenuItem;
    HTTPPPSaveCookiesMenuItem: TMenuItem;
    HTTPPPSaveResponseMenuItem: TMenuItem;
    HTTPPPSaveDataMenuItem: TMenuItem;
    N3: TMenuItem;
    UserPasswordMenuItem: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    ProxyMenuItem: TMenuItem;
    OptionsMenuItem: TMenuItem;
    HTTPGDTabSheet: TTabSheet;
    HTTPGDSplitter1: TSplitter;
    HTTPGDSplitter2: TSplitter;
    HTTPPPTabSheet: TTabSheet;
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure HTTPGDExecuteButtonClick(Sender: TObject);
    procedure HTTPGDLoadCookiesMenuItemClick(Sender: TObject);
    procedure HTTPGDSaveCookiesMenuItemClick(Sender: TObject);
    procedure HTTPGDSaveDocumentMenuItemClick(Sender: TObject);
    procedure HTTPGDSaveHeadersMenuItemClick(Sender: TObject);
    procedure HTTPGDSaveResponseMenuItemClick(Sender: TObject);
    procedure HTTPGDTabSheetShow(Sender: TObject);
    procedure HTTPGDURLEditKeyPress(Sender: TObject; var Key: char);
    procedure HTTPMenuItemClick(Sender: TObject);
    procedure HTTPPPExecuteButtonClick(Sender: TObject);
    procedure HTTPPPLoadCookiesMenuItemClick(Sender: TObject);
    procedure HTTPPPSaveCookiesMenuItemClick(Sender: TObject);
    procedure HTTPPPSaveDataMenuItemClick(Sender: TObject);
    procedure HTTPPPSaveHeadersMenuItemClick(Sender: TObject);
    procedure HTTPPPSaveResponseMenuItemClick(Sender: TObject);
    procedure HTTPPPTabSheetShow(Sender: TObject);
    procedure HTTPPPURLEditKeyPress(Sender: TObject; var Key: char);
    procedure ProxyMenuItemClick(Sender: TObject);
    procedure UserPasswordMenuItemClick(Sender: TObject);
  private
    FHTTPGD: THTTPSend;
    FHTTPPP: THTTPSend;
    FPPData: TMemoryStream;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute;
    procedure HTTPGDExecute;
    procedure HTTPPPExecute;
  end;

const
  CDefFileName = '*.txt';
  CFilter = 'Text file (*.txt)|*.txt|All file (*.*)|*.*';
  CForceExt = 'txt';

var
  LSHTTPToolForm: TLSHTTPToolForm;

implementation

{$R *.lfm}

{ TLSHTTPToolForm }

constructor TLSHTTPToolForm.Create(AOwner: TComponent);
var
  VConfigPath: string;
begin
  inherited Create(AOwner);
  ConfigLSJSONPropStorage.FileName := LSGetExpertsConfigFileName;
  VConfigPath := LSGetExpertsConfigPath;
  LSGlobalProxyConfig.ConfigFileName := VConfigPath +
    CLSHTTPSENDGLOBALPROXYCONFIGFILENAME;
  LSGlobalUPConfig.ConfigFileName := VConfigPath +
    CLSHTTPSENDGLOBALUPCONFIGFILENAME;
  FHTTPGD := THTTPSend.Create;
  FHTTPPP := THTTPSend.Create;
  FPPData := TMemoryStream.Create;
end;

destructor TLSHTTPToolForm.Destroy;
begin
  FHTTPGD.Free;
  FPPData.Free;
  FHTTPPP.Free;
  ConfigLSJSONPropStorage.StoredValue['FirstOpening'] := 'false';
  ConfigLSJSONPropStorage.Save;
  inherited Destroy;
end;

{$HINTS OFF}
procedure TLSHTTPToolForm.ProxyMenuItemClick(Sender: TObject);
begin
  LSGlobalProxyDialog;
end;

procedure TLSHTTPToolForm.UserPasswordMenuItemClick(Sender: TObject);
begin
  LSGlobalUPDialog;
end;

procedure TLSHTTPToolForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TLSHTTPToolForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  LSHTTPToolForm := nil;
  CloseAction :=  caFree;
end;

procedure TLSHTTPToolForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close;
    Key := #0;
  end;
end;

procedure TLSHTTPToolForm.HTTPGDExecuteButtonClick(Sender: TObject);
begin
  HTTPGDExecute;
end;

procedure TLSHTTPToolForm.HTTPGDLoadCookiesMenuItemClick(Sender: TObject);
begin
  HTTPGDCookiesMemo.Lines.LoadFromFile(LSOpenDialog);
end;

procedure TLSHTTPToolForm.HTTPGDSaveCookiesMenuItemClick(Sender: TObject);
begin
  HTTPGDCookiesMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPGDSaveDocumentMenuItemClick(Sender: TObject);
begin
  FHTTPGD.Document.SaveToFile(LSSaveDialogS);
end;

procedure TLSHTTPToolForm.HTTPGDSaveHeadersMenuItemClick(Sender: TObject);
begin
  HTTPGDHeadersMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPGDSaveResponseMenuItemClick(Sender: TObject);
begin
  HTTPGDResponseMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPGDTabSheetShow(Sender: TObject);
begin
  if HTTPGDURLEdit.CanFocus then
    HTTPGDURLEdit.SetFocus;
end;

procedure TLSHTTPToolForm.HTTPGDURLEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then
  begin
    HTTPGDExecute;
    Key := #0;
  end;
end;

procedure TLSHTTPToolForm.HTTPMenuItemClick(Sender: TObject);
begin
  HTTPGDSaveHeadersMenuItem.Enabled := HTTPGDHeadersMemo.Lines.Count > 0;
  HTTPGDSaveCookiesMenuItem.Enabled := HTTPGDCookiesMemo.Lines.Count > 0;
  HTTPGDSaveResponseMenuItem.Enabled := HTTPGDResponseMemo.Lines.Count > 0;
  HTTPGDSaveDocumentMenuItem.Enabled := Assigned(FHTTPGD.Document) and
    (FHTTPGD.Document.Size > 0);
  HTTPPPSaveHeadersMenuItem.Enabled := HTTPPPHeadersMemo.Lines.Count > 0;
  HTTPPPSaveCookiesMenuItem.Enabled := HTTPPPCookiesMemo.Lines.Count > 0;
  HTTPPPSaveResponseMenuItem.Enabled := HTTPPPDataMemo.Lines.Count > 0;
  HTTPPPSaveDataMenuItem.Enabled := Assigned(FPPData) and
    (FPPData.Size > 0);
end;

procedure TLSHTTPToolForm.HTTPPPExecuteButtonClick(Sender: TObject);
begin
  HTTPPPExecute;
end;

procedure TLSHTTPToolForm.HTTPPPLoadCookiesMenuItemClick(Sender: TObject);
begin
  HTTPPPCookiesMemo.Lines.LoadFromFile(LSOpenDialog);
end;

procedure TLSHTTPToolForm.HTTPPPSaveCookiesMenuItemClick(Sender: TObject);
begin
  HTTPPPCookiesMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPPPSaveDataMenuItemClick(Sender: TObject);
begin
  FHTTPPP.Document.SaveToFile(LSSaveDialogS);
end;

procedure TLSHTTPToolForm.HTTPPPSaveHeadersMenuItemClick(Sender: TObject);
begin
  HTTPPPHeadersMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPPPSaveResponseMenuItemClick(Sender: TObject);
begin
  HTTPPPDataMemo.Lines.SaveToFile(LSSaveDialogS(CDefFileName, CFilter,
    CForceExt));
end;

procedure TLSHTTPToolForm.HTTPPPTabSheetShow(Sender: TObject);
begin
  if HTTPPPURLEdit.CanFocus then
    HTTPPPURLEdit.SetFocus;
end;

procedure TLSHTTPToolForm.HTTPPPURLEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then
  begin
    HTTPPPExecute;
    Key := #0;
  end;
end;
{$HINTS ON}

class procedure TLSHTTPToolForm.Execute;
begin
  if not Assigned(LSHTTPToolForm) then
  begin
    Application.CreateForm(TLSHTTPToolForm, LSHTTPToolForm);
    LSHTTPToolForm.ConfigLSJSONPropStorage.Restore;
    if LSHTTPToolForm.ConfigLSJSONPropStorage.
        StoredValue['FirstOpening'] = '' then
      LSHTTPToolForm.Position := poDesktopCenter;
  end;
  LSHTTPToolForm.Show;
end;

procedure TLSHTTPToolForm.HTTPGDExecute;
begin
  if Trim(HTTPGDURLEdit.Text) = '' then
    Exit;
  FHTTPGD.Clear;
  HTTPGDResponseMemo.Clear;
  FHTTPGD.Cookies.Assign(HTTPGDCookiesMemo.Lines);
  case HTTPGDMethodComboBox.ItemIndex of
    0: LSHTTPGetTextEx(FHTTPGD, HTTPGDURLEdit.Text,
        HTTPGDResponseMemo.Lines, False);
    1: LSHTTPDeleteTextEx(FHTTPGD, HTTPGDURLEdit.Text,
        HTTPGDResponseMemo.Lines, False);
  end;
  if (HTTPGDCookiesMemo.Lines.Count = 0) and (FHTTPGD.Cookies.Count > 0) then
    HTTPGDCookiesMemo.Lines.Assign(FHTTPGD.Cookies);
  HTTPGDHeadersMemo.Lines.Assign(FHTTPGD.Headers);
end;

procedure TLSHTTPToolForm.HTTPPPExecute;
begin
  if Trim(HTTPPPURLEdit.Text) = '' then
    Exit;
  FHTTPPP.Clear;
  FPPData.Clear;
  HTTPPPDataMemo.Clear;
  FHTTPPP.Cookies.Assign(HTTPPPCookiesMemo.Lines);
  case HTTPPPContentTypeRadioGroup.ItemIndex of
    0:
      case HTTPPPMethodComboBox.ItemIndex of
        0: LSHTTPPostURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False);
        1: LSHTTPPutURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False);
      end;
    1:
      case HTTPPPMethodComboBox.ItemIndex of
        0: LSHTTPPostURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False,
            CLSHTTPSendContentType_json);
        1: LSHTTPPutURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False,
            CLSHTTPSendContentType_json);
      end;
    2:
      case HTTPPPMethodComboBox.ItemIndex of
        0: LSHTTPPostURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False,
            CLSHTTPSendContentType_xml);
        1: LSHTTPPutURLEx(FHTTPPP, HTTPPPURLEdit.Text,
            HTTPPPURLDataMemo.Text, FPPData, False,
            CLSHTTPSendContentType_xml);
      end;
  end;
  if (HTTPPPCookiesMemo.Lines.Count = 0) and (FHTTPPP.Cookies.Count > 0) then
    HTTPPPCookiesMemo.Lines.Assign(FHTTPPP.Cookies);
  FPPData.Position := 0;
  HTTPPPDataMemo.Lines.LoadFromStream(FPPData);
  HTTPPPHeadersMemo.Lines.Assign(FHTTPPP.Headers);
end;

end.

