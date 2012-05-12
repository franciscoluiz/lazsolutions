unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ComCtrls, ExtCtrls, StdCtrls, Buttons, Menus, SysUtils,
  Dialogs, XMLPropStorage;

type

  { TMainForm }

  TMainForm = class(TForm)
    AttachedLabel: TLabel;
    AttachedListBox: TListBox;
    AttachedPanel: TPanel;
    AttachedSplitter: TSplitter;
    AttachedPopupMenu: TPopupMenu;
    AddAttachedMenuItem: TMenuItem;
    CCEdit: TEdit;
    BCCEdit: TEdit;
    CCLabel: TLabel;
    BCCLabel: TLabel;
    CloseBitBtn: TBitBtn;
    ConfirmReadingCheckBox: TCheckBox;
    FromEdit: TEdit;
    FromLabel: TLabel;
    HeaderPanel: TPanel;
    MailBottomPanel: TPanel;
    CancelBitBtn: TBitBtn;
    PriorityComboBox: TComboBox;
    PriorityLabel: TLabel;
    SendBitBtn: TBitBtn;
    StatusLabel: TLabel;
    SubjectEdit: TEdit;
    SubjectLabel: TLabel;
    SubjectPanel: TPanel;
    ToEdit: TEdit;
    ToLabel: TLabel;
    TopPanel: TPanel;
    TLSCheckBox: TCheckBox;
    N1: TMenuItem;
    RemoveAttachedMenuItem: TMenuItem;
    HostEdit: TEdit;
    HostLabel: TLabel;
    TipLabel: TLabel;
    SMPTPanel: TPanel;
    PasswordEdit: TEdit;
    PasswordLabel: TLabel;
    PortEdit: TEdit;
    PortLabel: TLabel;
    SSLCheckBox: TCheckBox;
    HeaderSplitter: TSplitter;
    AboutTabSheet: TTabSheet;
    MessageMemo: TMemo;
    MainPageControl: TPageControl;
    MessagePanel: TPanel;
    MailTabSheet: TTabSheet;
    ConfigTabSheet: TTabSheet;
    UserEdit: TEdit;
    UserLabel: TLabel;
    XMLPropStorage: TXMLPropStorage;
    procedure AddAttachedMenuItemClick(Sender: TObject);
    procedure AttachedListBoxDblClick(Sender: TObject);
    procedure CancelBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RemoveAttachedMenuItemClick(Sender: TObject);
    procedure SendBitBtnClick(Sender: TObject);
  private
    FThreadID: TThreadID;
    FStatus: string;
  protected
    procedure DoStart;
    procedure DoStop;
  public
    procedure Send;
    function Cancel: Boolean;
    procedure OnStart;
    procedure OnStop(const AStatus: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSConsts, LSUtils, LSDialogs, SynaCode, SynaUtil;

function FormatParam(const AString: string): string;
begin
  Result := QuoteStr(EncodeURL(AString),
{$IFDEF MSWINDOWS}
  '"'
{$ENDIF}
{$IFDEF UNIX}
  ''''
{$ENDIF});
end;

function BoolToYN(const B: Boolean): string;
begin
  if B then
    Result := 'y'
  else
    Result := 'n';
end;

procedure InternalSend;

  function _FormatAttached: string;
  var
    I, VAttachedCount: Integer;
    VAttached: string = '';
  begin
    Result := '';
    with MainForm do
    begin
      VAttachedCount := AttachedListBox.Items.Count;
      for I := 0 to Pred(VAttachedCount) do
        if I < Pred(VAttachedCount) then
          VAttached := VAttached + AttachedListBox.Items.Strings[I] + ';'
        else
          VAttached := VAttached + AttachedListBox.Items.Strings[I];
    end;
    if VAttachedCount > 0 then
      Result := ' -attached=' + FormatParam(VAttached);
  end;

  function _FormatCC: string;
  begin
    Result := '';
    with MainForm do
      if CCEdit.Text <> '' then
        Result := ' -cc=' + FormatParam(CCEdit.Text);
  end;

  function _FormatBCC: string;
  begin
    Result := '';
    with MainForm do
      if BCCEdit.Text <> '' then
        Result := ' -bcc=' +  FormatParam(BCCEdit.Text);
  end;

begin
  with MainForm do
  begin
    TThread.Synchronize(nil, @DoStart);
    FStatus := LSExecProcess(CLSSendMailPath +
      ' -from=' + FormatParam(FromEdit.Text) +
      ' -to=' + FormatParam(ToEdit.Text) + _FormatCC + _FormatBCC +
      ' -subject=' + FormatParam(Utf8ToAnsi(SubjectEdit.Text)) +
      ' -message=' + FormatParam(MessageMemo.Text) +
      ' -messagetype=' + FormatParam('txt') + _FormatAttached +
      ' -priority=' + FormatParam(LowerCase(PriorityComboBox.Text)) +
      ' -confirmreading=' + FormatParam(BoolToYN(ConfirmReadingCheckBox.Checked)) +
      ' -user=' + FormatParam(UserEdit.Text) +
      ' -password=' + FormatParam(PasswordEdit.Text) +
      ' -host=' + FormatParam(HostEdit.Text) +
      ' -port=' + FormatParam(PortEdit.Text) +
      ' -ssl=' + FormatParam(BoolToYN(SSLCheckBox.Checked)) +
      ' -tls=' + FormatParam(BoolToYN(TLSCheckBox.Checked)) +
      ' -attempt=' + FormatParam('10'));
    TThread.Synchronize(nil, @DoStop);
  end;
end;

{ TMainForm }

procedure TMainForm.RemoveAttachedMenuItemClick(Sender: TObject);
begin
  if AttachedListBox.ItemIndex <> -1 then
    AttachedListBox.Items.Delete(AttachedListBox.ItemIndex);
end;

procedure TMainForm.SendBitBtnClick(Sender: TObject);
begin
  Send;
end;

procedure TMainForm.DoStart;
begin
  OnStart;
end;

procedure TMainForm.DoStop;
begin
  FThreadID := 0;
  OnStop(FStatus);
end;

procedure TMainForm.Send;
begin
  FThreadID := BeginThread(TThreadFunc(@InternalSend));
end;

function TMainForm.Cancel: Boolean;
begin
  Result := FThreadID <> 0;
  if Result then
    SuspendThread(FThreadID);
end;

procedure TMainForm.OnStart;
begin
  StatusLabel.Show;
  SendBitBtn.Enabled := False;
  CancelBitBtn.Enabled := True;
end;

procedure TMainForm.OnStop(const AStatus: string);
begin
  SendBitBtn.Enabled := True;
  CancelBitBtn.Enabled := False;
  StatusLabel.Hide;
  ShowMessage(AStatus);
end;

procedure TMainForm.AddAttachedMenuItemClick(Sender: TObject);
begin
  AttachedListBox.Items.Add(LSOpenDialog);
end;

procedure TMainForm.AttachedListBoxDblClick(Sender: TObject);
begin
  AddAttachedMenuItem.Click;
end;

procedure TMainForm.CancelBitBtnClick(Sender: TObject);
begin
  if Cancel then
  begin
    FStatus := 'Send mail canceled for user.';
    DoStop;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  StatusLabel.Caption := 'Sending...';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Cancel;
end;

end.

