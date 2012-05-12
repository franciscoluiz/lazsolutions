unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, Dialogs, Classes;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddAttachedMenuItem: TMenuItem;
    AttachedLabel: TLabel;
    AttachedListBox: TListBox;
    AttachedPanel: TPanel;
    AttachedPopupMenu: TPopupMenu;
    AttachedSplitter: TSplitter;
    BCCEdit: TEdit;
    BCCLabel: TLabel;
    CancelBitBtn: TBitBtn;
    CCEdit: TEdit;
    CCLabel: TLabel;
    CloseBitBtn: TBitBtn;
    ConfirmReadingCheckBox: TCheckBox;
    FromEdit: TEdit;
    FromLabel: TLabel;
    HeaderPanel: TPanel;
    MailBottomPanel: TPanel;
    MailTabSheet: TTabSheet;
    MainPageControl: TPageControl;
    MessageMemo: TMemo;
    MessagePanel: TPanel;
    N1: TMenuItem;
    PriorityComboBox: TComboBox;
    PriorityLabel: TLabel;
    RemoveAttachedMenuItem: TMenuItem;
    SendBitBtn: TBitBtn;
    StatusLabel: TLabel;
    SubjectEdit: TEdit;
    SubjectLabel: TLabel;
    SubjectPanel: TPanel;
    ToEdit: TEdit;
    ToLabel: TLabel;
    TopPanel: TPanel;
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
  LSSMTPSend, LSDialogs, LSMessages;

procedure _Send;
begin
  with MainForm do
  begin
    TThread.Synchronize(nil, @DoStart);
    FStatus := LSSendMail(FromEdit.Text, ToEdit.Text, SubjectEdit.Text,
      MessageMemo.Text, LowerCase(PriorityComboBox.Text), CCEdit.Text,
      BCCEdit.Text, 'your-nickname@gmail.com', 'your-password',
      'smtp.gmail.com', '465', AttachedListBox.Items,
      ConfirmReadingCheckBox.Checked, True, True);
    TThread.Synchronize(nil, @DoStop);
  end;
end;

{ TMainForm }

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
  FThreadID := BeginThread(TThreadFunc(@_Send));
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

procedure TMainForm.RemoveAttachedMenuItemClick(Sender: TObject);
begin
  if AttachedListBox.ItemIndex <> -1 then
    AttachedListBox.Items.Delete(AttachedListBox.ItemIndex);
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

