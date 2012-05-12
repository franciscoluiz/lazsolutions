unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, Dialogs, SysUtils,
  IpHtml, Ipfilebroker, Classes;

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
    CCEdit: TEdit;
    CCLabel: TLabel;
    CloseBitBtn: TBitBtn;
    ConfirmReadingCheckBox: TCheckBox;
    FromEdit: TEdit;
    FromLabel: TLabel;
    HeaderPanel: TPanel;
    IpFileDataProvider: TIpFileDataProvider;
    IpHtmlPanel: TIpHtmlPanel;
    MailBottomPanel: TPanel;
    MailTabSheet: TTabSheet;
    MainPageControl: TPageControl;
    MessagePanel: TPanel;
    N1: TMenuItem;
    PriorityComboBox: TComboBox;
    PriorityLabel: TLabel;
    RemoveAttachedMenuItem: TMenuItem;
    SendBitBtn: TBitBtn;
    SubjectEdit: TEdit;
    SubjectLabel: TLabel;
    SubjectPanel: TPanel;
    ToEdit: TEdit;
    ToLabel: TLabel;
    TopPanel: TPanel;
    procedure AddAttachedMenuItemClick(Sender: TObject);
    procedure AttachedListBoxDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveAttachedMenuItemClick(Sender: TObject);
    procedure SendBitBtnClick(Sender: TObject);
  private
    FCurrURL: string;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSSMTPSend, LSUtils, LSDialogs;

{ TMainForm }

procedure TMainForm.SendBitBtnClick(Sender: TObject);
begin
  ShowMessage(LSSendMail(FromEdit.Text, ToEdit.Text, SubjectEdit.Text,
    LSLoadFile(FCurrURL), LowerCase(PriorityComboBox.Text), CCEdit.Text,
    BCCEdit.Text, 'your-nickname@gmail.com', 'your-password', 'smtp.gmail.com',
    '465', AttachedListBox.Items, ConfirmReadingCheckBox.Checked, True, True,
    mtHTML));
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

procedure TMainForm.FormCreate(Sender: TObject);
var
  VPath, VTempFileName: string;
  VStrTemp: TStringList;
begin
  AttachedListBox.Items.Add(LSCurrentPath + 'image.jpg');
  VStrTemp := TStringList.Create;
  try
    VPath := LSCurrentPath;
    FCurrURL := VPath + 'test.html';
    VStrTemp.LoadFromFile(FCurrURL);
    VStrTemp.Text := LSRemoveCid(VStrTemp.Text);
    VTempFileName := VPath + '~test.html';
    VStrTemp.SaveToFile(VTempFileName);
    IpHtmlPanel.OpenURL(ExpandLocalHtmlFileName(VTempFileName));
  finally
    DeleteFile(VTempFileName);
    VStrTemp.Free;
  end;
end;

end.

