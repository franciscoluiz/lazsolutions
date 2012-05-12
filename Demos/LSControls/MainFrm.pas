unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  LSControls, LSRegExList, LSJSONPropStorage, Forms, ComCtrls, StdCtrls,
  SysUtils, Dialogs, ExtCtrls, Controls, Spin, Menus, Buttons, Classes;

type

  { TMainForm }

  TMainForm = class(TForm)
    Base64LSImage: TLSImage;
    AnimationImageList: TImageList;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    LSComboBox1: TLSComboBox;
    LSDateEdit1: TLSDateEdit;
    LSDateEdit2: TLSDateEdit;
    LSDateEdit3: TLSDateEdit;
    LSExpandPanel1: TLSExpandPanel;
    LSExpandPanel2: TLSExpandPanel;
    LSJSONPropStorageTestComboBox: TComboBox;
    LSJSONPropStorageTestEdit: TEdit;
    LSJSONPropStorageTestCheckBox: TCheckBox;
    LSJSONPropStorageSaveButton: TButton;
    LSJSONPropStorageLoadButton: TButton;
    LSJSONPropStorage1: TLSJSONPropStorage;
    LSTimeEdit1: TLSTimeEdit;
    LSTimeEdit2: TLSTimeEdit;
    LSTimeEdit3: TLSTimeEdit;
    LSTrayIconAnimateCheckBox: TCheckBox;
    LSTrayIconCloseToTrayButton: TButton;
    LSTrayIconShowNotifierOSButton: TButton;
    LSCurrencyEditTestButton: TButton;
    DragDropSearchCheckBox1: TCheckBox;
    ImageListLSImage: TLSImage;
    LSCurrencyEdit1: TLSCurrencyEdit;
    LSEditButton1: TLSEditButton;
    LSGadgetEdit1: TLSGadgetEdit;
    LSImage1: TLSImage;
    LSImageAnimator1: TLSImageAnimator;
    LSNumericEdit1: TLSNumericEdit;
    LSNumericEdit2: TLSNumericEdit;
    LSTrayIcon1: TLSTrayIcon;
    MenuGroupBox1: TGroupBox;
    LSTrayIconRestoreMenuItem: TMenuItem;
    LSTrayIconCloseMenuItem: TMenuItem;
    N1: TMenuItem;
    MenuLSCheckListBox: TLSCheckListBox;
    MoveDownItemBitBtn1: TButton;
    MoveItemsPanel1: TPanel;
    MoveUpItemBitBtn1: TButton;
    LSTrayIconPopupMenu: TPopupMenu;
    LSJSONPropStorageTestRadioGroup: TRadioGroup;
    CheckEnabledPanel: TPanel;
    SearchButton1: TButton;
    SearchEdit1: TEdit;
    SearchGroupBox1: TGroupBox;
    SearchLabel1: TLabel;
    SearchLSCheckListBox: TLSCheckListBox;
    SearchPanel1: TPanel;
    LSCheckListBoxTabSheet: TTabSheet;
    LSGadgetEditTabSheet: TTabSheet;
    LSEditButtonTabSheet: TTabSheet;
    LSNumericEditTabSheet: TTabSheet;
    LSCurrencyEditTabSheet: TTabSheet;
    LSTrayIconTabSheet: TTabSheet;
    LSJSONPropStorageTabSheet: TTabSheet;
    CheckAllSpeedButton: TSpeedButton;
    LSDateEditTabSheet: TTabSheet;
    LSTimeEditTabSheet: TTabSheet;
    LSExpandPanelTabSheet: TTabSheet;
    UnCheckAllSpeedButton: TSpeedButton;
    ReverseCheckSpeedButton: TSpeedButton;
    EnableAllSpeedButton: TSpeedButton;
    DisableAllSpeedButton: TSpeedButton;
    ReverseEnabledSpeedButton: TSpeedButton;
    TelLSMaskEdit: TLSMaskEdit;
    LSRegExList1: TLSRegExList;
    OpenImageButton: TButton;
    SaveImageButton: TButton;
    OpenImageBase64Button: TButton;
    SaveImageBase64Button: TButton;
    DragDropSearchCheckBox: TCheckBox;
    LSMaskEditTabSheet: TTabSheet;
    TestImageList: TImageList;
    LSBitBtn1: TLSBitBtn;
    LSPanel1: TLSPanel;
    LSPanel2: TLSPanel;
    LSPanelTabSheet: TTabSheet;
    LSBitBtnTabSheet: TTabSheet;
    LSSpeedButton1: TLSSpeedButton;
    LSSpeedButtonTabSheet: TTabSheet;
    LSImageTabSheet: TTabSheet;
    LSImagePageControl: TPageControl;
    Base64TabSheet: TTabSheet;
    Base64Panel: TPanel;
    Base64ScrollBox: TScrollBox;
    ImageListTabSheet: TTabSheet;
    ImageListSpinEdit: TSpinEdit;
    AnimationTabSheet: TTabSheet;
    TestLSComboBoxLabel: TLabel;
    TestLSComboBox: TLSComboBox;
    MoveDownItemBitBtn: TButton;
    MoveItemsPanel: TPanel;
    MoveUpItemBitBtn: TButton;
    SearchButton: TButton;
    CashPaymentButton: TButton;
    SearchEdit: TEdit;
    SearchGroupBox: TGroupBox;
    MenuGroupBox: TGroupBox;
    SearchLabel: TLabel;
    SearchLSListBox: TLSListBox;
    MenuLSListBox: TLSListBox;
    MoveMeEdit: TEdit;
    LSLabel1: TLSLabel;
    MoveControlGroupBox: TGroupBox;
    InstallmentPaymentButton: TButton;
    BuyButton: TButton;
    LSListBoxTabSheet: TTabSheet;
    SearchPanel: TPanel;
    LSComboBoxTabSheet: TTabSheet;
    TestLSButton: TLSButton;
    LSCheckBoxGroupBox: TGroupBox;
    LinesNumberLabel: TLabel;
    AZ09LSMemo: TLSMemo;
    AZ09GroupBox: TGroupBox;
    CheckConfirmLSCheckBox: TLSCheckBox;
    LSMemoTabSheet: TTabSheet;
    LSCheckBoxTabSheet: TTabSheet;
    LSButtonTabSheet: TTabSheet;
    ValidateAllButton: TButton;
    URLGroupBox: TGroupBox;
    URLLSEdit: TLSEdit;
    ExecuteEditorGroupBox: TGroupBox;
    ExecEditorLSLabel: TLSLabel;
    TelephoneGroupBox: TGroupBox;
    EmailGroupBox: TGroupBox;
    OnlyNumbersLSEdit: TLSEdit;
    OnlyNumbersGroupBox: TGroupBox;
    TelephoneLSEdit: TLSEdit;
    EmailLSEdit: TLSEdit;
    OpenCurrProjectGroupBox: TGroupBox;
    LSEditTabSheet: TTabSheet;
    VisitSiteLSLabel: TLSLabel;
    OpenFileLSLabel: TLSLabel;
    OpenURLGroupBox: TGroupBox;
    SendMailGroupBox: TGroupBox;
    EmailLSLabel: TLSLabel;
    MainPageControl: TPageControl;
    LSLabelTabSheet: TTabSheet;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BuyButtonClick(Sender: TObject);
    procedure CashPaymentButtonClick(Sender: TObject);
    procedure CheckAllSpeedButtonClick(Sender: TObject);
    procedure DisableAllSpeedButtonClick(Sender: TObject);
    procedure EnableAllSpeedButtonClick(Sender: TObject);
    procedure LSTrayIconAnimateCheckBoxChange(Sender: TObject);
    procedure CheckConfirmLSCheckBoxValidate(ASender: TObject;
      var AIsValid: Boolean);
    procedure DragDropSearchCheckBox1Change(Sender: TObject);
    procedure DragDropSearchCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AZ09LSMemoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageListSpinEditChange(Sender: TObject);
    procedure InstallmentPaymentButtonClick(Sender: TObject);
    procedure LSCurrencyEditTestButtonClick(Sender: TObject);
    procedure LSEditButton1ButtonClick(Sender: TObject);
    procedure LSJSONPropStorageLoadButtonClick(Sender: TObject);
    procedure LSJSONPropStorageSaveButtonClick(Sender: TObject);
    procedure LSTrayIcon1NotifierOSClick(Sender: TObject);
    procedure LSTrayIconCloseMenuItemClick(Sender: TObject);
    procedure LSTrayIconCloseToTrayButtonClick(Sender: TObject);
    procedure LSTrayIconShowNotifierOSButtonClick(Sender: TObject);
    procedure LSTrayIconRestoreMenuItemClick(Sender: TObject);
    procedure MoveDownItemBitBtn1Click(Sender: TObject);
    procedure MoveDownItemBitBtnClick(Sender: TObject);
    procedure MoveUpItemBitBtn1Click(Sender: TObject);
    procedure MoveUpItemBitBtnClick(Sender: TObject);
    procedure OpenImageBase64ButtonClick(Sender: TObject);
    procedure OpenImageButtonClick(Sender: TObject);
    procedure ReverseCheckSpeedButtonClick(Sender: TObject);
    procedure ReverseEnabledSpeedButtonClick(Sender: TObject);
    procedure SaveImageBase64ButtonClick(Sender: TObject);
    procedure SaveImageButtonClick(Sender: TObject);
    procedure SearchButton1Click(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchLSCheckListBoxMoveItem(ASender: TObject; AUpEnabled,
      ADownEnabled: Boolean);
    procedure SearchLSListBoxMoveItem(ASender: TObject; AUpEnabled,
      ADownEnabled: Boolean);
    procedure TestLSButtonClick(Sender: TObject);
    procedure UnCheckAllSpeedButtonClick(Sender: TObject);
    procedure ValidateAllButtonClick(Sender: TObject);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LSDialogs, LSGraphics;

const
  JSON_ITEMS = '{ "Item 1": "A", "Item 2": 123, "Item 3": true }';
  CCheckConfirmCaption = 'C&heck to confirm (option %d)';

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SessionProperties := // Works with LSJSONPropStorage.
    'Left;Top;WindowState;MainPageControl.TabIndex;' +
    'LSJSONPropStorageTestCheckBox.Checked;' +
    'LSJSONPropStorageTestComboBox.ItemIndex;LSJSONPropStorageTestEdit.Text;' +
    'LSJSONPropStorageTestRadioGroup.ItemIndex';
  OpenFileLSLabel.Caption := ExtractFilePath(ParamStr(0)) + 'demo.lpi';
{$IFDEF UNIX}
{$IFDEF LCLQt}
  ExecEditorLSLabel.Caption := 'kwrite';
{$ENDIF}
{$IFDEF LCLGtk2}
  ExecEditorLSLabel.Caption := 'gedit';
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  ExecEditorLSLabel.Caption := 'notepad';
{$ENDIF}
  ImageListSpinEdit.MinValue := -1;
  ImageListSpinEdit.MaxValue := Pred(TestImageList.Count);
  ImageListSpinEdit.Value := ImageListLSImage.ImageIndex;
  LSTrayIcon1.Icons.Clear;
  LSTrayIcon1.Icons.AddImages(AnimationImageList);
  LSTrayIcon1.AnimateInterval := 100;
  LSComboBox1.LoadJSON(JSON_ITEMS);
end;

procedure TMainForm.CashPaymentButtonClick(Sender: TObject);
begin
  CheckConfirmLSCheckBox.Caption := Format(CCheckConfirmCaption, [1]);
end;

procedure TMainForm.CheckAllSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.CheckAll;
end;

procedure TMainForm.DisableAllSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.DisableAll;
end;

procedure TMainForm.EnableAllSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.EnableAll;
end;

procedure TMainForm.LSTrayIconAnimateCheckBoxChange(Sender: TObject);
begin
  LSTrayIcon1.Animate := LSTrayIconAnimateCheckBox.Checked;
  if LSTrayIcon1.Animate then
  begin
    LSTrayIconCloseToTrayButton.Click;
    LSTrayIcon1.MakeOldIcon;
  end
  else
    LSTrayIcon1.RestoreOldIcon;
end;

procedure TMainForm.CheckConfirmLSCheckBoxValidate(ASender: TObject;
  var AIsValid: Boolean);
begin
  if not AIsValid then
    ShowMessage('Payment option unavailable. Please, choose another option.');
end;

procedure TMainForm.DragDropSearchCheckBox1Change(Sender: TObject);
begin
  SearchLSCheckListBox.DragDropItem := DragDropSearchCheckBox1.Checked;
end;

procedure TMainForm.DragDropSearchCheckBoxChange(Sender: TObject);
begin
  SearchLSListBox.DragDropItem := DragDropSearchCheckBox.Checked;
end;

procedure TMainForm.BuyButtonClick(Sender: TObject);
begin
  if CheckConfirmLSCheckBox.Checked then
    ShowMessage('Purchase was successful!')
  else
    ShowMessage('Check the "Check to confirm" option to make a purchase.')
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  if Assigned(LSComboBox1.Item) then
    ShowMessage(LSComboBox1.Item.AsString);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  LSComboBox1.Value := 123;
end;

procedure TMainForm.AZ09LSMemoChange(Sender: TObject);
const
  CInfoMemo = 'Lines: %d, Characters: %d';
begin
  LinesNumberLabel.Caption := Format(CInfoMemo, [AZ09LSMemo.Lines.Count,
    Length(AZ09LSMemo.Text)]);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LSImageAnimator1.Active := True;
end;

procedure TMainForm.ImageListSpinEditChange(Sender: TObject);
begin
  ImageListLSImage.ImageIndex := ImageListSpinEdit.Value;
end;

procedure TMainForm.InstallmentPaymentButtonClick(Sender: TObject);
begin
  CheckConfirmLSCheckBox.Caption := Format(CCheckConfirmCaption, [2]);
end;

procedure TMainForm.LSCurrencyEditTestButtonClick(Sender: TObject);
begin
  LSCurrencyEdit1.Value := 3.14159265;
end;

procedure TMainForm.LSEditButton1ButtonClick(Sender: TObject);
begin
  ShowMessage('Test OK');
end;

procedure TMainForm.LSJSONPropStorageLoadButtonClick(Sender: TObject);
begin
  LSJSONPropStorage1.Restore;
end;

procedure TMainForm.LSJSONPropStorageSaveButtonClick(Sender: TObject);
begin
  LSJSONPropStorage1.Save;
end;

procedure TMainForm.LSTrayIcon1NotifierOSClick(Sender: TObject);
begin
  ShowMessage('Yes, nice NotifierOS!');
end;

procedure TMainForm.LSTrayIconCloseMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.LSTrayIconCloseToTrayButtonClick(Sender: TObject);
begin
  LSTrayIcon1.HideMainForm;
end;

procedure TMainForm.LSTrayIconShowNotifierOSButtonClick(Sender: TObject);
begin
  LSTrayIcon1.ShowNotifierOS(Caption, 'Hello! :)');
end;

procedure TMainForm.LSTrayIconRestoreMenuItemClick(Sender: TObject);
begin
  LSTrayIcon1.RestoreMainForm;
end;

procedure TMainForm.MoveDownItemBitBtn1Click(Sender: TObject);
begin
  SearchLSCheckListBox.MoveItemToDown;
end;

procedure TMainForm.MoveDownItemBitBtnClick(Sender: TObject);
begin
  SearchLSListBox.MoveItemToDown;
end;

procedure TMainForm.MoveUpItemBitBtn1Click(Sender: TObject);
begin
  SearchLSCheckListBox.MoveItemToUp;
end;

procedure TMainForm.MoveUpItemBitBtnClick(Sender: TObject);
begin
  SearchLSListBox.MoveItemToUp;
end;

procedure TMainForm.OpenImageBase64ButtonClick(Sender: TObject);
begin
  Base64LSImage.LoadFromBase64File(LSOpenDialog(odtFile, '', '',
    -1, False, True));
// LSImage.LoadFromBase64String(Query.FieldByName('text_or_memo_field').AsString);
end;

procedure TMainForm.OpenImageButtonClick(Sender: TObject);
begin
  Base64LSImage.Picture.LoadFromFile(LSOpenDialog(odtFile, '', '', -1,
    False, True));
end;

procedure TMainForm.ReverseCheckSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.ReverseCheck;
end;

procedure TMainForm.ReverseEnabledSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.ReverseEnabled;
end;

procedure TMainForm.SaveImageBase64ButtonClick(Sender: TObject);
var
  VFileName: string;
begin
  if Base64LSImage.IsEmpty then
    Exit;
  VFileName := 'Image.' + LSGetFileExtFromPicture(Base64LSImage.Picture) +
    '.base64';
  if LSSaveDialog(VFileName) then
    Base64LSImage.SaveToBase64File(VFileName);
// Query.ParamByName('text_or_memo_param').AsString := LSImage.AsBase64;
end;

procedure TMainForm.SaveImageButtonClick(Sender: TObject);
var
  VFileName: string;
begin
  if Base64LSImage.IsEmpty then
    Exit;
  VFileName := 'Image.' + LSGetFileExtFromPicture(Base64LSImage.Picture);
  if LSSaveDialog(VFileName) then
    Base64LSImage.Picture.SaveToFile(VFileName);
end;

procedure TMainForm.SearchButton1Click(Sender: TObject);
begin
  SearchLSCheckListBox.Find(SearchEdit1.Text);
end;

procedure TMainForm.SearchButtonClick(Sender: TObject);
begin
  SearchLSListBox.Find(SearchEdit.Text);
end;

procedure TMainForm.SearchLSCheckListBoxMoveItem(ASender: TObject; AUpEnabled,
  ADownEnabled: Boolean);
begin
  MoveUpItemBitBtn1.Enabled := AUpEnabled;
  MoveDownItemBitBtn1.Enabled := ADownEnabled;
end;

procedure TMainForm.SearchLSListBoxMoveItem(ASender: TObject; AUpEnabled,
  ADownEnabled: Boolean);
begin
  MoveUpItemBitBtn.Enabled := AUpEnabled;
  MoveDownItemBitBtn.Enabled := ADownEnabled;
end;

procedure TMainForm.TestLSButtonClick(Sender: TObject);
begin
  ShowMessage('Yes, this is a shortcut button too. :)');
end;

procedure TMainForm.UnCheckAllSpeedButtonClick(Sender: TObject);
begin
  SearchLSCheckListBox.UnCheckAll;
end;

procedure TMainForm.ValidateAllButtonClick(Sender: TObject);
begin
  LSValidateAllControls(Self);
end;

end.

