(*
  LazSolutions, About Form
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSAboutFrm;

{$I lazsolutions.inc}

interface

uses
  LSPackageIntf, LSConsts, PackageIntf, Forms, ExtCtrls, LCLIntf, StdCtrls,
  Menus, SysUtils, Clipbrd, Classes, LCLType;

type

  { TLSAboutForm }

  TLSAboutForm = class(TForm)
    CloseImage: TImage;
    AboutPopupMenu: TPopupMenu;
    CopyVerInfoToClipboardMenuItem: TMenuItem;
    VersionLabel: TLabel;
    LogoImage: TImage;
    procedure CloseImageClick(Sender: TObject);
    procedure CopyVerInfoToClipboardMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LogoImageClick(Sender: TObject);
  public
    class function Execute: Integer;
    class function GetVersion: string;
    procedure CopyVerInfoToClipboard;
  end;

const
  LS_VERSION_FRTM_STR = 'RT %d.%d.%d.%d, DT %d.%d.%d.%d, EX %d.%d.%d.%d';

implementation

{$R *.lfm}

{ TLSAboutForm }

{$HINTS OFF}
procedure TLSAboutForm.CloseImageClick(Sender: TObject);
begin
  Close;
end;

procedure TLSAboutForm.CopyVerInfoToClipboardMenuItemClick(Sender: TObject);
begin
  CopyVerInfoToClipboard;
end;

procedure TLSAboutForm.FormCreate(Sender: TObject);
begin
  Width := LogoImage.Width;
  Height := LogoImage.Height + VersionLabel.Height;
end;

procedure TLSAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_C) then
    CopyVerInfoToClipboard;
  if Key = VK_ESCAPE then
  begin
    Close;
    Key := VK_UNKNOWN;
  end;
end;

procedure TLSAboutForm.LogoImageClick(Sender: TObject);
begin
  OpenURL(CLazSolutionsHomePage);
  Close;
end;
{$HINTS ON}

class function TLSAboutForm.Execute: Integer;
begin
  with Self.Create(nil) do
  try
    VersionLabel.Caption := 'Version: ' + TLSAboutForm.GetVersion;
    Result := ShowModal;
  finally
    Free;
  end;
end;

class function TLSAboutForm.GetVersion: string;
var
  VPkgVerRT, VPkgVerDT, VPkgVerEx: TPkgVersion;
begin
  VPkgVerRT := LSGetPackageVersion('lazsolutionsrt');
  VPkgVerDT := LSGetPackageVersion('lazsolutionsdt');
  VPkgVerEx := LSGetPackageVersion('lazsolutionsexperts');
  Result := Format(LS_VERSION_FRTM_STR, [
    VPkgVerRT.Major, VPkgVerRT.Minor, VPkgVerRT.Release, VPkgVerRT.Build,
    VPkgVerDT.Major, VPkgVerDT.Minor, VPkgVerDT.Release, VPkgVerDT.Build,
    VPkgVerEx.Major, VPkgVerEx.Minor, VPkgVerEx.Release, VPkgVerEx.Build]);
end;

procedure TLSAboutForm.CopyVerInfoToClipboard;
begin
  Clipboard.AsText := 'LazSolutions version: ' + TLSAboutForm.GetVersion;
end;

end.

