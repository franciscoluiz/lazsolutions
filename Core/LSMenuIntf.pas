(*
  LazSolutions, Menu intf unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSMenuIntf;

{$I lazsolutions.inc}

interface

uses
  LSRegExList, LSRegExPropertyEditorFrm, LSAboutFrm, LSJSONValidatorFrm,
  LSHTTPToolFrm, LSConsts, LSHTTPSend, MenuIntf, LCLIntf;

const
  LS_MAINMENU_NAME = 'LazSolutionsMainMenu';
  LS_MAINMENU_DESCRIPTION = CLazSolutionsDescription;

{ Create all menus of LazSolutions on IDE. }
procedure LSCreateIDEMenus;
{ Create a menu separator. }
procedure LSCreateIDEMenuSeparator(AParent: TIDEMenuSection);

implementation

{$HINTS OFF}
procedure LSRegExValidatorProc(ASender: TObject);
var
  VDummy: TLSRegEx = nil;
begin
  TLSRegExPropertyEditorForm.Execute(VDummy);
end;

procedure LSJSONValidatorProc(ASender: TObject);
begin
  TLSJSONValidatorForm.Execute;
end;

procedure LSHTTPToolProc(ASender: TObject);
begin
  TLSHTTPToolForm.Execute;
end;

procedure LSOpenHomePageProc(ASender: TObject);
begin
  OpenURL(CLazSolutionsHomePage);
end;

procedure LSOpenWikiProc(ASender: TObject);
begin
  OpenURL(CLazSolutionsHomePage + '/w/list');
end;

procedure LSOpenIssuesProc(ASender: TObject);
begin
  OpenURL(CLazSolutionsHomePage + '/issues/list');
end;

procedure LSOpenDownloadsProc(ASender: TObject);
begin
  OpenURL(CLazSolutionsHomePage + '/downloads/list');
end;

procedure LSOpenDonorsProc(ASender: TObject);
begin
  OpenURL(CLazSolutionsHomePage + '/wiki/Donors');
end;

procedure LSOpenAboutProc(ASender: TObject);
begin
  TLSAboutForm.Execute;
end;
{$HINTS ON}

procedure LSCreateIDEMenus;
var
  VLSMenu: TIDEMenuSection;
begin
  VLSMenu := RegisterIDESubMenu(mnuMain, LS_MAINMENU_NAME,
    LS_MAINMENU_DESCRIPTION);
  RegisterIDEMenuCommand(VLSMenu, 'LSRegExValidatorMenu', 'RegEx validator',
    nil, @LSRegExValidatorProc, nil, 'LSRegExValidatorMenu');
  LSCreateIDEMenuSeparator(VLSMenu);
  RegisterIDEMenuCommand(VLSMenu, 'LSJSONValidatorMenu', 'JSON validator',
    nil, @LSJSONValidatorProc, nil, 'LSJSONValidatorMenu');
  LSCreateIDEMenuSeparator(VLSMenu);
  RegisterIDEMenuCommand(VLSMenu, 'LSHTTPTool', 'HTTP tool', nil,
    @LSHTTPToolProc, nil, 'LSHTTPToolMenu');
  LSCreateIDEMenuSeparator(VLSMenu);
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenHomePageMenu', 'Home page',
    nil, @LSOpenHomePageProc, nil, 'LSOpenHomePageMenu');
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenWikiMenu', 'Wiki',
    nil, @LSOpenWikiProc, nil, 'LSOpenWikiMenu');
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenIssuesMenu', 'Issues',
    nil, @LSOpenIssuesProc, nil, 'LSOpenIssuesMenu');
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenDownloadsMenu', 'Downloads',
    nil, @LSOpenDownloadsProc, nil, 'LSOpenDownloadsMenu');
  LSCreateIDEMenuSeparator(VLSMenu);
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenDonorsMenu', 'Donors',
    nil, @LSOpenDonorsProc, nil, 'LSOpenDonorsMenu');
  LSCreateIDEMenuSeparator(VLSMenu);
  RegisterIDEMenuCommand(VLSMenu, 'LSOpenAboutMenu', 'About',
    nil, @LSOpenAboutProc, nil, 'LSOpenAboutMenu');
end;

procedure LSCreateIDEMenuSeparator(AParent: TIDEMenuSection);
begin
  RegisterIDEMenuCommand(AParent, '', '-');
end;

end.

