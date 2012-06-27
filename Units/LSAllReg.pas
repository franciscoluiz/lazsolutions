(*
  LazSolutions, Registry all classes/components/controls/editors... unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSAllReg;

{$I lazsolutions.inc}

interface

uses
  LSPropEdits, LSComponentEditors, LSRegExList, LSControls, LSGrids,
  LSJSONPropStorage, PropEdits, ComponentEditors, LResources, ImgList, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterClass(TLSRegEx);
  RegisterClass(TLSRegExs);
  RegisterPropertyEditor(TypeInfo(string), nil, 'About',
    TLSAboutPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TLSRegExList, 'Tip',
    TLSRegExTipPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), nil, 'Pattern',
    TLSPatternPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TLSRegEx, 'Expression',
    TLSExpressionPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TLSBitBtn, 'ImageIndex',
    TLSImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TLSSpeedButton, 'ImageIndex',
    TLSImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TLSImage, 'ImageIndex',
    TLSImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TLSDateEdit, 'DisplayFormat',
    TLSDateEditDisplayFrmtPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime), TLSTimeEdit, 'Value',
    TTimePropertyEditor);
  RegisterComponents('LazSolutions', [TLSRegExList, TLSImageAnimator,
    TLSTrayIcon, TLSJSONPropStorage, TLSPaletteSep, TLSLabel, TLSImage,
    TLSPanel, TLSExpandPanel, TLSCheckBox, TLSButton, TLSBitBtn, TLSSpeedButton,
    TLSEdit, TLSMaskEdit, TLSNumericEdit, TLSCurrencyEdit, TLSDateEdit,
    TLSTimeEdit, TLSEditButton, TLSGadgetEdit, TLSComboBox, TLSMemo, TLSListBox,
    TLSCheckListBox, TLSStringGrid]);
  RegisterComponentEditor(TLSRegExList, TLSRegExListComponentEditor);
  RegisterComponentEditor(TLSCheckListBox, TLSCheckListBoxComponentEditor);
  RegisterComponentEditor(TLSMaskEdit, TLSMaskEditEditor);
  RegisterComponentEditor(TLSEditButton, TLSMaskEditEditor);
  RegisterComponentEditor(TLSDateEdit, TLSMaskEditEditor);
  RegisterComponentEditor(TLSStringGrid, TLSStringGridComponentEditor);
{$IFNDEF LSNEWFPC}
  LSRegisterComponentEditor
{$ELSE}
  RegisterComponentEditor
{$ENDIF}([TLSPaletteSep, TLSImageAnimator, TLSTrayIcon, TLSJSONPropStorage,
    TLSLabel, TLSImage, TLSPanel, TLSExpandPanel, TLSCheckBox, TLSButton,
    TLSBitBtn, TLSSpeedButton, TLSEdit, TLSNumericEdit, TLSCurrencyEdit,
    TLSMemo, TLSListBox, TLSTimeEdit, TLSGadgetEdit, TLSComboBox],
    TLSDefaultComponentEditor);
end;

initialization
  {$I LSAllReg.lrs}

end.

