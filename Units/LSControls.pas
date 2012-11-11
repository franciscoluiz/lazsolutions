(*
  LazSolutions, Controls unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSControls;

{$I lazsolutions.inc}

interface

uses
  LSCommonIntf, LSUtils, LSMessages, LSConsts, LSRegExList, LSCalendarPopup,
  LSRegEx, LSNotifierOS, LSExceptionUtils, LMessages, LResources, LCLIntf,
  LCLType, LCLProc, FileUtil, Controls, Forms, Buttons, SysUtils, StdCtrls,
  ExtCtrls, Classes, ImgList, Graphics, GraphType, Clipbrd, Base64, MaskEdit,
  CheckLst, Math, StrUtils, Calendar, ComCtrls, Types, FPJSON;

const
  LM_NONE = -1;
{$J+}
  DEFAULT_PLACEHOLDERDISABLEDCOLOR: TColor = clBtnFace;
{$J-}
  LM_LSBASE = LM_USER + 1024;
  LM_LSPLACEHOLDERMOUSEACT = LM_LSBASE + 1;
  LM_LSLISTFREEEDIT = LM_LSBASE + 2;
  LSEDITBUTTONPREFIX = 'Button';
  LSEDITUPDOWNPREFIX = 'UpDown';
  LSEXPANDPANELTOPPANELPREFIX = 'Panel';
  LSGADGETEDITDESCRIPTIONPREFIX = 'Description';
  LSNUMERICEDITDEFAULTDISPLAYFORMAT = ',0.00;-,0.00';
  LSEXPANDPANELCOLLAPSELRSNAME = 'LSExpandPanelCollapse';
  LSEXPANDPANELEXPANDLRSNAME = 'LSExpandPanelExpand';

type

  { Events }

  TLSValidateEvent = procedure(ASender: TObject;
    var AIsValid: Boolean) of object;

  TLSListBoxMoveItemEvent = procedure(ASender: TObject; AUpEnabled,
    ADownEnabled: Boolean) of object;

  TLSListBoxDragDropItemEvent = procedure(ASender: TObject;
    const AFromPos, AToPos: Integer) of object;

  TLSImageAnimatorEvent = procedure(ASender: TObject;
    var AImageIndex: TImageIndex) of object;

  { TLSLabelType }

  TLSLabelType = (ltLabel, ltEmail, ltURL, ltDocument, ltExecutable);

  { TLSLabelPosition }

  TLSLabelPosition = (lpTopLeft, lpTopCenter, lpTopRight, lpRightTop,
    lpRightCenter, lpRightBottom, lpBottomRight, lpBottomCenter, lpBottomLeft,
    lpLeftBottom, lpLeftCenter, lpLeftTop);

  { TLSNumericEditInputType }

  TLSNumericEditInputType = (itInteger, itFloat);

  { TLSTimeEditDisplayFormat }

  TLSTimeEditDisplayFormat = (dfHM, dfHMS);

  { TLSPaletteSep }

  TLSPaletteSep = class(TComponent, ILSAboutComponent)
  private
    function GetAbout: string;
    procedure SetAbout(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property About: string read GetAbout write SetAbout stored False;
  end;

  { TLSNumericEditFormatStyle }

  TLSNumericEditFormatStyle = (fsFormatFloat, fsFloatToStrF);

  { TLSPlaceHolder }

  TLSPlaceHolder = class(TStaticText)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TLSCustomButton }

  TLSCustomButton = class(TCustomButton, ILSValidationComponent)
  private
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FShortCut: TShortCut;
    FShortCutCanFocus: Boolean;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
  protected
    procedure DoAppKeyPress(ASender: TObject; var AKey: Word;
      AShift: TShiftState); virtual;
    procedure Click; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property ShortCutCanFocus: Boolean read FShortCutCanFocus
      write FShortCutCanFocus default False;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomLabel }

  TLSCustomLabel = class(TCustomLabel, ILSValidationComponent)
  private
    FIsVisitedURL: Boolean;
    FControl: TWinControl;
    FEllipsis: Boolean;
    FHighlightColor: TGraphicsColor;
    FLabelPosition: TLSLabelPosition;
    FLabelSpacing: Integer;
    FLabelType: TLSLabelType;
    FOnValidate: TLSValidateEvent;
    FPath: string;
    FPattern: string;
    FPatterns: TLSRegExList;
    FVisitedURLColor: TColor;
    procedure ControlVisibleChange(Sender: TObject);
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetControl(AValue: TWinControl);
    procedure SetEllipsis(AValue: Boolean);
    procedure SetLabelPosition(AValue: TLSLabelPosition);
    procedure SetLabelSpacing(AValue: Integer);
    procedure SetLabelType(AValue: TLSLabelType);
  protected
    procedure ResetAnchorSideControl;
    procedure UpdateLabelPosition;
    procedure Click; override;
    procedure Loaded; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure FontChanged(ASender: TObject); override;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure UpdateCaption; virtual;
    procedure UpdateHighlightColor; virtual;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure CMMouseEnter(var AMessage: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMessage: TLMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var AMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var AMessage: TLMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetVisitedURL(const AVisited: Boolean);
    procedure Paint; override;
    function Validate: Boolean; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property About: string read GetAbout write SetAbout stored False;
    property Control: TWinControl read FControl write SetControl;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property HighlightColor: TGraphicsColor read FHighlightColor
      write FHighlightColor default $00E63900;
    property LabelPosition: TLSLabelPosition read FLabelPosition
      write SetLabelPosition default lpTopLeft;
    property LabelSpacing: Integer read FLabelSpacing
      write SetLabelSpacing default 3;
    property LabelType: TLSLabelType read FLabelType
      write SetLabelType default ltLabel;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property Path: string read FPath write FPath;
    property VisitedURLColor: TColor read FVisitedURLColor
      write FVisitedURLColor default clPurple;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSEditType }

  TLSEditType = (etEdit, etEmail, etURL, etTel);

  { TLSEditValidationType }

  TLSEditValidationType = (vtEditingDone, vtExit, vtKeyPress);

  { TLSCustomEdit }

  TLSCustomEdit = class(TCustomEdit, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FOldColor: TColor;
    FIsValidationColorShowed: Boolean;
    FIsFocusColorShowed: Boolean;
    FOldPatternChangeHandler: TLSRegExChangeHandler;
    FAutoSkipe: Boolean;
    FPlaceHolderFrame: TLSPlaceHolder;
    FIsValidationHintShowed: Boolean;
    FPlaceHolder: string;
    FPlaceHolderColor: TColor;
    FUTF8Char: TUTF8Char;
    FEditType: TLSEditType;
    FHoldsFocus: Boolean;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FRequired: Boolean;
    FShowValidationHint: Boolean;
    FValidationColor: TColor;
    FValidationType: TLSEditValidationType;
    FValidationHint: string;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetPattern(AValue: string);
    function IsPlaceHolderApplicable: Boolean;
    procedure SetPatterns(const AValue: TLSRegExList);
    procedure SetPlaceHolder(const AValue: string);
    procedure SetPlaceHolderColor(const AValue: TColor);
  protected
    procedure UpdateValidationHintShowed; virtual;
    procedure UpdateValidationColorShowed; virtual;
    procedure UpdateFocusColorShowed; virtual;
    procedure PatternChangeHandler(ASender: TObject;
      var ANewName, ANewDefMsg: string); virtual;
    procedure ShowPlaceHolder; virtual;
    procedure HidePlaceHolder; virtual;
    procedure AdjustPlaceHolderPosition; virtual;
    procedure MouseDownPlaceHolder(ASender: TObject; AButton: TMouseButton;
                AShift: TShiftState; AX, AY: Integer); virtual;
    function InternalValidate(const ACanFocus: Boolean): Boolean; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure CMColorChanged(var AMessage: TLMessage); message CM_COLORCHANGED;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
    procedure UTF8KeyPress(var AUTF8Key: TUTF8Char); override;
    procedure CMVisibleChanged(var AMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var AMessage: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure LMLSPlaceHolderMouseAct(var AMessage: TLMessage); message LM_LSPLACEHOLDERMOUSEACT;
  public
    constructor Create(AOwner: TComponent); override;
    function HandleObjectShouldBeVisible: Boolean; override;
    procedure EditingDone; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AutoSkipe: Boolean read FAutoSkipe write FAutoSkipe default False;
    property EditType: TLSEditType read FEditType write FEditType default etEdit;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HoldsFocus: Boolean read FHoldsFocus write FHoldsFocus default True;
    property Pattern: string read FPattern write SetPattern;
    property Patterns: TLSRegExList read FPatterns write SetPatterns;
    property PlaceHolder: string read FPlaceHolder write SetPlaceHolder;
    property PlaceHolderColor: TColor read FPlaceHolderColor
      write SetPlaceHolderColor default clBtnShadow;
    property Required: Boolean read FRequired write FRequired default False;
    property ShowValidationHint: Boolean read FShowValidationHint
      write FShowValidationHint default True;
    property ValidationColor: TColor read FValidationColor
      write FValidationColor default clNone;
    property ValidationType: TLSEditValidationType read FValidationType
      write FValidationType default vtEditingDone;
    property ValidationHint: string read FValidationHint write FValidationHint;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomMemo }

  TLSCustomMemo = class(TCustomMemo, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FOldColor: TColor;
    FIsFocusColorShowed: Boolean;
    FIsValidationColorShowed: Boolean;
    FOldPatternChangeHandler: TLSRegExChangeHandler;
    FAutoSkipe: Boolean;
    FPlaceHolderFrame: TLSPlaceHolder;
    FIsValidationHintShowed: Boolean;
    FPlaceHolder: string;
    FPlaceHolderColor: TColor;
    FUTF8Char: TUTF8Char;
    FEditType: TLSEditType;
    FHoldsFocus: Boolean;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FRequired: Boolean;
    FShowValidationHint: Boolean;
    FValidationColor: TColor;
    FValidationType: TLSEditValidationType;
    FValidationHint: string;
    function IsPlaceHolderApplicable: Boolean;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetPattern(AValue: string);
    procedure SetPatterns(const AValue: TLSRegExList);
    procedure SetPlaceHolder(const AValue: string);
    procedure SetPlaceHolderColor(const AValue: TColor);
  protected
    procedure UpdateValidationHintShowed; virtual;
    procedure UpdateValidationColorShowed; virtual;
    procedure UpdateFocusColorShowed; virtual;
    procedure PatternChangeHandler(ASender: TObject;
      var ANewName, ANewDefMsg: string); virtual;
    procedure ShowPlaceHolder; virtual;
    procedure HidePlaceHolder; virtual;
    procedure AdjustPlaceHolderPosition; virtual;
    procedure MouseDownPlaceHolder(ASender: TObject; AButton: TMouseButton;
                AShift: TShiftState; AX, AY: Integer); virtual;
    function InternalValidate(const ACanFocus: Boolean): Boolean; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure ParentFormHandleInitialized; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure CMColorChanged(var AMessage: TLMessage); message CM_COLORCHANGED;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
    procedure UTF8KeyPress(var AUTF8Key: TUTF8Char); override;
    procedure CMVisibleChanged(var AMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var AMessage: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure LMLSPlaceHolderMouseAct(var AMessage: TLMessage); message LM_LSPLACEHOLDERMOUSEACT;
  public
    constructor Create(AOwner: TComponent); override;
    function HandleObjectShouldBeVisible: Boolean; override;
    procedure EditingDone; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AutoSkipe: Boolean read FAutoSkipe write FAutoSkipe default False;
    property EditType: TLSEditType read FEditType write FEditType default etEdit;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HoldsFocus: Boolean read FHoldsFocus write FHoldsFocus default True;
    property Pattern: string read FPattern write SetPattern;
    property Patterns: TLSRegExList read FPatterns write SetPatterns;
    property PlaceHolder: string read FPlaceHolder write SetPlaceHolder;
    property PlaceHolderColor: TColor read FPlaceHolderColor
      write SetPlaceHolderColor default clBtnShadow;
    property Required: Boolean read FRequired write FRequired default False;
    property ShowValidationHint: Boolean read FShowValidationHint
      write FShowValidationHint default True;
    property ValidationColor: TColor read FValidationColor
      write FValidationColor default clNone;
    property ValidationType: TLSEditValidationType read FValidationType
      write FValidationType default vtEditingDone;
    property ValidationHint: string read FValidationHint write FValidationHint;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomCheckBox }

  TLSCustomCheckBox = class(TCustomCheckBox, ILSValidationComponent)
  private
    FUnValidationState: TCheckBoxState;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FTextChangedState: TCheckBoxState;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
  protected
    procedure DoClickOnChange; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property UnValidationState: TCheckBoxState read FUnValidationState
      write FUnValidationState default cbGrayed;
    property TextChangedState: TCheckBoxState read FTextChangedState
      write FTextChangedState default cbGrayed;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomListBox }

  TLSCustomListBox = class(TCustomListBox, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FIsItemEditing: Boolean;
    FIsFocusColorShowed: Boolean;
    FItemEditable: Boolean;
    FOldColor: TColor;
    FOldItemIndex: Integer;
    FEdit: TEdit;
    FJSONObject: TJSONObject;
    FClickedItem: Integer;
    FHintWindow: THintWindow;
    FHintForLongItems: Boolean;
    FOnDragDropItem: TLSListBoxDragDropItemEvent;
    FAlternateColor: TColor;
    FDragDropItem: Boolean;
    FMenuStyle: Boolean;
    FOnMoveItem: TLSListBoxMoveItemEvent;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    function GetItem: TJSONObject;
    function GetJSONObject: TJSONObject;
    function GetAbout: string;
    function GetValue: Variant;
    procedure SetAbout(AValue: string);
    procedure SetAlternateColor(AValue: TColor);
    procedure SetDragDropItem(AValue: Boolean);
    procedure SetItemEditable(const AValue: Boolean);
    procedure LMListFreeEdit(var AMessage: TLMessage); message LM_LSLISTFREEEDIT;
    procedure SetValue(const AValue: Variant);
  protected
    procedure Loaded; override;
    procedure DoEditKeyDown(ASender: TObject; var AKey: Word;
      AShift: TShiftState); virtual;
{$IFDEF LCLGtk2}
    procedure DoEditExit(ASender: TObject);
{$ENDIF}
    procedure DoSelectionChange(AUser: Boolean); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
{$IFDEF MSWINDOWS}
    procedure DrawBox(ANewPos: Integer); virtual;
    procedure DragCanceled; override;
{$ENDIF}
    procedure DragOver(ASource: TObject; AX, AY: Integer; AState: TDragState;
       var AAccept: Boolean); override;
    procedure DrawItem(AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure MouseDown(AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer); override;
    procedure WMMouseMove(var AMessage: TLMMouseMove); message LM_MOUSEMOVE;
    procedure CMMouseLeave(var AMessage: TLMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
    procedure UpdateFocusColorShowed; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DefaultHandler(var AMessage); override;
    procedure LoadJSON(const AJSON: TJSONStringType);
    procedure ItemEdit; virtual;
    procedure ItemEditingDone(const AApplyChanges: Boolean = True); virtual;
    procedure Click; override;
    procedure DragDrop(ASource: TObject; AX, AY: Integer); override;
    function Find(const AText: string; const ACaseSensitive: Boolean = False;
      const AFindNext: Boolean = True): Integer; virtual;
    procedure Clear; override;
    procedure Delete(const AIndex: Integer); virtual;
    procedure DoOnMoveItem; virtual;
    procedure MoveItemToUp; virtual;
    procedure MoveItemToDown; virtual;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AlternateColor: TColor read FAlternateColor
      write SetAlternateColor default clWindow;
    property DragDropItem: Boolean read FDragDropItem write
      SetDragDropItem default False;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HintForLongItems: Boolean read FHintForLongItems
      write FHintForLongItems default False;
    property Item: TJSONObject read GetItem default nil;
    property ItemEditable: Boolean read FItemEditable
      write SetItemEditable default False;
    property MenuStyle: Boolean read FMenuStyle write FMenuStyle default False;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property Value: Variant read GetValue write SetValue;
    property OnDragDropItem: TLSListBoxDragDropItemEvent read FOnDragDropItem
      write FOnDragDropItem;
    property OnMoveItem: TLSListBoxMoveItemEvent read FOnMoveItem
      write FOnMoveItem;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomCheckListBox }

  TLSCustomCheckListBox = class(TCustomCheckListBox, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FIsItemEditing: Boolean;
    FItemEditable: Boolean;
    FOldItemIndex: Integer;
    FIsFocusColorShowed: Boolean;
    FEdit: TEdit;
    FJSONObject: TJSONObject;
    FClickedItem: Integer;
    FHintWindow: THintWindow;
    FHintForLongItems: Boolean;
    FOnDragDropItem: TLSListBoxDragDropItemEvent;
    FOldColor: TColor;
    FAlternateColor: TColor;
    FDragDropItem: Boolean;
    FMenuStyle: Boolean;
    FOnMoveItem: TLSListBoxMoveItemEvent;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    function GetItem: TJSONObject;
    function GetJSONObject: TJSONObject;
    function GetAbout: string;
    function GetValue: Variant;
    procedure SetAbout(AValue: string);
    procedure SetAlternateColor(AValue: TColor);
    procedure SetDragDropItem(AValue: Boolean);
    procedure SetItemEditable(const AValue: Boolean);
    procedure LMListFreeEdit(var AMessage: TLMessage); message LM_LSLISTFREEEDIT;
    procedure SetValue(const AValue: Variant);
  protected
    procedure Loaded; override;
    procedure DoEditKeyDown(ASender: TObject; var AKey: Word;
      AShift: TShiftState); virtual;
{$IFDEF LCLGtk2}
    procedure DoEditExit(ASender: TObject);
{$ENDIF}
    procedure DoSelectionChange(AUser: Boolean); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
{$IFDEF MSWINDOWS}
    procedure DrawBox(ANewPos: Integer); virtual;
    procedure DragCanceled; override;
{$ENDIF}
    procedure DragOver(ASource: TObject; AX, AY: Integer; AState: TDragState;
       var AAccept: Boolean); override;
    procedure DrawItem(AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState); override;
    procedure UpdateFocusColorShowed; virtual;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure MouseDown(AButton: TMouseButton;
      AShift: TShiftState; AX, AY: Integer); override;
    procedure WMMouseMove(var AMessage: TLMMouseMove); message LM_MOUSEMOVE;
    procedure CMMouseLeave(var AMessage: TLMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure LoadJSON(const AJSON: TJSONStringType);
    procedure ItemEdit; virtual;
    procedure ItemEditingDone(const AApplyChanges: Boolean = True); virtual;
    procedure CheckAll;
    procedure UnCheckAll;
    procedure ReverseCheck;
    procedure EnableAll;
    procedure DisableAll;
    procedure ReverseEnabled;
    procedure DefaultHandler(var AMessage); override;
    procedure Click; override;
    procedure DragDrop(ASource: TObject; AX, AY: Integer); override;
    function Find(const AText: string; const ACaseSensitive: Boolean = False;
      const AFindNext: Boolean = True): Integer; virtual;
    function CheckedCount: Integer; virtual;
    function EnabledCount: Integer; virtual;
    procedure Clear; override;
    procedure Delete(const AIndex: Integer); virtual;
    procedure DoOnMoveItem; virtual;
    procedure MoveItemToUp; virtual;
    procedure MoveItemToDown; virtual;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AlternateColor: TColor read FAlternateColor
      write SetAlternateColor default clWindow;
    property DragDropItem: Boolean read FDragDropItem write
      SetDragDropItem default False;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HintForLongItems: Boolean read FHintForLongItems
      write FHintForLongItems default False;
    property Item: TJSONObject read GetItem default nil;
    property ItemEditable: Boolean read FItemEditable
      write SetItemEditable default False;
    property MenuStyle: Boolean read FMenuStyle write FMenuStyle default False;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property Value: Variant read GetValue write SetValue;
    property OnDragDropItem: TLSListBoxDragDropItemEvent read FOnDragDropItem
      write FOnDragDropItem;
    property OnMoveItem: TLSListBoxMoveItemEvent read FOnMoveItem
      write FOnMoveItem;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomComboBox }

  TLSCustomComboBox = class(TCustomComboBox, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FJSONObject: TJSONObject;
    FOldColor: TColor;
    FIsFocusColorShowed: Boolean;
    FIsValidationColorShowed: Boolean;
    FOldPatternChangeHandler: TLSRegExChangeHandler;
    FAutoSkipe: Boolean;
    FIsValidationHintShowed: Boolean;
    FUTF8Char: TUTF8Char;
    FEditType: TLSEditType;
    FHoldsFocus: Boolean;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FRequired: Boolean;
    FShowValidationHint: Boolean;
    FValidationColor: TColor;
    FValidationType: TLSEditValidationType;
    FValidationHint: string;
    function GetItem: TJSONData;
    function GetJSONObject: TJSONObject;
    function GetAbout: string;
    function GetValue: Variant;
    procedure SetAbout(AValue: string);
    procedure SetPattern(AValue: string);
    procedure SetPatterns(const AValue: TLSRegExList);
    procedure SetValue(const AValue: Variant);
  protected
    procedure UpdateValidationHintShowed; virtual;
    procedure UpdateValidationColorShowed; virtual;
    procedure UpdateFocusColorShowed; virtual;
    procedure PatternChangeHandler(ASender: TObject;
      var ANewName, ANewDefMsg: string); virtual;
    function InternalValidate(const ACanFocus: Boolean): Boolean; virtual;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
    procedure UTF8KeyPress(var AUTF8Key: TUTF8Char); override;
    procedure CMColorChanged(var AMessage: TLMessage); message CM_COLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function HandleObjectShouldBeVisible: Boolean; override;
    procedure EditingDone; override;
    property Item: TJSONData read GetItem default nil;
    procedure LoadJSON(const AJSON: TJSONStringType);
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AutoSkipe: Boolean read FAutoSkipe write FAutoSkipe default False;
    property EditType: TLSEditType read FEditType write FEditType default etEdit;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HoldsFocus: Boolean read FHoldsFocus write FHoldsFocus default True;
    property Pattern: string read FPattern write SetPattern;
    property Patterns: TLSRegExList read FPatterns write SetPatterns;
    property Required: Boolean read FRequired write FRequired default False;
    property ShowValidationHint: Boolean read FShowValidationHint
      write FShowValidationHint default True;
    property ValidationColor: TColor read FValidationColor
      write FValidationColor default clNone;
    property ValidationType: TLSEditValidationType read FValidationType
      write FValidationType default vtEditingDone;
    property ValidationHint: string read FValidationHint write FValidationHint;
    property Value: Variant read GetValue write SetValue;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomMaskEdit }

  TLSCustomMaskEdit = class(TCustomMaskEdit, ILSValidationComponent)
  private
    FFocusColor: TColor;
    FOldColor: TColor;
    FIsValidationColorShowed: Boolean;
    FOldPatternChangeHandler: TLSRegExChangeHandler;
    FAutoSkipe: Boolean;
    FPlaceHolderFrame: TLSPlaceHolder;
    FIsFocusColorShowed: Boolean;
    FIsValidationHintShowed: Boolean;
    FPlaceHolder: string;
    FPlaceHolderColor: TColor;
    FUTF8Char: TUTF8Char;
    FEditType: TLSEditType;
    FHoldsFocus: Boolean;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FRequired: Boolean;
    FShowValidationHint: Boolean;
    FValidationColor: TColor;
    FValidationType: TLSEditValidationType;
    FValidationHint: string;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetPattern(AValue: string);
    function IsPlaceHolderApplicable: Boolean;
    procedure SetPatterns(const AValue: TLSRegExList);
    procedure SetPlaceHolder(const AValue: string);
    procedure SetPlaceHolderColor(const AValue: TColor);
  protected
    procedure UpdateValidationHintShowed; virtual;
    procedure UpdateValidationColorShowed; virtual;
    procedure UpdateFocusColorShowed; virtual;
    procedure PatternChangeHandler(ASender: TObject;
      var ANewName, ANewDefMsg: string); virtual;
    procedure ShowPlaceHolder; virtual;
    procedure HidePlaceHolder; virtual;
    procedure AdjustPlaceHolderPosition; virtual;
    procedure MouseDownPlaceHolder(ASender: TObject; AButton: TMouseButton;
                AShift: TShiftState; AX, AY: Integer); virtual;
    function InternalValidate(const ACanFocus: Boolean): Boolean; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyUp(var AKey: Word; AShift: TShiftState); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure CMColorChanged(var AMessage: TLMessage); message CM_COLORCHANGED;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
    procedure UTF8KeyPress(var AUTF8Key: TUTF8Char); override;
    procedure CMVisibleChanged(var AMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var AMessage: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure LMLSPlaceHolderMouseAct(var AMessage: TLMessage); message LM_LSPLACEHOLDERMOUSEACT;
  public
    constructor Create(AOwner: TComponent); override;
    function HandleObjectShouldBeVisible: Boolean; override;
    procedure EditingDone; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property AutoSkipe: Boolean read FAutoSkipe write FAutoSkipe default False;
    property EditType: TLSEditType read FEditType write FEditType default etEdit;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property HoldsFocus: Boolean read FHoldsFocus write FHoldsFocus default True;
    property Pattern: string read FPattern write SetPattern;
    property Patterns: TLSRegExList read FPatterns write SetPatterns;
    property PlaceHolder: string read FPlaceHolder write SetPlaceHolder;
    property PlaceHolderColor: TColor read FPlaceHolderColor
      write SetPlaceHolderColor default clBtnShadow;
    property Required: Boolean read FRequired write FRequired default False;
    property ShowValidationHint: Boolean read FShowValidationHint
      write FShowValidationHint default True;
    property ValidationColor: TColor read FValidationColor
      write FValidationColor default clNone;
    property ValidationType: TLSEditValidationType read FValidationType
      write FValidationType default vtEditingDone;
    property ValidationHint: string read FValidationHint write FValidationHint;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomPanel }

  TLSCustomPanel = class(TCustomPanel, ILSValidationComponent)
  private
    FGradientStart: TColor;
    FGradientStop: TColor;
    FGradientDirection: TGradientDirection;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetGradientStart(const AValue: TColor);
    procedure SetGradientStop(const AValue: TColor);
    procedure SetGradientDirection(const AValue: TGradientDirection);
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property GradientStart: TColor read FGradientStart
      write SetGradientStart default clNone;
    property GradientStop: TColor read FGradientStop
      write SetGradientStop default clNone;
    property GradientDirection: TGradientDirection read FGradientDirection
      write SetGradientDirection default gdVertical;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomBitBtn }

  TLSCustomBitBtn = class(TCustomBitBtn, ILSValidationComponent)
  private
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    FShortCut: TShortCut;
    FShortCutCanFocus: Boolean;
    procedure ImageListChange(ASender: TObject);
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetImages(const AValue: TCustomImageList);
  protected
    procedure UpdateGlyph; virtual;
    procedure DoAppKeyPress(ASender: TObject; var AKey: Word;
      AShift: TShiftState); virtual;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property ImageIndex: TImageIndex read FImageIndex write
      SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property ShortCutCanFocus: Boolean read FShortCutCanFocus
      write FShortCutCanFocus default False;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomSpeedButton }

  TLSCustomSpeedButton = class(TCustomSpeedButton, ILSValidationComponent)
  private
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    procedure ImageListChange(ASender: TObject);
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetImages(const AValue: TCustomImageList);
  protected
    procedure UpdateGlyph; virtual;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property ImageIndex: TImageIndex read FImageIndex write
      SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSCustomImageAnimator }

  TLSCustomImageAnimator = class(TComponent, ILSAboutComponent)
  private
    FAutoAdjust: Boolean;
    FImageIndex: Integer;
    FAnimationHandler: TLSImageAnimatorEvent;
    FAnimator: TTimer;
    FActive: Boolean;
    FInterval: Integer;
    FStart: Integer;
    FStop: Integer;
    FOnAnimation: TLSImageAnimatorEvent;
    procedure DoAnimation(ASender: TObject);
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetActive(const AValue: Boolean);
    procedure SetInterval(const AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property About: string read GetAbout write SetAbout stored False;
    property Active: Boolean read FActive write SetActive default False;
    property AutoAdjust: Boolean read FAutoAdjust write FAutoAdjust default True;
    property Interval: Integer read FInterval
      write SetInterval default 1000;
    property Start: Integer read FStart write FStart default 0;
    property Stop: Integer read FStop write FStop default 0;
    property AnimationHandler: TLSImageAnimatorEvent read FAnimationHandler
      write FAnimationHandler;
    property OnAnimation: TLSImageAnimatorEvent read FOnAnimation
      write FOnAnimation;
  end;

  { TLSCustomImage }

  TLSCustomImage = class(TCustomImage, ILSValidationComponent)
  private
    FOldAnimationHandler: TLSImageAnimatorEvent;
    FAnimator: TLSCustomImageAnimator;
    FEmptyColor: TColor;
    FEmptyStyle: TBrushStyle;
    FPNGTemp: TPortableNetworkGraphic;
    FAlignment: TAlignment;
    FImageChangeLink: TChangeLink;
    FImageIndex: TImageIndex;
    FImages: TCustomImageList;
    FOnValidate: TLSValidateEvent;
    FPattern: string;
    FPatterns: TLSRegExList;
    procedure ImageListChange(ASender: TObject);
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetAnimator(const AValue: TLSCustomImageAnimator);
    procedure SetEmptyColor(const AValue: TColor);
    procedure SetEmptyStyle(const AValue: TBrushStyle);
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetImages(const AValue: TCustomImageList);
  protected
    procedure DoAnimationHandler(ASender: TObject;
      var AImageIndex: TImageIndex); virtual;
    procedure UpdatePicture; virtual;
    procedure UpdateAnimator; virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var AMessage: TLMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure LoadFromBase64Stream(var AStream: TStream);
    procedure LoadFromBase64File(const AFileName: TFileName);
    procedure LoadFromBase64String(const AString: string);
    procedure SaveToBase64Stream(var AStream: TStream);
    procedure SaveToBase64File(const AFileName: TFileName);
    procedure SaveToBase64String(out AString: string);
    function AsBase64: string;
    function IsEmpty: Boolean;
    function Validate: Boolean; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property Alignment: TAlignment read FAlignment
      write SetAlignment default taCenter;
    property Animator: TLSCustomImageAnimator read FAnimator write SetAnimator;
    property EmptyColor: TColor read FEmptyColor
      write SetEmptyColor default clYellow;
    property EmptyStyle: TBrushStyle read FEmptyStyle
      write SetEmptyStyle default bsDiagCross;
    property ImageIndex: TImageIndex read FImageIndex write
      SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property Pattern: string read FPattern write FPattern;
    property Patterns: TLSRegExList read FPatterns write FPatterns;
    property OnValidate: TLSValidateEvent read FOnValidate write FOnValidate;
  end;

  { TLSGadgetEditButton }

  TLSGadgetEditButton = class(TLSCustomBitBtn)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property BidiMode;
    property Cancel;
    property Color;
    property Default;
    property Font;
    property Glyph;
    property GlyphShowMode;
    property Height stored False;
    property ImageIndex default -1;
    property Images;
    property Kind;
    property Layout;
    property Left stored False;
    property Margin;
    property ModalResult;
    property Name stored False;
    property NumGlyphs;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShowHint;
    property ShortCut;
    property ShortCutCanFocus default False;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Top stored False;
    property Width stored False;
  end;

  { TLSGadgetEditCaption }

  TLSGadgetEditCaption = class(TLSCustomLabel)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment;
    property BidiMode;
    property Color default clSkyBlue;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Ellipsis default False;
    property Font;
    property Height stored False;
    property HighlightColor default $00E63900;
    property LabelType;
    property Layout;
    property Left stored False;
    property Name stored False;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Path;
    property Pattern;
    property Patterns;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Top stored False;
    property Width;
    property WordWrap;
    property OptimalFill;
  end;

  { TLSEditBtnButton }

  TLSEditBtnButton = class(TLSCustomSpeedButton)
  protected
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property BidiMode;
    property Color;
    property Font;
    property Flat;
    property Glyph;
    property Height stored False;
    property ImageIndex default -1;
    property Images;
    property Layout;
    property Left stored False;
    property Margin;
    property Name stored False;
    property NumGlyphs;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShowHint;
    property Spacing;
    property Top stored False;
    property Visible;
    property Width stored False;
  end;

  { TLSCustomGadgetEdit }

  TLSCustomGadgetEdit = class(TLSCustomEdit, ILSValidationComponent)
  private
    FButton: TLSGadgetEditButton;
    FDescription: TLSGadgetEditCaption;
    FOnButtonClick: TNotifyEvent;
    function GetButtonCaption: TCaption;
    function GetButtonHint: TTranslateString;
    function GetDescriptionCaption: TCaption;
    procedure SetButtonCaption(const AValue: TCaption);
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetDescriptionCaption(const AValue: TCaption);
  protected
    procedure Loaded; override;
    procedure DoButtonDescriptionPosition; virtual;
    procedure DoButtonClick(ASender: TObject); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetName(const AValue: TComponentName); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button: TLSGadgetEditButton read FButton default nil;
    property ButtonCaption: TCaption read GetButtonCaption write SetButtonCaption;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property Description: TLSGadgetEditCaption read FDescription default nil;
    property DescriptionCaption: TCaption read GetDescriptionCaption
      write SetDescriptionCaption;
    property OnButtonClick: TNotifyEvent read FOnButtonClick
      write FOnButtonClick;
  end;

  { TLSCustomEditButton }

  TLSCustomEditButton = class(TLSCustomMaskEdit, ILSValidationComponent)
  private
    FAutoButtonClick: Boolean;
    FButton: TLSEditBtnButton;
    FButtonOnlyWhenFocused: Boolean;
    FOnButtonClick: TNotifyEvent;
    procedure CheckButtonVisible;
    function GetButtonCaption: TCaption;
    function GetButtonHint: TTranslateString;
    procedure SetButtonCaption(const AValue: TCaption);
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonOnlyWhenFocused(const AValue: Boolean);
  protected
    procedure Loaded; override;
    procedure DoButtonPosition; virtual;
    procedure DoButtonClick(ASender: TObject); virtual;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetName(const AValue: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure WMSetFocus(var AMessage: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoButtonClick: Boolean read FAutoButtonClick
      write FAutoButtonClick default False;
    property Button: TLSEditBtnButton read FButton default nil;
    property ButtonCaption: TCaption read GetButtonCaption write SetButtonCaption;
    property ButtonOnlyWhenFocused: Boolean read FButtonOnlyWhenFocused write
      SetButtonOnlyWhenFocused default False;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property OnButtonClick: TNotifyEvent read FOnButtonClick
      write FOnButtonClick;
  end;

  { TLSCustomNumericEdit }

  TLSCustomNumericEdit = class(TLSCustomEdit, ILSValidationComponent)
  private
    FDigits: Integer;
    FFloatFormat: TFloatFormat;
    FFormatStyle: TLSNumericEditFormatStyle;
    FIsInitializing: Boolean;
{$IF DEFINED(LCLGtk2) OR DEFINED(MSWINDOWS)}
    FIsEntering: Boolean;
{$ENDIF}
    FIsUpdating: Boolean;
    FAsInteger: LongInt;
    FDisplayFormat: string;
    FMaxValue: Extended;
    FMinValue: Extended;
    FInputType: TLSNumericEditInputType;
    FPrecision: Integer;
    FRound: Boolean;
    FRoundingDigits: Integer;
    FRoundingMode: TFPURoundingMode;
    FValue: Extended;
    procedure SetDigits(const AValue: Integer);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetFloatFormat(const AValue: TFloatFormat);
    procedure SetFormatStyle(const AValue: TLSNumericEditFormatStyle);
    procedure SetInputType(const AValue: TLSNumericEditInputType);
    procedure SetMaxValue(const AValue: Extended);
    procedure SetMinValue(const AValue: Extended);
    procedure SetPrecision(const AValue: Integer);
    procedure SetRound(const AValue: Boolean);
    procedure SetRoundingDigits(const AValue: Integer);
    procedure SetRoundingMode(const AValue: TFPURoundingMode);
    procedure SetValue(const AValue: Extended);
  protected
    function UpdateRound: Boolean; virtual;
    procedure Loaded; override;
    procedure KeyPress(var AKey: Char); override;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
    procedure CMEnter(var AMessage: TLMessage); message CM_ENTER;
    procedure CMExit(var AMessage: TLMessage); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
    procedure Clear;
    function DefaultDisplayFormat: ShortString; virtual;
    procedure UpdateText; virtual;
    procedure CheckMinMaxValue;
    property AsInteger: LongInt read FAsInteger;
    property Value: Extended read FValue write SetValue;
    property Digits: Integer read FDigits write SetDigits default 2;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property FormatStyle: TLSNumericEditFormatStyle read FFormatStyle
      write SetFormatStyle default fsFormatFloat;
    property FloatFormat: TFloatFormat read FFloatFormat
      write SetFloatFormat default ffFixed;
    property InputType: TLSNumericEditInputType read FInputType
      write SetInputType default itFloat;
    property MinValue: Extended read FMinValue write SetMinValue;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property Precision: Integer read FPrecision write SetPrecision default 20;
    property Round: Boolean read FRound write SetRound default False;
    property RoundingDigits: Integer read FRoundingDigits
      write SetRoundingDigits default -2;
    property RoundingMode: TFPURoundingMode read FRoundingMode
      write SetRoundingMode default rmNearest;
  end;

  { TLSCustomCurrencyEdit }

  TLSCustomCurrencyEdit = class(TLSCustomNumericEdit, ILSValidationComponent)
  public
    function DefaultDisplayFormat: ShortString; override;
  end;

  { TLSCustomTrayIcon }

  TLSCustomTrayIcon = class(TTrayIcon, ILSAboutComponent)
  private
    FOldIcon: TIcon;
    FNotifierOSImage: TPicture;
    FOnNotifierOSClick: TNotifyEvent;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
  protected
{$IFNDEF LCLQt}
    procedure Loaded; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RestoreOldIcon; virtual;
    procedure MakeOldIcon; virtual;
    procedure HideMainForm; virtual;
    procedure RestoreMainForm; virtual;
    class procedure ShowNotifierOS(const ATitle, AMsg: string;
      const AGraphic: TGraphic; const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const AColor: TColor = clWhite;
      const ABorderColor: TColor = clSkyBlue;
      const ATextColor: TColor = clWindowText; const AClick: TNotifyEvent = nil);
    class procedure ShowNotifierOS(var ATheme: TGraphic;
      const ATitle, AMsg: string; const AGraphic: TGraphic;
      const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const ATextColor: TColor = clWindowText;
      const AClick: TNotifyEvent = nil);
    procedure ShowNotifierOS(const ATitle, AMsg: string;
      const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const AColor: TColor = clWhite;
      const ABorderColor: TColor = clSkyBlue;
      const ATextColor: TColor = clWindowText);
    procedure ShowNotifierOS(var ATheme: TGraphic; const ATitle, AMsg: string;
      const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const ATextColor: TColor = clWindowText);
    property About: string read GetAbout write SetAbout stored False;
    property NotifierOSImage: TPicture read FNotifierOSImage
      write FNotifierOSImage;
    property OnNotifierOSClick: TNotifyEvent read FOnNotifierOSClick
      write FOnNotifierOSClick;
  end;

  { TLSCustomDateEdit }

  TLSCustomDateEdit = class(TLSCustomEditButton, ILSValidationComponent)
  private
    FCalendarCloseWithDblClick: Boolean;
    FCalendarShowBtns: Boolean;
    FCalendarShowBtnsCaptions: Boolean;
    FCalendarShowBtnsGlyphs: Boolean;
    FDefaultToday: Boolean;
    FCalendarDisplaySettings: TDisplaySettings;
    FIsInitializing: Boolean;
    FIsUpdating: Boolean;
    FDisplayFormat: string;
    FMaxValue: TDateTime;
    FMinValue: TDateTime;
    FValue: TDateTime;
    FValidDateSeparator: Char;
    procedure SetDisplayFormat(const AValue: string);
    procedure SetMaxValue(const AValue: TDateTime);
    procedure SetMinValue(const AValue: TDateTime);
    procedure SetValue(const AValue: TDateTime);
  protected
    procedure DoCalendarPopupClose(ASender: TObject;
      const ADateTime: TDateTime); virtual;
    procedure DoButtonClick(ASender: TObject); override;
    procedure Loaded; override;
    procedure KeyPress(var AKey: Char); override;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
    procedure Clear;
    procedure UpdateText; virtual;
    procedure CheckMinMaxValue;
    property CalendarCloseWithDblClick: Boolean read FCalendarCloseWithDblClick
      write FCalendarCloseWithDblClick default True;
    property CalendarDisplaySettings: TDisplaySettings
      read FCalendarDisplaySettings write FCalendarDisplaySettings
      default [dsShowHeadings, dsShowDayNames];
    property CalendarShowBtns: Boolean read FCalendarShowBtns
      write FCalendarShowBtns default True;
    property CalendarShowBtnsCaptions: Boolean read FCalendarShowBtnsCaptions
      write FCalendarShowBtnsCaptions default True;
    property CalendarShowBtnsGlyphs: Boolean read FCalendarShowBtnsGlyphs
      write FCalendarShowBtnsGlyphs default True;
    property DefaultToday: Boolean read FDefaultToday
      write FDefaultToday default False;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property MinValue: TDateTime read FMinValue write SetMinValue;
    property MaxValue: TDateTime read FMaxValue write SetMaxValue;
    property ValidDateSeparator: Char read FValidDateSeparator
      write FValidDateSeparator;
    property Value: TDateTime read FValue write SetValue;
  end;

  { TLSTimeEditUpDown }

  TLSTimeEditUpDown = class(TCustomUpDown)
  protected
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Anchors;
    property ArrowKeys;
    property BorderSpacing;
    property Enabled;
    property Height stored False;
    property Hint;
    property Left stored False;
    property Constraints;
    property Name stored False;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Top stored False;
    property Visible;
    property Width stored False;
    property Wrap;
    property OnChanging;
    property OnClick;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TLSCustomTimeEdit }

  TLSCustomTimeEdit = class(TLSCustomMaskEdit, ILSValidationComponent)
  private
    FOnDownClick: TNotifyEvent;
    FOnUpClick: TNotifyEvent;
    FUpDown: TLSTimeEditUpDown;
    FUpDownOnlyWhenFocused: Boolean;
    FDefaultNow: Boolean;
    FIsInitializing: Boolean;
    FIsUpdating: Boolean;
    FDisplayFormat: TLSTimeEditDisplayFormat;
    FMaxValue: TDateTime;
    FMinValue: TDateTime;
    FValue: TDateTime;
    procedure CheckUpDownVisible;
    procedure SetUpDownOnlyWhenFocused(const AValue: Boolean);
    procedure SetDisplayFormat(const AValue: TLSTimeEditDisplayFormat);
    procedure SetMaxValue(const AValue: TDateTime);
    procedure SetMinValue(const AValue: TDateTime);
    procedure SetValue(const AValue: TDateTime);
  protected
    procedure Loaded; override;
    procedure DoChangeValue(const AValue: SmallInt); virtual;
    procedure DoNextPriorKeyDown(ASender: TObject; var AKey: Word); virtual;
    procedure DoUpDownClick(ASender: TObject; AButton: TUDBtnType); virtual;
    procedure DoUpDownPosition; virtual;
    function DoMouseWheel(AShift: TShiftState; AWheelDelta: Integer;
       AMousePos: TPoint): Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetName(const AValue: TComponentName); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure KeyPress(var AKey: Char); override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure WMSetFocus(var AMessage: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TLMKillFocus); message LM_KILLFOCUS;
    procedure CMTextChanged(var AMessage: TLMessage); message CM_TEXTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    procedure Clear;
    procedure UpdateText; virtual;
    procedure CheckMinMaxValue;
    property DefaultNow: Boolean read FDefaultNow
      write FDefaultNow default False;
    property DisplayFormat: TLSTimeEditDisplayFormat read FDisplayFormat
      write SetDisplayFormat default dfHM;
    property MinValue: TDateTime read FMinValue write SetMinValue;
    property MaxValue: TDateTime read FMaxValue write SetMaxValue;
    property UpDown: TLSTimeEditUpDown read FUpDown default nil;
    property UpDownOnlyWhenFocused: Boolean read FUpDownOnlyWhenFocused write
      SetUpDownOnlyWhenFocused default False;
    property Value: TDateTime read FValue write SetValue;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

  { TLSTopPanel }

  TLSTopPanel = class(TLSCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property GradientStart default clWhite;
    property GradientStop default $00EADA99;
    property GradientDirection default gdVertical;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUnDock;
  end;

  { TLSCustomExpandPanel }

  TLSCustomExpandPanel = class(TLSCustomPanel, ILSValidationComponent)
  private
    FOnButtonClick: TNotifyEvent;
    FTopPanel: TLSTopPanel;
    FOldTopPanelHeight: Integer;
    FButton: TImage;
    FExpand: Boolean;
    FExpandPic: TPicture;
    FCollapsePic: TPicture;
    function IsStoredOldTopPanelHeight: Boolean;
    procedure SetExpand(const AValue: Boolean);
  protected
    procedure UpdateSize(const AExpanded: Boolean);
    procedure DoChangePic(ASender: TObject); virtual;
    procedure DoButtonClick(ASender: TObject); virtual;
    class function GetControlClassDefaultSize: TSize; override;
    procedure SetName(const AValue: TComponentName); override;
    procedure WMSize(var AMessage: TLMSize); message LM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Expand: Boolean read FExpand write SetExpand default True;
    property ExpandPic: TPicture read FExpandPic write FExpandPic;
    property CollapsePic: TPicture read FCollapsePic write FCollapsePic;
    property TopPanel: TLSTopPanel read FTopPanel default nil;
    property OldTopPanelHeight: Integer read FOldTopPanelHeight
      write FOldTopPanelHeight stored IsStoredOldTopPanelHeight;
    property OnButtonClick: TNotifyEvent read FOnButtonClick
      write FOnButtonClick;
  end;

  { TLSButton }

  TLSButton = class(TLSCustomButton)
  published
    property About stored False;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShortCut;
    property ShortCutCanFocus default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  { TLSLabel }

  TLSLabel = class(TLSCustomLabel)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Color;
    property Control;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Ellipsis default False;
    property Enabled;
    property FocusControl;
    property Font;
    property HighlightColor default $00E63900;
    property LabelPosition default lpTopLeft;
    property LabelSpacing default 3;
    property LabelType;
    property Layout;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Path;
    property Pattern;
    property Patterns;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property VisitedURLColor default clPurple;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnChangeBounds;
    property OnContextPopup;
    property OnValidate;
    property OnResize;
    property OnStartDrag;
    property OptimalFill;
  end;

  { TLSEdit }

  TLSEdit = class(TLSCustomEdit)
  published
    property About stored False;
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property AutoSkipe default False;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property EditType default etEdit;
    property Enabled;
    property FocusColor default clNone;
    property HoldsFocus default True;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property Pattern;
    property Patterns;
    property ReadOnly;
    property Required default False;
    property ShowHint;
    property ShowValidationHint default True;
    property TabStop;
    property TabOrder;
    property Text;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Visible;
  end;

  { TLSMemo }

  TLSMemo = class(TLSCustomMemo)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSkipe default False;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditType default etEdit;
    property Enabled;
    property HoldsFocus default True;
    property FocusColor default clNone;
    property Font;
    property HideSelection;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentBidiMode;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property ReadOnly;
    property Required default False;
    property ScrollBars;
    property ShowHint;
    property ShowValidationHint default True;
    property TabOrder;
    property TabStop;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
  end;

  { TLSCheckBox }

  TLSCheckBox = class(TLSCustomCheckBox)
  published
    property About stored False;
    property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property UnValidationState default cbGrayed;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property TextChangedState default cbGrayed;
    property Visible;
  end;

  { TLSListBox }

  TLSListBox = class(TLSCustomListBox)
  published
    property About stored False;
    property Align;
    property AlternateColor default clWindow;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property ClickOnSelChange;
    property Color;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragDropItem default False;
    property DragKind;
    property DragMode;
    property ExtendedSelect;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HintForLongItems default False;
    property IntegralHeight;
    property Items;
    property ItemEditable default False;
    property ItemHeight;
    property MenuStyle default False;
    property MultiSelect;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragDropItem;
    property OnDragOver;
    property OnDrawItem;
    property OnEnter;
    property OnEndDrag;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMoveItem;
    property OnResize;
    property OnSelectionChange;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentBidiMode;
    property ParentColor;
    property ParentShowHint;
    property ParentFont;
    property Pattern;
    property Patterns;
    property PopupMenu;
{$IFDEF LSNEWFPC}
    property ScrollWidth;
{$ENDIF}
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;

  { TLSCheckListBox }

  TLSCheckListBox = class(TLSCustomCheckListBox)
  published
    property About stored False;
    property Align;
    property AllowGrayed;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragDropItem default False;
    property DragMode;
    property ExtendedSelect;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HintForLongItems default False;
    property IntegralHeight;
    property Items;
    property ItemEditable default False;
    property ItemHeight;
    property MenuStyle default False;
    property MultiSelect;
    property OnChangeBounds;
    property OnClick;
    property OnClickCheck;
    property OnContextPopup;
    property OnDblClick;
    property OnDrawItem;
    property OnDragDrop;
    property OnDragDropItem;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnItemClick;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMoveItem;
    property OnResize;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;

  { TLSComboBox }

  TLSComboBox = class(TLSCustomComboBox)
  published
    property About stored False;
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property AutoSkipe default False;
    property BidiMode;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property EditType default etEdit;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HoldsFocus default True;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ReadOnly;
    property Required default False;
    property ShowHint;
    property ShowValidationHint default True;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Visible;
  end;

  { TLSMaskEdit }

  TLSMaskEdit = class(TLSCustomMaskEdit)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSkipe default False;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditType default etEdit;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HoldsFocus default True;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property EditMask;
    property Pattern;
    property Patterns;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Text;
    property Required default False;
    property SpaceChar;
    property ShowValidationHint default True;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
  end;

  { TLSPanel }

  TLSPanel = class(TLSCustomPanel)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property GradientStart default clNone;
    property GradientStop default clNone;
    property GradientDirection default gdVertical;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  { TLSBitBtn }

  TLSBitBtn = class(TLSCustomBitBtn)
  published
    property About stored False;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphShowMode;
    property ImageIndex default -1;
    property Images;
    property Kind;
    property Layout;
    property Margin;
    property ModalResult;
    property NumGlyphs;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
    property ShowHint;
    property ShortCut;
    property ShortCutCanFocus default False;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  { TLSSpeedButton }

  TLSSpeedButton = class(TLSCustomSpeedButton)
  published
    property About stored False;
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GroupIndex;
    property ImageIndex default -1;
    property Images;
    property Layout;
    property Margin;
    property NumGlyphs;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property OnValidate;
    property ShowCaption;
    property ShowHint;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property PopupMenu;
  end;

  { TLSImageAnimator }

  TLSImageAnimator = class(TLSCustomImageAnimator)
  published
    property About stored False;
    property AutoAdjust default True;
    property Active default False;
    property Interval default 1000;
    property Start default 0;
    property Stop default 0;
    property OnAnimation;
  end;

  { TLSImage }

  TLSImage = class(TLSCustomImage)
  published
    property About stored False;
    property Alignment default taCenter;
    property Animator;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property Center;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EmptyColor default clYellow;
    property EmptyStyle default bsDiagCross;
    property Font;
    property ImageIndex default -1;
    property Images;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPictureChanged;
    property OnResize;
    property OnStartDrag;
    property OnValidate;
    property ParentShowHint;
    property Pattern;
    property Patterns;
    property Picture;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property Visible;
  end;

  { TLSGadgetEdit }

  TLSGadgetEdit = class(TLSCustomGadgetEdit)
  published
    property About stored False;
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property AutoSkipe default False;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property Button;
    property ButtonCaption;
    property ButtonHint;
    property CharCase;
    property Color;
    property Constraints;
    property Description;
    property DescriptionCaption;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property EditType default etEdit;
    property Enabled;
    property FocusColor default clNone;
    property HoldsFocus default True;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property OnButtonClick;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property Pattern;
    property Patterns;
    property ReadOnly;
    property Required default False;
    property ShowHint;
    property ShowValidationHint default True;
    property TabStop;
    property TabOrder;
    property Text;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Visible;
  end;

  { TLSEditButton }

  TLSEditButton = class(TLSCustomEditButton)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoButtonClick default False;
    property AutoSelect;
    property AutoSkipe default False;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Button;
    property ButtonCaption;
    property ButtonOnlyWhenFocused default False;
    property ButtonHint;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditType default etEdit;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HoldsFocus default True;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property EditMask;
    property Pattern;
    property Patterns;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Text;
    property Required default False;
    property SpaceChar;
    property ShowValidationHint default True;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
  end;

  { TLSNumericEdit }

  TLSNumericEdit = class(TLSCustomNumericEdit)
  published
    property About stored False;
    property Action;
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property AutoSkipe default False;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property Digits default 2;
    property DisplayFormat;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property FocusColor default clNone;
    property HoldsFocus default True;
    property Font;
    property FormatStyle default fsFormatFloat;
    property FloatFormat default ffFixed;
    property HideSelection;
    property InputType default itFloat;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentBidiMode;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Precision default 20;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property Pattern;
    property Patterns;
    property ReadOnly;
    property Required default False;
    property Round default False;
    property RoundingDigits default -2;
    property RoundingMode default rmNearest;
    property ShowHint;
    property ShowValidationHint default True;
    property TabStop;
    property TabOrder;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Value;
    property Visible;
  end;

  { TLSCurrencyEdit }

  TLSCurrencyEdit = class(TLSCustomCurrencyEdit)
  published
    property About stored False;
    property Action;
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property AutoSkipe default False;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property Digits default 2;
    property DisplayFormat;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property HoldsFocus default True;
    property FocusColor default clNone;
    property Font;
    property FormatStyle default fsFormatFloat;
    property FloatFormat default ffFixed;
    property HideSelection;
    property InputType default itFloat;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentBidiMode;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Precision default 20;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property Pattern;
    property Patterns;
    property ReadOnly;
    property Required default False;
    property Round default False;
    property RoundingDigits default -2;
    property RoundingMode default rmNearest;
    property ShowHint;
    property ShowValidationHint default True;
    property TabStop;
    property TabOrder;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Value;
    property Visible;
  end;

  { TLSTrayIcon }

  TLSTrayIcon = class(TLSCustomTrayIcon)
  published
    property About stored False;
    property NotifierOSImage;
    property OnNotifierOSClick;
  end;

  { TLSDateEdit }

  TLSDateEdit = class(TLSCustomDateEdit)
  published
    property About stored False;
    property Align;
    property Anchors;
    property AutoButtonClick default False;
    property AutoSelect;
    property AutoSkipe default False;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Button;
    property ButtonCaption;
    property ButtonOnlyWhenFocused default False;
    property ButtonHint;
    property CalendarCloseWithDblClick default True;
    property CalendarDisplaySettings default [dsShowHeadings, dsShowDayNames];
    property CalendarShowBtns default True;
    property CalendarShowBtnsCaptions default True;
    property CalendarShowBtnsGlyphs default True;
    property CharCase;
    property Color;
    property Constraints;
    property DefaultToday default False;
    property DisplayFormat;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HoldsFocus default True;
    property MinValue;
    property MaxValue;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValidDateSeparator;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnValidate;
    property EditMask;
    property Pattern;
    property Patterns;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Required default False;
    property SpaceChar;
    property ShowValidationHint default True;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Value;
  end;

  { TLSTimeEdit }

  TLSTimeEdit = class(TLSCustomTimeEdit)
  published
    property About stored False;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSkipe default False;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DefaultNow default False;
    property DisplayFormat default dfHM;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusColor default clNone;
    property Font;
    property HoldsFocus default True;
    property MaxLength;
    property MinValue;
    property MaxValue;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDownClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick;
    property OnUTF8KeyPress;
    property OnValidate;
    property Pattern;
    property Patterns;
    property PlaceHolder;
    property PlaceHolderColor default clBtnShadow;
    property Required default False;
    property ShowValidationHint default True;
    property UpDown;
    property UpDownOnlyWhenFocused default False;
    property ValidationColor default clNone;
    property ValidationType default vtEditingDone;
    property ValidationHint;
    property Value;
  end;

  { TLSExpandPanel }

  TLSExpandPanel = class(TLSCustomExpandPanel)
  published
    property About stored False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property CollapsePic;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Expand default True;
    property ExpandPic;
    property Font;
    property FullRepaint;
    property GradientStart default $00EADA99;
    property GradientStop default clWhite;
    property GradientDirection default gdVertical;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TopPanel default nil;
    property UseDockManager default True;
    property Visible;
    property OldTopPanelHeight stored True;
    property OnClick;
    property OnButtonClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ Calls the "Validate" method of all controls of LazSolutions. }
function LSValidateAllControls(const AControl: TControl = nil;
  const AHighlightEdges: Boolean = True;
  const AHighlightColor: TColor = clRed;
  const ABreakOnNonValidation: Boolean = True): Boolean;
{ Validate string with pattern. }
function LSValidatePattern(const AString, APattern: string;
  const APatterns: TLSRegExList): Boolean;

implementation

function LSValidateAllControls(const AControl: TControl;
  const AHighlightEdges: Boolean; const AHighlightColor: TColor;
  const ABreakOnNonValidation: Boolean): Boolean;
var
  VRect: TRect;
  VControl: TWinControl;
  VComponent: TComponent;
  VCanvas: TControlCanvas;
begin
  VCanvas := TControlCanvas.Create;
  try
    if not Assigned(AControl) then
      VComponent := Screen.ActiveControl;
    for VComponent in AControl do
      if Supports(VComponent, ILSValidationComponent) and
        (VComponent is TWinControl) then
      begin
        VControl := VComponent as TWinControl;
        VControl.Repaint;
        Result := (VComponent as ILSValidationComponent).Validate;
        if not Result then
        begin
          VCanvas.Control := VControl;
          if VControl.CanFocus then
            VControl.SetFocus;
          if VControl.CanFocus and AHighlightEdges then
          begin
            VRect := VControl.ClientRect;
{$IFDEF LSNEWFPC}
            VCanvas.Frame3D(VRect, AHighlightColor, AHighlightColor, 1);
{$ELSE}
            VCanvas.Brush.Color := AHighlightColor;
            VCanvas.FrameRect(VRect);
{$ENDIF}
          end;
          if ABreakOnNonValidation then
            Break;
        end;
      end;
  finally
    VCanvas.Free;
  end;
end;

function LSValidatePattern(const AString, APattern: string;
  const APatterns: TLSRegExList): Boolean;
var
  VRegEx: TLSRegEx;
begin
  Result := True;
  if APattern <> '' then
  begin
    if Assigned(APatterns) then
    begin
      VRegEx := APatterns.FindByName(APattern);
      if Assigned(VRegEx) then
      begin
        if VRegEx.Active then
          Result := VRegEx.Execute(AString) <> ''
        else
          Result := True;
      end;
    end
    else
      Result := LSExtractStringUsingRegEx(AString, APattern) <> '';
  end;
end;

{ TLSPaletteSep }

function TLSPaletteSep.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSPaletteSep.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

constructor TLSPaletteSep.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.MessageBox(
    PChar('Palette Separator - Only for use on Component Palette'),
    PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
end;

{ TLSPlaceHolder }

constructor TLSPlaceHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := True;
  Cursor := crIBeam;
  ParentBiDiMode := False;
  ParentColor := False;
  ParentFont := False;
  TabStop := False;
  Transparent := False;
  ShowAccelChar := False;
  ShowHint := False;
end;

destructor TLSPlaceHolder.Destroy;
begin
  OnMouseDown := nil;
  inherited Destroy;
end;

{ TLSCustomButton }

constructor TLSCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.AddOnKeyDownHandler(@DoAppKeyPress);
  FShortCutCanFocus := False;
end;

destructor TLSCustomButton.Destroy;
begin
  Application.RemoveOnKeyDownHandler(@DoAppKeyPress);
  inherited Destroy;
end;

function TLSCustomButton.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

function TLSCustomButton.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomButton.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

{$HINTS OFF}
procedure TLSCustomButton.DoAppKeyPress(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  if FShortCut <> KeyToShortCut(AKey, AShift) then
    Exit;
  if FShortCutCanFocus then
  begin
    if CanFocus then
      Click;
  end
  else
    if Enabled and Visible then
      Click;
end;
{$HINTS ON}

procedure TLSCustomButton.Click;
begin
  if Validate then
    inherited;
end;

{ TLSCustomLabel }

constructor TLSCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHighlightColor := $00E63900;
  FEllipsis := False;
  FLabelPosition := lpTopLeft;
  FLabelSpacing := 3;
  FLabelType := ltLabel;
  FIsVisitedURL := False;
  FVisitedURLColor := clPurple;
end;

destructor TLSCustomLabel.Destroy;
begin
  SetControl(nil);
  inherited Destroy;
end;

procedure TLSCustomLabel.SetVisitedURL(const AVisited: Boolean);
begin
  FIsVisitedURL := AVisited;
  UpdateHighlightColor;
end;

function TLSCustomLabel.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomLabel.SetLabelType(AValue: TLSLabelType);
begin
  if FLabelType <> AValue then
  begin
    FLabelType := AValue;
    if AValue <> ltLabel then
      SetCursor(crHandPoint);
    UpdateHighlightColor;
    UpdateCaption;
  end;
end;

{$HINTS OFF}
procedure TLSCustomLabel.Paint;
var
  VRect: TRect;
  VLabelText: string;
  VUseEllipsis: Boolean;
  VOldFontColor: TColor;
  VTextStyle: TTextStyle;
  VTextLeft, VTextTop: Integer;
begin
  VUseEllipsis := FEllipsis and ((FLabelType = ltLabel) or
    (FLabelType = ltEmail) or (FLabelType = ltURL));
  VRect := Rect(0, 0, Width, Height);
  with Canvas do
  begin
    if Enabled then
      Brush.Color := Color
    else
      Brush.Color := clNone;
    Font := Font;
    if (Color <> clNone) and not Transparent then
    begin
      Brush.Style := bsSolid;
      FillRect(VRect);
    end
    else
      Brush.Style := bsClear;
    FillChar(VTextStyle, SizeOf(VTextStyle), 0);
    with VTextStyle do
    begin
      Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
      WordBreak := WordWrap;
      SingleLine := not WordWrap and not HasMultiLine;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
      SystemFont := False;
      RightToLeft := UseRightToLeftReading;
      ExpandTabs := True;
    end;
    DoMeasureTextPosition(VTextTop, VTextLeft);
    VLabelText := GetLabelText;
    VOldFontColor := Font.Color;
    if not Enabled then
    begin
      Font.Color := clBtnHighlight;
      if not VUseEllipsis then
        TextRect(VRect, VTextLeft + 1, VTextTop + 1, VLabelText, VTextStyle);
      Font.Color := clBtnShadow;
    end;
    if VUseEllipsis then
      DrawText(Canvas.Handle, PChar(VLabelText), Length(VLabelText), VRect,
        DT_END_ELLIPSIS or DT_MODIFYSTRING or DT_NOPREFIX)
    else
      TextRect(VRect, VTextLeft, VTextTop, VLabelText, VTextStyle);
    Font.Color := VOldFontColor;
  end;
end;
{$HINTS ON}

procedure TLSCustomLabel.ResetAnchorSideControl;
begin
  if AnchorSideLeft.Control = FControl then
    AnchorSideLeft.Control := nil;
  if AnchorSideTop.Control = FControl then
    AnchorSideTop.Control := nil;
  if AnchorSideRight.Control = FControl then
    AnchorSideRight.Control := nil;
  if AnchorSideBottom.Control = FControl then
    AnchorSideBottom.Control := nil;
end;

procedure TLSCustomLabel.UpdateLabelPosition;
begin
  if not Assigned(FControl) then
    Exit;
  try
    if Assigned(FControl.Parent) then
      FControl.Parent.DisableAlign;
    ResetAnchorSideControl;
    Anchors := [akLeft, akTop];
    AnchorSideLeft.Side := asrLeft;
    AnchorSideTop.Side := asrLeft;
    AnchorSideRight.Side := asrLeft;
    AnchorSideBottom.Side := asrLeft;
    BorderSpacing.Left := 0;
    BorderSpacing.Top := 0;
    BorderSpacing.Right := 0;
    BorderSpacing.Bottom := 0;
    Parent := FControl.Parent;
    case FLabelPosition of
      lpTopLeft:
        begin
          Anchors := [akLeft, akBottom];
          AnchorSideLeft.Control := FControl;
          AnchorSideBottom.Control := FControl;
          BorderSpacing.Bottom := FLabelSpacing;
        end;
      lpTopCenter:
        begin
          Anchors := [akLeft, akBottom];
          AnchorSideLeft.Control := FControl;
          AnchorSideLeft.Side := asrCenter;
          AnchorSideBottom.Control := FControl;
          BorderSpacing.Bottom := FLabelSpacing;
        end;
      lpTopRight:
        begin
          Anchors := [akRight, akBottom];
          AnchorSideRight.Control := FControl;
          AnchorSideRight.Side := asrRight;
          AnchorSideBottom.Control := FControl;
          BorderSpacing.Bottom := FLabelSpacing;
        end;
      lpRightTop:
        begin
          AnchorSideLeft.Control := FControl;
          AnchorSideLeft.Side := asrRight;
          AnchorSideTop.Control := FControl;
          BorderSpacing.Left := FLabelSpacing;
        end;
      lpRightCenter:
        begin
          AnchorSideLeft.Control := FControl;
          AnchorSideLeft.Side := asrRight;
          AnchorSideTop.Control := FControl;
          AnchorSideTop.Side := asrCenter;
          BorderSpacing.Left := FLabelSpacing;
        end;
      lpRightBottom:
        begin
          Anchors := [akLeft, akBottom];
          AnchorSideLeft.Control := FControl;
          AnchorSideLeft.Side := asrRight;
          AnchorSideBottom.Control := FControl;
          AnchorSideBottom.Side := asrBottom;
          BorderSpacing.Left := FLabelSpacing;
        end;
      lpBottomRight:
        begin
          Anchors := [akTop, akRight];
          AnchorSideTop.Control := FControl;
          AnchorSideTop.Side := asrRight;
          AnchorSideRight.Control := FControl;
          AnchorSideRight.Side := asrRight;
          BorderSpacing.Top := FLabelSpacing;
        end;
      lpBottomCenter:
        begin
          Anchors := [akTop, akRight];
          AnchorSideTop.Control := FControl;
          AnchorSideTop.Side := asrRight;
          AnchorSideRight.Control := FControl;
          AnchorSideRight.Side := asrCenter;
          BorderSpacing.Top := FLabelSpacing;
        end;
      lpBottomLeft:
        begin
          AnchorSideLeft.Control := FControl;
          AnchorSideTop.Control := FControl;
          AnchorSideTop.Side := asrRight;
          BorderSpacing.Top := FLabelSpacing;
        end;
      lpLeftBottom:
        begin
          Anchors := [akRight, akBottom];
          AnchorSideRight.Control := FControl;
          AnchorSideBottom.Control := FControl;
          AnchorSideBottom.Side := asrBottom;
          BorderSpacing.Right := FLabelSpacing;
        end;
      lpLeftCenter:
        begin
          Anchors := [akRight, akBottom];
          AnchorSideRight.Control := FControl;
          AnchorSideBottom.Control := FControl;
          AnchorSideBottom.Side := asrCenter;
          BorderSpacing.Right := FLabelSpacing;
        end;
      lpLeftTop:
        begin
          Anchors := [akTop, akRight];
          AnchorSideTop.Control := FControl;
          AnchorSideRight.Control := FControl;
          BorderSpacing.Right := FLabelSpacing;
        end;
    end;
  finally
    if Assigned(FControl.Parent) then
      FControl.Parent.EnableAlign;
  end;
end;

procedure TLSCustomLabel.UpdateCaption;
begin
  if FEllipsis and (FLabelType = ltDocument) then
    Caption := LSShortenPathName(ClientRect, Font, Text);
end;

procedure TLSCustomLabel.UpdateHighlightColor;
begin
  if FLabelType <> ltLabel then
  begin
    if FIsVisitedURL then
      Font.Color := FVisitedURLColor
    else
      Font.Color := FHighlightColor;
  end;
end;

procedure TLSCustomLabel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) then
  begin
    if AComponent = FPatterns then
      Patterns := nil;
    if AComponent = FControl then
      SetControl(nil);
  end;
end;

procedure TLSCustomLabel.SetEllipsis(AValue: Boolean);
begin
  if AValue <> FEllipsis then
  begin
    FEllipsis := AValue;
    AutoSize := False;
    if AValue and (FLabelType = ltDocument) then
      UpdateCaption;
  end;
end;

procedure TLSCustomLabel.SetLabelPosition(AValue: TLSLabelPosition);
begin
  if AValue <> FLabelPosition then
  begin
    FLabelPosition := AValue;
    UpdateLabelPosition;
  end;
end;

procedure TLSCustomLabel.SetLabelSpacing(AValue: Integer);
begin
  if AValue <> FLabelSpacing then
  begin
    FLabelSpacing := AValue;
    UpdateLabelPosition;
  end;
end;

{$HINTS OFF}
procedure TLSCustomLabel.ControlVisibleChange(Sender: TObject);
begin
  Visible := FControl.Visible;
end;

procedure TLSCustomLabel.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

function TLSCustomLabel.GetAbout: string;
begin
  Result := '';
end;

procedure TLSCustomLabel.SetControl(AValue: TWinControl);
begin
  if Assigned(FControl) then
  begin
    if not Assigned(AValue) then
      ResetAnchorSideControl;
    FControl.RemoveAllHandlersOfObject(Self);
    FControl := nil;
    FocusControl := nil;
    UpdateLabelPosition;
  end;
  if Assigned(AValue) then
  begin
    FControl := AValue;
    FocusControl := FControl;
    UpdateLabelPosition;
    FControl.AddHandlerOnVisibleChanged(@ControlVisibleChange, True);
  end;
end;

procedure TLSCustomLabel.Click;
var
  VPath: string;
begin
  if not Validate then
    Exit;
  if FPath <> '' then
    VPath := FPath
  else
    VPath := FindDefaultExecutablePath(Caption);
  if VPath = '' then
    VPath := Caption;
  case FLabelType of
    ltEmail: OpenURL('mailto:' + Caption);
    ltURL:
    begin
      if FPath <> '' then
        VPath := FPath
      else
        VPath := Caption;
      if Pos('http://', VPath) <> 0 then
        FIsVisitedURL := OpenURL(VPath)
      else
        FIsVisitedURL := OpenURL('http://' + VPath);
      if FIsVisitedURL then
        Font.Color := FVisitedURLColor;
    end;
    ltDocument: OpenDocument(VPath);
    ltExecutable: ExecuteProcess(VPath, '');
  end;
  inherited;
end;

procedure TLSCustomLabel.Loaded;
begin
  inherited Loaded;
  UpdateLabelPosition;
end;

procedure TLSCustomLabel.SetParent(AParent: TWinControl);
begin
  if Assigned(FControl) then
    inherited SetParent(FControl.Parent)
  else
    inherited;
end;

procedure TLSCustomLabel.FontChanged(ASender: TObject);
begin
  UpdateCaption;
  inherited;
end;

procedure TLSCustomLabel.CMMouseEnter(var AMessage: TLMessage);
begin
  UpdateHighlightColor;
  if FLabelType <> ltLabel then
    Font.Style := Font.Style + [fsUnderline];
  inherited;
end;

procedure TLSCustomLabel.CMMouseLeave(var AMessage: TLMessage);
begin
  UpdateHighlightColor;
  if FLabelType <> ltLabel then
    Font.Style := Font.Style - [fsUnderline];
  inherited;
end;

procedure TLSCustomLabel.CMTextChanged(var AMessage: TLMessage);
begin
  UpdateCaption;
  inherited;
end;

procedure TLSCustomLabel.CMVisibleChanged(var AMessage: TLMessage);
begin
  if Assigned(FControl) then
    Visible := FControl.Visible;
  inherited;
end;

procedure TLSCustomLabel.CMEnabledChanged(var AMessage: TLMessage);
begin
  if Assigned(FControl) then
    Enabled := FControl.Enabled;
  inherited;
end;

procedure TLSCustomLabel.SetAutoSize(AValue: Boolean);
begin
  if AValue then
    FEllipsis := False;
  inherited;
end;

procedure TLSCustomLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  UpdateCaption;
  inherited;
end;

{ TLSCustomEdit }

constructor TLSCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSkipe := False;
  FEditType := etEdit;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FIsValidationHintShowed := False;
  FPlaceHolderColor := clBtnShadow;
  FRequired := False;
  FHoldsFocus := True;
  FShowValidationHint := True;
  FValidationColor := clNone;
  FValidationType := vtEditingDone;
end;

procedure TLSCustomEdit.EditingDone;
begin
  if FValidationType = vtEditingDone then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  inherited;
end;

procedure TLSCustomEdit.UTF8KeyPress(var AUTF8Key: TUTF8Char);
begin
  inherited;
  if (FValidationType = vtKeyPress) and not (ssCtrl in GetKeyShiftState) then
  begin
    FUTF8Char := AUTF8Key;
    if (AUTF8Key[1] in [#8, #13, #27]) then
      Exit;
    if not InternalValidate(FHoldsFocus) then
      AUTF8Key := #0;
  end;
end;

procedure TLSCustomEdit.CMVisibleChanged(var AMessage: TLMessage);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Visible := Visible;
  inherited;
end;

procedure TLSCustomEdit.CMEnabledChanged(var AMessage: TLMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (Text = '') and IsPlaceHolderApplicable then
    begin
      FPlaceHolderFrame.Enabled := Enabled;
      if Enabled then
        FPlaceHolderFrame.Color := Color
      else
        FPlaceHolderFrame.Color := DEFAULT_PLACEHOLDERDISABLEDCOLOR;
    end;
  end;
  inherited;
end;

procedure TLSCustomEdit.CMTextChanged(var AMessage: TLMessage);
begin
  if csDesigning in ComponentState then
  begin
    if (FPlaceHolder <> '') and (Text = '') then
      ShowPlaceHolder
    else
      HidePlaceHolder;
  end
  else
    if (FPlaceHolder <> '') and (Length(Text) > 0) then
      HidePlaceHolder;
  inherited;
end;

{$HINTS OFF}
procedure TLSCustomEdit.LMLSPlaceHolderMouseAct(var AMessage: TLMessage);
var
  VPoint: TPoint;
begin
  if (TObject(AMessage.LParam) is TLSPlaceHolder) and CanFocus then
  begin
    SetFocus;
    if TMouseButton(AMessage.WParam) = mbRight then
    begin
      GetCursorPos(VPoint);
      PostMessage(Handle, LM_CONTEXTMENU, Handle,
        MakeLParam(VPoint.X, VPoint.Y));
    end;
  end;
end;
{$HINTS ON}

function TLSCustomEdit.HandleObjectShouldBeVisible: Boolean;
begin
  Result := inherited HandleObjectShouldBeVisible;
  if csDesigning in ComponentState then
    Exit;
  if not Result and not CanFocus then
    UpdateValidationHintShowed;
end;

function TLSCustomEdit.Validate: Boolean;
begin
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  Result := InternalValidate(False);
end;

function TLSCustomEdit.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomEdit.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomEdit.SetPattern(AValue: string);
var
  VRegEx: TLSRegEx;
begin
  if AValue <> FPattern then
  begin
    FPattern := AValue;
    FEditType := etEdit;
    if Assigned(FPatterns) then
    begin
      VRegEx := FPatterns.FindByName(FPattern);
      if Assigned(VRegEx) and (VRegEx.DefaultMessage <> '') then
        FValidationHint := VRegEx.DefaultMessage;
    end;
  end;
end;

function TLSCustomEdit.IsPlaceHolderApplicable: Boolean;
begin
  Result := (FPlaceHolder <> '') and Assigned(FPlaceHolderFrame);
end;

procedure TLSCustomEdit.SetPatterns(const AValue: TLSRegExList);
begin
  if AValue <> FPatterns then
  begin
    FPatterns := AValue;
    if Assigned(FPatterns) then
    begin
      FOldPatternChangeHandler := FPatterns.Items.ChangeHandler;
      FPatterns.Items.ChangeHandler := @PatternChangeHandler;
    end;
  end;
end;

procedure TLSCustomEdit.SetPlaceHolder(const AValue: string);
begin
  if AValue <> FPlaceHolder then
  begin
    FPlaceHolder := AValue;
    if csDesigning in ComponentState then
    begin
      if (AValue <> '') and (Text = '') then
      begin
        ShowPlaceHolder;
        FPlaceHolderFrame.Caption := AValue;
      end
      else
        HidePlaceHolder;
      Exit;
    end;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Caption := AValue;
  end;
end;

procedure TLSCustomEdit.SetPlaceHolderColor(const AValue: TColor);
begin
  if AValue <> FPlaceHolderColor then
  begin
    FPlaceHolderColor := AValue;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Font.Color := AValue;
  end;
end;

procedure TLSCustomEdit.UpdateValidationHintShowed;
begin
  if FIsValidationHintShowed then
  begin
    FIsValidationHintShowed := False;
    LSCloseHint(Self);
  end;
end;

procedure TLSCustomEdit.UpdateValidationColorShowed;
begin
  if FIsValidationColorShowed then
  begin
    FIsValidationColorShowed := False;
    if (FFocusColor <> clNone) and Focused then
      Color := FFocusColor
    else
      Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomEdit.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomEdit.PatternChangeHandler(ASender: TObject; var ANewName,
  ANewDefMsg: string);
begin
  if FPattern = '' then
    Exit;
  if ANewName <> FPattern then
    FPattern := ANewName;
  if ANewDefMsg <> FValidationHint then
    FValidationHint := ANewDefMsg;
  if Assigned(FOldPatternChangeHandler) then
    FOldPatternChangeHandler(ASender, ANewName, ANewDefMsg);
end;

procedure TLSCustomEdit.ShowPlaceHolder;
begin
  if not Assigned(FPlaceHolderFrame) then
    FPlaceHolderFrame := TLSPlaceHolder.Create(Self);
  AdjustPlaceHolderPosition;
  FPlaceHolderFrame.Caption := FPlaceHolder;
  FPlaceHolderFrame.Parent := Parent;
{$IFDEF LCLGtk2}
  if Color <> clDefault then
    FPlaceHolderFrame.Color := Color
  else
    FPlaceHolderFrame.Color := clWindow;
{$ELSE}
  FPlaceHolderFrame.Color := Color;
{$ENDIF}
  FPlaceHolderFrame.Alignment := Alignment;
  FPlaceHolderFrame.Font.Color := FPlaceHolderColor;
  FPlaceHolderFrame.Font.Name := Font.Name;
  FPlaceHolderFrame.Font.Size := Font.Size;
  FPlaceHolderFrame.OnMouseDown := @MouseDownPlaceHolder;
end;

procedure TLSCustomEdit.HidePlaceHolder;
begin
  if Assigned(FPlaceHolderFrame) then
    FreeAndNil(FPlaceHolderFrame);
end;

procedure TLSCustomEdit.AdjustPlaceHolderPosition;
begin
  FPlaceHolderFrame.Anchors := [akLeft, akRight, akTop];
  FPlaceHolderFrame.AnchorSideTop.Control := Self;
  FPlaceHolderFrame.AnchorSideTop.Side := asrCenter;
  FPlaceHolderFrame.AnchorSideLeft.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Side := asrRight;
  FPlaceHolderFrame.BorderSpacing.Left := 5;
  FPlaceHolderFrame.BorderSpacing.Right := 5;
end;

{$HINTS OFF}
procedure TLSCustomEdit.MouseDownPlaceHolder(ASender: TObject;
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  PostMessage(Handle, LM_LSPLACEHOLDERMOUSEACT, PtrInt(AButton),
    PtrInt(ASender));
end;
{$HINTS ON}

function TLSCustomEdit.InternalValidate(const ACanFocus: Boolean): Boolean;

  procedure LaterValidation(const AHint: string);
  begin
    if FValidationColor <> clNone then
    begin
      FIsValidationColorShowed := True;
      if FFocusColor = clNone then
        FOldColor := Color;
      Color := FValidationColor;
    end;
    if CanFocus then
    begin
      if ACanFocus
{$IF DEFINED(LCLQt) OR DEFINED(MSWINDOWS)}and
        (FValidationType <> vtEditingDone)
{$ENDIF}then
        SetFocus;
      if FShowValidationHint then
      begin
        FIsValidationHintShowed := True;
        LSShowHint(Self, AHint);
      end;
    end;
  end;

var
  VString, VValidateMsg: string;
begin
  VString := inherited RealGetText;
  Result := FRequired and (VString = '');
  if Result then
    LaterValidation(SLSControlsRequiredFieldMsg)
  else
  begin
    case FEditType of
      etEdit:
        begin
          if (FValidationType = vtKeyPress) and Focused then
            Result := LSValidatePattern(FUTF8Char, FPattern, FPatterns)
          else
            Result := LSValidatePattern(VString, FPattern, FPatterns);
          VValidateMsg := SLSControlsPatternFieldMsg;
        end;
      etEmail:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractEmailRegEx) <> '');
          VValidateMsg := SLSControlsEmailFieldMsg;
        end;
      etURL:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractURLRegEx) <> '');
          VValidateMsg := SLSControlsURLFieldMsg;
        end;
      etTel:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractTelRegEx) <> '');
          VValidateMsg := SLSControlsTelFieldMsg;
        end;
    end;
    if not Result then
    begin
      if FValidationHint <> '' then
        LaterValidation(FValidationHint)
      else
        LaterValidation(VValidateMsg);
    end;
  end;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomEdit.SetParent(AParent: TWinControl);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Parent := Parent;
  inherited;
end;

procedure TLSCustomEdit.Loaded;
begin
  if (FPlaceHolder <> '') and (Text = '') then
    ShowPlaceHolder;
  inherited;
end;

procedure TLSCustomEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomEdit.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FValidationType <> vtKeyPress then
    Exit;
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  if AKey = VK_TAB then
  begin
    FUTF8Char := Text;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
    Exit;
  end;
  if ((ssCtrl in AShift) and (AKey = VK_V)) then
  begin
    FUTF8Char := Clipboard.AsText;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
  end;
end;

procedure TLSCustomEdit.KeyUp(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FAutoSkipe and (UTF8Length(Text) >= MaxLength) and
    IsEditableTextKey(AKey) and not ((ssAlt in AShift) or (ssCtrl in AShift)) then
    PerformTab(True);
end;

procedure TLSCustomEdit.CMColorChanged(var AMessage: TLMessage);
begin
  if (not FIsValidationColorShowed) and (not FIsFocusColorShowed) then
    FOldColor := Color;
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Color := Color;
  inherited;
end;

procedure TLSCustomEdit.CMEnter(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Text = '') then
    HidePlaceHolder;
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) and
      (not FIsValidationColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomEdit.CMExit(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Text = '') then
    ShowPlaceHolder;
  if (FValidationType = vtExit) or (FValidationType = vtKeyPress) then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  if not FIsValidationColorShowed then
    UpdateFocusColorShowed;
  inherited;
end;

{ TLSCustomMemo }

constructor TLSCustomMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSkipe := False;
  FEditType := etEdit;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FIsValidationHintShowed := False;
  FPlaceHolderColor := clBtnShadow;
  FRequired := False;
  FHoldsFocus := True;
  FShowValidationHint := True;
  FValidationColor := clNone;
  FValidationType := vtEditingDone;
end;

function TLSCustomMemo.HandleObjectShouldBeVisible: Boolean;
begin
  Result := inherited HandleObjectShouldBeVisible;
  if csDesigning in ComponentState then
    Exit;
  if not Result and not CanFocus then
    UpdateValidationHintShowed;
end;

procedure TLSCustomMemo.EditingDone;
begin
  if FValidationType = vtEditingDone then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  inherited;
end;

procedure TLSCustomMemo.UTF8KeyPress(var AUTF8Key: TUTF8Char);
begin
  inherited;
  if (FValidationType = vtKeyPress) and not (ssCtrl in GetKeyShiftState) then
  begin
    FUTF8Char := AUTF8Key;
    if (AUTF8Key[1] in [#8, #13, #27]) then
      Exit;
    if not InternalValidate(FHoldsFocus) then
      AUTF8Key := #0;
  end;
end;

procedure TLSCustomMemo.CMVisibleChanged(var AMessage: TLMessage);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Visible := Visible;
 inherited;
end;

procedure TLSCustomMemo.CMEnabledChanged(var AMessage: TLMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (Lines.Count = 0) and IsPlaceHolderApplicable then
    begin
      FPlaceHolderFrame.Enabled := Enabled;
      if Enabled then
        FPlaceHolderFrame.Color := Color
      else
        FPlaceHolderFrame.Color := DEFAULT_PLACEHOLDERDISABLEDCOLOR;
    end;
  end;
  inherited;
end;

procedure TLSCustomMemo.CMTextChanged(var AMessage: TLMessage);
begin
  if csDesigning in ComponentState then
  begin
    if (FPlaceHolder <> '') and (Lines.Count = 0) then
      ShowPlaceHolder
    else
      HidePlaceHolder;
  end
  else
    if (FPlaceHolder <> '') and (Lines.Count > 0) then
      HidePlaceHolder;
  inherited;
end;

{$HINTS OFF}
procedure TLSCustomMemo.LMLSPlaceHolderMouseAct(var AMessage: TLMessage);
var
  VPoint: TPoint;
begin
  if (TObject(AMessage.LParam) is TLSPlaceHolder) and CanFocus then
  begin
    SetFocus;
    if TMouseButton(AMessage.WParam) = mbRight then
    begin
      GetCursorPos(VPoint);
      PostMessage(Handle, LM_CONTEXTMENU, Handle,
        MakeLParam(VPoint.X, VPoint.Y));
    end;
  end;
end;
{$HINTS ON}

function TLSCustomMemo.Validate: Boolean;
begin
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  Result := InternalValidate(False);
end;

function TLSCustomMemo.IsPlaceHolderApplicable: Boolean;
begin
  Result := (FPlaceHolder <> '') and Assigned(FPlaceHolderFrame);
end;

function TLSCustomMemo.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomMemo.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomMemo.SetPattern(AValue: string);
var
  VRegEx: TLSRegEx;
begin
  if AValue <> FPattern then
  begin
    FPattern := AValue;
    FEditType := etEdit;
    if Assigned(FPatterns) and (FValidationHint = '') then
    begin
      VRegEx := FPatterns.FindByName(FPattern);
      if Assigned(VRegEx) and (VRegEx.DefaultMessage <> '') then
        FValidationHint := VRegEx.DefaultMessage;
    end;
  end;
end;

procedure TLSCustomMemo.SetPatterns(const AValue: TLSRegExList);
begin
  if AValue <> FPatterns then
  begin
    FPatterns := AValue;
    if Assigned(FPatterns) then
    begin
      FOldPatternChangeHandler := FPatterns.Items.ChangeHandler;
      FPatterns.Items.ChangeHandler := @PatternChangeHandler;
    end;
  end;
end;

procedure TLSCustomMemo.SetPlaceHolder(const AValue: string);
begin
  if AValue <> FPlaceHolder then
  begin
    FPlaceHolder := AValue;
    if csDesigning in ComponentState then
    begin
      if (AValue <> '') and (Lines.Count = 0) then
      begin
        ShowPlaceHolder;
        FPlaceHolderFrame.Caption := AValue;
      end
      else
        HidePlaceHolder;
      Exit;
    end;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Caption := AValue;
  end;
end;

procedure TLSCustomMemo.SetPlaceHolderColor(const AValue: TColor);
begin
  if AValue <> FPlaceHolderColor then
  begin
    FPlaceHolderColor := AValue;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Font.Color := AValue;
  end;
end;

procedure TLSCustomMemo.UpdateValidationHintShowed;
begin
  if FIsValidationHintShowed then
  begin
    FIsValidationHintShowed := False;
    LSCloseHint(Self);
  end;
end;

procedure TLSCustomMemo.UpdateValidationColorShowed;
begin
  if FIsValidationColorShowed then
  begin
    FIsValidationColorShowed := False;
    if (FFocusColor <> clNone) and Focused then
      Color := FFocusColor
    else
      Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomMemo.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomMemo.PatternChangeHandler(ASender: TObject; var ANewName,
  ANewDefMsg: string);
begin
  if FPattern = '' then
    Exit;
  if ANewName <> FPattern then
    FPattern := ANewName;
  if ANewDefMsg <> FValidationHint then
    FValidationHint := ANewDefMsg;
  if Assigned(FOldPatternChangeHandler) then
    FOldPatternChangeHandler(ASender, ANewName, ANewDefMsg);
end;

procedure TLSCustomMemo.ShowPlaceHolder;
begin
  if not Assigned(FPlaceHolderFrame) then
    FPlaceHolderFrame := TLSPlaceHolder.Create(Self);
  AdjustPlaceHolderPosition;
  FPlaceHolderFrame.Alignment := Alignment;
  FPlaceHolderFrame.Caption := FPlaceHolder;
  FPlaceHolderFrame.Parent := Parent;
  FPlaceHolderFrame.Color := Color;
  FPlaceHolderFrame.Font.Color := FPlaceHolderColor;
  FPlaceHolderFrame.Font.Name := Font.Name;
  FPlaceHolderFrame.Font.Size := Font.Size;
  FPlaceHolderFrame.OnMouseDown := @MouseDownPlaceHolder;
end;

procedure TLSCustomMemo.HidePlaceHolder;
begin
  if Assigned(FPlaceHolderFrame) then
    FreeAndNil(FPlaceHolderFrame);
end;

procedure TLSCustomMemo.AdjustPlaceHolderPosition;
begin
  FPlaceHolderFrame.Anchors := [akLeft, akRight, akTop];
  FPlaceHolderFrame.AnchorSideTop.Control := Self;
  FPlaceHolderFrame.AnchorSideTop.Side := asrTop;
  FPlaceHolderFrame.AnchorSideLeft.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Side := asrRight;
  FPlaceHolderFrame.BorderSpacing.Left := 5;
  FPlaceHolderFrame.BorderSpacing.Right := 5;
  FPlaceHolderFrame.BorderSpacing.Top := 4;
end;

{$HINTS OFF}
procedure TLSCustomMemo.MouseDownPlaceHolder(ASender: TObject;
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  PostMessage(Handle, LM_LSPLACEHOLDERMOUSEACT, PtrInt(AButton),
    PtrInt(ASender));
end;
{$HINTS ON}

function TLSCustomMemo.InternalValidate(const ACanFocus: Boolean): Boolean;

  procedure LaterValidation(const AHint: string);
  begin
    if FValidationColor <> clNone then
    begin
      FIsValidationColorShowed := True;
      if FFocusColor = clNone then
        FOldColor := Color;
      Color := FValidationColor;
    end;
    if CanFocus then
    begin
      if ACanFocus
{$IF DEFINED(LCLQt) OR DEFINED(MSWINDOWS)}and
        (FValidationType <> vtEditingDone)
{$ENDIF}then
        SetFocus;
      if FShowValidationHint then
      begin
        FIsValidationHintShowed := True;
        LSShowHint(Self, AHint);
      end;
    end;
  end;

var
  VString, VValidateMsg: string;
begin
  VString := inherited RealGetText;
  Result := FRequired and (VString = '');
  if Result then
    LaterValidation(SLSControlsRequiredFieldMsg)
  else
  begin
    case FEditType of
      etEdit:
        begin
          if (FValidationType = vtKeyPress) and Focused then
            Result := LSValidatePattern(FUTF8Char, FPattern, FPatterns)
          else
            Result := LSValidatePattern(VString, FPattern, FPatterns);
          VValidateMsg := SLSControlsPatternFieldMsg;
        end;
      etEmail:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractEmailRegEx) <> '');
          VValidateMsg := SLSControlsEmailFieldMsg;
        end;
      etURL:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractURLRegEx) <> '');
          VValidateMsg := SLSControlsURLFieldMsg;
        end;
      etTel:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractTelRegEx) <> '');
          VValidateMsg := SLSControlsTelFieldMsg;
        end;
    end;
    if not Result then
    begin
      if FValidationHint <> '' then
        LaterValidation(FValidationHint)
      else
        LaterValidation(VValidateMsg);
    end;
  end;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomMemo.SetParent(AParent: TWinControl);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Parent := Parent;
  inherited;
end;

procedure TLSCustomMemo.ParentFormHandleInitialized;
begin
  inherited;
  if (FPlaceHolder <> '') and (Lines.Count = 0) then
    ShowPlaceHolder;
end;

procedure TLSCustomMemo.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomMemo.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FValidationType <> vtKeyPress then
    Exit;
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  if AKey = VK_TAB then
  begin
    FUTF8Char := Text;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
    Exit;
  end;
  if ((ssCtrl in AShift) and (AKey = VK_V)) then
  begin
    FUTF8Char := Clipboard.AsText;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
  end;
end;

procedure TLSCustomMemo.KeyUp(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FAutoSkipe and (UTF8Length(Text) >= MaxLength) and
    IsEditableTextKey(AKey) and not ((ssAlt in AShift) or (ssCtrl in AShift)) then
    PerformTab(True);
end;

procedure TLSCustomMemo.CMColorChanged(var AMessage: TLMessage);
begin
  if (not FIsValidationColorShowed) and (not FIsFocusColorShowed) then
    FOldColor := Color;
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Color := Color;
  inherited;
end;

procedure TLSCustomMemo.CMEnter(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Lines.Count = 0) then
    HidePlaceHolder;
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) and
    (not FIsValidationColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomMemo.CMExit(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Lines.Count = 0) then
    ShowPlaceHolder;
  if (FValidationType = vtExit) or (FValidationType = vtKeyPress) then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  if not FIsValidationColorShowed then
    UpdateFocusColorShowed;
  inherited;
end;

{ TLSCustomCheckBox }

constructor TLSCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csCheckbox;
  TabStop := True;
  AutoSize := True;
  FUnValidationState := cbGrayed;
  FTextChangedState := cbGrayed;
end;

function TLSCustomCheckBox.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

function TLSCustomCheckBox.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomCheckBox.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomCheckBox.DoClickOnChange;
var
  VChecked: Boolean;
begin
  VChecked := Validate;
  ClicksDisabled := not VChecked;
  if not VChecked then
    State := FUnValidationState;
  inherited;
end;

procedure TLSCustomCheckBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomCheckBox.CMTextChanged(var AMessage: TLMessage);
begin
  State := FTextChangedState;
  inherited;
end;

{ TLSCustomListBox }

constructor TLSCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSONObject := nil;
  FAlternateColor := clWindow;
  FClickedItem := -1;
  FDragDropItem := False;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FHintForLongItems := False;
  FItemEditable := False;
  FMenuStyle := False;
end;

destructor TLSCustomListBox.Destroy;
begin
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  inherited Destroy;
end;

procedure TLSCustomListBox.DefaultHandler(var AMessage);
var
  VMsg: TMsg;
begin
  VMsg.Message := TlMessage(AMessage).Msg;
  inherited;
  if FHintForLongItems and Assigned(FHintWindow) and
    (VMsg.Message = 15) { VSCROLL } then
    FHintWindow.ReleaseHandle;
end;

procedure TLSCustomListBox.LoadJSON(const AJSON: TJSONStringType);
begin
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  LSJSONToJSONObjectStrings(FJSONObject, AJSON, Items);
end;

procedure TLSCustomListBox.ItemEditingDone(const AApplyChanges: Boolean);
begin
  FIsItemEditing := False;
  if not Assigned(FEdit) then
    Exit;
  if AApplyChanges then
    Items[FOldItemIndex] := FEdit.Text;
  PostMessage(Handle, LM_LSLISTFREEEDIT, -1, -1);
end;

function TLSCustomListBox.GetJSONObject: TJSONObject;
begin
  if not Assigned(FJSONObject) then
    LSShowJSONEmptyObjectError(ClassName);
  Result := FJSONObject;
end;

function TLSCustomListBox.GetItem: TJSONObject;
begin
  if ItemIndex > -1 then
    Result := TJSONObject(GetJSONObject.Items[ItemIndex])
  else
    Result := nil;
end;

procedure TLSCustomListBox.ItemEdit;
var
  VRect: TRect;
  VIndex: Integer;
begin
  if MultiSelect or not FItemEditable then
    Exit;
  VIndex := ItemIndex;
  if VIndex = -1 then
    Exit;
  FIsItemEditing := True;
  if not Assigned(FEdit) then
    FEdit := TEdit.Create(Self);
  FEdit.Parent := {$IFDEF LCLGtk2}Parent{$ELSE}Self{$ENDIF};
{$IFDEF LCLGtk2}
  FEdit.OnExit := @DoEditExit;
{$ENDIF}
  FEdit.OnKeyDown := @DoEditKeyDown;
  VRect := ItemRect(VIndex);
  FEdit.Top := {$IFDEF LCLGtk2}Top +{$ENDIF}VRect.Top;
  FEdit.Left := {$IFDEF LCLGtk2}Left +{$ENDIF}VRect.Left;
  FEdit.Height := VRect.Bottom - VRect.Top;
  FEdit.Width := VRect.Right;
  FOldItemIndex := VIndex;
  FEdit.Text := Items[VIndex];
  FEdit.SelectAll;
  FEdit.SetFocus;
end;

function TLSCustomListBox.GetAbout: string;
begin
  Result := '';
end;

function TLSCustomListBox.GetValue: Variant;
begin
  Result := GetItem.Value;
end;

{$HINTS OFF}
procedure TLSCustomListBox.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomListBox.SetAlternateColor(AValue: TColor);
begin
  if AValue <> FAlternateColor then
  begin
    FAlternateColor := AValue;
    Invalidate;
    if Style = lbStandard then
      Style := lbOwnerDrawFixed;
  end;
end;

procedure TLSCustomListBox.SetDragDropItem(AValue: Boolean);
begin
  if AValue <> FDragDropItem then
  begin
    FDragDropItem := AValue;
    if AValue then
      DragMode := dmAutomatic
    else
      DragMode := dmManual;
  end;
end;

procedure TLSCustomListBox.Click;
begin
  if Validate then
    inherited;
  DoOnMoveItem;
end;

procedure TLSCustomListBox.DragDrop(ASource: TObject; AX, AY: Integer);
var
  VNewPos: Integer;
begin
  inherited;
  if FDragDropItem then
  begin
    if ASource = Self then
    begin
      VNewPos := ItemAtPos(Point(AX, AY), True);
      Items.Move(ItemIndex, VNewPos);
      if Assigned(FOnDragDropItem) then
        FOnDragDropItem(Self, ItemIndex, Succ(VNewPos));
      ItemIndex := VNewPos;
{$IFDEF MSWINDOWS}
      DrawBox(VNewPos);
{$ENDIF}
    end;
  end;
end;

procedure TLSCustomListBox.DragOver(ASource: TObject; AX, AY: Integer;
  AState: TDragState; var AAccept: Boolean);
var
  VNewPos: Integer;
begin
  inherited;
  if FDragDropItem then
  begin
    AAccept := Self = ASource;
    if not AAccept then
      Exit;
    VNewPos := ItemAtPos(Point(AX, AY), True);
    AAccept := VNewPos <> ItemIndex;
{$IFDEF MSWINDOWS}
    Repaint;
    DrawBox(VNewPos);
{$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}
procedure TLSCustomListBox.DragCanceled;
begin
  Repaint;
  inherited;
end;
{$ENDIF}

procedure TLSCustomListBox.DrawItem(AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  VIsAlternateColor, VIsFocusColor: Boolean;
begin
  VIsAlternateColor := (FAlternateColor <> clWindow) and Odd(AIndex);
  VIsFocusColor := (FFocusColor <> clNone) and Focused;
  if not (odSelected in AState) and (VIsAlternateColor or VIsFocusColor) then
  begin
    Canvas.Brush.Style := bsSolid;
    if VIsAlternateColor then
      Canvas.Brush.Color := FAlternateColor
    else
      if VIsFocusColor then
        Canvas.Brush.Color := FFocusColor;
    Canvas.FillRect(ARect);
{$IFDEF LCLGtk2}
    Canvas.TextOut(ARect.Left + 4, ARect.Top + 3, Items[AIndex]);
{$ENDIF}
  end;
  inherited;
end;

procedure TLSCustomListBox.DoOnMoveItem;
var
  VIndex: Integer;
begin
  if Assigned(FOnMoveItem) then
  begin
    if MultiSelect then
      FOnMoveItem(Self, False, False)
    else
    begin
      VIndex := ItemIndex;
      FOnMoveItem(Self, VIndex > 0, (VIndex > -1) and
        (VIndex < Pred(Items.Count)));
    end;
  end;
end;

procedure TLSCustomListBox.Loaded;
begin
  inherited;
  DoOnMoveItem;
end;

{$HINTS OFF}
procedure TLSCustomListBox.DoEditKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  if AShift = [] then
    case AKey of
      VK_RETURN, VK_UP, VK_DOWN:
        begin
          ItemEditingDone;
          SetFocus;
          ItemIndex := FOldItemIndex;
          AKey := VK_UNKNOWN;
        end;
      VK_ESCAPE:
        begin
          ItemEditingDone(False);
{$IFNDEF LCLGtk2}
          SetFocus;
{$ENDIF}
          ItemIndex := FOldItemIndex;
          AKey := VK_UNKNOWN;
        end;
    end;
end;
{$HINTS ON}

{$IFDEF LCLGtk2}
{$HINTS OFF}
procedure TLSCustomListBox.DoEditExit(ASender: TObject);
begin
  ItemEditingDone;
end;
{$HINTS ON}
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TLSCustomListBox.DrawBox(ANewPos: Integer);
var
  VRect: TRect;
begin
  if ANewPos <> -1 then
  begin
    VRect := ItemRect(ANewPos);
    Canvas.DrawFocusRect(VRect);
  end;
end;
{$ENDIF}

procedure TLSCustomListBox.WMMouseMove(var AMessage: TLMMouseMove);
var
  VText: string;
  VOldFont: TFont;
  VItemIndex: Integer;
  VRect, VItemRect: TRect;
  VItemRectTopLeft: TPoint;
begin
  VItemIndex := GetIndexAtXY(AMessage.XPos, AMessage.YPos);
  if FMenuStyle then
    ItemIndex := VItemIndex;
  if FHintForLongItems and (VItemIndex > -1) and
    (VItemIndex <> FClickedItem) then
  begin
    try
      FClickedItem := -1;
      VOldFont := Canvas.Font;
      Canvas.Font := Font;
      VText := Items[VItemIndex];
      VItemRect := ItemRect(VItemIndex);
      if (Canvas.TextWidth(VText) > ClientWidth) and
        (PtInRect(VItemRect, Point(AMessage.XPos, AMessage.YPos))) then
      begin
        if not Assigned(FHintWindow) then
          FHintWindow := THintWindow.Create(Self);
        FHintWindow.AutoHide := True;
        FHintWindow.Canvas.Font := Font;
        FHintWindow.Canvas.Font.Color := Screen.HintFont.Color;
        VRect := FHintWindow.CalcHintRect(Screen.DesktopWidth, VText, nil);
        VItemRectTopLeft := ClientToScreen(VRect.TopLeft);
        VRect.Left := VItemRect.Left + VItemRectTopLeft.X;
        VRect.Right += VRect.Left;
        VRect.Top := VItemRect.Top + VItemRectTopLeft.Y;
        VRect.Bottom += VRect.Top;
        FHintWindow.ActivateHint(VRect, Items[VItemIndex]);
      end
      else
        if Assigned(FHintWindow) then
          FHintWindow.ReleaseHandle;
    finally
      Canvas.Font := VOldFont;
    end;
  end;
  inherited;
end;

procedure TLSCustomListBox.CMMouseLeave(var AMessage: TLMessage);
begin
  inherited;
  if FHintForLongItems and Assigned(FHintWindow) then
    FHintWindow.ReleaseHandle;
end;

procedure TLSCustomListBox.CMEnter(var AMessage: TLMessage);
begin
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ItemEditingDone;
  inherited;
end;

procedure TLSCustomListBox.CMExit(var AMessage: TLMessage);
begin
{$IFNDEF LCLGtk2}
  ItemEditingDone;
{$ENDIF}
  UpdateFocusColorShowed;
  inherited;
end;

procedure TLSCustomListBox.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomListBox.DoSelectionChange(AUser: Boolean);
begin
  if not AUser then
    ItemEditingDone;
  inherited;
end;

procedure TLSCustomListBox.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if (AShift = []) and (AKey = VK_F2) then
    ItemEdit;
end;

procedure TLSCustomListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomListBox.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
    ItemEditingDone;
  if FHintForLongItems then
  begin
    FClickedItem := ItemIndex;
    if Assigned(FHintWindow) then
      FHintWindow.ReleaseHandle;
  end;
  inherited;
end;

{$HINTS OFF}
procedure TLSCustomListBox.LMListFreeEdit(var AMessage: TLMessage);
begin
  FreeAndNil(FEdit);
end;
{$HINTS ON}

procedure TLSCustomListBox.SetValue(const AValue: Variant);
var
  I: Integer;
  VItems: TJSONObject;
begin
  if Items.Count = 0 then
    Exit;
  VItems := GetJSONObject;
  for I := 0 to Pred(VItems.Count) do
    if VItems.Items[I].AsString = string(AValue) then
    begin
      ItemIndex := I;
      Break;
    end;
end;

procedure TLSCustomListBox.SetItemEditable(const AValue: Boolean);
begin
  if AValue <> FItemEditable then
  begin
    FItemEditable := AValue;
    if not FItemEditable then
      ItemEditingDone;
  end;
end;

function TLSCustomListBox.Find(const AText: string;
  const ACaseSensitive: Boolean; const AFindNext: Boolean): Integer;
begin
  Result := LSFindItemInListBox(TCustomListBox(Self), AText, ACaseSensitive,
    AFindNext);
end;

procedure TLSCustomListBox.Clear;
begin
  inherited Clear;
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  DoOnMoveItem;
end;

procedure TLSCustomListBox.Delete(const AIndex: Integer);
begin
  Items.Delete(AIndex);
  DoOnMoveItem;
end;

procedure TLSCustomListBox.MoveItemToUp;
var
  VIndex, VNewIndex: Integer;
begin
  if MultiSelect or FIsItemEditing then
    Exit;
  VIndex := ItemIndex;
  if VIndex > 0 then
  begin
    VNewIndex := Pred(VIndex);
    Items.Move(VIndex, VNewIndex);
    ItemIndex := VNewIndex;
    DoOnMoveItem;
  end;
end;

procedure TLSCustomListBox.MoveItemToDown;
var
  VIndex, VNewIndex, VCount: Integer;
begin
  if MultiSelect or FIsItemEditing then
    Exit;
  VIndex := ItemIndex;
  VCount := Items.Count;
  if (VIndex > -1) and (VIndex < Pred(VCount)) then
  begin
    VNewIndex := Succ(VIndex);
    Items.Move(VIndex, VNewIndex);
    ItemIndex := VNewIndex;
    DoOnMoveItem;
  end;
end;

function TLSCustomListBox.Validate: Boolean;
begin
  Result := LSValidatePattern(GetSelectedText, FPattern, FPatterns) and not
    FIsItemEditing;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

{ TLSCustomCheckListBox }

constructor TLSCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSONObject := nil;
  FAlternateColor := clWindow;
  FClickedItem := -1;
  FDragDropItem := False;
  FHintForLongItems := False;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FItemEditable := False;
  FMenuStyle := False;
end;

destructor TLSCustomCheckListBox.Destroy;
begin
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  inherited Destroy;
end;

procedure TLSCustomCheckListBox.LoadJSON(const AJSON: TJSONStringType);
begin
  LSJSONToJSONObjectStrings(FJSONObject, AJSON, Items);
end;

procedure TLSCustomCheckListBox.ItemEditingDone(const AApplyChanges: Boolean);
begin
  FIsItemEditing := False;
  if not Assigned(FEdit) then
    Exit;
  if AApplyChanges then
    Items[FOldItemIndex] := FEdit.Text;
  PostMessage(Handle, LM_LSLISTFREEEDIT, -1, -1);
end;

function TLSCustomCheckListBox.GetJSONObject: TJSONObject;
begin
  if not Assigned(FJSONObject) then
    LSShowJSONEmptyObjectError(ClassName);
  Result := FJSONObject;
end;

function TLSCustomCheckListBox.GetItem: TJSONObject;
begin
  if ItemIndex > -1 then
    Result := TJSONObject(GetJSONObject.Items[ItemIndex])
  else
    Result := nil;
end;

procedure TLSCustomCheckListBox.ItemEdit;
var
  VRect: TRect;
  VIndex: Integer;
begin
  if MultiSelect or not FItemEditable then
    Exit;
  VIndex := ItemIndex;
  if VIndex = -1 then
    Exit;
  FIsItemEditing := True;
  if not Assigned(FEdit) then
    FEdit := TEdit.Create(Self);
  FEdit.Parent := {$IFDEF LCLGtk2}Parent{$ELSE}Self{$ENDIF};
{$IFDEF LCLGtk2}
  FEdit.OnExit := @DoEditExit;
{$ENDIF}
  FEdit.OnKeyDown := @DoEditKeyDown;
  VRect := ItemRect(VIndex);
  FEdit.Top := {$IFDEF LCLGtk2}Top +{$ENDIF}VRect.Top;
  FEdit.Left := {$IFDEF LCLGtk2}Left +{$ENDIF}VRect.Left + 18;
  FEdit.Height := VRect.Bottom - VRect.Top;
  FEdit.Width := {$IFDEF LCLGtk2}ClientWidth - 20{$ELSE}VRect.Right - 18{$ENDIF};
  FOldItemIndex := VIndex;
  FEdit.Text := Items[VIndex];
  FEdit.SelectAll;
  FEdit.SetFocus;
end;

procedure TLSCustomCheckListBox.CheckAll;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Checked[I] := True;
end;

procedure TLSCustomCheckListBox.UnCheckAll;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Checked[I] := False;
end;

procedure TLSCustomCheckListBox.ReverseCheck;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Checked[I] := not Checked[I];
end;

procedure TLSCustomCheckListBox.EnableAll;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    ItemEnabled[I] := True;
end;

procedure TLSCustomCheckListBox.DisableAll;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    ItemEnabled[I] := False;
end;

procedure TLSCustomCheckListBox.ReverseEnabled;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    ItemEnabled[I] := not ItemEnabled[I];
end;

procedure TLSCustomCheckListBox.DefaultHandler(var AMessage);
var
  VMsg: TMsg;
begin
  VMsg.Message := TlMessage(AMessage).Msg;
  inherited;
  if FHintForLongItems and Assigned(FHintWindow) and
    (VMsg.Message = 15) { VSCROLL } then
    FHintWindow.ReleaseHandle;
end;

function TLSCustomCheckListBox.GetAbout: string;
begin
  Result := '';
end;

function TLSCustomCheckListBox.GetValue: Variant;
begin
  Result := GetItem.Value;
end;

{$HINTS OFF}
procedure TLSCustomCheckListBox.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomCheckListBox.SetAlternateColor(AValue: TColor);
begin
  if AValue <> FAlternateColor then
  begin
    FAlternateColor := AValue;
    Invalidate;
    if Style = lbStandard then
      Style := lbOwnerDrawFixed;
  end;
end;

procedure TLSCustomCheckListBox.SetDragDropItem(AValue: Boolean);
begin
  if AValue <> FDragDropItem then
  begin
    FDragDropItem := AValue;
    if AValue then
      DragMode := dmAutomatic
    else
      DragMode := dmManual;
  end;
end;

procedure TLSCustomCheckListBox.Click;
begin
  if Validate then
    inherited;
  DoOnMoveItem;
end;

procedure TLSCustomCheckListBox.DragDrop(ASource: TObject; AX, AY: Integer);
var
  VNewPos: Integer;
begin
  inherited;
  if FDragDropItem then
  begin
    if ASource = Self then
    begin
      VNewPos := ItemAtPos(Point(AX, AY), True);
      Items.Move(ItemIndex, VNewPos);
      if Assigned(FOnDragDropItem) then
        FOnDragDropItem(Self, ItemIndex, Succ(VNewPos));
      ItemIndex := VNewPos;
{$IFDEF MSWINDOWS}
      DrawBox(VNewPos);
{$ENDIF}
    end;
  end;
end;

procedure TLSCustomCheckListBox.DragOver(ASource: TObject; AX, AY: Integer;
  AState: TDragState; var AAccept: Boolean);
var
  VNewPos: Integer;
begin
  inherited;
  if FDragDropItem then
  begin
    AAccept := Self = ASource;
    if not AAccept then
      Exit;
    VNewPos := ItemAtPos(Point(AX, AY), True);
    AAccept := VNewPos <> ItemIndex;
{$IFDEF MSWINDOWS}
    Repaint;
    DrawBox(VNewPos);
{$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}
procedure TLSCustomCheckListBox.DragCanceled;
begin
  Repaint;
  inherited;
end;
{$ENDIF}

procedure TLSCustomCheckListBox.DrawItem(AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  VIsAlternateColor, VIsFocusColor: Boolean;
begin
  VIsAlternateColor := (FAlternateColor <> clWindow) and Odd(AIndex);
  VIsFocusColor := (FFocusColor <> clNone) and Focused;
  if not (odSelected in AState) and (VIsAlternateColor or VIsFocusColor) then
  begin
    Canvas.Brush.Style := bsSolid;
    if VIsAlternateColor then
      Canvas.Brush.Color := FAlternateColor
    else
      if VIsFocusColor then
        Canvas.Brush.Color := FFocusColor;
    Canvas.FillRect(ARect);
{$IFDEF LCLGtk2}
    Canvas.TextOut(ARect.Left + 4, ARect.Top + 3, Items[AIndex]);
{$ENDIF}
  end;
  inherited;
end;

procedure TLSCustomCheckListBox.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomCheckListBox.DoOnMoveItem;
var
  VIndex: Integer;
begin
  if Assigned(FOnMoveItem) then
  begin
    if MultiSelect then
      FOnMoveItem(Self, False, False)
    else
    begin
      VIndex := ItemIndex;
      FOnMoveItem(Self, VIndex > 0, (VIndex > -1) and
        (VIndex < Pred(Items.Count)));
    end;
  end;
end;

procedure TLSCustomCheckListBox.Loaded;
begin
  inherited;
  DoOnMoveItem;
end;

{$HINTS OFF}
procedure TLSCustomCheckListBox.DoEditKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  if AShift = [] then
    case AKey of
      VK_RETURN, VK_UP, VK_DOWN:
        begin
          ItemEditingDone;
          SetFocus;
          ItemIndex := FOldItemIndex;
          AKey := VK_UNKNOWN;
        end;
      VK_ESCAPE:
        begin
          ItemEditingDone(False);
{$IFNDEF LCLGtk2}
          SetFocus;
{$ENDIF}
          ItemIndex := FOldItemIndex;
          AKey := VK_UNKNOWN;
        end;
    end;
end;
{$HINTS ON}

{$IFDEF LCLGtk2}
{$HINTS OFF}
procedure TLSCustomCheckListBox.DoEditExit(ASender: TObject);
begin
  ItemEditingDone;
end;
{$HINTS ON}
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TLSCustomCheckListBox.DrawBox(ANewPos: Integer);
var
  VRect: TRect;
begin
  if ANewPos <> -1 then
  begin
    VRect := ItemRect(ANewPos);
    Canvas.DrawFocusRect(VRect);
  end;
end;
{$ENDIF}

procedure TLSCustomCheckListBox.WMMouseMove(var AMessage: TLMMouseMove);
var
  VText: string;
  VOldFont: TFont;
  VItemIndex: Integer;
  VRect, VItemRect: TRect;
  VItemRectTopLeft: TPoint;
begin
  VItemIndex := GetIndexAtXY(AMessage.XPos, AMessage.YPos);
  if FMenuStyle then
    ItemIndex := VItemIndex;
  if FHintForLongItems and (VItemIndex > -1) and
    (VItemIndex <> FClickedItem) then
  begin
    try
      FClickedItem := -1;
      VOldFont := Canvas.Font;
      Canvas.Font := Font;
      VText := Items[VItemIndex];
      VItemRect := ItemRect(VItemIndex);
      if (Canvas.TextWidth(VText) > ClientWidth) and
        (PtInRect(VItemRect, Point(AMessage.XPos, AMessage.YPos))) then
      begin
        if not Assigned(FHintWindow) then
          FHintWindow := THintWindow.Create(Self);
        FHintWindow.AutoHide := True;
        FHintWindow.Canvas.Font := Font;
        FHintWindow.Canvas.Font.Color := Screen.HintFont.Color;
        VRect := FHintWindow.CalcHintRect(Screen.DesktopWidth, VText, nil);
        VItemRectTopLeft := ClientToScreen(VRect.TopLeft);
        VRect.Left := VItemRect.Left + VItemRectTopLeft.X;
        VRect.Right += VRect.Left;
        VRect.Top := VItemRect.Top + VItemRectTopLeft.Y;
        VRect.Bottom += VRect.Top;
        FHintWindow.ActivateHint(VRect, Items[VItemIndex]);
      end
      else
        if Assigned(FHintWindow) then
          FHintWindow.ReleaseHandle;
    finally
      Canvas.Font := VOldFont;
    end;
  end;
  inherited;
end;

procedure TLSCustomCheckListBox.CMMouseLeave(var AMessage: TLMessage);
begin
  inherited;
  if FHintForLongItems and Assigned(FHintWindow) then
    FHintWindow.ReleaseHandle;
end;

procedure TLSCustomCheckListBox.CMEnter(var AMessage: TLMessage);
begin
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomCheckListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ItemEditingDone;
  inherited;
end;

procedure TLSCustomCheckListBox.CMExit(var AMessage: TLMessage);
begin
{$IFNDEF LCLGtk2}
  ItemEditingDone;
{$ENDIF}
  UpdateFocusColorShowed;
  inherited;
end;

procedure TLSCustomCheckListBox.DoSelectionChange(AUser: Boolean);
begin
  if not AUser then
    ItemEditingDone;
  inherited;
end;

procedure TLSCustomCheckListBox.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if (AShift = []) and (AKey = VK_F2) then
    ItemEdit;
end;

procedure TLSCustomCheckListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomCheckListBox.MouseDown(AButton: TMouseButton;
  AShift: TShiftState; AX, AY: Integer);
begin
  if AButton = mbLeft then
    ItemEditingDone;
  if FHintForLongItems then
  begin
    FClickedItem := ItemIndex;
    if Assigned(FHintWindow) then
      FHintWindow.ReleaseHandle;
  end;
  inherited;
end;

{$HINTS OFF}
procedure TLSCustomCheckListBox.LMListFreeEdit(var AMessage: TLMessage);
begin
  FreeAndNil(FEdit);
end;
{$HINTS ON}

procedure TLSCustomCheckListBox.SetValue(const AValue: Variant);
var
  I: Integer;
  VItems: TJSONObject;
begin
  if Items.Count = 0 then
    Exit;
  VItems := GetJSONObject;
  for I := 0 to Pred(VItems.Count) do
    if VItems.Items[I].AsString = string(AValue) then
    begin
      ItemIndex := I;
      Break;
    end;
end;

procedure TLSCustomCheckListBox.SetItemEditable(const AValue: Boolean);
begin
  if AValue <> FItemEditable then
  begin
    FItemEditable := AValue;
    if not FItemEditable then
      ItemEditingDone;
  end;
end;

function TLSCustomCheckListBox.Find(const AText: string;
  const ACaseSensitive: Boolean; const AFindNext: Boolean): Integer;
begin
  Result := LSFindItemInListBox(TCustomListBox(Self), AText, ACaseSensitive,
    AFindNext);
end;

function TLSCustomCheckListBox.CheckedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Items.Count) do
    if Checked[I] then
      Inc(Result);
end;

function TLSCustomCheckListBox.EnabledCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(Items.Count) do
    if ItemEnabled[I] then
      Inc(Result);
end;

procedure TLSCustomCheckListBox.Clear;
begin
  inherited Clear;
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  DoOnMoveItem;
end;

procedure TLSCustomCheckListBox.Delete(const AIndex: Integer);
begin
  Items.Delete(AIndex);
  DoOnMoveItem;
end;

procedure TLSCustomCheckListBox.MoveItemToUp;
var
  VIndex, VNewIndex: Integer;
begin
  if MultiSelect or FIsItemEditing then
    Exit;
  VIndex := ItemIndex;
  if VIndex > 0 then
  begin
    VNewIndex := Pred(VIndex);
    Items.Move(VIndex, VNewIndex);
    ItemIndex := VNewIndex;
    DoOnMoveItem;
  end;
end;

procedure TLSCustomCheckListBox.MoveItemToDown;
var
  VIndex, VNewIndex, VCount: Integer;
begin
  if MultiSelect or FIsItemEditing then
    Exit;
  VIndex := ItemIndex;
  VCount := Items.Count;
  if (VIndex > -1) and (VIndex < Pred(VCount)) then
  begin
    VNewIndex := Succ(VIndex);
    Items.Move(VIndex, VNewIndex);
    ItemIndex := VNewIndex;
    DoOnMoveItem;
  end;
end;

function TLSCustomCheckListBox.Validate: Boolean;
begin
  Result := LSValidatePattern(GetSelectedText, FPattern, FPatterns) and not
    FIsItemEditing;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

{ TLSCustomComboBox }

constructor TLSCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSONObject := nil;
  FAutoSkipe := False;
  FEditType := etEdit;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FIsValidationHintShowed := False;
  FRequired := False;
  FHoldsFocus := True;
  FShowValidationHint := True;
  FValidationColor := clNone;
  FValidationType := vtEditingDone;
end;

destructor TLSCustomComboBox.Destroy;
begin
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
  inherited Destroy;
end;

procedure TLSCustomComboBox.Clear;
begin
  inherited Clear;
  if Assigned(FJSONObject) then
    FreeAndNil(FJSONObject);
end;

procedure TLSCustomComboBox.EditingDone;
begin
  if FValidationType = vtEditingDone then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  inherited;
end;

procedure TLSCustomComboBox.LoadJSON(const AJSON: TJSONStringType);
begin
  LSJSONToJSONObjectStrings(FJSONObject, AJSON, Items);
end;

function TLSCustomComboBox.GetJSONObject: TJSONObject;
begin
  if not Assigned(FJSONObject) then
    LSShowJSONEmptyObjectError(ClassName);
  Result := FJSONObject;
end;

function TLSCustomComboBox.GetItem: TJSONData;
begin
  if ItemIndex > -1 then
    Result := TJSONObject(GetJSONObject.Items[ItemIndex])
  else
    Result := nil;
end;

procedure TLSCustomComboBox.UTF8KeyPress(var AUTF8Key: TUTF8Char);
begin
  inherited;
  if (FValidationType = vtKeyPress) and not (ssCtrl in GetKeyShiftState) then
  begin
    FUTF8Char := AUTF8Key;
    if (AUTF8Key[1] in [#8, #13, #27]) then
      Exit;
    if not InternalValidate(FHoldsFocus) then
      AUTF8Key := #0;
  end;
end;

procedure TLSCustomComboBox.CMColorChanged(var AMessage: TLMessage);
begin
  inherited;
  if (not FIsValidationColorShowed) and (not FIsFocusColorShowed) then
    FOldColor := Color;
end;

function TLSCustomComboBox.HandleObjectShouldBeVisible: Boolean;
begin
  Result := inherited HandleObjectShouldBeVisible;
  if csDesigning in ComponentState then
    Exit;
  if not Result and not CanFocus then
    UpdateValidationHintShowed;
end;

function TLSCustomComboBox.Validate: Boolean;
begin
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  Result := InternalValidate(False);
end;

function TLSCustomComboBox.GetAbout: string;
begin
  Result := '';
end;

function TLSCustomComboBox.GetValue: Variant;
begin
  Result := GetItem.Value;
end;

{$HINTS OFF}
procedure TLSCustomComboBox.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomComboBox.SetPattern(AValue: string);
var
  VRegEx: TLSRegEx;
begin
  if AValue <> FPattern then
  begin
    FPattern := AValue;
    FEditType := etEdit;
    if Assigned(FPatterns) and (FValidationHint = '') then
    begin
      VRegEx := FPatterns.FindByName(FPattern);
      if Assigned(VRegEx) and (VRegEx.DefaultMessage <> '') then
        FValidationHint := VRegEx.DefaultMessage;
    end;
  end;
end;

procedure TLSCustomComboBox.SetPatterns(const AValue: TLSRegExList);
begin
  if AValue <> FPatterns then
  begin
    FPatterns := AValue;
    if Assigned(FPatterns) then
    begin
      FOldPatternChangeHandler := FPatterns.Items.ChangeHandler;
      FPatterns.Items.ChangeHandler := @PatternChangeHandler;
    end;
  end;
end;

procedure TLSCustomComboBox.SetValue(const AValue: Variant);
var
  I: Integer;
  VItems: TJSONObject;
begin
  if Items.Count = 0 then
    Exit;
  VItems := GetJSONObject;
  for I := 0 to Pred(VItems.Count) do
    if VItems.Items[I].AsString = string(AValue) then
    begin
      ItemIndex := I;
      Break;
    end;
end;

procedure TLSCustomComboBox.UpdateValidationHintShowed;
begin
  if FIsValidationHintShowed then
  begin
    FIsValidationHintShowed := False;
    LSCloseHint(Self);
  end;
end;

procedure TLSCustomComboBox.UpdateValidationColorShowed;
begin
  if FIsValidationColorShowed then
  begin
    FIsValidationColorShowed := False;
    if (FFocusColor <> clNone) and Focused then
      Color := FFocusColor
    else
      Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomComboBox.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomComboBox.PatternChangeHandler(ASender: TObject;
  var ANewName, ANewDefMsg: string);
begin
  if FPattern = '' then
    Exit;
  if ANewName <> FPattern then
    FPattern := ANewName;
  if ANewDefMsg <> FValidationHint then
    FValidationHint := ANewDefMsg;
  if Assigned(FOldPatternChangeHandler) then
    FOldPatternChangeHandler(ASender, ANewName, ANewDefMsg);
end;

function TLSCustomComboBox.InternalValidate(const ACanFocus: Boolean): Boolean;

  procedure LaterValidation(const AHint: string);
  begin
    if FValidationColor <> clNone then
    begin
      FIsValidationColorShowed := True;
      if FFocusColor = clNone then
        FOldColor := Color;
      Color := FValidationColor;
    end;
    if CanFocus then
    begin
      if ACanFocus{$IF DEFINED(MSWINDOWS) OR DEFINED(LCLQt)}and
        (FValidationType <> vtEditingDone){$ENDIF}then
        SetFocus;
      if FShowValidationHint then
      begin
        FIsValidationHintShowed := True;
        LSShowHint(Self, AHint);
      end;
    end;
  end;

var
  VString, VValidateMsg: string;
begin
  VString := inherited RealGetText;
  Result := FRequired and (VString = '');
  if Result then
    LaterValidation(SLSControlsRequiredFieldMsg)
  else
  begin
    case FEditType of
      etEdit:
        begin
          if (FValidationType = vtKeyPress) and Focused then
            Result := LSValidatePattern(FUTF8Char, FPattern, FPatterns)
          else
            Result := LSValidatePattern(VString, FPattern, FPatterns);
          VValidateMsg := SLSControlsPatternFieldMsg;
        end;
      etEmail:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractEmailRegEx) <> '');
          VValidateMsg := SLSControlsEmailFieldMsg;
        end;
      etURL:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractURLRegEx) <> '');
          VValidateMsg := SLSControlsURLFieldMsg;
        end;
      etTel:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractTelRegEx) <> '');
          VValidateMsg := SLSControlsTelFieldMsg;
        end;
    end;
    if not Result then
    begin
      if FValidationHint <> '' then
        LaterValidation(FValidationHint)
      else
        LaterValidation(VValidateMsg);
    end;
  end;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomComboBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomComboBox.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FValidationType <> vtKeyPress then
    Exit;
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  if AKey = VK_TAB then
  begin
    FUTF8Char := Text;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
    Exit;
  end;
  if ((ssCtrl in AShift) and (AKey = VK_V)) then
  begin
    FUTF8Char := Clipboard.AsText;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
  end;
end;

procedure TLSCustomComboBox.CMEnter(var AMessage: TLMessage);
begin
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) and (
    not FIsValidationColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomComboBox.KeyUp(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FAutoSkipe and (UTF8Length(Text) >= MaxLength) and
    IsEditableTextKey(AKey) and not ((ssAlt in AShift) or (ssCtrl in AShift)) then
    PerformTab(True);
end;

procedure TLSCustomComboBox.CMExit(var AMessage: TLMessage);
begin
  if (FValidationType = vtExit) or (FValidationType = vtKeyPress) then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  if not FIsValidationColorShowed then
    UpdateFocusColorShowed;
  inherited;
end;

{ TLSCustomMaskEdit }

constructor TLSCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSkipe := False;
  FEditType := etEdit;
  FFocusColor := clNone;
  FIsFocusColorShowed := False;
  FIsValidationHintShowed := False;
  FPlaceHolderColor := clBtnShadow;
  FRequired := False;
  FHoldsFocus := True;
  FShowValidationHint := True;
  FValidationColor := clNone;
  FValidationType := vtEditingDone;
end;

procedure TLSCustomMaskEdit.EditingDone;
begin
  if FValidationType = vtEditingDone then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  inherited;
end;

procedure TLSCustomMaskEdit.UTF8KeyPress(var AUTF8Key: TUTF8Char);
begin
  inherited;
  if (FValidationType = vtKeyPress) and not (ssCtrl in GetKeyShiftState) then
  begin
    FUTF8Char := AUTF8Key;
    if (AUTF8Key[1] in [#8, #13, #27]) then
      Exit;
    if not InternalValidate(FHoldsFocus) then
      AUTF8Key := #0;
  end;
end;

procedure TLSCustomMaskEdit.CMVisibleChanged(var AMessage: TLMessage);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Visible := Visible;
  inherited;
end;

procedure TLSCustomMaskEdit.CMEnabledChanged(var AMessage: TLMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (Text = '') and IsPlaceHolderApplicable then
    begin
      FPlaceHolderFrame.Enabled := Enabled;
      if Enabled then
        FPlaceHolderFrame.Color := Color
      else
        FPlaceHolderFrame.Color := DEFAULT_PLACEHOLDERDISABLEDCOLOR;
    end;
  end;
  inherited;
end;

procedure TLSCustomMaskEdit.CMTextChanged(var AMessage: TLMessage);
begin
  if csDesigning in ComponentState then
  begin
    if (FPlaceHolder <> '') and (Text = '') then
      ShowPlaceHolder
    else
      HidePlaceHolder;
  end
  else
    if (FPlaceHolder <> '') and (Length(Text) > 0) then
      HidePlaceHolder;
  inherited;
end;

{$HINTS OFF}
procedure TLSCustomMaskEdit.LMLSPlaceHolderMouseAct(var AMessage: TLMessage);
var
  VPoint: TPoint;
begin
  if (TObject(AMessage.LParam) is TLSPlaceHolder) and CanFocus then
  begin
    SetFocus;
    if TMouseButton(AMessage.WParam) = mbRight then
    begin
      GetCursorPos(VPoint);
      PostMessage(Handle, LM_CONTEXTMENU, Handle,
        MakeLParam(VPoint.X, VPoint.Y));
    end;
  end;
end;
{$HINTS ON}

function TLSCustomMaskEdit.HandleObjectShouldBeVisible: Boolean;
begin
  Result := inherited HandleObjectShouldBeVisible;
  if csDesigning in ComponentState then
    Exit;
  if not Result and not CanFocus then
    UpdateValidationHintShowed;
end;

function TLSCustomMaskEdit.Validate: Boolean;
begin
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  Result := InternalValidate(False);
end;

function TLSCustomMaskEdit.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomMaskEdit.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomMaskEdit.SetPattern(AValue: string);
var
  VRegEx: TLSRegEx;
begin
  if AValue <> FPattern then
  begin
    FPattern := AValue;
    FEditType := etEdit;
    if Assigned(FPatterns) then
    begin
      VRegEx := FPatterns.FindByName(FPattern);
      if Assigned(VRegEx) and (VRegEx.DefaultMessage <> '') then
        FValidationHint := VRegEx.DefaultMessage;
    end;
  end;
end;

function TLSCustomMaskEdit.IsPlaceHolderApplicable: Boolean;
begin
  Result := (FPlaceHolder <> '') and Assigned(FPlaceHolderFrame);
end;

procedure TLSCustomMaskEdit.SetPatterns(const AValue: TLSRegExList);
begin
  if AValue <> FPatterns then
  begin
    FPatterns := AValue;
    if Assigned(FPatterns) then
    begin
      FOldPatternChangeHandler := FPatterns.Items.ChangeHandler;
      FPatterns.Items.ChangeHandler := @PatternChangeHandler;
    end;
  end;
end;

procedure TLSCustomMaskEdit.SetPlaceHolder(const AValue: string);
begin
  if AValue <> FPlaceHolder then
  begin
    FPlaceHolder := AValue;
    if csDesigning in ComponentState then
    begin
      if (AValue <> '') and (Text = '') then
      begin
        ShowPlaceHolder;
        FPlaceHolderFrame.Caption := AValue;
      end
      else
        HidePlaceHolder;
      Exit;
    end;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Caption := AValue;
  end;
end;

procedure TLSCustomMaskEdit.SetPlaceHolderColor(const AValue: TColor);
begin
  if AValue <> FPlaceHolderColor then
  begin
    FPlaceHolderColor := AValue;
    if IsPlaceHolderApplicable then
      FPlaceHolderFrame.Font.Color := AValue;
  end;
end;

procedure TLSCustomMaskEdit.UpdateValidationHintShowed;
begin
  if FIsValidationHintShowed then
  begin
    FIsValidationHintShowed := False;
    LSCloseHint(Self);
  end;
end;

procedure TLSCustomMaskEdit.UpdateValidationColorShowed;
begin
  if FIsValidationColorShowed then
  begin
    FIsValidationColorShowed := False;
    if (FFocusColor <> clNone) and Focused then
      Color := FFocusColor
    else
      Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomMaskEdit.UpdateFocusColorShowed;
begin
  if FIsFocusColorShowed then
  begin
    FIsFocusColorShowed := False;
    Color := FOldColor;
{$IFDEF LCLQt}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TLSCustomMaskEdit.PatternChangeHandler(ASender: TObject; var ANewName,
  ANewDefMsg: string);
begin
  if FPattern = '' then
    Exit;
  if ANewName <> FPattern then
    FPattern := ANewName;
  if ANewDefMsg <> FValidationHint then
    FValidationHint := ANewDefMsg;
  if Assigned(FOldPatternChangeHandler) then
    FOldPatternChangeHandler(ASender, ANewName, ANewDefMsg);
end;

procedure TLSCustomMaskEdit.ShowPlaceHolder;
begin
  if not Assigned(FPlaceHolderFrame) then
    FPlaceHolderFrame := TLSPlaceHolder.Create(Self);
  AdjustPlaceHolderPosition;
  FPlaceHolderFrame.Caption := FPlaceHolder;
  FPlaceHolderFrame.Parent := Parent;
{$IFDEF LCLGtk2}
  if Color <> clDefault then
    FPlaceHolderFrame.Color := Color
  else
    FPlaceHolderFrame.Color := clWindow;
{$ELSE}
  FPlaceHolderFrame.Color := Color;
{$ENDIF}
  FPlaceHolderFrame.Alignment := Alignment;
  FPlaceHolderFrame.Font.Color := FPlaceHolderColor;
  FPlaceHolderFrame.Font.Name := Font.Name;
  FPlaceHolderFrame.Font.Size := Font.Size;
  FPlaceHolderFrame.OnMouseDown := @MouseDownPlaceHolder;
end;

procedure TLSCustomMaskEdit.HidePlaceHolder;
begin
  if Assigned(FPlaceHolderFrame) then
    FreeAndNil(FPlaceHolderFrame);
end;

procedure TLSCustomMaskEdit.AdjustPlaceHolderPosition;
begin
  FPlaceHolderFrame.Anchors := [akLeft, akRight, akTop];
  FPlaceHolderFrame.AnchorSideTop.Control := Self;
  FPlaceHolderFrame.AnchorSideTop.Side := asrCenter;
  FPlaceHolderFrame.AnchorSideLeft.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Control := Self;
  FPlaceHolderFrame.AnchorSideRight.Side := asrRight;
  FPlaceHolderFrame.BorderSpacing.Left := 5;
  FPlaceHolderFrame.BorderSpacing.Right := 5;
end;

{$HINTS OFF}
procedure TLSCustomMaskEdit.MouseDownPlaceHolder(ASender: TObject;
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer);
begin
  PostMessage(Handle, LM_LSPLACEHOLDERMOUSEACT, PtrInt(AButton),
    PtrInt(ASender));
end;
{$HINTS ON}

function TLSCustomMaskEdit.InternalValidate(const ACanFocus: Boolean): Boolean;

  procedure LaterValidation(const AHint: string);
  begin
    if FValidationColor <> clNone then
    begin
      FIsValidationColorShowed := True;
      if FFocusColor = clNone then
        FOldColor := Color;
      Color := FValidationColor;
    end;
    if CanFocus then
    begin
      if ACanFocus then
        SetFocus;
      if FShowValidationHint then
      begin
        FIsValidationHintShowed := True;
        LSShowHint(Self, AHint);
      end;
    end;
  end;

var
  VString, VValidateMsg: string;
begin
  VString := inherited GetText;
  Result := FRequired and (VString = '');
  if Result then
    LaterValidation(SLSControlsRequiredFieldMsg)
  else
  begin
    case FEditType of
      etEdit:
        begin
          if (FValidationType = vtKeyPress) and Focused then
            Result := LSValidatePattern(FUTF8Char, FPattern, FPatterns)
          else
            Result := LSValidatePattern(VString, FPattern, FPatterns);
          VValidateMsg := SLSControlsPatternFieldMsg;
        end;
      etEmail:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractEmailRegEx) <> '');
          VValidateMsg := SLSControlsEmailFieldMsg;
        end;
      etURL:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractURLRegEx) <> '');
          VValidateMsg := SLSControlsURLFieldMsg;
        end;
      etTel:
        begin
          Result := (VString = '') or (LSExtractStringUsingRegEx(VString,
            CLSConstsExtractTelRegEx) <> '');
          VValidateMsg := SLSControlsTelFieldMsg;
        end;
    end;
    if not Result then
    begin
      if FValidationHint <> '' then
        LaterValidation(FValidationHint)
      else
        LaterValidation(VValidateMsg);
    end;
  end;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomMaskEdit.SetParent(AParent: TWinControl);
begin
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Parent := Parent;
  inherited;
end;

procedure TLSCustomMaskEdit.Loaded;
begin
  if (FPlaceHolder <> '') and (Text = '') then
    ShowPlaceHolder;
  inherited;
end;

procedure TLSCustomMaskEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

procedure TLSCustomMaskEdit.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FValidationType <> vtKeyPress then
    Exit;
  UpdateValidationColorShowed;
  UpdateValidationHintShowed;
  if AKey = VK_TAB then
  begin
    FUTF8Char := Text;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
    Exit;
  end;
  if ((ssCtrl in AShift) and (AKey = VK_V)) then
  begin
    FUTF8Char := Clipboard.AsText;
    if not InternalValidate(FHoldsFocus) then
      AKey := VK_UNKNOWN;
  end;
end;

procedure TLSCustomMaskEdit.KeyUp(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if FAutoSkipe and (UTF8Length(Text) >= MaxLength) and
    IsEditableTextKey(AKey) and not ((ssAlt in AShift) or (ssCtrl in AShift)) then
    PerformTab(True);
end;

procedure TLSCustomMaskEdit.CMColorChanged(var AMessage: TLMessage);
begin
  if (not FIsValidationColorShowed) and (not FIsFocusColorShowed) then
    FOldColor := Color;
  if IsPlaceHolderApplicable then
    FPlaceHolderFrame.Color := Color;
  inherited;
end;

procedure TLSCustomMaskEdit.CMEnter(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Text = '') then
    HidePlaceHolder;
  if (FFocusColor <> clNone) and (not FIsFocusColorShowed) and
    (not FIsValidationColorShowed) then
  begin
    FIsFocusColorShowed := True;
    FOldColor := Color;
    Color := FFocusColor;
  end;
  inherited;
end;

procedure TLSCustomMaskEdit.CMExit(var AMessage: TLMessage);
begin
  if (FPlaceHolder <> '') and (Text = '') then
    ShowPlaceHolder;
  if (FValidationType = vtExit) or (FValidationType = vtKeyPress) then
  begin
    UpdateValidationColorShowed;
    UpdateValidationHintShowed;
    InternalValidate(FHoldsFocus);
  end;
  if not FIsValidationColorShowed then
    UpdateFocusColorShowed;
  inherited;
end;

{ TLSCustomPanel }

constructor TLSCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradientDirection := gdVertical;
  FGradientStart := clNone;
  FGradientStop := clNone;
end;

function TLSCustomPanel.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) and (AComponent = FPatterns) then
    Patterns := nil;
end;

function TLSCustomPanel.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomPanel.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomPanel.SetGradientDirection(const AValue: TGradientDirection
  );
begin
  if AValue <> FGradientDirection then
  begin
    FGradientDirection := AValue;
    Invalidate;
  end;
end;

procedure TLSCustomPanel.SetGradientStart(const AValue: TColor);
begin
  if AValue <> FGradientStart then
  begin
    FGradientStart := AValue;
    Invalidate;
  end;
end;

procedure TLSCustomPanel.SetGradientStop(const AValue: TColor);
begin
  if AValue <> FGradientStop then
  begin
    FGradientStop := AValue;
    Invalidate;
  end;
end;

procedure TLSCustomPanel.Click;
begin
  if Validate then
    inherited;
end;

procedure TLSCustomPanel.Paint;
begin
  if (FGradientStart <> clNone) or (FGradientStop <> clNone) then
{$IFDEF LCLQt}
  begin
{$ENDIF}
    Canvas.GradientFill(ClientRect, FGradientStart, FGradientStop,
      FGradientDirection);
{$IFDEF LCLQt}
    BevelOuter := bvNone;
  end
  else
    BevelOuter := bvRaised;
{$ENDIF}
  inherited;
end;

{ TLSCustomBitBtn }

constructor TLSCustomBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  Application.AddOnKeyDownHandler(@DoAppKeyPress);
  FImageIndex := -1;
  FShortCutCanFocus := False;
end;

destructor TLSCustomBitBtn.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  Application.RemoveOnKeyDownHandler(@DoAppKeyPress);
  inherited Destroy;
end;

function TLSCustomBitBtn.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomBitBtn.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) then
  begin
    if AComponent = FPatterns then
      Patterns := nil;
    if AComponent = FImages then
      Images := nil;
  end;
end;

procedure TLSCustomBitBtn.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
    UpdateGlyph;
end;

function TLSCustomBitBtn.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomBitBtn.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

{$HINTS OFF}
procedure TLSCustomBitBtn.DoAppKeyPress(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  if FShortCut <> KeyToShortCut(AKey, AShift) then
    Exit;
  if FShortCutCanFocus then
  begin
    if CanFocus then
      Click;
  end
  else
    if Enabled and Visible then
      Click;
end;
{$HINTS ON}

procedure TLSCustomBitBtn.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    UpdateGlyph;
  end;
end;

procedure TLSCustomBitBtn.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if Assigned(FImages) then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateGlyph;
end;

procedure TLSCustomBitBtn.UpdateGlyph;
begin
  if (FImageIndex < 0) or not Assigned(FImages) or
    (FImageIndex >= FImages.Count) then
    Glyph := nil
  else
    FImages.GetBitmap(FImageIndex, Glyph);
end;

procedure TLSCustomBitBtn.Click;
begin
  if Validate then
    inherited;
end;

{ TLSCustomSpeedButton }

constructor TLSCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FImageIndex := -1;
end;

destructor TLSCustomSpeedButton.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

function TLSCustomSpeedButton.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomSpeedButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) then
  begin
    if AComponent = FPatterns then
      Patterns := nil;
    if AComponent = FImages then
      Images := nil;
  end;
end;

procedure TLSCustomSpeedButton.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
    UpdateGlyph;
end;

function TLSCustomSpeedButton.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomSpeedButton.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomSpeedButton.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    UpdateGlyph;
  end;
end;

procedure TLSCustomSpeedButton.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if Assigned(FImages) then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateGlyph;
end;

procedure TLSCustomSpeedButton.UpdateGlyph;
begin
  if (FImageIndex < 0) or not Assigned(FImages) or
    (FImageIndex >= FImages.Count) then
    Glyph := nil
  else
    FImages.GetBitmap(FImageIndex, Glyph);
end;

procedure TLSCustomSpeedButton.Click;
begin
  if Validate then
    inherited;
end;

{ TLSCustomImageAnimator }

constructor TLSCustomImageAnimator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimator := TTimer.Create(nil);
  FAnimator.Enabled := False;
  FAnimator.OnTimer := @DoAnimation;
  FActive := False;
  FAutoAdjust := True;
  FImageIndex := -1;
  FInterval := 1000;
  FStart := 0;
  FStop := 0;
end;

destructor TLSCustomImageAnimator.Destroy;
begin
  FAnimator.Free;
  inherited Destroy;
end;

procedure TLSCustomImageAnimator.DoAnimation(ASender: TObject);
begin
  if FStart <> FStop then
  begin
    if FStart < FStop then
    begin
      if FImageIndex = -1 then
        FImageIndex := FStart
      else
        if FImageIndex < FStop then
          Inc(FImageIndex)
        else
          FImageIndex := FStart;
    end
    else
    begin
      if FImageIndex = -1 then
        FImageIndex := FStart
      else
        if FImageIndex > FStop then
          Dec(FImageIndex)
        else
          FImageIndex := FStart;
    end;
  end;
  if Assigned(FAnimationHandler) then
    FAnimationHandler(ASender, FImageIndex);
  if Assigned(FOnAnimation) then
    FOnAnimation(ASender, FImageIndex);
end;

function TLSCustomImageAnimator.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomImageAnimator.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomImageAnimator.SetActive(const AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    if Assigned(FAnimator) then
      FAnimator.Enabled := AValue;
  end;
end;

procedure TLSCustomImageAnimator.SetInterval(const AValue: Integer);
begin
  if AValue <> FInterval then
  begin
    FInterval := AValue;
    if Assigned(FAnimator) then
      FAnimator.Interval := AValue;
  end;
end;

{ TLSCustomImage }

constructor TLSCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FAlignment := taCenter;
  FEmptyColor := clYellow;
  FEmptyStyle := bsDiagCross;
  FImageIndex := -1;
end;

destructor TLSCustomImage.Destroy;
begin
  if Assigned(FAnimator) then
  begin
    FAnimator.AnimationHandler := nil;
    if Assigned(FOldAnimationHandler) then
      FAnimator.AnimationHandler := FOldAnimationHandler;
  end;
  if Assigned(FPNGTemp) then
    FPNGTemp.Free;
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

function TLSCustomImage.Validate: Boolean;
begin
  Result := LSValidatePattern(Caption, FPattern, FPatterns);
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TLSCustomImage.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AOperation = opRemove) then
  begin
    if AComponent = FPatterns then
      Patterns := nil;
    if AComponent = FImages then
      Images := nil;
    if AComponent = FAnimator then
      Animator := nil;
  end;
end;

procedure TLSCustomImage.CMTextChanged(var AMessage: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TLSCustomImage.CMFontChanged(var AMessage: TLMessage);
begin
  inherited;
  Invalidate;
end;

procedure TLSCustomImage.Paint;
var
  VRect: TRect;
  VTextStyle: TTextStyle;
begin
  inherited;
  if Caption <> '' then
  begin
    if not Assigned(Picture.Graphic) then
    begin
      if not Assigned(FPNGTemp) then
        FPNGTemp := TPortableNetworkGraphic.Create;
      FPNGTemp.SetSize(Width, Height);
      FPNGTemp.Canvas.Brush.Color := FEmptyColor;
      FPNGTemp.Canvas.Brush.Style := FEmptyStyle;
      FPNGTemp.Canvas.FillRect(ClientRect);
      Picture.PNG.Assign(FPNGTemp);
    end;
    VRect := GetClientRect;
    VTextStyle := Canvas.TextStyle;
    VTextStyle.Alignment:= Alignment;
    VTextStyle.Layout:= tlCenter;
    VTextStyle.Opaque:= False;
    VTextStyle.Clipping:= False;
    VTextStyle.SystemFont := Canvas.Font.IsDefault;
    if not Enabled then
    begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(VRect, 1, 1);
      Canvas.TextRect(VRect, VRect.Left, VRect.Top, Caption, VTextStyle);
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(VRect, -1, -1);
    end
    else
      Canvas.Font.Color := Font.Color;
    Canvas.TextRect(VRect,VRect.Left,VRect.Top, Caption, VTextStyle);
  end;
end;

procedure TLSCustomImage.ImageListChange(ASender: TObject);
begin
  if ASender = Images then
  begin
    UpdatePicture;
    UpdateAnimator;
  end;
end;

function TLSCustomImage.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomImage.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomImage.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Invalidate;
  end;
end;

procedure TLSCustomImage.SetAnimator(const AValue: TLSCustomImageAnimator);
begin
  if AValue <> FAnimator then
  begin
    FAnimator := AValue;
    if not Assigned(FAnimator) then
      Exit;
    FAnimator.AnimationHandler := @DoAnimationHandler;
    UpdateAnimator;
  end;
end;

procedure TLSCustomImage.SetEmptyColor(const AValue: TColor);
begin
  if AValue <> FEmptyColor then
  begin
    FEmptyColor := AValue;
    Picture := nil;
    Invalidate;
  end;
end;

procedure TLSCustomImage.SetEmptyStyle(const AValue: TBrushStyle);
begin
  if AValue <> FEmptyStyle then
  begin
    FEmptyStyle := AValue;
    Picture := nil;
    Invalidate;
  end;
end;

procedure TLSCustomImage.SetImageIndex(const AValue: TImageIndex);
begin
  if AValue <> FImageIndex then
  begin
    FImageIndex := AValue;
    UpdatePicture;
  end;
end;

procedure TLSCustomImage.SetImages(const AValue: TCustomImageList);
begin
  if FImages = AValue then
    Exit;
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := AValue;
  if Assigned(FImages) then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
    UpdateAnimator;
  end;
  UpdatePicture;
end;

procedure TLSCustomImage.DoAnimationHandler(ASender: TObject;
  var AImageIndex: TImageIndex);
begin
  if Assigned(FImages) and (FImages.Count > 0) then
    SetImageIndex(AImageIndex);
  if Assigned(FOldAnimationHandler) then
    FOldAnimationHandler(ASender, AImageIndex);
end;

procedure TLSCustomImage.UpdatePicture;
var
  VRaw: TRawImage;
begin
  if (FImageIndex < 0) or not Assigned(FImages) or
    (FImageIndex >= FImages.Count) then
    Picture := nil
  else
  begin
    FImages.GetRawImage(FImageIndex, VRaw);
    Picture.PNG.LoadFromRawImage(VRaw, False);
  end;
end;

procedure TLSCustomImage.UpdateAnimator;
begin
  if Assigned(FAnimator) and Assigned(FImages) and (FImages.Count > 0) and
    FAnimator.AutoAdjust then
  begin
    FAnimator.Start := 0;
    FAnimator.Stop := Pred(FImages.Count);
  end;
end;

procedure TLSCustomImage.Click;
begin
  if Validate then
    inherited;
end;

procedure TLSCustomImage.LoadFromBase64Stream(var AStream: TStream);
var
  VDecodedStream: TMemoryStream;
  VBase64Decoder: TBase64DecodingStream;
begin
  VDecodedStream := TMemoryStream.Create;
  VBase64Decoder := TBase64DecodingStream.Create(AStream);
  try
    VDecodedStream.CopyFrom(VBase64Decoder, VBase64Decoder.Size);
    VDecodedStream.Position := 0;
    Picture.LoadFromStream(VDecodedStream);
  finally
    VDecodedStream.Free;
    VBase64Decoder.Free;
  end;
end;

procedure TLSCustomImage.LoadFromBase64File(const AFileName: TFileName);
var
  VEncodedStream: TFileStream;
begin
  VEncodedStream := TFileStream.Create(AFileName,
    fmOpenRead or fmShareDenyWrite);
  try
    LoadFromBase64Stream(TStream(VEncodedStream));
  finally
    VEncodedStream.Free;
  end;
end;

procedure TLSCustomImage.LoadFromBase64String(const AString: string);
var
  VEncodedStream: TStringStream;
begin
  VEncodedStream := TStringStream.Create(AString);
  try
    LoadFromBase64Stream(TStream(VEncodedStream));
  finally
    VEncodedStream.Free;
  end;
end;

procedure TLSCustomImage.SaveToBase64Stream(var AStream: TStream);
var
  VDecodedStream: TMemoryStream;
  VBase64Encoder: TBase64EncodingStream;
begin
  VDecodedStream := TMemoryStream.Create;
  VBase64Encoder := TBase64EncodingStream.Create(AStream);
  try
    Picture.SaveToStream(VDecodedStream);
    VDecodedStream.Position := 0;
    VBase64Encoder.CopyFrom(VDecodedStream, VDecodedStream.Size);
  finally
    VDecodedStream.Free;
    VBase64Encoder.Free;
  end;
end;

procedure TLSCustomImage.SaveToBase64File(const AFileName: TFileName);
var
  VEncodedStream: TFileStream;
begin
  VEncodedStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToBase64Stream(TStream(VEncodedStream));
  finally
    VEncodedStream.Free;
  end;
end;

procedure TLSCustomImage.SaveToBase64String(out AString: string);
var
  VEncodedStream: TStringStream;
begin
  VEncodedStream := TStringStream.Create('');
  try
    SaveToBase64Stream(TStream(VEncodedStream));
    AString := VEncodedStream.DataString;
  finally
    VEncodedStream.Free;
  end;
end;

function TLSCustomImage.AsBase64: string;
begin
  SaveToBase64String(Result);
end;

function TLSCustomImage.IsEmpty: Boolean;
begin
  Result := (Picture.Graphic = nil) or (Picture.Graphic.Empty) or
    (Picture.Graphic.Height = 0) or (Picture.Graphic.Width = 0);
end;

{ TLSGadgetEditButton }

constructor TLSGadgetEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
  SetAutoSize(True);
  ControlStyle := ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  Spacing := 0;
end;

{ TLSGadgetEditCaption }

constructor TLSGadgetEditCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
  SetAutoSize(False);
  ControlStyle := ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  Color := clSkyBlue;
  Layout := tlCenter;
  Width := 192;
end;

{ TLSCustomGadgetEdit }

constructor TLSCustomGadgetEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TLSGadgetEditButton.Create(Self);
  FDescription := TLSGadgetEditCaption.Create(Self);
  FButton.FreeNotification(Self);
  FDescription.FreeNotification(Self);
  ControlStyle := ControlStyle - [csSetCaption];
  FButton.OnClick := @DoButtonClick;
  Width := 48;
end;

destructor TLSCustomGadgetEdit.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FDescription);
  inherited Destroy;
end;

procedure TLSCustomGadgetEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if not Assigned(AParent) then
    Exit;
  FButton.SetParent(AParent);
  FDescription.SetParent(AParent);
  DoButtonDescriptionPosition;
end;

procedure TLSCustomGadgetEdit.SetEnabled(AValue: Boolean);
begin
  inherited;
  FButton.SetEnabled(AValue);
  FDescription.SetEnabled(AValue);
end;

procedure TLSCustomGadgetEdit.SetVisible(AValue: Boolean);
begin
  inherited;
  FButton.SetVisible(AValue);
  FDescription.SetVisible(AValue);
end;

procedure TLSCustomGadgetEdit.SetName(const AValue: TComponentName);
begin
  inherited SetName(AValue);
  FButton.SetName(AValue + LSEDITBUTTONPREFIX);
  FDescription.SetName(AValue + LSGADGETEDITDESCRIPTIONPREFIX);
end;

procedure TLSCustomGadgetEdit.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  inherited;
  if (ssAlt in AShift) and (AKey = VK_DOWN) then
    DoButtonClick(Self);
end;

procedure TLSCustomGadgetEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = FButton then
      FButton := nil;
    if AComponent = FDescription then
      FDescription := nil;
  end;
end;

procedure TLSCustomGadgetEdit.SetDescriptionCaption(const AValue: TCaption);
begin
  FDescription.Caption := AValue;
end;

procedure TLSCustomGadgetEdit.Loaded;
begin
  inherited Loaded;
  DoButtonDescriptionPosition;
  FButton.Width := Height;
end;

procedure TLSCustomGadgetEdit.DoButtonDescriptionPosition;
begin
  FButton.AnchorToCompanion(akLeft, 0, Self);
  FDescription.AnchorToCompanion(akLeft, 0, FButton);
end;

procedure TLSCustomGadgetEdit.SetButtonCaption(const AValue: TCaption);
begin
  FButton.Caption := AValue;
end;

procedure TLSCustomGadgetEdit.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint := AValue;
end;

function TLSCustomGadgetEdit.GetButtonCaption: TCaption;
begin
  Result := FButton.Caption;
end;

function TLSCustomGadgetEdit.GetButtonHint: TTranslateString;
begin
  Result := FButton.Hint;
end;

function TLSCustomGadgetEdit.GetDescriptionCaption: TCaption;
begin
  Result := FDescription.Caption;
end;

procedure TLSCustomGadgetEdit.DoButtonClick(ASender: TObject);
begin
  if not ReadOnly and Assigned(FOnButtonClick) then
    FOnButtonClick(ASender);
end;

{ TLSEditBtnButton }

constructor TLSEditBtnButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
  SetAutoSize(True);
  ControlStyle := ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  Spacing := 0;
end;

procedure TLSEditBtnButton.VisibleChanged;
var
  VForm: TCustomForm;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    VForm := GetParentForm(Self);
    if Visible then
      ControlStyle := ControlStyle - [csNoDesignVisible]
    else
      ControlStyle := ControlStyle + [csNoDesignVisible];
    if Assigned(VForm) then
      VForm.Invalidate;
  end;
end;

{ TLSCustomEditButton }

constructor TLSCustomEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TLSEditBtnButton.Create(Self);
  FButton.FreeNotification(Self);
  ControlStyle := ControlStyle - [csSetCaption];
  FButton.OnClick := @DoButtonClick;
  FAutoButtonClick := False;
  FButtonOnlyWhenFocused := False;
end;

destructor TLSCustomEditButton.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TLSCustomEditButton.CheckButtonVisible;
begin
  FButton.Visible := Visible and FButton.Visible;
end;

function TLSCustomEditButton.GetButtonCaption: TCaption;
begin
  Result := FButton.Caption;
end;

function TLSCustomEditButton.GetButtonHint: TTranslateString;
begin
  Result := FButton.Hint;
end;

procedure TLSCustomEditButton.SetButtonCaption(const AValue: TCaption);
begin
  FButton.Caption := AValue;
end;

procedure TLSCustomEditButton.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint := AValue;
end;

procedure TLSCustomEditButton.SetButtonOnlyWhenFocused(const AValue: Boolean);
begin
  if AValue <> FButtonOnlyWhenFocused then
  begin
    FButtonOnlyWhenFocused := AValue;
    CheckButtonVisible;
  end;
end;

procedure TLSCustomEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
  DoButtonPosition;
  if FButtonOnlyWhenFocused then
    FButton.Hide;
end;

procedure TLSCustomEditButton.DoButtonPosition;
begin
  FButton.AnchorToCompanion(akLeft, 0, Self);
end;

procedure TLSCustomEditButton.DoButtonClick(ASender: TObject);
begin
  if not ReadOnly and Assigned(FOnButtonClick) then
    FOnButtonClick(ASender);
end;

procedure TLSCustomEditButton.SetEnabled(AValue: Boolean);
begin
  inherited;
  FButton.SetEnabled(AValue);
end;

procedure TLSCustomEditButton.SetVisible(AValue: Boolean);
begin
  inherited;
  CheckButtonVisible;
end;

procedure TLSCustomEditButton.SetName(const AValue: TComponentName);
begin
  inherited SetName(AValue);
  FButton.SetName(AValue + LSEDITBUTTONPREFIX);
end;

procedure TLSCustomEditButton.SetParent(AParent: TWinControl);
begin
  inherited;
  if not Assigned(AParent) then
    Exit;
  FButton.SetParent(AParent);
  CheckButtonVisible;
  DoButtonPosition;
end;

procedure TLSCustomEditButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AComponent = FButton) and (AOperation = opRemove) then
    FButton := nil;
end;

procedure TLSCustomEditButton.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  if (ssAlt in AShift) and (AKey = VK_DOWN) then
    DoButtonClick(Self);
  inherited;
end;

procedure TLSCustomEditButton.CMEnter(var AMessage: TLMessage);
begin
  inherited;
  if FAutoButtonClick then
    DoButtonClick(Self);
end;

procedure TLSCustomEditButton.WMSetFocus(var AMessage: TLMSetFocus);
begin
  if FButtonOnlyWhenFocused then
    FButton.Show;
  inherited;
end;

procedure TLSCustomEditButton.WMKillFocus(var AMessage: TLMKillFocus);
begin
  if FButtonOnlyWhenFocused then
    FButton.Hide;
  inherited;
end;

{ TLSCustomNumericEdit }

constructor TLSCustomNumericEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Alignment := taRightJustify;
  ControlStyle := ControlStyle - [csSetCaption];
  FDigits := 2;
  FDisplayFormat := DefaultDisplayFormat;
  FIsInitializing := True;
  FInputType := itFloat;
  FFormatStyle := fsFormatFloat;
  FFloatFormat := ffFixed;
  FPrecision := 20;
  FRound := False;
  FRoundingDigits := -2;
  FRoundingMode := rmNearest;
{$IFNDEF LCLQt}
  if csDesigning in ComponentState then
{$ENDIF}
    UpdateText;
end;

procedure TLSCustomNumericEdit.Clear;
begin
  FAsInteger := 0;
  FValue := 0;
  CheckMinMaxValue;
end;

function TLSCustomNumericEdit.DefaultDisplayFormat: ShortString;
begin
  Result := LSNUMERICEDITDEFAULTDISPLAYFORMAT;
end;

procedure TLSCustomNumericEdit.UpdateText;
begin
  if Focused and (inherited RealGetText = '') then
    Exit;
  FIsUpdating := True;
  case FInputType of
    itInteger: inherited RealSetText(IntToStr(FAsInteger));
    itFloat:
      case FFormatStyle of
        fsFormatFloat: inherited RealSetText(FormatFloat(FDisplayFormat, FValue));
        fsFloatToStrF: inherited RealSetText(FloatToStrF(FValue, FFloatFormat,
          FPrecision, FDigits));
      end;
  end;
end;

procedure TLSCustomNumericEdit.CheckMinMaxValue;
begin
  if (FMinValue <> 0) and (FValue < FMinValue) then
    FValue := FMinValue;
  if (FMaxValue <> 0) and (FValue > FMaxValue) then
    FValue := FMaxValue;
  FAsInteger := Trunc(FValue);
  UpdateText;
end;

procedure TLSCustomNumericEdit.SetMinValue(const AValue: Extended);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomNumericEdit.SetPrecision(const AValue: Integer);
begin
  if AValue <> FPrecision then
  begin
    FPrecision := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomNumericEdit.SetRound(const AValue: Boolean);
begin
  if AValue <> FRound then
  begin
    FRound := AValue;
    Modified := True;
    UpdateRound;
  end;
end;

procedure TLSCustomNumericEdit.SetRoundingDigits(const AValue: Integer);
begin
  if AValue <> FRoundingDigits then
  begin
    FRoundingDigits := AValue;
    Modified := True;
    UpdateRound;
  end;
end;

procedure TLSCustomNumericEdit.SetRoundingMode(const AValue: TFPURoundingMode);
begin
  if AValue <> FRoundingMode then
  begin
    FRoundingMode := AValue;
    Modified := True;
    UpdateRound;
  end;
end;

procedure TLSCustomNumericEdit.SetValue(const AValue: Extended);
begin
  if AValue <> FValue then
  begin
    if FRound and (FInputType = itFloat) then
      FValue := LSRoundAnExtended(AValue, FRoundingMode, FRoundingDigits)
    else
      FValue := AValue;
    FAsInteger := Trunc(AValue);
    CheckMinMaxValue;
  end;
end;

function TLSCustomNumericEdit.UpdateRound: Boolean;
begin
  Result := FRound and (FInputType = itFloat) and Modified;
  if Result then
  begin
    FValue := LSRoundAnExtended(FValue, FRoundingMode, FRoundingDigits);
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomNumericEdit.Loaded;
begin
  inherited;
  UpdateText;
end;

procedure TLSCustomNumericEdit.KeyPress(var AKey: char);
begin
  inherited;
  if FInputType = itInteger then
  begin
    if not (AKey in ['0'..'9', '+', '-', #0..#31{$IFDEF LCLQt},#127{$ENDIF}]) then
      AKey := #0;
    Exit;
  end;
  if AKey in ['.', ','] then
    AKey := DefaultFormatSettings.Decimalseparator;
  if not (AKey in ['0'..'9', '+', '-', #0..#31{$IFDEF LCLQt},#127{$ENDIF},
    DefaultFormatSettings.DecimalSeparator]) then
    AKey := #0;
  if (AKey = DefaultFormatSettings.DecimalSeparator) and
    (Pos(DefaultFormatSettings.DecimalSeparator, Text) > 0) then
    AKey := #0;
end;

procedure TLSCustomNumericEdit.EditingDone;
begin
  if not UpdateRound then
    CheckMinMaxValue;
  inherited;
end;

procedure TLSCustomNumericEdit.CMTextChanged(var AMessage: TLMessage);
var
  VText: TTranslateString;
begin
  inherited;
  if FIsInitializing then
  begin
    FIsInitializing := False;
    Exit;
  end;
  if FIsUpdating then
  begin
    FIsUpdating := False;
    Exit;
  end;
{$IF DEFINED(LCLGtk2) OR DEFINED(MSWINDOWS)}
  if FIsEntering then
  begin
    FIsEntering := False;
    Exit;
  end;
{$ENDIF}
  VText := inherited RealGetText;
  if VText <> '' then
  begin
    FValue := StrToFloatDef(VText, FValue);
    FAsInteger := Trunc(FValue);
  end
  else
    Clear;
end;

procedure TLSCustomNumericEdit.CMEnter(var AMessage: TLMessage);
begin
{$IF DEFINED(LCLGtk2) OR DEFINED(MSWINDOWS)}
  FIsEntering := True;
{$ENDIF}
  if FValue = 0 then
    inherited RealSetText('')
  else
    case FInputType of
      itInteger: inherited RealSetText(IntToStr(FAsInteger));
      itFloat: inherited RealSetText(FloatToStr(FValue));
    end;
  inherited;
end;

procedure TLSCustomNumericEdit.CMExit(var AMessage: TLMessage);
begin
  CheckMinMaxValue;
  inherited;
end;

procedure TLSCustomNumericEdit.SetMaxValue(const AValue: Extended);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomNumericEdit.SetDisplayFormat(const AValue: string);
begin
  if AValue <> FDisplayFormat then
  begin
    FDisplayFormat := AValue;
    UpdateText;
  end;
end;

procedure TLSCustomNumericEdit.SetFloatFormat(const AValue: TFloatFormat);
begin
  if AValue <> FFloatFormat then
  begin
    FFloatFormat := AValue;
    UpdateText;
  end;
end;

procedure TLSCustomNumericEdit.SetDigits(const AValue: Integer);
begin
  if AValue <> FDigits then
  begin
    FDigits := AValue;
    UpdateText;
  end;
end;

procedure TLSCustomNumericEdit.SetFormatStyle(
  const AValue: TLSNumericEditFormatStyle);
begin
  if AValue <> FFormatStyle then
  begin
    FFormatStyle := AValue;
    UpdateText;
  end;
end;

procedure TLSCustomNumericEdit.SetInputType(
  const AValue: TLSNumericEditInputType);
begin
  if AValue <> FInputType then
  begin
    FInputType := AValue;
    UpdateText;
  end;
end;

{ TLSCustomCurrencyEdit }

function TLSCustomCurrencyEdit.DefaultDisplayFormat: ShortString;
var
  I: Integer;
  VChar: Char;
  VCurrStr: ShortString;
begin
  Result := ',0.' + DupeString('0', DefaultFormatSettings.CurrencyDecimals);
  VCurrStr := '';
  for I := 1 to Length(DefaultFormatSettings.CurrencyString) do
  begin
    VChar := DefaultFormatSettings.CurrencyString[I];
    if VChar in [',', '.'] then
      VCurrStr += '''' + VChar + ''''
    else
      VCurrStr += VChar;
  end;
  if Length(VCurrStr) > 0 then
    case DefaultFormatSettings.CurrencyFormat of
      0: Result := VCurrStr + Result; { $1 }
      1: Result += VCurrStr; { 1$ }
      2: Result := VCurrStr + ' ' + Result; { $ 1 }
      3: Result += ' ' + VCurrStr; { 1 $ }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

{ TLSCustomTrayIcon }

constructor TLSCustomTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifierOSImage := TPicture.Create;
end;

destructor TLSCustomTrayIcon.Destroy;
begin
  if Assigned(FOldIcon) then
    FOldIcon.Free;
  FNotifierOSImage.Free;
  inherited Destroy;
end;

procedure TLSCustomTrayIcon.HideMainForm;
begin
  if not Assigned(Application.MainForm) then
    Application.ShowMainForm := False
  else
    Application.MainForm.Hide;
  Visible := True;
end;

procedure TLSCustomTrayIcon.RestoreMainForm;
begin
  if not Application.ShowMainForm then
    Application.Restore;
  if Assigned(Application.MainForm) and not Application.MainForm.Visible then
    Application.MainForm.Show;
end;

class procedure TLSCustomTrayIcon.ShowNotifierOS(const ATitle, AMsg: string;
  const AGraphic: TGraphic; const APosition: TLSNotifyPosition;
  const AAnimation: Boolean; const ADuration: TLSNotifyDuration;
  const AMargin: SmallInt; const AColor: TColor; const ABorderColor: TColor;
  const ATextColor: TColor; const AClick: TNotifyEvent);
begin
  TLSNotifierOS.Execute(nil, ATitle, AMsg, AGraphic, APosition,
    AAnimation, ADuration, AMargin, AColor, ABorderColor, ATextColor, AClick);
end;

class procedure TLSCustomTrayIcon.ShowNotifierOS(var ATheme: TGraphic;
  const ATitle, AMsg: string; const AGraphic: TGraphic;
  const APosition: TLSNotifyPosition; const AAnimation: Boolean;
  const ADuration: TLSNotifyDuration; const AMargin: SmallInt;
  const ATextColor: TColor; const AClick: TNotifyEvent);
begin
  TLSNotifierOS.Execute(nil, ATheme, ATitle, AMsg, AGraphic,
    APosition, AAnimation, ADuration, AMargin, ATextColor, AClick);
end;

procedure TLSCustomTrayIcon.ShowNotifierOS(const ATitle, AMsg: string;
  const APosition: TLSNotifyPosition; const AAnimation: Boolean;
  const ADuration: TLSNotifyDuration; const AMargin: SmallInt;
  const AColor: TColor; const ABorderColor: TColor; const ATextColor: TColor);
begin
  TLSNotifierOS.Execute(Self, ATitle, AMsg, FNotifierOSImage.Graphic, APosition,
    AAnimation, ADuration, AMargin, AColor, ABorderColor, ATextColor,
    FOnNotifierOSClick);
end;

procedure TLSCustomTrayIcon.ShowNotifierOS(var ATheme: TGraphic; const ATitle,
  AMsg: string; const APosition: TLSNotifyPosition; const AAnimation: Boolean;
  const ADuration: TLSNotifyDuration; const AMargin: SmallInt;
  const ATextColor: TColor);
begin
  TLSNotifierOS.Execute(Self, ATheme, ATitle, AMsg, FNotifierOSImage.Graphic,
    APosition, AAnimation, ADuration, AMargin, ATextColor, FOnNotifierOSClick);
end;

function TLSCustomTrayIcon.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomTrayIcon.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

{$IFNDEF LCLQt}
procedure TLSCustomTrayIcon.Loaded;
var
  VIcon: TIcon;
begin
  if csDesigning in ComponentState then
    Exit;
  if Icon.Empty then
  begin
    VIcon := TIcon.Create;
    try
      VIcon.Handle := Application.SmallIconHandle;
      Icon.Assign(VIcon);
    finally
      VIcon.ReleaseHandle;
      VIcon.Free;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TLSCustomTrayIcon.RestoreOldIcon;
begin
  if Assigned(FOldIcon) then
    Icon.Assign(FOldIcon);
end;

procedure TLSCustomTrayIcon.MakeOldIcon;
begin
  if not Assigned(FOldIcon) then
    FOldIcon := TIcon.Create;
  FOldIcon.Assign(Icon);
end;

{ TLSCustomDateEdit }

constructor TLSCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCalendarCloseWithDblClick := True;
  FCalendarDisplaySettings := [dsShowHeadings, dsShowDayNames];
  FCalendarShowBtns := True;
  FCalendarShowBtnsCaptions := True;
  FCalendarShowBtnsGlyphs := True;
  FDefaultToday := False;
  FIsInitializing := True;
  FValidDateSeparator := DefaultFormatSettings.DateSeparator;
{$IFNDEF LCLQt}
  if csDesigning in ComponentState then
{$ENDIF}
    UpdateText;
  if FButton.Glyph.Empty then
    FButton.LoadGlyphFromLazarusResource('LSDateEditBtn');
end;

procedure TLSCustomDateEdit.EditingDone;
begin
  inherited;
  if Focused then
    CheckMinMaxValue;
end;

procedure TLSCustomDateEdit.SetDisplayFormat(const AValue: string);
begin
  if AValue <> FDisplayFormat then
  begin
    FDisplayFormat := AValue;
    if csDesigning in ComponentState then
    begin
      EditMask := '';
      if AValue = CLSDateEditDisplayFrmtMDY then
        EditMask := CLSDateEditMaskMDY;
      if AValue = CLSDateEditDisplayFrmtDMY then
        EditMask := CLSDateEditMaskDMY;
      if AValue = CLSDateEditDisplayFrmtYMD then
        EditMask := CLSDateEditMaskYMD;
    end;
    UpdateText;
  end;
end;

procedure TLSCustomDateEdit.SetMaxValue(const AValue: TDateTime);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomDateEdit.SetMinValue(const AValue: TDateTime);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomDateEdit.SetValue(const AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    CheckMinMaxValue;
  end;
end;

{$HINTS OFF}
procedure TLSCustomDateEdit.DoCalendarPopupClose(ASender: TObject;
  const ADateTime: TDateTime);
begin
  SetValue(ADateTime);
end;
{$HINTS ON}

procedure TLSCustomDateEdit.DoButtonClick(ASender: TObject);
var
  VPopupOrigin: TPoint;
begin
  VPopupOrigin := ControlToScreen(Point(0, Height));
  TLSCalendarPopupForm.Execute(FValue, VPopupOrigin, FCalendarCloseWithDblClick,
    FCalendarShowBtns, FCalendarShowBtnsCaptions, FCalendarShowBtnsGlyphs,
    FCalendarDisplaySettings, @DoCalendarPopupClose);
  inherited;
end;

procedure TLSCustomDateEdit.Loaded;
begin
  inherited;
  if FDefaultToday then
    SetValue(Date);
end;

procedure TLSCustomDateEdit.KeyPress(var AKey: Char);
begin
  inherited;
  if not (AKey in [DefaultFormatSettings.DateSeparator, FValidDateSeparator,
    '0'..'9', #0..#31{$IFDEF LCLQt}, #127{$ENDIF}]) then
    AKey := #0;
end;

procedure TLSCustomDateEdit.CMTextChanged(var AMessage: TLMessage);
begin
  inherited;
  if FIsInitializing then
  begin
    FIsInitializing := False;
    Exit;
  end;
  if FIsUpdating then
  begin
    FIsUpdating := False;
    Exit;
  end;
  if Trim(FDisplayFormat) <> '' then
  begin
    if not TryStrToDate(Text, FValue, FDisplayFormat,
      DefaultFormatSettings.DateSeparator) then
      FValue := CLSConstsNullDate;
  end
  else
    if not TryStrToDate(Text, FValue) then
      FValue := CLSConstsNullDate;
end;

procedure TLSCustomDateEdit.Clear;
begin
  if FDefaultToday then
    SetValue(Date)
  else
    SetValue(CLSConstsNullDate);
end;

procedure TLSCustomDateEdit.UpdateText;
var
  VSelStart: Integer;
begin
  FIsUpdating := True;
  if FValue <> CLSConstsNullDate then
    Text := FormatDateTime(FDisplayFormat, FValue)
  else
  begin
    VSelStart := GetSelStart;
    Text := '';
    SetSelStart(VSelStart);
  end;
end;

procedure TLSCustomDateEdit.CheckMinMaxValue;
begin
  if (FMinValue <> CLSConstsNullDate) and (FValue < FMinValue) then
    FValue := FMinValue;
  if (FMaxValue <> CLSConstsNullDate) and (FValue > FMaxValue) then
    FValue := FMaxValue;
  UpdateText;
end;

{ TLSTimeEditUpDown }

constructor TLSTimeEditUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSubComponent(True);
  ControlStyle := ControlStyle + [csNoDesignSelectable] - [csSetCaption];
end;

procedure TLSTimeEditUpDown.VisibleChanged;
var
  VForm: TCustomForm;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    VForm := GetParentForm(Self);
    if Visible then
      ControlStyle := ControlStyle - [csNoDesignVisible]
    else
      ControlStyle := ControlStyle + [csNoDesignVisible];
    if Assigned(VForm) then
      VForm.Invalidate;
  end;
end;

{ TLSCustomTimeEdit }

constructor TLSCustomTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpDown := TLSTimeEditUpDown.Create(Self);
  FUpDown.FreeNotification(Self);
  FUpDown.OnClick := @DoUpDownClick;
  ControlStyle := ControlStyle - [csSetCaption];
  FDisplayFormat := dfHM;
  FDefaultNow := False;
  FUpDownOnlyWhenFocused := False;
  FIsInitializing := True;
  EditMask := CLSTimeEditMaskHM;
{$IFNDEF LCLQt}
  if csDesigning in ComponentState then
{$ENDIF}
    UpdateText;
end;

destructor TLSCustomTimeEdit.Destroy;
begin
  FreeAndNil(FUpDown);
  inherited Destroy;
end;

procedure TLSCustomTimeEdit.EditingDone;
begin
  inherited;
  if Focused then
    CheckMinMaxValue;
end;

procedure TLSCustomTimeEdit.Clear;
begin
  if FDefaultNow then
    SetValue(Now)
  else
    SetValue(CLSConstsNullTime);
end;

procedure TLSCustomTimeEdit.UpdateText;
var
  VSelStart: Integer;
begin
  FIsUpdating := True;
  if FValue <> CLSConstsNullTime then
  begin
    case FDisplayFormat of
      dfHM: Text := FormatDateTime(CLSTimeEditDisplayFrmtHM, FValue);
      dfHMS: Text := FormatDateTime(CLSTimeEditDisplayFrmtHMS, FValue);
    end;
  end
  else
  begin
    VSelStart := GetSelStart;
    Text := '';
    SetSelStart(VSelStart);
  end;
end;

procedure TLSCustomTimeEdit.CheckMinMaxValue;
begin
  if (FMinValue <> CLSConstsNullTime) and (FValue < FMinValue) then
    FValue := FMinValue;
  if (FMaxValue <> CLSConstsNullTime) and (FValue > FMaxValue) then
    FValue := FMaxValue;
  UpdateText;
end;

procedure TLSCustomTimeEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if not Assigned(AParent) then
    Exit;
  FUpDown.SetParent(AParent);
  CheckUpDownVisible;
  DoUpDownPosition;
end;

procedure TLSCustomTimeEdit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if (AComponent = FUpDown) and (AOperation = opRemove) then
    FUpDown := nil;
end;

procedure TLSCustomTimeEdit.KeyPress(var AKey: Char);
begin
  inherited;
  if not (AKey in [DefaultFormatSettings.TimeSeparator, '0'..'9',
    #0..#31{$IFDEF LCLQt}, #127{$ENDIF}]) then
    AKey := #0;
end;

procedure TLSCustomTimeEdit.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  if AShift = [] then
    case AKey of
      VK_UP: DoUpDownClick(Self, btNext);
      VK_DOWN: DoUpDownClick(Self, btPrev)
    else
      DoNextPriorKeyDown(Self, AKey);
    end;
  inherited;
end;

procedure TLSCustomTimeEdit.WMSetFocus(var AMessage: TLMSetFocus);
begin
  if FUpDownOnlyWhenFocused then
    FUpDown.Show;
  inherited;
end;

procedure TLSCustomTimeEdit.WMKillFocus(var AMessage: TLMKillFocus);
begin
  if FUpDownOnlyWhenFocused then
    FUpDown.Hide;
  inherited;
end;

procedure TLSCustomTimeEdit.CMTextChanged(var AMessage: TLMessage);
begin
  inherited;
  if FIsInitializing then
  begin
    FIsInitializing := False;
    Exit;
  end;
  if FIsUpdating then
  begin
    FIsUpdating := False;
    Exit;
  end;
  if not TryStrToTime(Text, FValue, DefaultFormatSettings.TimeSeparator) then
    FValue := CLSConstsNullTime;
end;

procedure TLSCustomTimeEdit.CheckUpDownVisible;
begin
  FUpDown.Visible := Visible and FUpDown.Visible;
end;

procedure TLSCustomTimeEdit.SetUpDownOnlyWhenFocused(const AValue: Boolean);
begin
  if AValue <> FUpDownOnlyWhenFocused then
  begin
    FUpDownOnlyWhenFocused := AValue;
    CheckUpDownVisible;
  end;
end;

procedure TLSCustomTimeEdit.SetDisplayFormat(
  const AValue: TLSTimeEditDisplayFormat);
begin
  if AValue <> FDisplayFormat then
  begin
    FDisplayFormat := AValue;
    case FDisplayFormat of
      dfHM: EditMask := CLSTimeEditMaskHM;
      dfHMS: EditMask := CLSTimeEditMaskHMS;
    end;
    UpdateText;
  end;
end;

procedure TLSCustomTimeEdit.SetMaxValue(const AValue: TDateTime);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomTimeEdit.SetMinValue(const AValue: TDateTime);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomTimeEdit.SetValue(const AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    CheckMinMaxValue;
  end;
end;

procedure TLSCustomTimeEdit.Loaded;
begin
  inherited Loaded;
  CheckUpDownVisible;
  DoUpDownPosition;
  if FUpDownOnlyWhenFocused then
    FUpDown.Hide;
  if FDefaultNow then
    SetValue(Now);
end;

procedure TLSCustomTimeEdit.DoChangeValue(const AValue: SmallInt);
var
  VText: ShortString;
  I, VTextLength, VSelStart, VHour, VMinute, VSecond: Integer;

  procedure _IncHour;
  begin
    VHour += AValue;
    if VHour > 23 then
      VHour := 0
    else
    if VHour < 0 then
      VHour := 23;
  end;

  procedure _IncMinute;
  begin
    VMinute += AValue;
    if VMinute > 59 then
      VMinute := 0
    else
    if VMinute < 0 then
      VMinute := 59
    else
      Exit;
    _IncHour;
  end;

  procedure _IncSecond;
  begin
    VSecond += AValue;
    if VSecond > 59 then
      VSecond := 0
    else
    if VSecond < 0 then
      VSecond := 59
    else
      Exit;
    _IncMinute;
  end;

begin
  VText := Text;
  VTextLength := Length(VText);
  for I := 1 to Length(VText) do
    if VText[I] = ' ' then
      VText[I] := '0';
  VHour := StrToInt(VText[1] + VText[2]);
  if VTextLength > 4 then
    VMinute := StrToInt(VText[4] + VText[5])
  else
    VMinute := 0;
  if VTextLength > 7 then
    VSecond := StrToInt(VText[7] + VText[8])
  else
    VSecond := 0;
  VSelStart := GetSelStart;
  if VSelStart < 3 then
    _IncHour
  else
  if VSelStart < 6 then
    _IncMinute
  else
    _IncSecond;
  SetValue(EncodeTime(VHour, VMinute, VSecond, 0));
  SetSelStart(VSelStart);
end;

procedure TLSCustomTimeEdit.DoNextPriorKeyDown(ASender: TObject;
  var AKey: Word);
begin
  if ReadOnly then
    Exit;
  case AKey of
    VK_PRIOR:
      begin
        DoChangeValue(10);
        if Assigned(FOnUpClick) then
          FOnUpClick(ASender);
      end;
    VK_NEXT:
      begin
        DoChangeValue(-10);
        if Assigned(FOnDownClick) then
          FOnDownClick(ASender);
      end;
  end;
end;

procedure TLSCustomTimeEdit.DoUpDownClick(ASender: TObject; AButton: TUDBtnType
  );
begin
  if ReadOnly then
    Exit;
  case AButton of
    btNext:
      begin
        DoChangeValue(1);
        if Assigned(FOnUpClick) then
          FOnUpClick(ASender);
      end;
    btPrev:
      begin
        DoChangeValue(-1);
        if Assigned(FOnDownClick) then
          FOnDownClick(ASender);
      end;
  end;
end;

procedure TLSCustomTimeEdit.DoUpDownPosition;
begin
  FUpDown.AnchorToCompanion(akLeft, 0, Self);
end;

function TLSCustomTimeEdit.DoMouseWheel(AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(AShift, AWheelDelta, AMousePos);
  DoChangeValue(AWheelDelta div 120);
end;

procedure TLSCustomTimeEdit.SetEnabled(AValue: Boolean);
begin
  inherited;
  FUpDown.SetEnabled(AValue);
end;

procedure TLSCustomTimeEdit.SetVisible(AValue: Boolean);
begin
  inherited;
  CheckUpDownVisible;
end;

procedure TLSCustomTimeEdit.SetName(const AValue: TComponentName);
begin
  inherited SetName(AValue);
  FUpDown.SetName(AValue + LSEDITUPDOWNPREFIX);
end;

{ TLSTopPanel }

constructor TLSTopPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alTop;
  BevelOuter := bvLowered;
  Font.Bold := True;
  Font.Color := clTeal;
  Height := 20;
  GradientStart := clWhite;
  GradientStop := $00EADA99;
  SetSubComponent(True);
  ControlStyle := ControlStyle + [csSetCaption];
  Caption := 'Your caption';
end;

{ TLSCustomExpandPanel }

constructor TLSCustomExpandPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTopPanel := TLSTopPanel.Create(Self);
  FButton := TImage.Create(FTopPanel);
  FExpandPic := TPicture.Create;
  FCollapsePic := TPicture.Create;
  FTopPanel.FreeNotification(Self);
  FButton.FreeNotification(FTopPanel);
  FTopPanel.Parent := Self;
  FButton.Parent := FTopPanel;
  FButton.SetSubComponent(True);
  FButton.Align := alRight;
  FButton.BorderSpacing.Right := 2;
  FButton.Width := 20;
  FButton.OnClick := @DoButtonClick;
  FExpandPic.OnChange := @DoChangePic;
  FCollapsePic.OnChange := @DoChangePic;
  FExpandPic.LoadFromLazarusResource(LSEXPANDPANELEXPANDLRSNAME);
  FCollapsePic.LoadFromLazarusResource(LSEXPANDPANELCOLLAPSELRSNAME);
  FButton.Picture.Assign(FCollapsePic);
  FButton.AutoSize := True;
  FButton.Cursor := crHandPoint;
  FButton.Center := True;
  FExpand := True;
  GradientStart := $00EADA99;
  GradientStop := clWhite;
end;

destructor TLSCustomExpandPanel.Destroy;
begin
  FExpandPic.Free;
  FCollapsePic.Free;
  FreeAndNil(FButton);
  FreeAndNil(FTopPanel);
  inherited Destroy;
end;

procedure TLSCustomExpandPanel.SetExpand(const AValue: Boolean);
begin
  if AValue <> FExpand then
  begin
    FExpand := AValue;
    UpdateSize(FExpand);
  end;
end;

procedure TLSCustomExpandPanel.UpdateSize(const AExpanded: Boolean);
begin
  if AExpanded then
    Height := FOldTopPanelHeight
  else
  begin
    FOldTopPanelHeight := Height;
    Height := FTopPanel.BoundsRect.Bottom + BorderWidth + BevelWidth + 1;
  end;
  DoChangePic(Self);
end;

function TLSCustomExpandPanel.IsStoredOldTopPanelHeight: Boolean;
begin
  Result := not FExpand;
end;

{$HINTS OFF}
procedure TLSCustomExpandPanel.DoChangePic(ASender: TObject);
begin
  if FExpand then
    FButton.Picture.Assign(FCollapsePic)
  else
    FButton.Picture.Assign(FExpandPic);
end;
{$HINTS ON}

procedure TLSCustomExpandPanel.DoButtonClick(ASender: TObject);
begin
  SetExpand(not FExpand);
  if Assigned(FOnButtonClick) then
    FOnButtonClick(ASender);
end;

class function TLSCustomExpandPanel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 220;
  Result.CY := 170;
end;

procedure TLSCustomExpandPanel.SetName(const AValue: TComponentName);
begin
  inherited SetName(AValue);
  FTopPanel.SetName(AValue + LSEXPANDPANELTOPPANELPREFIX);
  FButton.Name := AValue + LSEDITBUTTONPREFIX;
end;

procedure TLSCustomExpandPanel.WMSize(var AMessage: TLMSize);
begin
  inherited;
  if FExpand then
    FOldTopPanelHeight := AMessage.Height
  else
    Height := FTopPanel.BoundsRect.Bottom + BorderWidth + BevelWidth;
end;

initialization
  {$I LSDateEditBtn.lrs}
  {$I LSExpandPanel.lrs}

end.
