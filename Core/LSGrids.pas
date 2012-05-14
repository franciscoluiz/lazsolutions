(*
  LazSolutions, Grids unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSGrids;

{$I lazsolutions.inc}

interface

uses
  LSCommonIntf, LSMessages, LSGridsColumnsManagerFrm, LSExceptionUtils,
  LResources, Grids, Classes, Controls, Graphics, SysUtils, StdCtrls, LMessages,
  LCLIntf, FPJSON, JSONParser;

const
  CLSDefaultGridIndicatorWidth = 12;

type

  TLSGridType = (gtNormal, gtJSON);

  { TLSCustomStringGrid }

  TLSCustomStringGrid = class(TCustomStringGrid, ILSAboutComponent)
  private
    FBookmark: Integer;
    FSelectedRow: TJSONObject;
    FSelectedRows: TJSONArray;
    FColumnsManager: Boolean;
    FMenuStyleCaptionColor: TColor;
    FPNGSortAsc: TPortableNetworkGraphic;
    FPNGSortDesc: TPortableNetworkGraphic;
    FPNGColsManager: TPortableNetworkGraphic;
    FSortColIndex: Integer;
    FGridType: TLSGridType;
    FIndicatorWidth: Integer;
    FMenuStyleDirection: TGradientDirection;
    FMenuStyleStop: TColor;
    FMouseCol: Integer;
    FMouseRow: Integer;
    FMenuStyleStart: TColor;
    FSortable: Boolean;
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    procedure SetGridType(const AValue: TLSGridType);
    procedure SetIndicatorWidth(const AValue: Integer);
    procedure SetColumnsManager(const AValue: Boolean);
    procedure SetMenuStyleCaptionColor(const AValue: TColor);
    procedure SetMenuStyleStart(const AValue: TColor);
    procedure SetMenuStyleStop(const AValue: TColor);
    procedure SetSortable(const AValue: Boolean);
  protected
    procedure Loaded; override;
    procedure DoAutoSizeColumns; virtual;
    procedure DoShowColumnsManager; virtual;
    procedure ColRowMoved(AIsColumn: Boolean;
      AFromIndex, AToIndex: Integer); override;
    procedure HeaderClick(AIsColumn: Boolean; AIndex: Integer); override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState); override;
    procedure WMMouseMove(var AMessage: TLMMouseMove); message LM_MOUSEMOVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Find(const AText: string; const ACaseSensitive: Boolean = False;
      const AFindNext: Boolean = True): Boolean;
    procedure LoadFromJSONStream(var AStream: TStream;
      const AShowErrorMsg: Boolean = True;
      const AAutoSizeColumns: Boolean = True);
    procedure LoadFromJSONString(const AString: string;
      const AShowErrorMsg: Boolean = True;
      const AAutoSizeColumns: Boolean = True);
    procedure LoadFromJSONFile(const AFileName: TFileName;
      const AShowErrorMsg: Boolean = True;
      const AAutoSizeColumns: Boolean = True);
    procedure SaveToJSONStream(var AStream: TStream;
      const ASaveAllAsString: Boolean = False);
    procedure SaveToJSONString(var AString: string;
      const ASaveAllAsString: Boolean = False);
    procedure SaveToJSONFile(const AFileName: TFileName;
      const ASaveAllAsString: Boolean = False);
    procedure EmptyGrid; virtual;
    procedure SetBookmark;
    procedure GetBookmark;
    procedure SelectFirstRow;
    procedure SelectLastRow;
    function SelectedRow: TJSONObject;
    function SelectedRows: TJSONArray;
    property About: string read GetAbout write SetAbout stored False;
    property IndicatorWidth: Integer read FIndicatorWidth
      write SetIndicatorWidth default CLSDefaultGridIndicatorWidth;
    property GridType: TLSGridType read FGridType
      write SetGridType default gtJSON;
    property ColumnsManager: Boolean read FColumnsManager
      write SetColumnsManager default True;
    property MenuStyleDirection: TGradientDirection read FMenuStyleDirection
      write FMenuStyleDirection default gdVertical;
    property MenuStyleStart: TColor read FMenuStyleStart
      write SetMenuStyleStart default clNone;
    property MenuStyleStop: TColor read FMenuStyleStop
      write SetMenuStyleStop default clNone;
    property MenuStyleCaptionColor: TColor read FMenuStyleCaptionColor
      write SetMenuStyleCaptionColor default clNone;
    property Sortable: Boolean read FSortable write SetSortable default True;
  end;

  { TLSStringGrid }

  TLSStringGrid = class(TLSCustomStringGrid)
  published
    property About stored False;
    property Align;
    property AlternateColor;
    property AltColorStartNormal;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderColor;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property ColCount default 2;
{$IFDEF LSNEWLAZARUS}
    property ColumnClickSorts;
{$ENDIF}
    property Columns;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property FixedCols;
    property FixedHotColor;
    property FixedRows;
    property FocusColor;
    property FocusRectVisible;
    property Flat;
    property Font;
    property GridLineColor;
    property GridLineStyle;
    property GridLineWidth;
    property GridType default gtJSON;
    property HeaderHotZones;
    property HeaderPushZones;
    property IndicatorWidth default CLSDefaultGridIndicatorWidth;
    property ColumnsManager default True;
    property MenuStyleDirection default gdVertical;
    property MenuStyleStart default clNone;
    property MenuStyleStop default clNone;
    property MenuStyleCaptionColor default clNone;
    property MouseWheelOption;
    property Options default [goColMoving, goColSizing, goDblClickAutoSize,
      goFixedHorzLine, goFixedVertLine, goHorzLine, goRowSelect, goSmoothScroll,
      goVertLine];
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount default 2;
    property SaveOptions;
    property SelectedColor default clSkyBlue;
    property Sortable default True;
    property SortOrder;
    property ScrollBars default ssBoth;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property OnBeforeSelection;
    property OnChangeBounds;
    property OnCheckboxToggled;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnDrawCell;
    property OnEditButtonClick; deprecated;
{$IFDEF LSNEWLAZARUS}
    property OnButtonClick;
{$ENDIF}
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCheckboxState;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnHeaderSizing;
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
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnResize;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetCheckboxState;
    property OnSetEditText;
    property OnShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnUserCheckboxBitmap;
    property OnUTF8KeyPress;
    property OnValidateEntry;
  end;

{ Load JSON stream to grid. }
procedure LSLoadJSONStreamToGrid(var AGrid: TCustomStringGrid;
  var AStream: TStream; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
{ Load JSON string to grid. }
procedure LSLoadJSONStringToGrid(var AGrid: TCustomStringGrid;
  const AString: string; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
{ Load JSON file to grid. }
procedure LSLoadJSONFileToGrid(var AGrid: TCustomStringGrid;
  const AFileName: TFileName; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
{ Save grid to JSON stream. }
procedure LSSaveJSONStreamToGrid(var AGrid: TCustomStringGrid;
  var AStream: TStream; const ASaveAllAsString: Boolean);
{ Save grid to JSON string. }
procedure LSSaveJSONStringToGrid(var AGrid: TCustomStringGrid;
  var AString: string; const ASaveAllAsString: Boolean);
{ Save grid to JSON file. }
procedure LSSaveJSONFileToGrid(var AGrid: TCustomStringGrid;
  const AFileName: TFileName; const ASaveAllAsString: Boolean);
{ Find item in a StringGrid. }
function LSFindItemInStringGrid(var AGrid: TCustomStringGrid;
  const AText: string; const ACaseSensitive: Boolean = False;
  const AFindNext: Boolean = True): Boolean;

var
  LS_JSON_UNKNOWN_STR: ShortString = '[UNKNOWN]';
  LS_JSON_NULL_STR: ShortString = '[NULL]';
  LS_JSON_ARRAY_STR: ShortString = '[ARRAY]';
  LS_JSON_OBJECT_STR: ShortString = '[OBJECT]';
  LS_JSON_BOOL_FALSE_STR: ShortString = 'FALSE';
  LS_JSON_BOOL_TRUE_STR: ShortString = 'TRUE';
  LS_JSON_FORMAT_FLOAT_STR: ShortString = '%f';
  LS_JSON_FORMAT_INT_STR: ShortString = '%d';

implementation

procedure LSLoadJSONStreamToGrid(var AGrid: TCustomStringGrid;
  var AStream: TStream; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
var
  VIsObject: Boolean;
  VJSONCols: TJSONObject;
  VJSONParser: TJSONParser;
  VJSONRows, VRecord: TJSONData;
  I, J, VFixedCols, VFixedRows: Integer;
begin
  VRecord := nil;
  VJSONRows := nil;
  VJSONParser := TJSONParser.Create(AStream);
  try
    VJSONRows := VJSONParser.Parse;
    if not Assigned(VJSONRows) then
    begin
      if AShowErrorMsg then
        LSShowJSONEmptyDatabaseError(AGrid.ClassName)
      else
        Exit;
    end;
    if VJSONRows.JSONType <> jtArray then
    begin
      if AShowErrorMsg then
        LSShowJSONIsNotArrayError(AGrid.ClassName, VJSONRows.ClassName)
      else
        Exit;
    end;
    if VJSONRows.Count < 1 then
    begin
      if AShowErrorMsg then
        LSShowJSONEmptyArrayError(AGrid.ClassName)
      else
        Exit;
    end;
    VJSONCols := TJSONObject(VJSONRows.Items[0]);
    VIsObject := VJSONCols.JSONType = jtObject;
    if VIsObject and (VJSONCols.Count < 1) then
    begin
      if AShowErrorMsg then
        LSShowJSONEmptyObjectError(AGrid.ClassName)
      else
        Exit;
    end;
    VFixedCols := AGrid.FixedCols;
    VFixedRows := AGrid.FixedRows;
    AGrid.BeginUpdate;
    try
      if not AGrid.Columns.Enabled then
        AGrid.ColCount := VFixedCols + VJSONCols.Count;
      AGrid.RowCount := VFixedRows + VJSONRows.Count;
      for I := 0 to Pred(VJSONRows.Count) do
      begin
        VJSONCols := TJSONObject(VJSONRows.Items[I]);
        for J := 0 to Pred(VJSONCols.Count) do
        begin
          if Pred(AGrid.ColCount - AGrid.FixedCols) < J then
            Continue;
          if (I = 0) and VIsObject then
            AGrid.Cols[VFixedCols + J].Text := VJSONCols.Names[J];
          VRecord := VJSONCols.Items[J];
          case VRecord.JSONType of
            jtUnknown: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              LS_JSON_UNKNOWN_STR;
            jtNumber:
              begin
                if VRecord is TJSONFloatNumber then
                  AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
                    Format(LS_JSON_FORMAT_FLOAT_STR, [VRecord.AsFloat])
                else
                  AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
                    Format(LS_JSON_FORMAT_INT_STR, [VRecord.AsInt64]);
              end;
            jtString: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              VRecord.AsString;
            jtBoolean: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              BoolToStr(VRecord.AsBoolean, LS_JSON_BOOL_TRUE_STR,
                LS_JSON_BOOL_FALSE_STR);
            jtNull: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              LS_JSON_NULL_STR;
            jtArray: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              LS_JSON_ARRAY_STR;
            jtObject: AGrid.Cells[J + VFixedCols, I + VFixedRows] :=
              LS_JSON_OBJECT_STR;
          end;
        end;
      end;
      if AAutoSizeColumns then
        for I := 1 to Pred(AGrid.ColCount) do
          AGrid.AutoSizeColumn(I);
    finally
      AGrid.EndUpdate;
    end;
  finally
    VJSONRows.Free;
    VJSONParser.Free;
  end;
end;

procedure LSLoadJSONStringToGrid(var AGrid: TCustomStringGrid;
  const AString: string; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
var
  VStream: TStringStream;
begin
  VStream := TStringStream.Create(AString);
  try
    LSLoadJSONStreamToGrid(AGrid, TStream(VStream), AShowErrorMsg,
      AAutoSizeColumns);
  finally
    VStream.Free;
  end;
end;

procedure LSLoadJSONFileToGrid(var AGrid: TCustomStringGrid;
  const AFileName: TFileName; const AShowErrorMsg: Boolean;
  const AAutoSizeColumns: Boolean);
var
  VStream: TFileStream;
begin
  VStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LSLoadJSONStreamToGrid(AGrid, TStream(VStream), AShowErrorMsg,
      AAutoSizeColumns);
  finally
    VStream.Free;
  end;
end;

procedure LSSaveJSONStreamToGrid(var AGrid: TCustomStringGrid;
  var AStream: TStream; const ASaveAllAsString: Boolean);
var
  VIsObject: Boolean;
  VInt64Value: Int64;
  VFloatValue: TJSONFloat;
  VJSONObjectData: TJSONObject = nil;
  I, J, VLength, VRowCount: Integer;
  VJSON, VObjectName, VCellValue: string;
  VJSONArrayData, VJSONArray: TJSONArray;
begin
  VRowCount := AGrid.RowCount;
  VIsObject := (VRowCount > 0) and (Trim(AGrid.Rows[0].Text) <> '');
  VJSONArray := TJSONArray.Create;
  try
    for J := AGrid.FixedRows to Pred(AGrid.RowCount) do
    begin
      if VIsObject then
        VJSONObjectData := TJSONObject.Create
      else
        VJSONArrayData := TJSONArray.Create;
      for I := AGrid.FixedCols to Pred(AGrid.ColCount) do
      begin
        VObjectName := AGrid.Cells[I, 0];
        VCellValue := AGrid.Cells[I, J];
        if ASaveAllAsString then
        begin
          if VIsObject then
            VJSONObjectData.Add(VObjectName, VCellValue)
          else
            VJSONArrayData.Add(VCellValue);
        end
        else
        begin
          if SameText(VCellValue, LS_JSON_UNKNOWN_STR) or SameText(VCellValue,
            LS_JSON_NULL_STR) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, TJSONNull.Create)
            else
              VJSONArrayData.Add(TJSONNull.Create);
          end
          else
          if SameText(VCellValue, LS_JSON_ARRAY_STR) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, TJSONArray.Create)
            else
              VJSONArrayData.Add(TJSONArray.Create);
          end
          else
          if SameText(VCellValue, LS_JSON_OBJECT_STR) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, TJSONObject.Create)
            else
              VJSONArrayData.Add(TJSONObject.Create);
          end
          else
          if SameText(VCellValue, LS_JSON_BOOL_FALSE_STR) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, False)
            else
              VJSONArrayData.Add(False);
          end
          else
          if SameText(VCellValue, LS_JSON_BOOL_TRUE_STR) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, True)
            else
              VJSONArrayData.Add(True);
          end
          else
          if TryStrToInt64(VCellValue, VInt64Value) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, VInt64Value)
            else
              VJSONArrayData.Add(VInt64Value);
          end
          else
          if TryStrToFloat(VCellValue, VFloatValue) then
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, VFloatValue)
            else
              VJSONArrayData.Add(VFloatValue);
          end
          else
          begin
            if VIsObject then
              VJSONObjectData.Add(VObjectName, VCellValue)
            else
              VJSONArrayData.Add(VCellValue);
          end;
        end;
      end;
      if VIsObject then
        VJSONArray.Add(VJSONObjectData)
      else
        VJSONArray.Add(VJSONArrayData);
    end;
    VJSON := VJSONArray.AsJSON;
    VLength := Length(VJSON);
    if VLength > 0 then
      AStream.Write(Pointer(VJSON)^, VLength);
  finally
    VJSONArray.Free;
  end;
end;

procedure LSSaveJSONStringToGrid(var AGrid: TCustomStringGrid;
  var AString: string; const ASaveAllAsString: Boolean);
var
  VStream: TStringStream;
begin
  VStream := TStringStream.Create('');
  try
    LSSaveJSONStreamToGrid(AGrid, TStream(VStream), ASaveAllAsString);
    AString := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

procedure LSSaveJSONFileToGrid(var AGrid: TCustomStringGrid;
  const AFileName: TFileName; const ASaveAllAsString: Boolean);
var
  VFile: TFileStream;
begin
  VFile := TFileStream.Create(AFileName, fmCreate);
  try
    LSSaveJSONStreamToGrid(AGrid, TStream(VFile), ASaveAllAsString);
  finally
    VFile.Free;
  end;
end;

function LSFindItemInStringGrid(var AGrid: TCustomStringGrid;
  const AText: string; const ACaseSensitive: Boolean;
  const AFindNext: Boolean): Boolean;
var
  VGridRect: TGridRect;
  VTargetText, VCellText: string;
  I, X, Y, VCurX, VCurY, VGridWidth, VGridHeight: Integer;
begin
  Result := False;
  if AFindNext then
  begin
    VCurY := AGrid.Selection.Top;
    VCurX := AGrid.Selection.Left + 1;
  end
  else
  begin
    VCurY := 0;
    VCurX := 0;
    VGridRect.Left := 0;
    VGridRect.Right := 0;
    VGridRect.Top := 0;
    VGridRect.Bottom := 0;
    AGrid.Selection := VGridRect;
  end;
  VGridWidth := AGrid.ColCount;
  VGridHeight := AGrid.RowCount;
  Y := VCurY;
  X := VCurX;
  if ACaseSensitive then
    VTargetText := AText
  else
    VTargetText := AnsiLowerCase(AText);
  while Y < VGridHeight do
  begin
    while X < VGridWidth do
    begin
      if ACaseSensitive then
        VCellText := AGrid.Cells[X, Y]
      else
        VCellText := AnsiLowerCase(AGrid.Cells[X, Y]);
      I := Pos(VTargetText, VCellText);
      if I > 0 then
      begin
        VGridRect.Left := X;
        VGridRect.Right := X;
        VGridRect.Top := Y;
        VGridRect.Bottom := Y;
        AGrid.Selection := VGridRect;
        Result := True;
        Exit;
      end;
      Inc(X);
    end;
    Inc(Y);
    X := AGrid.FixedCols;
  end;
  if AFindNext {and not Result} then
  begin
    VGridRect.Left := 0;
    VGridRect.Right := 0;
    VGridRect.Top := 0;
    VGridRect.Bottom := 0;
    AGrid.Selection := VGridRect;
  end;
end;

{ TLSCustomStringGrid }

constructor TLSCustomStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPNGSortAsc := TPortableNetworkGraphic.Create;
  FPNGSortDesc := TPortableNetworkGraphic.Create;
  FPNGColsManager := TPortableNetworkGraphic.Create;
  FPNGSortAsc.LoadFromLazarusResource('LSGridSortAsc');
  FPNGSortDesc.LoadFromLazarusResource('LSGridSortDesc');
  FPNGColsManager.LoadFromLazarusResource('LSGridColumnsManager');
  FColumnsManager := True;
  FIndicatorWidth := CLSDefaultGridIndicatorWidth;
  FGridType := gtJSON;
  FMouseCol := -1;
  FMouseRow := -1;
  FMenuStyleDirection := gdVertical;
  FMenuStyleStart := clNone;
  FMenuStyleStop := clNone;
  FMenuStyleCaptionColor := clNone;
  Options := Options + [goColMoving, goColSizing, goDblClickAutoSize,
    goRowSelect] - [goRangeSelect];
  SelectedColor := clSkyBlue;
  ScrollBars := ssBoth;
  FSortable := True;
  FSortColIndex := -1;
  EmptyGrid;
end;

destructor TLSCustomStringGrid.Destroy;
begin
  FPNGSortAsc.Free;
  FPNGSortDesc.Free;
  FPNGColsManager.Free;
  FreeAndNil(FSelectedRow);
  FreeAndNil(FSelectedRows);
  inherited Destroy;
end;

function TLSCustomStringGrid.Find(const AText: string;
  const ACaseSensitive: Boolean; const AFindNext: Boolean): Boolean;
begin
  Result := LSFindItemInStringGrid(TCustomStringGrid(Self), AText,
    ACaseSensitive, AFindNext);
end;

procedure TLSCustomStringGrid.LoadFromJSONStream(var AStream: TStream;
  const AShowErrorMsg: Boolean; const AAutoSizeColumns: Boolean);
begin
  EmptyGrid;
  LSLoadJSONStreamToGrid(TCustomStringGrid(Self), AStream, AShowErrorMsg,
    AAutoSizeColumns);
  FGridType := gtJSON;
end;

procedure TLSCustomStringGrid.LoadFromJSONString(const AString: string;
  const AShowErrorMsg: Boolean; const AAutoSizeColumns: Boolean);
begin
  EmptyGrid;
  LSLoadJSONStringToGrid(TCustomStringGrid(Self), AString, AShowErrorMsg,
    AAutoSizeColumns);
  FGridType := gtJSON;
end;

procedure TLSCustomStringGrid.LoadFromJSONFile(const AFileName: TFileName;
  const AShowErrorMsg: Boolean; const AAutoSizeColumns: Boolean);
begin
  EmptyGrid;
  LSLoadJSONFileToGrid(TCustomStringGrid(Self), AFileName, AShowErrorMsg,
    AAutoSizeColumns);
  FGridType := gtJSON;
end;

procedure TLSCustomStringGrid.SaveToJSONStream(var AStream: TStream;
  const ASaveAllAsString: Boolean);
begin
  LSSaveJSONStreamToGrid(TCustomStringGrid(Self), AStream, ASaveAllAsString);
end;

procedure TLSCustomStringGrid.SaveToJSONString(var AString: string;
  const ASaveAllAsString: Boolean);
begin
  LSSaveJSONStringToGrid(TCustomStringGrid(Self), AString, ASaveAllAsString);
end;

procedure TLSCustomStringGrid.SaveToJSONFile(const AFileName: TFileName;
  const ASaveAllAsString: Boolean);
begin
  LSSaveJSONFileToGrid(TCustomStringGrid(Self), AFileName, ASaveAllAsString);
end;

procedure TLSCustomStringGrid.EmptyGrid;
var
  I: Integer;
begin
  BeginUpdate;
  try
    case FGridType of
      gtNormal:
        begin
          ColCount := 5;
          RowCount := 5;
          FixedCols := 1;
          FixedRows := 1;
          ColWidths[0] := DefaultColWidth;
        end;
      gtJSON:
        begin
          if not Columns.Enabled then
          begin
            ColCount := 1 + FixedCols;
            ColWidths[0] := FIndicatorWidth;
            for I := 1 to Pred(ColCount) do
              ColWidths[I] := DefaultColWidth;
          end;
          RowCount := 1 + FixedRows;
        end;
    end;
    Clean;
  finally
    EndUpdate;
  end;
end;

procedure TLSCustomStringGrid.SetBookmark;
begin
  FBookmark := Row;
end;

{$HINTS OFF}
procedure TLSCustomStringGrid.GetBookmark;
var
  VRect: TRect;
begin
  SetRect(VRect, -1, -1, -1, -1);
  Selection := TGridRect(VRect);
  Row := FBookmark;
end;
{$HINTS ON}

procedure TLSCustomStringGrid.SelectFirstRow;
begin
  Row := 0;
end;

procedure TLSCustomStringGrid.SelectLastRow;
begin
  Row := RowCount;
end;

function TLSCustomStringGrid.SelectedRow: TJSONObject;
var
  I: Integer;
begin
  if not Assigned(FSelectedRow) then
    FSelectedRow := TJSONObject.Create;
  FSelectedRow.Clear;
  for I := 1 to Pred(ColCount) do
    FSelectedRow.Add(Cols[I][0], Rows[Row][I]);
  if (FSelectedRow.Count > 0) and (FSelectedRow.Names[0] = '') and
    (FSelectedRow.Items[0].AsString = '') then
    FSelectedRow.Clear;
  Result := FSelectedRow;
end;

function TLSCustomStringGrid.SelectedRows: TJSONArray;
var
  I, J: Integer;
  VJSONObject: TJSONObject;
begin
  if not Assigned(FSelectedRows) then
    FSelectedRows := TJSONArray.Create;
  FSelectedRows.Clear;
  for I := Selection.Top to Selection.Bottom do
  begin
    VJSONObject := TJSONObject.Create;
    for J := 1 to Pred(ColCount) do
      VJSONObject.Add(Cols[J][0], Rows[I][J]);
    FSelectedRows.Add(VJSONObject);
  end;
  if Assigned(VJSONObject) and (VJSONObject.Count > 0) and
    (VJSONObject.Names[0] = '') and (VJSONObject.Items[0].AsString = '') then
    FSelectedRows.Clear;
  Result := FSelectedRows;
end;

procedure TLSCustomStringGrid.SetMenuStyleStart(const AValue: TColor);
begin
  if AValue <> FMenuStyleStart then
  begin
    FMenuStyleStart := AValue;
    if FMenuStyleStop <> clNone then
      DoubleBuffered := True;
  end;
end;

procedure TLSCustomStringGrid.SetMenuStyleStop(const AValue: TColor);
begin
  if AValue <> FMenuStyleStop then
  begin
    FMenuStyleStop := AValue;
    if FMenuStyleStart <> clNone then
      DoubleBuffered := True;
  end;
end;

procedure TLSCustomStringGrid.SetMenuStyleCaptionColor(const AValue: TColor);
begin
  if AValue <> FMenuStyleCaptionColor then
  begin
    FMenuStyleCaptionColor := AValue;
    if FMenuStyleCaptionColor <> clNone then
      DoubleBuffered := True;
  end;
end;

procedure TLSCustomStringGrid.SetIndicatorWidth(const AValue: Integer);
begin
  if AValue <> FIndicatorWidth then
  begin
    FIndicatorWidth := AValue;
    ColWidths[0] := AValue;
  end;
end;

procedure TLSCustomStringGrid.SetColumnsManager(const AValue: Boolean);
begin
  if AValue <> FColumnsManager then
  begin
    FColumnsManager := AValue;
    if (FixedCols < 1) and (FixedRows < 1) then
      FColumnsManager := False
    else
      if Columns.Count = 0 then
        DoAutoSizeColumns;
    Repaint;
  end;
end;

procedure TLSCustomStringGrid.SetGridType(const AValue: TLSGridType);
begin
  if AValue <> FGridType then
  begin
    FGridType := AValue;
    EmptyGrid;
  end;
end;

function TLSCustomStringGrid.GetAbout: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomStringGrid.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

procedure TLSCustomStringGrid.SetSortable(const AValue: Boolean);
begin
  if AValue <> FSortable then
  begin
    FSortable := AValue;
    if not AValue then
    begin
      FSortColIndex := -1;
      Repaint;
    end;
  end;
end;

procedure TLSCustomStringGrid.Loaded;
begin
  if FColumnsManager and (Columns.Count = 0) then
    DoAutoSizeColumns;
  inherited;
end;

procedure TLSCustomStringGrid.DoAutoSizeColumns;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 1 to Pred(ColCount) do
      AutoAdjustColumn(I);
  finally
    EndUpdate;
  end;
end;

procedure TLSCustomStringGrid.DoShowColumnsManager;
var
  I: Integer;
  VForm: TLSGridsColumnsManagerForm;
begin
  VForm := TLSGridsColumnsManagerForm.Create(nil);
  try
    VForm.SetBounds(ClientOrigin.X + 2, ClientOrigin.Y + 2, 140, 160);
{$IFDEF LCLQt}
    VForm.OKButton.AutoSize := False;
    VForm.OKButton.Width := 46;
    VForm.CancelButton.AutoSize := False;
    VForm.CancelButton.Width := 74;
{$ENDIF}
    VForm.Caption := SLSGridsColumnsManagerCaption;
    VForm.OKButton.Caption := SLSGridsColumnsManagerOKBtnCaption;
    VForm.CancelButton.Caption := SLSGridsColumnsManagerCancelBtnCaption;
    for I := 1 to Pred(ColCount) do
    begin
      VForm.ColumnsCheckListBox.Items.Add(Cols[I].Strings[0]);
      VForm.ColumnsCheckListBox.Checked[Pred(I)] := ColWidths[I] <> 0;
    end;
    if VForm.ShowModal = mrOK then
      for I := 0 to Pred(VForm.ColumnsCheckListBox.Count) do
      begin
        BeginUpdate;
        try
          if Columns.Count > 0 then
            Columns[I].Visible := VForm.ColumnsCheckListBox.Checked[I]
          else
          begin
            if VForm.ColumnsCheckListBox.Checked[I] then
              AutoAdjustColumn(Succ(I))
            else
              ColWidths[Succ(I)] := 0;
          end;
        finally
          EndUpdate;
        end;
      end;
  finally
    VForm.Free;
  end;
end;

procedure TLSCustomStringGrid.ColRowMoved(AIsColumn: Boolean; AFromIndex,
  AToIndex: Integer);
begin
  if FSortable and AIsColumn and (FSortColIndex <> -1) then
  begin
    FSortColIndex := AToIndex;
    Repaint;
  end;
  inherited;
end;

procedure TLSCustomStringGrid.HeaderClick(AIsColumn: Boolean; AIndex: Integer);
begin
  if FSortable and AIsColumn and (AIndex >= FixedCols) and (RowCount > 2) then
  begin
    if FSortColIndex = AIndex then
    begin
      if SortOrder = soAscending then
        SortOrder := soDescending
      else
        SortOrder := soAscending;
    end
    else
      SortOrder := soAscending;
    if AIsColumn then
      FSortColIndex := AIndex;
    SortColRow(AIsColumn, AIndex);
    Repaint;
  end;
  if FColumnsManager and AIsColumn and (ColCount > 1) and (AIndex = 0) then
    DoShowColumnsManager;
  inherited;
end;

procedure TLSCustomStringGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  X: Integer;
begin
  inherited;
  if (FMenuStyleStart <> clNone) and (FMenuStyleStop <> clNone) and
    (FMouseCol >= FixedCols) and (FMouseRow >= FixedRows) and
    (ACol > Pred(FixedCols)) and (ARow = FMouseRow) then
  begin
    Canvas.Font.Color := FMenuStyleCaptionColor;
    if FMenuStyleStart = FMenuStyleStop then
    begin
      Canvas.Brush.Color := FMenuStyleStart;
      Canvas.FillRect(ARect);
    end
    else
      Canvas.GradientFill(ARect, FMenuStyleStart, FMenuStyleStop,
        FMenuStyleDirection);
    DrawCellGrid(ACol, ARow, ARect, AState);
    DrawTextInCell(ACol, ARow, ARect, AState);
  end;
  if FSortable and (FSortColIndex = ACol) and (ARow = 0) then
  begin
    X := ((ARect.Left + ARect.Right) div 2) - 3;
    case SortOrder of
      soAscending: Canvas.Draw(X, ARect.Top + 1, FPNGSortAsc);
      soDescending: Canvas.Draw(X, ARect.Top + 1, FPNGSortDesc);
    end;
  end;
  if FColumnsManager and (ColCount >= 0) and (ACol = 0) and (RowCount >= 0) and
    (ARow = 0) then
  begin
    X := ((ARect.Left + ARect.Right) div 2) - 4;
    Canvas.Draw(X, ((ARect.Top + ARect.Bottom) div 2) - 2, FPNGColsManager)
  end;
end;

procedure TLSCustomStringGrid.WMMouseMove(var AMessage: TLMMouseMove);
var
  VRect: TRect;
  X, Y: Integer;
begin
  inherited;
  if (FMenuStyleStart = clNone) or (FMenuStyleStop = clNone) then
    Exit;
  X := AMessage.XPos;
  Y := AMessage.YPos;
  MouseToCell(X, Y, FMouseCol, FMouseRow);
  VRect := CellRect(FMouseCol, FMouseRow);
  if (X <= 1) or (Y <= 1) or not PtInRect(VRect, Point(X, Y)) then
  begin
    FMouseCol := -1;
    FMouseRow := -1;
  end;
  Repaint;
end;

initialization
  {$I LSGrigMedia.lrs}

end.

