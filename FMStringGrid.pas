{
FM SOFT IW Tiny Component pack version 1.0.0
================================================

-These components are provided 'as-is', without warranty of any kind, expressed
or implied.
In no event will the author be held liable for any damages arising from the use
of this software.

-Distribution and modifications are allowed as long as the credits to the
original author are preserved and explicitly stated.

-These Components are free to use for personal or commercial purpose as long as
they are not included or distributed as a part of another development tool library.
Contact the author if you want to distribute them as a part of another IW related library.

Developed by:
Farshad Mohajeri
FM SOFT CO. LTD. (c) 2009
Contact: farshad@fmsoft.net
}

unit FMStringGrid;

interface

uses
  SysUtils, Graphics, Classes, IWGrids, Grids;

type
  TIWFMStringGrid = class(TIWGrid)
  private
    fFixedColor : TColor;
    fFixedRows,
    fFixedCols : Integer;
    fDefaultColWidth,
    fDefaultRowHeight : Integer;
    fOnSelectCell: TSelectCellEvent;
    FCellsClickable,
    FNumbered : Boolean;
    SelectedLine  : Integer;
    procedure SetColors;
    procedure SetCellSizes(sRow:Integer=0; sCol:Integer=0);
  protected
    procedure SetExtraProperties;
    function GetColCount:Integer;
    procedure SetColCount(Value:Integer);
    function GetRowCount:Integer;
    procedure SetRowCount(Value:Integer);
    procedure SetFixedRows(Value:Integer);
    procedure SetFixedColor(Value:TColor);
    procedure SetFixedCols(Value:Integer);
    function GetColor:TColor;
    procedure SetColor(Value:TColor);
    procedure SetDefaultColWidth(Value:Integer);
    procedure SetDefaultRowHeight(Value:Integer);
    procedure DoRender; override;
    function GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    function GetColWidths(Index: Longint): Integer;
    procedure SetColWidths(Index: Longint; Value: Integer);
    function GetRowHeights(Index: Longint): Integer;
    procedure SetRowHeights(Index: Longint; Value: Integer);
    procedure SetNumbered(Value:Boolean);
    procedure SetCellFonts;

    procedure DoCellClick(const ARow: Integer; const AColumn: Integer); override;
    procedure SetCellsClickable(Value: Boolean);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property RowHeights[Index: Longint]: Integer read GetRowHeights write SetRowHeights;
  published
    property FixedColor: TColor read FFixedColor write SetFixedColor;
    property FixedCols: Integer read FFixedCols write SetFixedCols;
    property FixedRows: Integer read FFixedRows write SetFixedRows;
    property RowCount:Integer read GetRowCount write SetRowCount;
    property ColumnCount:Integer read GetColCount write SetColCount;
    property Color:TColor read GetColor write SetColor;
    property DefaultColWidth:Integer read fDefaultColWidth write SetDefaultColWidth;
    property DefaultRowHeight:Integer read fDefaultRowHeight write SetDefaultRowHeight;
    // events
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    property Numbered:Boolean read FNumbered write SetNumbered;
    property CellsClickable:Boolean read FCellsClickable write SetCellsClickable;
  end;

procedure Register;

implementation

uses
  IWGridCommon;


constructor TIWFMStringGrid.Create(AOwner:TComponent);
begin
  inherited;
  ColumnCount:=3;
  RowCount:=3;
  FixedRows:=1;
  FixedCols:=1;
  FixedColor:=clBtnFace;
  Color:=clWindow;
  DefaultColWidth:=64;
  DefaultRowHeight:=24;
  UseSize:=False;
  BorderStyle:=tfDefault;
  SelectedLine:=-1;
end;

destructor TIWFMStringGrid.Destroy;
begin
  inherited;
end;

procedure TIWFMStringGrid.DoCellClick(const ARow: Integer; const AColumn: Integer);
var
  CanSelect: Boolean;
begin
  CanSelect:=False;
  SelectedLine:=ARow;
  if Assigned(fOnSelectCell) then
    fOnSelectCell(Self, AColumn, ARow, CanSelect);
end;

procedure TIWFMStringGrid.SetCellsClickable(Value: Boolean);
var
  I,J : Integer;
begin
  FCellsClickable:=Value;
  for I := 0 to RowCount- 1 do
    for J := 0 to ColumnCount- 1 do
      if (J>=FFixedCols) then
        Cell[I, J].Clickable:=FCellsClickable;
end;

function TIWFMStringGrid.GetCells(ACol, ARow: Integer): string;
begin
  Result:=Cell[ARow, ACol].Text;
end;

procedure TIWFMStringGrid.SetCells(ACol, ARow: Integer; const Value: string);
begin
  Cell[ARow, ACol].Text:=Value;
end;

function TIWFMStringGrid.GetColWidths(Index: Longint): Integer;
begin
  Result:=StrToIntDef(Cell[0, Index].Width,0);
end;

procedure TIWFMStringGrid.SetColWidths(Index: Longint; Value: Integer);
var
  I : Integer;
begin
  for I := 0 to RowCount - 1 do
    Cell[I, Index].Width:=IntToStr(Value);
end;

function TIWFMStringGrid.GetRowHeights(Index: Longint): Integer;
begin
  Result:=StrToIntDef(Cell[Index, 0].Height,0);
end;

procedure TIWFMStringGrid.SetRowHeights(Index: Longint; Value: Integer);
var
  I : Integer;
begin
  for I := 0 to ColumnCount - 1 do
    Cell[Index, I].Height:=IntToStr(Value);
end;

procedure TIWFMStringGrid.SetDefaultColWidth(Value:Integer);
begin
  fDefaultColWidth:=Value;
  SetCellSizes;
end;

procedure TIWFMStringGrid.SetDefaultRowHeight(Value:Integer);
begin
  fDefaultRowHeight:=Value;
  SetCellSizes;
end;

function TIWFMStringGrid.GetColor:TColor;
begin
  Result:=BGColor;
end;

procedure TIWFMStringGrid.SetColor(Value:TColor);
begin
  BGColor:=Value;
end;

procedure TIWFMStringGrid.SetNumbered(Value:Boolean);
begin
  FNumbered:=Value;
  SetCellSizes;
end;

procedure TIWFMStringGrid.SetCellFonts;
var
  I,J : Integer;
begin
  for I := 0 to RowCount- 1 do
    for J := 0 to ColumnCount- 1 do
      Cell[I, J].Font:=Font;
end;

procedure TIWFMStringGrid.SetCellSizes(sRow:Integer=0; sCol:Integer=0);
var
  I,J : Integer;
begin
  for I := sRow to RowCount- 1 do
    for J := sCol to ColumnCount- 1 do
    begin
      if sRow=0 then
        Cell[I, J].Width:=IntToStr(fDefaultColWidth)
      else
        Cell[I, J].Width:=Cell[0, J].Width;

      if sCol=0 then
        Cell[I, J].Height:=IntToStr(fDefaultRowHeight)
      else
        Cell[I, J].Height:=Cell[I, 0].Height;

      Cell[I,J].Clickable:=FCellsClickable;

      if (J=0) and (I>0) then
        if (fFixedCols>0) then
        begin
          Cell[I,0].Clickable:=True;
          Cell[I,0].Alignment:=taCenter;
          if FNumbered then
            Cell[I,0].Text:=IntToStr(I)
          else if Cell[I,0].Text='' then
            Cell[I,0].Text:='-';
        end;
    end;
end;

procedure TIWFMStringGrid.SetColors;
var
  I,J : Integer;
begin
  if SelectedLine>=RowCount then SelectedLine:=-1;

  for I := 0 to RowCount- 1 do
    for J := 0 to ColumnCount- 1 do
      if (I<fFixedRows) or (J<fFixedCols) then
        Cell[I, J].BGColor:=fFixedColor
      else if (I=SelectedLine) and (I>=fFixedRows) then
        Cell[I, J].BGColor:=clSkyBlue
      else
        Cell[I, J].BGColor:=Color;
end;

procedure TIWFMStringGrid.DoRender;
begin
  SetColors;
  inherited;
end;

procedure TIWFMStringGrid.SetFixedColor(Value:TColor);
begin
  fFixedColor:=Value;
end;

procedure TIWFMStringGrid.SetFixedRows(Value:Integer);
begin
  fFixedRows:=Value;
  if fFixedRows>RowCount then fFixedRows:=RowCount;
end;

procedure TIWFMStringGrid.SetFixedCols(Value:Integer);
begin
  fFixedCols:=Value;
  if fFixedCols>ColumnCount then fFixedCols:=ColumnCount;
end;

function TIWFMStringGrid.GetRowCount:Integer;
begin
  Result:=inherited RowCount;
end;

procedure TIWFMStringGrid.SetRowCount(Value:Integer);
var
  pRow  : Integer;
begin
  pRow:=inherited RowCount;
  inherited RowCount:=Value;
  SetCellSizes(pRow, 0);
end;

function TIWFMStringGrid.GetColCount:Integer;
begin
  Result:=inherited ColumnCount;
end;

procedure TIWFMStringGrid.SetColCount(Value:Integer);
var
  pCol  : Integer;
begin
  pCol:=ColumnCount;
  inherited ColumnCount:=Value;
  SetCellSizes(0, pCol);
end;

procedure TIWFMStringGrid.SetExtraProperties;
begin
  ;
end;

procedure Register;
begin
  RegisterComponents('FM IW', [TIWFMStringGrid]);
end;

end.
