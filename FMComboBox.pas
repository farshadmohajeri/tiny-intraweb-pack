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
Please contact the author if you want to distribute them as a part of another IW related library.

Developed by:
Farshad Mohajeri
FM SOFT CO. LTD. (c) 2009
Contact: farshad@fmsoft.net
}

unit FMComboBox;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, IWControl, Graphics,
  IWCompListbox, IWHTMLTag, IWRenderContext, IWServer, SWSystem, IWTypes,
  IWScriptEvents, IWXMLTag, IWBaseInterfaces, IWHTML40Interfaces, IWColor,
  FMConst, IWFont;

type

  TIWFMComboBox  = class;

  TIWFMComboBoxEditor = class(TIWCustomControl, IIWInputControl, IIWSubmitControl, IIWInputControl40)
  private
    FOwnerCombo  : TIWFMComboBox;
    FSubmitParam : String;
  protected
    procedure InitControl; override;
    procedure SetValue(const AValue: string); virtual;
    procedure Submit(const AValue: string); override;
  public
    function GetSubmitParam : String;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;  override;
    function RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag; override;
  end;

  TIWFMComboBox  = class(TIWComboBox)
  private
    FStyle  : TComboBoxStyle;
    FEmbeddedEditor  : TIWFMComboBoxEditor;
    FMaxLength  : Integer;
    FText : string;
  protected
    function GetStyle:TComboBoxStyle;
    procedure SetStyle(Value: TComboBoxStyle);
    function GetText:TCaption; reintroduce;
    procedure SetText(Value: TCaption); reintroduce;
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure HookEvents(AContext: TIWPageContext40; AScriptEvents: TIWScriptEvents); override;
    function GetColor:TColor;
    procedure SetColor(Value:TColor);
    procedure SetMaxLength(Value: Integer);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;  override;
    function RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag; override;
    procedure Clear; override;
  published
    property Style : TComboBoxStyle read GetStyle write SetStyle;
    property Text:TCaption read GetText write SetText;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Color:TColor read GetColor write SetColor;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
  end;

procedure Register;

implementation

{$R FMComboBox.res}

constructor TIWFMComboBox.Create(AOwner:TComponent);
begin
  inherited;
  NoSelectionText:='';
  if not (csDesigning in ComponentState) then
  begin
    FEmbeddedEditor:=TIWFMComboBoxEditor.Create(AOwner);
    FEmbeddedEditor.SetBounds(Left, Top, Width, Height);
    FEmbeddedEditor.FOwnerCombo:=Self;
    FEmbeddedEditor.ZIndex:=ZIndex+1;
    SetName(Name);
  end;
end;

destructor TIWFMComboBox.Destroy;
begin
  if Owner=nil then FEmbeddedEditor.Free;
  inherited Destroy;
end;

function TIWFMComboBox.GetStyle:TComboBoxStyle;
begin
  Result:=FStyle;
end;

procedure TIWFMComboBox.SetStyle(Value: TComboBoxStyle);
begin
  FStyle:=Value;
end;

function TIWFMComboBox.GetText: TCaption;
begin
  Result:='';
  if FStyle=csDropDown then
  begin
    if FEmbeddedEditor<>nil then
      Result:=FEmbeddedEditor.Text
    else
      Result:=Ftext;
  end
  else if ItemIndex>=0 then Result:=Items[ItemIndex];
end;

procedure TIWFMComboBox.SetText(Value: TCaption);
var
  I : Integer;
begin
  if Value<>FText then
  begin
    FText:=Value;
    if FEmbeddedEditor<>nil then
      FEmbeddedEditor.Text:=FText;

    if Items.Count>0 then
      for I := 0 to Items.Count - 1 do
        if SameText(FText, Items[I]) then
        begin
          inherited SetItemIndex(I);
          Exit;
        end;
  end;
end;

procedure TIWFMComboBox.HookEvents(AContext: TIWPageContext40; AScriptEvents: TIWScriptEvents);
begin
  AScriptEvents.HookEvent('OnChange', 'FMComboBox_ApplyChange("'+HTMLName+'")');
  inherited;
end;

function TIWFMComboBox.RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag;
begin
  if Width<20 then Width:=20;
  if Height<12 then Height:=12;

  Result:=inherited RenderAsync(AContext);
  if Result<>nil then
  begin
    AddAsyncStyle(Result, 'background-color:' + ColorToRGBString(Color) + ';');
  end;
end;

function TIWFMComboBox.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
begin
  if Width<20 then Width:=20;
  if Height<12 then Height:=12;

  Result:=inherited RenderHTML(AContext);

  (AContext as TIWComponent40Context).AddScriptFile('/js/FMComboBox.js'+FMIW_JSVersion);
end;

function TIWFMComboBox.GetColor:TColor;
begin
  Result:=BGColor;
end;

procedure TIWFMComboBox.SetColor(Value:TColor);
begin
  if BGColor<>Value then
  begin
    BGColor:=Value;
    if FEmbeddedEditor<>nil then FEmbeddedEditor.Invalidate;
  end;
end;

function TIWFMComboBox.GetItemIndex: Integer;
begin
  Result:=inherited ItemIndex;
end;

procedure TIWFMComboBox.SetItemIndex(AIndex: Integer);
begin
  if AIndex<>ItemIndex then
    if AIndex<Items.Count then
    begin
      inherited SetItemIndex(AIndex);
      if ItemIndex>-1 then
        Text:=Items[ItemIndex]
      else
        Text:='';
    end;
end;

procedure TIWFMComboBox.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if FEmbeddedEditor<>nil then
    FEmbeddedEditor.SetName(Value+'EDITOR');
end;

procedure TIWFMComboBox.SetMaxLength(Value: Integer);
begin
  if FMaxLength<>Value then
  begin
    FMaxLength:=Value;
    if FEmbeddedEditor<>nil then FEmbeddedEditor.Invalidate;
  end;
end;

procedure TIWFMComboBox.Clear;
begin
  if FEmbeddedEditor<>nil then FEmbeddedEditor.Text:='';
  inherited Clear;
end;

procedure TIWFMComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FEmbeddedEditor<>nil then
    FEmbeddedEditor.SetParent(AParent);
end;

procedure TIWFMComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FEmbeddedEditor<>nil then
    FEmbeddedEditor.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TIWFMComboBoxEditor.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
begin
  Result := TIWHTMLTag.CreateTag('INPUT');
  try
    with Result do
    begin
      AddStringParam('name', HTMLName);
      AddStringParam('id', HTMLName);
      AddStringParam('style', 'position:absolute;'+
                      'left:'+IntToStr(Left+2)+'px;'+
                      'top:'+IntToStr(Top+2)+'px;'+
                      'width:'+IntToStr(Width-21)+'px;'+
                      iif(AContext.WebApplication.Browser in [brNetscape6, brGecko],
                      'height:'+IntToStr(Height-5)+'px;',
                      'height:'+IntToStr(Height-7)+'px;')+
                      'border: none;'+
                      'background-color:' + ColorToRGBString(FOwnerCombo.Color) + ';'+
                      'visibility:'+iif(FOwnerCombo.Style=csDropDown, 'visible;', 'hidden;')+
                      FOwnerCombo.Font.FontToStringStyle(AContext.Browser) );

      AddStringParam('value', TextToHTML(Text));
      if FOwnerCombo.MaxLength = 0 then begin
        AddIntegerParam('MAXLENGTH', maxInt);
      end else begin
        AddIntegerParam('MAXLENGTH', FOwnerCombo.MaxLength);
      end;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function TIWFMComboBoxEditor.RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag;
begin
  Result := TIWXMLTag.CreateTag('control');
  try
    Result.AddStringParam('id', HTMLName);
    Result.AddStringParam('type', 'IWEDIT'); // what is its role?

    AddAsyncStyle(Result, 'width:'+ IntToStr(Width-21)+'px');
    AddAsyncStyle(Result, 'left:'+ IntToStr(Left+2)+'px');
    AddAsyncStyle(Result, 'top:'+ IntToStr(Top+2)+'px');
    AddAsyncStyle(Result, 'height:'+
                      iif(AContext.WebApplication.Browser in [brNetscape6, brGecko],
                      IntToStr(Height-5)+'px;', IntToStr(Height-7)+'px;'));
    AddAsyncStyle(Result, 'background-color:' + ColorToRGBString(FOwnerCombo.Color) + ';');
    AddAsyncStyle(Result, 'visibility:'+iif(FOwnerCombo.Style=csDropDown, 'visible;', 'hidden;'));
    AddAsyncStyle(Result, FOwnerCombo.Font.FontToStringStyle(AContext.Browser));
    RenderAsyncPropertyAsString('MaxLength', Result, IntToStr(FOwnerCombo.MaxLength), true);
    RenderAsyncPropertyAsString('Text', Result, Text, false);
//    RenderAsyncCommonProperties(AContext, Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TIWFMComboBoxEditor.Submit(const AValue: string);
begin
  FSubmitParam := AValue;
end;

procedure TIWFMComboBoxEditor.SetValue(const AValue: string);
begin
  if Text <> AValue then
  begin
    Text := AValue;
    DoRefreshControl := False;
  end;
end;

procedure TIWFMComboBoxEditor.InitControl;
begin
  inherited;
  FNeedsFormTag := True;
  FCanReceiveFocus := True;
  StyleRenderOptions.RenderSize:=False;
  StyleRenderOptions.RenderPosition:=False;
  StyleRenderOptions.RenderFont:=False;
end;

function TIWFMComboBoxEditor.GetSubmitParam: String;
begin
  Result := FSubmitParam;
end;

procedure Register;
begin
  RegisterComponents('FM IW', [TIWFMComboBox]);
end;

initialization
  TIWServer.AddInternalFile('IW_JS_FMCOMBOBOX', '/js/FMComboBox.js_'+FMIW_JSVersion);
end.
