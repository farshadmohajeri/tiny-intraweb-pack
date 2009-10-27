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

unit FMTabControl;

interface

uses
  Graphics, SysUtils, Classes, Controls, ComCtrls, IWCompTabControl,
  IWVCLComponent, IWControl, IWContainerBorderOptions, IWRenderContext, IWHTMLTag,
  IWServer, IWColor, IWBaseRenderContext, IWRegion, InGlobal,
  FMCommonImages, IWXMLTag, IWAppForm;


type
  TIWFMTabControl = class(TIWTabControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    function RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag; override;
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag; override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

type
  TIWRegionHack  = class(TIWCustomRegion)
  end;

function RenderAsyncRegion(Region: TIWCustomRegion; AContext: TIWBaseHTMLComponentContext):TIWXMLTag; forward;

constructor TIWFMTabControl.Create(AOwner:TComponent);
begin
  inherited;
  if AOwner is TIWAppForm then
    Color:=TIWAppForm(AOwner).BGColor;

  Width:=400;
  Height:=300;
  InActiveTabFont.Color:=$505050;
  ActiveTabFont.Color:=clBlack;

  ActiveTabColor:=clBtnFace;
  InActiveTabColor:=clBtnFace;
  ActiveTabFont.Size:=8;
  InActiveTabFont.Size:=8;
end;

function RenderAsyncRegion(Region: TIWCustomRegion; AContext: TIWBaseHTMLComponentContext):TIWXMLTag;
begin
  with TIWRegionHack(Region) do
  begin
    Result := TIWXMLTag.CreateTag('control');
    try
      Result.AddStringParam('id', HTMLName);
      Result.AddStringParam('type', 'IWREGION');

      AddAsyncStyle(Result, 'background-color:' + ColorToRGBString(Color) + ';');
      AddAsyncStyle(Result, 'color:' + ColorToRGBString(BorderOptions.Color) + ';');
      AddAsyncStyle(Result, 'height:' + IntToStr(Height - BorderOptions.NumericWidth * 2) + 'px');
      AddAsyncStyle(Result, 'width:' + IntToStr(Width - BorderOptions.NumericWidth * 2) + 'px');
      if ClipRegion then begin
        AddAsyncStyle(Result, 'clip: rect(0px,' + IntToStr(Width) + 'px,' +IntToStr(Height)+'px,0px)');
        if HorzScrollBar.Visible then
          AddAsyncStyle(Result, 'overflow-x: auto')
        else
          AddAsyncStyle(Result, 'overflow-x: hidden');
        if VertScrollBar.Visible then
          AddAsyncStyle(Result, 'overflow-y: auto')
        else
          AddAsyncStyle(Result, 'overflow-y: hidden')
      end else begin
        AddAsyncStyle(Result, 'clip:rect(auto auto auto auto)');
       //don't rerender width and height via common properties
        AddAsyncStyle(Result, 'overflow-x: visible');
        AddAsyncStyle(Result, 'overflow-y: visible');
      end;
      ControlImplementation.RenderAsyncCommonProperties(AContext, Result, [acpEnabled..acpAlignment] - [acpZIndex, acpWidth, acpHeight]);
    except
      FreeAndNil(Result);
    end;
  end;
end;


function TIWFMTabControl.RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag;
begin
  Result:=RenderAsyncRegion(Self, AContext);
end;

function TIWFMTabControl.RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
var
  iURL, sStyle : string;
  I,J : Integer;
  TagA,TagB : TIWHTMLTag;
begin
  Result:=inherited RenderHTML(AContext);
  iURL:=OwnerForm.WebApplication.InternalURLBase;

  for I := 0 to Result.Contents.Count - 1 do
    if Result.Contents[I] is TIWHTMLTag then
    begin
      TagA:=TIWHTMLTag(Result.Contents[I]);
      sStyle:=TagA.Params.Values['style'];

      if I=0 then
      begin
        sStyle:=sStyle+'position:absolute;left:0;top:0;'+
                'background-color:'+ColorToRGBString(Color)+';';
        sStyle:=StringReplace(sStyle, 'z-index:1;', 'z-index:3;', []);
        sStyle:=StringReplace(sStyle, 'height:20;', 'height:21;', []);
        TagA.Params.Values['style']:=sStyle;
      end;

      for J:= 0 to TagA.Contents.Count - 1 do
        if TagA.Contents[J] is TIWHTMLTag then
        begin
          TagB:=TIWHTMLTag(TagA.Contents[J]);
          sStyle:=TagB.Params.Values['style'];
          if Pos('_ACTIVE',TagB.Params.Values['id'])>0 then
          begin
            sStyle:=StringReplace(sStyle, 'height: 20px;', '', []);    // we dont need height, height is set by fontsize+padding
            sStyle:=StringReplace(sStyle, 'padding: 3px;', '', []);
            sStyle:=sStyle+'border: 1px solid #0F0F0F;position: relative;float:left;'+
                    'padding: 4px 7px;'+
                    'border-bottom-color: '+ColorToRGBString(ActiveTabColor)+';';
            TagB.Params.Values['style']:=sStyle;
          end
          else if Pos('_INACTIVE', TagB.Params.Values['id'])>0 then
          begin
            sStyle:=StringReplace(sStyle, 'height: 20px;', '', []);
            sStyle:=StringReplace(sStyle, 'padding: 3px;', '', []);
            sStyle:=sStyle+'border: 1px solid #0F0F0F;position: relative;float:left;top: 1px;'+
                    'padding: 2px 7px;background-image: url('+iURL+'/gfx/shade_inactive.png);';
            TagB.Params.Values['style']:=sStyle;
          end
        end;
    end;
end;

procedure Register;
begin
  RegisterComponents('FM IW', [TIWFMTabControl]);
end;

end.
