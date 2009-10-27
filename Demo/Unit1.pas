unit Unit1;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, IWCompMemo,
  Controls, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWControl,
  IWCompListbox, FMComboBox, IWCompButton, IWCompTabControl, Forms,
  IWVCLBaseContainer, IWContainer, IWHTMLContainer, IWHTML40Container, IWRegion,
  FMTabControl, IWCompEdit, IWGrids, FMStringGrid;

type
  TIWForm2 = class(TIWAppForm)
    IWFMComboBox1: TIWFMComboBox;
    IWMemo1: TIWMemo;
    IWButton1: TIWButton;
    IWFMTabControl1: TIWFMTabControl;
    IWFMTabControl1Page0: TIWTabPage;
    IWFMTabControl1Page1: TIWTabPage;
    IWFMTabControl1Page2: TIWTabPage;
    IWEdit1: TIWEdit;
    IWEdit2: TIWEdit;
    IWEdit3: TIWEdit;
    IWFMStringGrid1: TIWFMStringGrid;
    procedure IWFMComboBox1AsyncChange(Sender: TObject;
      EventParams: TStringList);
    procedure IWButton1AsyncClick(Sender: TObject; EventParams: TStringList);
    procedure IWAppFormCreate(Sender: TObject);
    procedure IWFMStringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  public
  end;

implementation

{$R *.dfm}


procedure TIWForm2.IWAppFormCreate(Sender: TObject);
begin
  IWFMStringGrid1.Cells[1,1]:='Cell 1,1';
end;

procedure TIWForm2.IWButton1AsyncClick(Sender: TObject;
  EventParams: TStringList);
begin
  IWMemo1.Lines.Add(IWFMComboBox1.Text);
end;

procedure TIWForm2.IWFMComboBox1AsyncChange(Sender: TObject;
  EventParams: TStringList);
begin
  IWMemo1.Lines.Add(IWFMComboBox1.Text);
end;

procedure TIWForm2.IWFMStringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  IWFMStringGrid1.Cells[ACol, ARow]:='Clicked';
end;

initialization
  TIWForm2.SetAsMainForm;

end.
